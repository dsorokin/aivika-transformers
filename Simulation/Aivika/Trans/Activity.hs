
-- |
-- Module     : Simulation.Aivika.Trans.Activity
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It models an activity that can be utilised. The activity is similar to a 'Server'
-- but destined for simulation within 'Net' computation.
module Simulation.Aivika.Trans.Activity
       (-- * Activity
        Activity,
        ActivityInterruption(..),
        newActivity,
        newStateActivity,
        newInterruptibleActivity,
        newInterruptibleStateActivity,
        -- * Processing
        activityNet,
        -- * Activity Properties
        activityInitState,
        activityState,
        activityTotalUtilisationTime,
        activityTotalIdleTime,
        activityUtilisationTime,
        activityIdleTime,
        activityUtilisationFactor,
        activityIdleFactor,
        -- * Summary
        activitySummary,
        -- * Derived Signals for Properties
        activityStateChanged,
        activityStateChanged_,
        activityTotalUtilisationTimeChanged,
        activityTotalUtilisationTimeChanged_,
        activityTotalIdleTimeChanged,
        activityTotalIdleTimeChanged_,
        activityUtilisationTimeChanged,
        activityUtilisationTimeChanged_,
        activityIdleTimeChanged,
        activityIdleTimeChanged_,
        activityUtilisationFactorChanged,
        activityUtilisationFactorChanged_,
        activityIdleFactorChanged,
        activityIdleFactorChanged_,
        -- * Basic Signals
        activityUtilising,
        activityUtilised,
        activityInterrupted,
        -- * Overall Signal
        activityChanged_) where

import Data.Monoid

import Control.Monad
import Control.Monad.Trans
import Control.Arrow

import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.Monad.DES
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Dynamics
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Signal
import Simulation.Aivika.Trans.Resource
import Simulation.Aivika.Trans.Cont
import Simulation.Aivika.Trans.Process
import Simulation.Aivika.Trans.Net
import Simulation.Aivika.Trans.Server
import Simulation.Aivika.Trans.Statistics

import Simulation.Aivika.Activity (ActivityInterruption(..))

-- | Like 'Server' it models an activity that takes @a@ and provides @b@ having state @s@.
-- But unlike the former the activity is destined for simulation within 'Net' computation.
data Activity m s a b =
  Activity { activityInitState :: s,
             -- ^ The initial state of the activity.
             activityStateRef :: Ref m s,
             -- ^ The current state of the activity.
             activityProcess :: s -> a -> Process m (s, b),
             -- ^ Provide @b@ by specified @a@.
             activityProcessInterruptible :: Bool,
             -- ^ Whether the process is interruptible.
             activityTotalUtilisationTimeRef :: Ref m Double,
             -- ^ The counted total time of utilising the activity.
             activityTotalIdleTimeRef :: Ref m Double,
             -- ^ The counted total time, when the activity was idle.
             activityUtilisationTimeRef :: Ref m (SamplingStats Double),
             -- ^ The statistics for the utilisation time.
             activityIdleTimeRef :: Ref m (SamplingStats Double),
             -- ^ The statistics for the time, when the activity was idle.
             activityUtilisingSource :: SignalSource m a,
             -- ^ A signal raised when starting to utilise the activity.
             activityUtilisedSource :: SignalSource m (a, b),
             -- ^ A signal raised when the activity has been utilised.
             activityInterruptedSource :: SignalSource m (ActivityInterruption a)
             -- ^ A signal raised when the utilisation was interrupted.
           }

-- | Create a new activity that can provide output @b@ by input @a@.
--
-- By default, it is assumed that the activity utilisation cannot be interrupted,
-- because the handling of possible task interruption is rather costly
-- operation.
newActivity :: MonadDES m
               => (a -> Process m b)
               -- ^ provide an output by the specified input
               -> Simulation m (Activity m () a b)
newActivity = newInterruptibleActivity False

-- | Create a new activity that can provide output @b@ by input @a@
-- starting from state @s@.
--
-- By default, it is assumed that the activity utilisation cannot be interrupted,
-- because the handling of possible task interruption is rather costly
-- operation.
newStateActivity :: MonadDES m
                    => (s -> a -> Process m (s, b))
                    -- ^ provide a new state and output by the specified 
                    -- old state and input
                    -> s
                    -- ^ the initial state
                    -> Simulation m (Activity m s a b)
newStateActivity = newInterruptibleStateActivity False

-- | Create a new interruptible activity that can provide output @b@ by input @a@.
newInterruptibleActivity :: MonadDES m
                            => Bool
                            -- ^ whether the activity can be interrupted
                            -> (a -> Process m b)
                            -- ^ provide an output by the specified input
                            -> Simulation m (Activity m () a b)
newInterruptibleActivity interruptible provide =
  flip (newInterruptibleStateActivity interruptible) () $ \s a ->
  do b <- provide a
     return (s, b)

-- | Create a new activity that can provide output @b@ by input @a@
-- starting from state @s@.
newInterruptibleStateActivity :: MonadDES m
                                 => Bool
                                 -- ^ whether the activity can be interrupted
                                 -> (s -> a -> Process m (s, b))
                                 -- ^ provide a new state and output by the specified 
                                 -- old state and input
                                 -> s
                                 -- ^ the initial state
                                 -> Simulation m (Activity m s a b)
newInterruptibleStateActivity interruptible provide state =
  do r0 <- newRef state
     r1 <- newRef 0
     r2 <- newRef 0
     r3 <- newRef emptySamplingStats
     r4 <- newRef emptySamplingStats
     s1 <- newSignalSource
     s2 <- newSignalSource
     s3 <- newSignalSource
     return Activity { activityInitState = state,
                       activityStateRef = r0,
                       activityProcess = provide,
                       activityProcessInterruptible = interruptible,
                       activityTotalUtilisationTimeRef = r1,
                       activityTotalIdleTimeRef = r2,
                       activityUtilisationTimeRef = r3,
                       activityIdleTimeRef = r4,
                       activityUtilisingSource = s1,
                       activityUtilisedSource = s2,
                       activityInterruptedSource = s3 }

-- | Return a network computation for the specified activity.
--
-- The computation updates the internal state of the activity. The usual case is when 
-- the computation is applied only once in a chain of data processing. Otherwise; 
-- every time the computation is used, the state of the activity changes. Sometimes 
-- it can be indeed useful if you want to aggregate the statistics for different 
-- activities simultaneously, but it would be more preferable to avoid this.
--
-- If you connect different activity computations returned by this function in a chain 
-- with help of '>>>' or other category combinator then this chain will act as one 
-- whole, where the first activity will take a new task only after the last activity 
-- finishes its current task and requests for the next one from the previous activity 
-- in the chain. This is not always that thing you might need.
activityNet :: MonadDES m => Activity m s a b -> Net m a b
activityNet act = Net $ loop (activityInitState act) Nothing
  where
    loop s r a =
      do t0 <- liftDynamics time
         liftEvent $
           do case r of
                Nothing -> return ()
                Just t' ->
                  do modifyRef (activityTotalIdleTimeRef act) (+ (t0 - t'))
                     modifyRef (activityIdleTimeRef act) $
                       addSamplingStats (t0 - t')
              triggerSignal (activityUtilisingSource act) a
         -- utilise the activity
         (s', b) <- if activityProcessInterruptible act
                    then activityProcessInterrupting act s a
                    else activityProcess act s a
         t1 <- liftDynamics time
         liftEvent $
           do writeRef (activityStateRef act) $! s'
              modifyRef (activityTotalUtilisationTimeRef act) (+ (t1 - t0))
              modifyRef (activityUtilisationTimeRef act) $
                addSamplingStats (t1 - t0)
              triggerSignal (activityUtilisedSource act) (a, b)
         return (b, Net $ loop s' (Just t1))

-- | Process the input with ability to handle a possible interruption.
activityProcessInterrupting :: MonadDES m => Activity m s a b -> s -> a -> Process m (s, b)
activityProcessInterrupting act s a =
  do pid <- processId
     t0  <- liftDynamics time
     finallyProcess
       (activityProcess act s a)
       (liftEvent $
        do cancelled <- processCancelled pid
           when cancelled $
             do t1 <- liftDynamics time
                modifyRef (activityTotalUtilisationTimeRef act) (+ (t1 - t0))
                modifyRef (activityUtilisationTimeRef act) $
                  addSamplingStats (t1 - t0)
                let x = ActivityInterruption a t0 t1
                triggerSignal (activityInterruptedSource act) x)

-- | Return the current state of the activity.
--
-- See also 'activityStateChanged' and 'activityStateChanged_'.
activityState :: MonadDES m => Activity m s a b -> Event m s
activityState act =
  Event $ \p -> invokeEvent p $ readRef (activityStateRef act)
  
-- | Signal when the 'activityState' property value has changed.
activityStateChanged :: MonadDES m => Activity m s a b -> Signal m s
activityStateChanged act =
  mapSignalM (const $ activityState act) (activityStateChanged_ act)
  
-- | Signal when the 'activityState' property value has changed.
activityStateChanged_ :: MonadDES m => Activity m s a b -> Signal m ()
activityStateChanged_ act =
  mapSignal (const ()) (activityUtilised act)

-- | Return the counted total time when the activity was utilised.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'activityTotalUtilisationTimeChanged' and 'activityTotalUtilisationTimeChanged_'.
activityTotalUtilisationTime :: MonadDES m => Activity m s a b -> Event m Double
activityTotalUtilisationTime act =
  Event $ \p -> invokeEvent p $ readRef (activityTotalUtilisationTimeRef act)
  
-- | Signal when the 'activityTotalUtilisationTime' property value has changed.
activityTotalUtilisationTimeChanged :: MonadDES m => Activity m s a b -> Signal m Double
activityTotalUtilisationTimeChanged act =
  mapSignalM (const $ activityTotalUtilisationTime act) (activityTotalUtilisationTimeChanged_ act)
  
-- | Signal when the 'activityTotalUtilisationTime' property value has changed.
activityTotalUtilisationTimeChanged_ :: MonadDES m => Activity m s a b -> Signal m ()
activityTotalUtilisationTimeChanged_ act =
  mapSignal (const ()) (activityUtilised act)

-- | Return the counted total time when the activity was idle.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'activityTotalIdleTimeChanged' and 'activityTotalIdleTimeChanged_'.
activityTotalIdleTime :: MonadDES m => Activity m s a b -> Event m Double
activityTotalIdleTime act =
  Event $ \p -> invokeEvent p $ readRef (activityTotalIdleTimeRef act)
  
-- | Signal when the 'activityTotalIdleTime' property value has changed.
activityTotalIdleTimeChanged :: MonadDES m => Activity m s a b -> Signal m Double
activityTotalIdleTimeChanged act =
  mapSignalM (const $ activityTotalIdleTime act) (activityTotalIdleTimeChanged_ act)
  
-- | Signal when the 'activityTotalIdleTime' property value has changed.
activityTotalIdleTimeChanged_ :: MonadDES m => Activity m s a b -> Signal m ()
activityTotalIdleTimeChanged_ act =
  mapSignal (const ()) (activityUtilising act)

-- | Return the statistics for the time when the activity was utilised.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'activityUtilisationTimeChanged' and 'activityUtilisationTimeChanged_'.
activityUtilisationTime :: MonadDES m => Activity m s a b -> Event m (SamplingStats Double)
activityUtilisationTime act =
  Event $ \p -> invokeEvent p $ readRef (activityUtilisationTimeRef act)
  
-- | Signal when the 'activityUtilisationTime' property value has changed.
activityUtilisationTimeChanged :: MonadDES m => Activity m s a b -> Signal m (SamplingStats Double)
activityUtilisationTimeChanged act =
  mapSignalM (const $ activityUtilisationTime act) (activityUtilisationTimeChanged_ act)
  
-- | Signal when the 'activityUtilisationTime' property value has changed.
activityUtilisationTimeChanged_ :: MonadDES m => Activity m s a b -> Signal m ()
activityUtilisationTimeChanged_ act =
  mapSignal (const ()) (activityUtilised act)

-- | Return the statistics for the time when the activity was idle.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'activityIdleTimeChanged' and 'activityIdleTimeChanged_'.
activityIdleTime :: MonadDES m => Activity m s a b -> Event m (SamplingStats Double)
activityIdleTime act =
  Event $ \p -> invokeEvent p $ readRef (activityIdleTimeRef act)
  
-- | Signal when the 'activityIdleTime' property value has changed.
activityIdleTimeChanged :: MonadDES m => Activity m s a b -> Signal m (SamplingStats Double)
activityIdleTimeChanged act =
  mapSignalM (const $ activityIdleTime act) (activityIdleTimeChanged_ act)
  
-- | Signal when the 'activityIdleTime' property value has changed.
activityIdleTimeChanged_ :: MonadDES m => Activity m s a b -> Signal m ()
activityIdleTimeChanged_ act =
  mapSignal (const ()) (activityUtilising act)
  
-- | It returns the factor changing from 0 to 1, which estimates how often
-- the activity was utilised.
--
-- This factor is calculated as
--
-- @
--   totalUtilisationTime \/ (totalUtilisationTime + totalIdleTime)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
--
-- See also 'activityUtilisationFactorChanged' and 'activityUtilisationFactorChanged_'.
activityUtilisationFactor :: MonadDES m => Activity m s a b -> Event m Double
activityUtilisationFactor act =
  Event $ \p ->
  do x1 <- invokeEvent p $ readRef (activityTotalUtilisationTimeRef act)
     x2 <- invokeEvent p $ readRef (activityTotalIdleTimeRef act)
     return (x1 / (x1 + x2))
  
-- | Signal when the 'activityUtilisationFactor' property value has changed.
activityUtilisationFactorChanged :: MonadDES m => Activity m s a b -> Signal m Double
activityUtilisationFactorChanged act =
  mapSignalM (const $ activityUtilisationFactor act) (activityUtilisationFactorChanged_ act)
  
-- | Signal when the 'activityUtilisationFactor' property value has changed.
activityUtilisationFactorChanged_ :: MonadDES m => Activity m s a b -> Signal m ()
activityUtilisationFactorChanged_ act =
  mapSignal (const ()) (activityUtilising act) <>
  mapSignal (const ()) (activityUtilised act)
  
-- | It returns the factor changing from 0 to 1, which estimates how often
-- the activity was idle.
--
-- This factor is calculated as
--
-- @
--   totalIdleTime \/ (totalUtilisationTime + totalIdleTime)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
--
-- See also 'activityIdleFactorChanged' and 'activityIdleFactorChanged_'.
activityIdleFactor :: MonadDES m => Activity m s a b -> Event m Double
activityIdleFactor act =
  Event $ \p ->
  do x1 <- invokeEvent p $ readRef (activityTotalUtilisationTimeRef act)
     x2 <- invokeEvent p $ readRef (activityTotalIdleTimeRef act)
     return (x2 / (x1 + x2))
  
-- | Signal when the 'activityIdleFactor' property value has changed.
activityIdleFactorChanged :: MonadDES m => Activity m s a b -> Signal m Double
activityIdleFactorChanged act =
  mapSignalM (const $ activityIdleFactor act) (activityIdleFactorChanged_ act)
  
-- | Signal when the 'activityIdleFactor' property value has changed.
activityIdleFactorChanged_ :: MonadDES m => Activity m s a b -> Signal m ()
activityIdleFactorChanged_ act =
  mapSignal (const ()) (activityUtilising act) <>
  mapSignal (const ()) (activityUtilised act)

-- | Raised when starting to utilise the activity after a new input task is received.
activityUtilising :: Activity m s a b -> Signal m a
activityUtilising = publishSignal . activityUtilisingSource

-- | Raised when the activity has been utilised after the current task is processed.
activityUtilised :: Activity m s a b -> Signal m (a, b)
activityUtilised = publishSignal . activityUtilisedSource

-- | Raised when the task utilisation by the activity was interrupted.
activityInterrupted :: Activity m s a b -> Signal m (ActivityInterruption a)
activityInterrupted = publishSignal . activityInterruptedSource

-- | Signal whenever any property of the activity changes.
activityChanged_ :: MonadDES m => Activity m s a b -> Signal m ()
activityChanged_ act =
  mapSignal (const ()) (activityUtilising act) <>
  mapSignal (const ()) (activityUtilised act) <>
  mapSignal (const ()) (activityInterrupted act)

-- | Return the summary for the activity with desciption of its
-- properties using the specified indent.
activitySummary :: MonadDES m => Activity m s a b -> Int -> Event m ShowS
activitySummary act indent =
  Event $ \p ->
  do tx1 <- invokeEvent p $ readRef (activityTotalUtilisationTimeRef act)
     tx2 <- invokeEvent p $ readRef (activityTotalIdleTimeRef act)
     let xf1 = tx1 / (tx1 + tx2)
         xf2 = tx2 / (tx1 + tx2)
     xs1 <- invokeEvent p $ readRef (activityUtilisationTimeRef act)
     xs2 <- invokeEvent p $ readRef (activityIdleTimeRef act)
     let tab = replicate indent ' '
     return $
       showString tab .
       showString "total utilisation time = " . shows tx1 .
       showString "\n" .
       showString tab .
       showString "total idle time = " . shows tx2 .
       showString "\n" .
       showString tab .
       showString "utilisation factor (from 0 to 1) = " . shows xf1 .
       showString "\n" .
       showString tab .
       showString "idle factor (from 0 to 1) = " . shows xf2 .
       showString "\n" .
       showString tab .
       showString "utilisation time (locked while awaiting the input):\n\n" .
       samplingStatsSummary xs1 (2 + indent) .
       showString "\n\n" .
       showString tab .
       showString "idle time:\n\n" .
       samplingStatsSummary xs2 (2 + indent)
