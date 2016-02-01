
-- |
-- Module     : Simulation.Aivika.Trans.Activity
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- It models an activity that can be utilised. The activity is similar to a 'Server'
-- but destined for simulation within 'Net' computation.
module Simulation.Aivika.Trans.Activity
       (-- * Activity
        Activity,
        newActivity,
        newStateActivity,
        newPreemptibleActivity,
        newPreemptibleStateActivity,
        -- * Processing
        activityNet,
        -- * Activity Properties
        activityInitState,
        activityState,
        activityTotalUtilisationTime,
        activityTotalIdleTime,
        activityTotalPreemptionTime,
        activityUtilisationTime,
        activityIdleTime,
        activityPreemptionTime,
        activityUtilisationFactor,
        activityIdleFactor,
        activityPreemptionFactor,
        -- * Summary
        activitySummary,
        -- * Derived Signals for Properties
        activityStateChanged,
        activityStateChanged_,
        activityTotalUtilisationTimeChanged,
        activityTotalUtilisationTimeChanged_,
        activityTotalIdleTimeChanged,
        activityTotalIdleTimeChanged_,
        activityTotalPreemptionTimeChanged,
        activityTotalPreemptionTimeChanged_,
        activityUtilisationTimeChanged,
        activityUtilisationTimeChanged_,
        activityIdleTimeChanged,
        activityIdleTimeChanged_,
        activityPreemptionTimeChanged,
        activityPreemptionTimeChanged_,
        activityUtilisationFactorChanged,
        activityUtilisationFactorChanged_,
        activityIdleFactorChanged,
        activityIdleFactorChanged_,
        activityPreemptionFactorChanged,
        activityPreemptionFactorChanged_,
        -- * Basic Signals
        activityUtilising,
        activityUtilised,
        activityPreemptionBeginning,
        activityPreemptionEnding,
        -- * Overall Signal
        activityChanged_) where

import Data.Monoid

import Control.Monad
import Control.Monad.Trans
import Control.Arrow

import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Dynamics
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Signal
import Simulation.Aivika.Trans.Cont
import Simulation.Aivika.Trans.Process
import Simulation.Aivika.Trans.Net
import Simulation.Aivika.Trans.Server
import Simulation.Aivika.Trans.Statistics

-- | Like 'Server' it models an activity that takes @a@ and provides @b@ having state @s@.
-- But unlike the former the activity is destined for simulation within 'Net' computation.
data Activity m s a b =
  Activity { activityInitState :: s,
             -- ^ The initial state of the activity.
             activityStateRef :: Ref m s,
             -- ^ The current state of the activity.
             activityProcess :: s -> a -> Process m (s, b),
             -- ^ Provide @b@ by specified @a@.
             activityProcessPreemptible :: Bool,
             -- ^ Whether the process is preemptible.
             activityTotalUtilisationTimeRef :: Ref m Double,
             -- ^ The counted total time of utilising the activity.
             activityTotalIdleTimeRef :: Ref m Double,
             -- ^ The counted total time when the activity was idle.
             activityTotalPreemptionTimeRef :: Ref m Double,
             -- ^ The counted total time when the activity was preempted. 
             activityUtilisationTimeRef :: Ref m (SamplingStats Double),
             -- ^ The statistics for the utilisation time.
             activityIdleTimeRef :: Ref m (SamplingStats Double),
             -- ^ The statistics for the time when the activity was idle.
             activityPreemptionTimeRef :: Ref m (SamplingStats Double),
             -- ^ The statistics for the time when the activity was preempted.
             activityUtilisingSource :: SignalSource m a,
             -- ^ A signal raised when starting to utilise the activity.
             activityUtilisedSource :: SignalSource m (a, b),
             -- ^ A signal raised when the activity has been utilised.
             activityPreemptionBeginningSource :: SignalSource m a,
             -- ^ A signal raised when the utilisation was preempted.
             activityPreemptionEndingSource :: SignalSource m a
             -- ^ A signal raised when the utilisation was proceeded after it had been preempted earlier.
           }

-- | Create a new activity that can provide output @b@ by input @a@.
--
-- By default, it is assumed that the activity utilisation cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newActivity :: MonadDES m
               => (a -> Process m b)
               -- ^ provide an output by the specified input
               -> Simulation m (Activity m () a b)
{-# INLINABLE newActivity #-}
newActivity = newPreemptibleActivity False

-- | Create a new activity that can provide output @b@ by input @a@
-- starting from state @s@.
--
-- By default, it is assumed that the activity utilisation cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newStateActivity :: MonadDES m
                    => (s -> a -> Process m (s, b))
                    -- ^ provide a new state and output by the specified 
                    -- old state and input
                    -> s
                    -- ^ the initial state
                    -> Simulation m (Activity m s a b)
{-# INLINABLE newStateActivity #-}
newStateActivity = newPreemptibleStateActivity False

-- | Create a new interruptible activity that can provide output @b@ by input @a@.
newPreemptibleActivity :: MonadDES m
                          => Bool
                          -- ^ whether the activity can be preempted
                          -> (a -> Process m b)
                          -- ^ provide an output by the specified input
                          -> Simulation m (Activity m () a b)
{-# INLINABLE newPreemptibleActivity #-}
newPreemptibleActivity preemptible provide =
  flip (newPreemptibleStateActivity preemptible) () $ \s a ->
  do b <- provide a
     return (s, b)

-- | Create a new activity that can provide output @b@ by input @a@
-- starting from state @s@.
newPreemptibleStateActivity :: MonadDES m
                               => Bool
                               -- ^ whether the activity can be preempted
                               -> (s -> a -> Process m (s, b))
                               -- ^ provide a new state and output by the specified 
                               -- old state and input
                               -> s
                               -- ^ the initial state
                               -> Simulation m (Activity m s a b)
{-# INLINABLE newPreemptibleStateActivity #-}
newPreemptibleStateActivity preemptible provide state =
  do r0 <- newRef state
     r1 <- newRef 0
     r2 <- newRef 0
     r3 <- newRef 0
     r4 <- newRef emptySamplingStats
     r5 <- newRef emptySamplingStats
     r6 <- newRef emptySamplingStats
     s1 <- newSignalSource
     s2 <- newSignalSource
     s3 <- newSignalSource
     s4 <- newSignalSource
     return Activity { activityInitState = state,
                       activityStateRef = r0,
                       activityProcess = provide,
                       activityProcessPreemptible = preemptible,
                       activityTotalUtilisationTimeRef = r1,
                       activityTotalIdleTimeRef = r2,
                       activityTotalPreemptionTimeRef = r3,
                       activityUtilisationTimeRef = r4,
                       activityIdleTimeRef = r5,
                       activityPreemptionTimeRef = r6,
                       activityUtilisingSource = s1,
                       activityUtilisedSource = s2,
                       activityPreemptionBeginningSource = s3,
                       activityPreemptionEndingSource = s4 }

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
{-# INLINABLE activityNet #-}
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
         (s', b, dt) <- if activityProcessPreemptible act
                        then activityProcessPreempting act s a
                        else do (s', b) <- activityProcess act s a
                                return (s', b, 0)
         t1 <- liftDynamics time
         liftEvent $
           do writeRef (activityStateRef act) $! s'
              modifyRef (activityTotalUtilisationTimeRef act) (+ (t1 - t0 - dt))
              modifyRef (activityUtilisationTimeRef act) $
                addSamplingStats (t1 - t0 - dt)
              triggerSignal (activityUtilisedSource act) (a, b)
         return (b, Net $ loop s' (Just t1))

-- | Process the input with ability to handle a possible preemption.
activityProcessPreempting :: MonadDES m => Activity m s a b -> s -> a -> Process m (s, b, Double)
{-# INLINABLE activityProcessPreempting #-}
activityProcessPreempting act s a =
  do pid <- processId
     t0  <- liftDynamics time
     rs  <- liftSimulation $ newRef 0
     r0  <- liftSimulation $ newRef t0
     h1  <- liftEvent $
            handleSignal (processPreemptionBeginning pid) $ \() ->
            do t0 <- liftDynamics time
               writeRef r0 t0
               triggerSignal (activityPreemptionBeginningSource act) a
     h2  <- liftEvent $
            handleSignal (processPreemptionEnding pid) $ \() ->
            do t0 <- readRef r0
               t1 <- liftDynamics time
               let dt = t1 - t0
               modifyRef rs (+ dt)
               modifyRef (activityTotalPreemptionTimeRef act) (+ dt)
               modifyRef (activityPreemptionTimeRef act) $
                 addSamplingStats dt
               triggerSignal (activityPreemptionEndingSource act) a 
     let m1 =
           do (s', b) <- activityProcess act s a
              dt <- liftEvent $ readRef rs
              return (s', b, dt)
         m2 =
           liftEvent $
           do disposeEvent h1
              disposeEvent h2
     finallyProcess m1 m2

-- | Return the current state of the activity.
--
-- See also 'activityStateChanged' and 'activityStateChanged_'.
activityState :: MonadDES m => Activity m s a b -> Event m s
{-# INLINABLE activityState #-}
activityState act =
  Event $ \p -> invokeEvent p $ readRef (activityStateRef act)
  
-- | Signal when the 'activityState' property value has changed.
activityStateChanged :: MonadDES m => Activity m s a b -> Signal m s
{-# INLINABLE activityStateChanged #-}
activityStateChanged act =
  mapSignalM (const $ activityState act) (activityStateChanged_ act)
  
-- | Signal when the 'activityState' property value has changed.
activityStateChanged_ :: MonadDES m => Activity m s a b -> Signal m ()
{-# INLINABLE activityStateChanged_ #-}
activityStateChanged_ act =
  mapSignal (const ()) (activityUtilised act)

-- | Return the counted total time when the activity was utilised.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'activityTotalUtilisationTimeChanged' and 'activityTotalUtilisationTimeChanged_'.
activityTotalUtilisationTime :: MonadDES m => Activity m s a b -> Event m Double
{-# INLINABLE activityTotalUtilisationTime #-}
activityTotalUtilisationTime act =
  Event $ \p -> invokeEvent p $ readRef (activityTotalUtilisationTimeRef act)
  
-- | Signal when the 'activityTotalUtilisationTime' property value has changed.
activityTotalUtilisationTimeChanged :: MonadDES m => Activity m s a b -> Signal m Double
{-# INLINABLE activityTotalUtilisationTimeChanged #-}
activityTotalUtilisationTimeChanged act =
  mapSignalM (const $ activityTotalUtilisationTime act) (activityTotalUtilisationTimeChanged_ act)
  
-- | Signal when the 'activityTotalUtilisationTime' property value has changed.
activityTotalUtilisationTimeChanged_ :: MonadDES m => Activity m s a b -> Signal m ()
{-# INLINABLE activityTotalUtilisationTimeChanged_ #-}
activityTotalUtilisationTimeChanged_ act =
  mapSignal (const ()) (activityUtilised act)

-- | Return the counted total time when the activity was idle.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'activityTotalIdleTimeChanged' and 'activityTotalIdleTimeChanged_'.
activityTotalIdleTime :: MonadDES m => Activity m s a b -> Event m Double
{-# INLINABLE activityTotalIdleTime #-}
activityTotalIdleTime act =
  Event $ \p -> invokeEvent p $ readRef (activityTotalIdleTimeRef act)
  
-- | Signal when the 'activityTotalIdleTime' property value has changed.
activityTotalIdleTimeChanged :: MonadDES m => Activity m s a b -> Signal m Double
{-# INLINABLE activityTotalIdleTimeChanged #-}
activityTotalIdleTimeChanged act =
  mapSignalM (const $ activityTotalIdleTime act) (activityTotalIdleTimeChanged_ act)
  
-- | Signal when the 'activityTotalIdleTime' property value has changed.
activityTotalIdleTimeChanged_ :: MonadDES m => Activity m s a b -> Signal m ()
{-# INLINABLE activityTotalIdleTimeChanged_ #-}
activityTotalIdleTimeChanged_ act =
  mapSignal (const ()) (activityUtilising act)

-- | Return the counted total time when the activity was preemted waiting for
-- the further proceeding.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'activityTotalPreemptionTimeChanged' and 'activityTotalPreemptionTimeChanged_'.
activityTotalPreemptionTime :: MonadDES m => Activity m s a b -> Event m Double
{-# INLINABLE activityTotalPreemptionTime #-}
activityTotalPreemptionTime act =
  Event $ \p -> invokeEvent p $ readRef (activityTotalPreemptionTimeRef act)
  
-- | Signal when the 'activityTotalPreemptionTime' property value has changed.
activityTotalPreemptionTimeChanged :: MonadDES m => Activity m s a b -> Signal m Double
{-# INLINABLE activityTotalPreemptionTimeChanged #-}
activityTotalPreemptionTimeChanged act =
  mapSignalM (const $ activityTotalPreemptionTime act) (activityTotalPreemptionTimeChanged_ act)
  
-- | Signal when the 'activityTotalPreemptionTime' property value has changed.
activityTotalPreemptionTimeChanged_ :: MonadDES m => Activity m s a b -> Signal m ()
{-# INLINABLE activityTotalPreemptionTimeChanged_ #-}
activityTotalPreemptionTimeChanged_ act =
  mapSignal (const ()) (activityPreemptionEnding act)

-- | Return the statistics for the time when the activity was utilised.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'activityUtilisationTimeChanged' and 'activityUtilisationTimeChanged_'.
activityUtilisationTime :: MonadDES m => Activity m s a b -> Event m (SamplingStats Double)
{-# INLINABLE activityUtilisationTime #-}
activityUtilisationTime act =
  Event $ \p -> invokeEvent p $ readRef (activityUtilisationTimeRef act)
  
-- | Signal when the 'activityUtilisationTime' property value has changed.
activityUtilisationTimeChanged :: MonadDES m => Activity m s a b -> Signal m (SamplingStats Double)
{-# INLINABLE activityUtilisationTimeChanged #-}
activityUtilisationTimeChanged act =
  mapSignalM (const $ activityUtilisationTime act) (activityUtilisationTimeChanged_ act)
  
-- | Signal when the 'activityUtilisationTime' property value has changed.
activityUtilisationTimeChanged_ :: MonadDES m => Activity m s a b -> Signal m ()
{-# INLINABLE activityUtilisationTimeChanged_ #-}
activityUtilisationTimeChanged_ act =
  mapSignal (const ()) (activityUtilised act)

-- | Return the statistics for the time when the activity was idle.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'activityIdleTimeChanged' and 'activityIdleTimeChanged_'.
activityIdleTime :: MonadDES m => Activity m s a b -> Event m (SamplingStats Double)
{-# INLINABLE activityIdleTime #-}
activityIdleTime act =
  Event $ \p -> invokeEvent p $ readRef (activityIdleTimeRef act)
  
-- | Signal when the 'activityIdleTime' property value has changed.
activityIdleTimeChanged :: MonadDES m => Activity m s a b -> Signal m (SamplingStats Double)
{-# INLINABLE activityIdleTimeChanged #-}
activityIdleTimeChanged act =
  mapSignalM (const $ activityIdleTime act) (activityIdleTimeChanged_ act)
  
-- | Signal when the 'activityIdleTime' property value has changed.
activityIdleTimeChanged_ :: MonadDES m => Activity m s a b -> Signal m ()
{-# INLINABLE activityIdleTimeChanged_ #-}
activityIdleTimeChanged_ act =
  mapSignal (const ()) (activityUtilising act)

-- | Return the statistics for the time when the activity was preempted
-- waiting for the further proceeding.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'activityPreemptionTimeChanged' and 'activityPreemptionTimeChanged_'.
activityPreemptionTime :: MonadDES m => Activity m s a b -> Event m (SamplingStats Double)
{-# INLINABLE activityPreemptionTime #-}
activityPreemptionTime act =
  Event $ \p -> invokeEvent p $ readRef (activityPreemptionTimeRef act)
  
-- | Signal when the 'activityPreemptionTime' property value has changed.
activityPreemptionTimeChanged :: MonadDES m => Activity m s a b -> Signal m (SamplingStats Double)
{-# INLINABLE activityPreemptionTimeChanged #-}
activityPreemptionTimeChanged act =
  mapSignalM (const $ activityPreemptionTime act) (activityPreemptionTimeChanged_ act)
  
-- | Signal when the 'activityPreemptionTime' property value has changed.
activityPreemptionTimeChanged_ :: MonadDES m => Activity m s a b -> Signal m ()
{-# INLINABLE activityPreemptionTimeChanged_ #-}
activityPreemptionTimeChanged_ act =
  mapSignal (const ()) (activityPreemptionEnding act)
  
-- | It returns the factor changing from 0 to 1, which estimates how often
-- the activity was utilised.
--
-- This factor is calculated as
--
-- @
--   totalUtilisationTime \/ (totalUtilisationTime + totalIdleTime + totalPreemptionTime)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
--
-- See also 'activityUtilisationFactorChanged' and 'activityUtilisationFactorChanged_'.
activityUtilisationFactor :: MonadDES m => Activity m s a b -> Event m Double
{-# INLINABLE activityUtilisationFactor #-}
activityUtilisationFactor act =
  Event $ \p ->
  do x1 <- invokeEvent p $ readRef (activityTotalUtilisationTimeRef act)
     x2 <- invokeEvent p $ readRef (activityTotalIdleTimeRef act)
     x3 <- invokeEvent p $ readRef (activityTotalPreemptionTimeRef act)
     return (x1 / (x1 + x2 + x3))
  
-- | Signal when the 'activityUtilisationFactor' property value has changed.
activityUtilisationFactorChanged :: MonadDES m => Activity m s a b -> Signal m Double
{-# INLINABLE activityUtilisationFactorChanged #-}
activityUtilisationFactorChanged act =
  mapSignalM (const $ activityUtilisationFactor act) (activityUtilisationFactorChanged_ act)
  
-- | Signal when the 'activityUtilisationFactor' property value has changed.
activityUtilisationFactorChanged_ :: MonadDES m => Activity m s a b -> Signal m ()
{-# INLINABLE activityUtilisationFactorChanged_ #-}
activityUtilisationFactorChanged_ act =
  mapSignal (const ()) (activityUtilising act) <>
  mapSignal (const ()) (activityUtilised act) <>
  mapSignal (const ()) (activityPreemptionEnding act)
  
-- | It returns the factor changing from 0 to 1, which estimates how often
-- the activity was idle.
--
-- This factor is calculated as
--
-- @
--   totalIdleTime \/ (totalUtilisationTime + totalIdleTime + totalPreemptionTime)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
--
-- See also 'activityIdleFactorChanged' and 'activityIdleFactorChanged_'.
activityIdleFactor :: MonadDES m => Activity m s a b -> Event m Double
{-# INLINABLE activityIdleFactor #-}
activityIdleFactor act =
  Event $ \p ->
  do x1 <- invokeEvent p $ readRef (activityTotalUtilisationTimeRef act)
     x2 <- invokeEvent p $ readRef (activityTotalIdleTimeRef act)
     x3 <- invokeEvent p $ readRef (activityTotalPreemptionTimeRef act)
     return (x2 / (x1 + x2 + x3))
  
-- | Signal when the 'activityIdleFactor' property value has changed.
activityIdleFactorChanged :: MonadDES m => Activity m s a b -> Signal m Double
{-# INLINABLE activityIdleFactorChanged #-}
activityIdleFactorChanged act =
  mapSignalM (const $ activityIdleFactor act) (activityIdleFactorChanged_ act)
  
-- | Signal when the 'activityIdleFactor' property value has changed.
activityIdleFactorChanged_ :: MonadDES m => Activity m s a b -> Signal m ()
{-# INLINABLE activityIdleFactorChanged_ #-}
activityIdleFactorChanged_ act =
  mapSignal (const ()) (activityUtilising act) <>
  mapSignal (const ()) (activityUtilised act) <>
  mapSignal (const ()) (activityPreemptionEnding act)

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the activity was preempted waiting for the further proceeding.
--
-- This factor is calculated as
--
-- @
--   totalUtilisationTime \/ (totalUtilisationTime + totalIdleTime + totalPreemptionTime)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
--
-- See also 'activityPreemptionFactorChanged' and 'activityPreemptionFactorChanged_'.
activityPreemptionFactor :: MonadDES m => Activity m s a b -> Event m Double
{-# INLINABLE activityPreemptionFactor #-}
activityPreemptionFactor act =
  Event $ \p ->
  do x1 <- invokeEvent p $ readRef (activityTotalUtilisationTimeRef act)
     x2 <- invokeEvent p $ readRef (activityTotalIdleTimeRef act)
     x3 <- invokeEvent p $ readRef (activityTotalPreemptionTimeRef act)
     return (x3 / (x1 + x2 + x3))
  
-- | Signal when the 'activityPreemptionFactor' property value has changed.
activityPreemptionFactorChanged :: MonadDES m => Activity m s a b -> Signal m Double
{-# INLINABLE activityPreemptionFactorChanged #-}
activityPreemptionFactorChanged act =
  mapSignalM (const $ activityPreemptionFactor act) (activityPreemptionFactorChanged_ act)
  
-- | Signal when the 'activityPreemptionFactor' property value has changed.
activityPreemptionFactorChanged_ :: MonadDES m => Activity m s a b -> Signal m ()
{-# INLINABLE activityPreemptionFactorChanged_ #-}
activityPreemptionFactorChanged_ act =
  mapSignal (const ()) (activityUtilising act) <>
  mapSignal (const ()) (activityUtilised act) <>
  mapSignal (const ()) (activityPreemptionEnding act)
  
-- | Raised when starting to utilise the activity after a new input task is received.
activityUtilising :: Activity m s a b -> Signal m a
{-# INLINABLE activityUtilising #-}
activityUtilising = publishSignal . activityUtilisingSource

-- | Raised when the activity has been utilised after the current task is processed.
activityUtilised :: Activity m s a b -> Signal m (a, b)
{-# INLINABLE activityUtilised #-}
activityUtilised = publishSignal . activityUtilisedSource

-- | Raised when the activity utilisation was preempted.
activityPreemptionBeginning :: Activity m s a b -> Signal m a
{-# INLINABLE activityPreemptionBeginning #-}
activityPreemptionBeginning = publishSignal . activityPreemptionBeginningSource

-- | Raised when the activity utilisation was proceeded after it had been preempted earlier.
activityPreemptionEnding :: Activity m s a b -> Signal m a
{-# INLINABLE activityPreemptionEnding #-}
activityPreemptionEnding = publishSignal . activityPreemptionEndingSource

-- | Signal whenever any property of the activity changes.
activityChanged_ :: MonadDES m => Activity m s a b -> Signal m ()
{-# INLINABLE activityChanged_ #-}
activityChanged_ act =
  mapSignal (const ()) (activityUtilising act) <>
  mapSignal (const ()) (activityUtilised act) <>
  mapSignal (const ()) (activityPreemptionEnding act)

-- | Return the summary for the activity with desciption of its
-- properties using the specified indent.
activitySummary :: MonadDES m => Activity m s a b -> Int -> Event m ShowS
{-# INLINABLE activitySummary #-}
activitySummary act indent =
  Event $ \p ->
  do tx1 <- invokeEvent p $ readRef (activityTotalUtilisationTimeRef act)
     tx2 <- invokeEvent p $ readRef (activityTotalIdleTimeRef act)
     tx3 <- invokeEvent p $ readRef (activityTotalPreemptionTimeRef act)
     let xf1 = tx1 / (tx1 + tx2 + tx3)
         xf2 = tx2 / (tx1 + tx2 + tx3)
         xf3 = tx3 / (tx1 + tx2 + tx3)
     xs1 <- invokeEvent p $ readRef (activityUtilisationTimeRef act)
     xs2 <- invokeEvent p $ readRef (activityIdleTimeRef act)
     xs3 <- invokeEvent p $ readRef (activityPreemptionTimeRef act)
     let tab = replicate indent ' '
     return $
       showString tab .
       showString "total utilisation time = " . shows tx1 .
       showString "\n" .
       showString tab .
       showString "total idle time = " . shows tx2 .
       showString "\n" .
       showString tab .
       showString "total preemption time = " . shows tx3 .
       showString "\n" .
       showString tab .
       showString "utilisation factor (from 0 to 1) = " . shows xf1 .
       showString "\n" .
       showString tab .
       showString "idle factor (from 0 to 1) = " . shows xf2 .
       showString "\n" .
       showString tab .
       showString "preemption factor (from 0 to 1) = " . shows xf3 .
       showString "\n" .
       showString tab .
       showString "utilisation time (locked while awaiting the input):\n\n" .
       samplingStatsSummary xs1 (2 + indent) .
       showString "\n\n" .
       showString tab .
       showString "idle time:\n\n" .
       samplingStatsSummary xs2 (2 + indent) .
       showString tab .
       showString "preemption time:\n\n" .
       samplingStatsSummary xs3 (2 + indent)
