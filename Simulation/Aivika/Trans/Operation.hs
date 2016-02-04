
-- |
-- Module     : Simulation.Aivika.Trans.Operation
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- It defines a stateless activity, some simplification of 'Server' and 'Activity'.
module Simulation.Aivika.Trans.Operation
       (-- * Operation
        Operation,
        newOperation,
        newPreemptibleOperation,
        -- * Processing
        operationProcess,
        -- * Operation Properties
        operationTotalUtilisationTime,
        operationTotalPreemptionTime,
        operationUtilisationTime,
        operationPreemptionTime,
        operationUtilisationFactor,
        operationPreemptionFactor,
        -- * Summary
        operationSummary,
        -- * Derived Signals for Properties
        operationTotalUtilisationTimeChanged,
        operationTotalUtilisationTimeChanged_,
        operationTotalPreemptionTimeChanged,
        operationTotalPreemptionTimeChanged_,
        operationUtilisationTimeChanged,
        operationUtilisationTimeChanged_,
        operationPreemptionTimeChanged,
        operationPreemptionTimeChanged_,
        operationUtilisationFactorChanged,
        operationUtilisationFactorChanged_,
        operationPreemptionFactorChanged,
        operationPreemptionFactorChanged_,
        -- * Basic Signals
        operationUtilising,
        operationUtilised,
        operationPreemptionBeginning,
        operationPreemptionEnding,
        -- * Overall Signal
        operationChanged_) where

import Data.Monoid

import Control.Monad
import Control.Monad.Trans

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
import Simulation.Aivika.Trans.Activity
import Simulation.Aivika.Trans.Server
import Simulation.Aivika.Trans.Statistics

-- | Like 'Server' it models an activity that takes @a@ and provides @b@.
-- But unlike the former this kind of activity has no state. Also it is destined
-- to be used within 'Process' computations.
data Operation m a b =
  Operation { operationInitProcess :: a -> Process m b,
              -- ^ Provide @b@ by specified @a@.
              operationProcessPreemptible :: Bool,
              -- ^ Whether the process is preemptible.
              operationStartTime :: Double,
              -- ^ The start time of creating the operation.
              operationLastTimeRef :: Ref m Double,
              -- ^ The last time of utilising the operation activity.
              operationTotalUtilisationTimeRef :: Ref m Double,
              -- ^ The counted total time of utilising the activity.
              operationTotalPreemptionTimeRef :: Ref m Double,
              -- ^ The counted total time when the activity was preempted. 
              operationUtilisationTimeRef :: Ref m (SamplingStats Double),
              -- ^ The statistics for the utilisation time.
              operationPreemptionTimeRef :: Ref m (SamplingStats Double),
              -- ^ The statistics for the time when the activity was preempted.
              operationUtilisingSource :: SignalSource m a,
              -- ^ A signal raised when starting to utilise the activity.
              operationUtilisedSource :: SignalSource m (a, b),
              -- ^ A signal raised when the activity has been utilised.
              operationPreemptionBeginningSource :: SignalSource m a,
              -- ^ A signal raised when the utilisation was preempted.
              operationPreemptionEndingSource :: SignalSource m a
              -- ^ A signal raised when the utilisation was proceeded after it had been preempted earlier.
           }

-- | Create a new operation that can provide output @b@ by input @a@.
--
-- By default, it is assumed that the activity utilisation cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newOperation :: MonadDES m
                => (a -> Process m b)
                -- ^ provide an output by the specified input
                -> Event m (Operation m a b)
{-# INLINABLE newOperation #-}
newOperation = newPreemptibleOperation False

-- | Create a new operation that can provide output @b@ by input @a@.
newPreemptibleOperation :: MonadDES m
                           => Bool
                           -- ^ whether the activity can be preempted
                           -> (a -> Process m b)
                           -- ^ provide an output by the specified input
                           -> Event m (Operation m a b)
{-# INLINABLE newPreemptibleOperation #-}
newPreemptibleOperation preemptible provide =
  do t0 <- liftDynamics time
     r0 <- liftSimulation $ newRef t0
     r1 <- liftSimulation $ newRef 0
     r2 <- liftSimulation $ newRef 0
     r3 <- liftSimulation $ newRef emptySamplingStats
     r4 <- liftSimulation $ newRef emptySamplingStats
     s1 <- liftSimulation newSignalSource
     s2 <- liftSimulation newSignalSource
     s3 <- liftSimulation newSignalSource
     s4 <- liftSimulation newSignalSource
     return Operation { operationInitProcess = provide,
                        operationProcessPreemptible = preemptible,
                        operationStartTime = t0,
                        operationLastTimeRef = r0,
                        operationTotalUtilisationTimeRef = r1,
                        operationTotalPreemptionTimeRef = r2,
                        operationUtilisationTimeRef = r3,
                        operationPreemptionTimeRef = r4,
                        operationUtilisingSource = s1,
                        operationUtilisedSource = s2,
                        operationPreemptionBeginningSource = s3,
                        operationPreemptionEndingSource = s4 }

-- | Return a computation for the specified operation. It updates internal counters.
--
-- The computation can be used only within one process at any time.
operationProcess :: MonadDES m => Operation m a b -> a -> Process m b
{-# INLINABLE operationProcess #-}
operationProcess op a =
  do t0 <- liftDynamics time
     liftEvent $
       triggerSignal (operationUtilisingSource op) a
     -- utilise the activity
     (b, dt) <- if operationProcessPreemptible op
                then operationProcessPreempting op a
                else do b <- operationInitProcess op a
                        return (b, 0)
     t1 <- liftDynamics time
     liftEvent $
       do modifyRef (operationTotalUtilisationTimeRef op) (+ (t1 - t0 - dt))
          modifyRef (operationUtilisationTimeRef op) $
            addSamplingStats (t1 - t0 - dt)
          writeRef (operationLastTimeRef op) t1
          triggerSignal (operationUtilisedSource op) (a, b)
     return b

-- | Process the input with ability to handle a possible preemption.
operationProcessPreempting :: MonadDES m => Operation m a b -> a -> Process m (b, Double)
{-# INLINABLE operationProcessPreempting #-}
operationProcessPreempting op a =
  do pid <- processId
     t0  <- liftDynamics time
     rs  <- liftSimulation $ newRef 0
     r0  <- liftSimulation $ newRef t0
     h1  <- liftEvent $
            handleSignal (processPreemptionBeginning pid) $ \() ->
            do t0 <- liftDynamics time
               writeRef r0 t0
               triggerSignal (operationPreemptionBeginningSource op) a
     h2  <- liftEvent $
            handleSignal (processPreemptionEnding pid) $ \() ->
            do t0 <- readRef r0
               t1 <- liftDynamics time
               let dt = t1 - t0
               modifyRef rs (+ dt)
               modifyRef (operationTotalPreemptionTimeRef op) (+ dt)
               modifyRef (operationPreemptionTimeRef op) $
                 addSamplingStats dt
               writeRef (operationLastTimeRef op) t1
               triggerSignal (operationPreemptionEndingSource op) a 
     let m1 =
           do b <- operationInitProcess op a
              dt <- liftEvent $ readRef rs
              return (b, dt)
         m2 =
           liftEvent $
           do disposeEvent h1
              disposeEvent h2
     finallyProcess m1 m2

-- | Return the counted total time when the operation activity was utilised.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'operationTotalUtilisationTimeChanged' and 'operationTotalUtilisationTimeChanged_'.
operationTotalUtilisationTime :: MonadDES m => Operation m a b -> Event m Double
{-# INLINABLE operationTotalUtilisationTime #-}
operationTotalUtilisationTime op =
  Event $ \p -> invokeEvent p $ readRef (operationTotalUtilisationTimeRef op)
  
-- | Signal when the 'operationTotalUtilisationTime' property value has changed.
operationTotalUtilisationTimeChanged :: MonadDES m => Operation m a b -> Signal m Double
{-# INLINABLE operationTotalUtilisationTimeChanged #-}
operationTotalUtilisationTimeChanged op =
  mapSignalM (const $ operationTotalUtilisationTime op) (operationTotalUtilisationTimeChanged_ op)
  
-- | Signal when the 'operationTotalUtilisationTime' property value has changed.
operationTotalUtilisationTimeChanged_ :: MonadDES m => Operation m a b -> Signal m ()
{-# INLINABLE operationTotalUtilisationTimeChanged_ #-}
operationTotalUtilisationTimeChanged_ op =
  mapSignal (const ()) (operationUtilised op)

-- | Return the counted total time when the operation activity was preemted waiting for
-- the further proceeding.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'operationTotalPreemptionTimeChanged' and 'operationTotalPreemptionTimeChanged_'.
operationTotalPreemptionTime :: MonadDES m => Operation m a b -> Event m Double
{-# INLINABLE operationTotalPreemptionTime #-}
operationTotalPreemptionTime op =
  Event $ \p -> invokeEvent p $ readRef (operationTotalPreemptionTimeRef op)
  
-- | Signal when the 'operationTotalPreemptionTime' property value has changed.
operationTotalPreemptionTimeChanged :: MonadDES m => Operation m a b -> Signal m Double
{-# INLINABLE operationTotalPreemptionTimeChanged #-}
operationTotalPreemptionTimeChanged op =
  mapSignalM (const $ operationTotalPreemptionTime op) (operationTotalPreemptionTimeChanged_ op)
  
-- | Signal when the 'operationTotalPreemptionTime' property value has changed.
operationTotalPreemptionTimeChanged_ :: MonadDES m => Operation m a b -> Signal m ()
{-# INLINABLE operationTotalPreemptionTimeChanged_ #-}
operationTotalPreemptionTimeChanged_ op =
  mapSignal (const ()) (operationPreemptionEnding op)

-- | Return the statistics for the time when the operation activity was utilised.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'operationUtilisationTimeChanged' and 'operationUtilisationTimeChanged_'.
operationUtilisationTime :: MonadDES m => Operation m a b -> Event m (SamplingStats Double)
{-# INLINABLE operationUtilisationTime #-}
operationUtilisationTime op =
  Event $ \p -> invokeEvent p $ readRef (operationUtilisationTimeRef op)
  
-- | Signal when the 'operationUtilisationTime' property value has changed.
operationUtilisationTimeChanged :: MonadDES m => Operation m a b -> Signal m (SamplingStats Double)
{-# INLINABLE operationUtilisationTimeChanged #-}
operationUtilisationTimeChanged op =
  mapSignalM (const $ operationUtilisationTime op) (operationUtilisationTimeChanged_ op)
  
-- | Signal when the 'operationUtilisationTime' property value has changed.
operationUtilisationTimeChanged_ :: MonadDES m => Operation m a b -> Signal m ()
{-# INLINABLE operationUtilisationTimeChanged_ #-}
operationUtilisationTimeChanged_ op =
  mapSignal (const ()) (operationUtilised op)

-- | Return the statistics for the time when the operation activity was preempted
-- waiting for the further proceeding.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'operationPreemptionTimeChanged' and 'operationPreemptionTimeChanged_'.
operationPreemptionTime :: MonadDES m => Operation m a b -> Event m (SamplingStats Double)
{-# INLINABLE operationPreemptionTime #-}
operationPreemptionTime op =
  Event $ \p -> invokeEvent p $ readRef (operationPreemptionTimeRef op)
  
-- | Signal when the 'operationPreemptionTime' property value has changed.
operationPreemptionTimeChanged :: MonadDES m => Operation m a b -> Signal m (SamplingStats Double)
{-# INLINABLE operationPreemptionTimeChanged #-}
operationPreemptionTimeChanged op =
  mapSignalM (const $ operationPreemptionTime op) (operationPreemptionTimeChanged_ op)
  
-- | Signal when the 'operationPreemptionTime' property value has changed.
operationPreemptionTimeChanged_ :: MonadDES m => Operation m a b -> Signal m ()
{-# INLINABLE operationPreemptionTimeChanged_ #-}
operationPreemptionTimeChanged_ op =
  mapSignal (const ()) (operationPreemptionEnding op)
  
-- | It returns the factor changing from 0 to 1, which estimates how often
-- the operation activity was utilised since the time of creating the operation.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'operationUtilisationFactorChanged' and 'operationUtilisationFactorChanged_'.
operationUtilisationFactor :: MonadDES m => Operation m a b -> Event m Double
{-# INLINABLE operationUtilisationFactor #-}
operationUtilisationFactor op =
  Event $ \p ->
  do let t0 = operationStartTime op
     t1 <- invokeEvent p $ readRef (operationLastTimeRef op)
     x  <- invokeEvent p $ readRef (operationTotalUtilisationTimeRef op)
     return (x / (t1 - t0))
  
-- | Signal when the 'operationUtilisationFactor' property value has changed.
operationUtilisationFactorChanged :: MonadDES m => Operation m a b -> Signal m Double
{-# INLINABLE operationUtilisationFactorChanged #-}
operationUtilisationFactorChanged op =
  mapSignalM (const $ operationUtilisationFactor op) (operationUtilisationFactorChanged_ op)
  
-- | Signal when the 'operationUtilisationFactor' property value has changed.
operationUtilisationFactorChanged_ :: MonadDES m => Operation m a b -> Signal m ()
{-# INLINABLE operationUtilisationFactorChanged_ #-}
operationUtilisationFactorChanged_ op =
  mapSignal (const ()) (operationUtilised op) <>
  mapSignal (const ()) (operationPreemptionEnding op)
  
-- | It returns the factor changing from 0 to 1, which estimates how often
-- the operation activity was preempted waiting for the further proceeding
-- since the time of creating the operation.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'operationPreemptionFactorChanged' and 'operationPreemptionFactorChanged_'.
operationPreemptionFactor :: MonadDES m => Operation m a b -> Event m Double
{-# INLINABLE operationPreemptionFactor #-}
operationPreemptionFactor op =
  Event $ \p ->
  do let t0 = operationStartTime op
     t1 <- invokeEvent p $ readRef (operationLastTimeRef op)
     x  <- invokeEvent p $ readRef (operationTotalPreemptionTimeRef op)
     return (x / (t1 - t0))
  
-- | Signal when the 'operationPreemptionFactor' property value has changed.
operationPreemptionFactorChanged :: MonadDES m => Operation m a b -> Signal m Double
{-# INLINABLE operationPreemptionFactorChanged #-}
operationPreemptionFactorChanged op =
  mapSignalM (const $ operationPreemptionFactor op) (operationPreemptionFactorChanged_ op)
  
-- | Signal when the 'operationPreemptionFactor' property value has changed.
operationPreemptionFactorChanged_ :: MonadDES m => Operation m a b -> Signal m ()
{-# INLINABLE operationPreemptionFactorChanged_ #-}
operationPreemptionFactorChanged_ op =
  mapSignal (const ()) (operationUtilised op) <>
  mapSignal (const ()) (operationPreemptionEnding op)
  
-- | Raised when starting to utilise the operation activity after a new input task is received.
operationUtilising :: MonadDES m => Operation m a b -> Signal m a
{-# INLINABLE operationUtilising #-}
operationUtilising = publishSignal . operationUtilisingSource

-- | Raised when the operation activity has been utilised after the current task is processed.
operationUtilised :: MonadDES m => Operation m a b -> Signal m (a, b)
{-# INLINABLE operationUtilised #-}
operationUtilised = publishSignal . operationUtilisedSource

-- | Raised when the operation activity utilisation was preempted.
operationPreemptionBeginning :: MonadDES m => Operation m a b -> Signal m a
{-# INLINABLE operationPreemptionBeginning #-}
operationPreemptionBeginning = publishSignal . operationPreemptionBeginningSource

-- | Raised when the operation activity utilisation was proceeded after it had been preempted earlier.
operationPreemptionEnding :: MonadDES m => Operation m a b -> Signal m a
{-# INLINABLE operationPreemptionEnding #-}
operationPreemptionEnding = publishSignal . operationPreemptionEndingSource

-- | Signal whenever any property of the operation changes.
operationChanged_ :: MonadDES m => Operation m a b -> Signal m ()
{-# INLINABLE operationChanged_ #-}
operationChanged_ op =
  mapSignal (const ()) (operationUtilising op) <>
  mapSignal (const ()) (operationUtilised op) <>
  mapSignal (const ()) (operationPreemptionEnding op)

-- | Return the summary for the operation with desciption of its
-- properties using the specified indent.
operationSummary :: MonadDES m => Operation m a b -> Int -> Event m ShowS
{-# INLINABLE operationSummary #-}
operationSummary op indent =
  Event $ \p ->
  do let t0 = operationStartTime op
     t1  <- invokeEvent p $ readRef (operationLastTimeRef op)
     tx1 <- invokeEvent p $ readRef (operationTotalUtilisationTimeRef op)
     tx2 <- invokeEvent p $ readRef (operationTotalPreemptionTimeRef op)
     let xf1 = tx1 / (t1 - t0)
         xf2 = tx2 / (t1 - t0)
     xs1 <- invokeEvent p $ readRef (operationUtilisationTimeRef op)
     xs2 <- invokeEvent p $ readRef (operationPreemptionTimeRef op)
     let tab = replicate indent ' '
     return $
       showString tab .
       showString "total utilisation time = " . shows tx1 .
       showString "\n" .
       showString tab .
       showString "total preemption time = " . shows tx2 .
       showString "\n" .
       showString tab .
       showString "utilisation factor (from 0 to 1) = " . shows xf1 .
       showString "\n" .
       showString tab .
       showString "preemption factor (from 0 to 1) = " . shows xf2 .
       showString "\n" .
       showString tab .
       showString "utilisation time:\n\n" .
       samplingStatsSummary xs1 (2 + indent) .
       showString "\n\n" .
       showString tab .
       showString "preemption time:\n\n" .
       samplingStatsSummary xs2 (2 + indent)
