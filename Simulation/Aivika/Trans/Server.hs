
-- |
-- Module     : Simulation.Aivika.Trans.Server
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- It models the server that prodives a service.
module Simulation.Aivika.Trans.Server
       (-- * Server
        Server,
        newServer,
        newStateServer,
        newPreemptibleServer,
        newPreemptibleStateServer,
        -- * Processing
        serverProcessor,
        -- * Server Properties and Activities
        serverInitState,
        serverState,
        serverTotalInputWaitTime,
        serverTotalProcessingTime,
        serverTotalOutputWaitTime,
        serverTotalPreemptionTime,
        serverInputWaitTime,
        serverProcessingTime,
        serverOutputWaitTime,
        serverPreemptionTime,
        serverInputWaitFactor,
        serverProcessingFactor,
        serverOutputWaitFactor,
        serverPreemptionFactor,
        -- * Summary
        serverSummary,
        -- * Derived Signals for Properties
        serverStateChanged,
        serverStateChanged_,
        serverTotalInputWaitTimeChanged,
        serverTotalInputWaitTimeChanged_,
        serverTotalProcessingTimeChanged,
        serverTotalProcessingTimeChanged_,
        serverTotalOutputWaitTimeChanged,
        serverTotalOutputWaitTimeChanged_,
        serverTotalPreemptionTimeChanged,
        serverTotalPreemptionTimeChanged_,
        serverInputWaitTimeChanged,
        serverInputWaitTimeChanged_,
        serverProcessingTimeChanged,
        serverProcessingTimeChanged_,
        serverOutputWaitTimeChanged,
        serverOutputWaitTimeChanged_,
        serverPreemptionTimeChanged,
        serverPreemptionTimeChanged_,
        serverInputWaitFactorChanged,
        serverInputWaitFactorChanged_,
        serverProcessingFactorChanged,
        serverProcessingFactorChanged_,
        serverOutputWaitFactorChanged,
        serverOutputWaitFactorChanged_,
        serverPreemptionFactorChanged,
        serverPreemptionFactorChanged_,
        -- * Basic Signals
        serverInputReceived,
        serverTaskPreemptionBeginning,
        serverTaskPreemptionEnding,
        serverTaskProcessed,
        serverOutputProvided,
        -- * Overall Signal
        serverChanged_) where

import Data.Monoid

import Control.Monad
import Control.Arrow

import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Dynamics
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Signal
import Simulation.Aivika.Trans.Resource
import Simulation.Aivika.Trans.Cont
import Simulation.Aivika.Trans.Process
import Simulation.Aivika.Trans.Processor
import Simulation.Aivika.Trans.Stream
import Simulation.Aivika.Trans.Statistics

-- | It models a server that takes @a@ and provides @b@ having state @s@ within underlying computation @m@.
data Server m s a b =
  Server { serverInitState :: s,
           -- ^ The initial state of the server.
           serverStateRef :: Ref m s,
           -- ^ The current state of the server.
           serverProcess :: s -> a -> Process m (s, b),
           -- ^ Provide @b@ by specified @a@.
           serverProcessPreemptible :: Bool,
           -- ^ Whether the process can be preempted.
           serverTotalInputWaitTimeRef :: Ref m Double,
           -- ^ The counted total time spent in awating the input.
           serverTotalProcessingTimeRef :: Ref m Double,
           -- ^ The counted total time spent to process the input and prepare the output.
           serverTotalOutputWaitTimeRef :: Ref m Double,
           -- ^ The counted total time spent for delivering the output.
           serverTotalPreemptionTimeRef :: Ref m Double,
           -- ^ The counted total time spent being preempted and waiting for the proceeding. 
           serverInputWaitTimeRef :: Ref m (SamplingStats Double),
           -- ^ The statistics for the time spent in awaiting the input.
           serverProcessingTimeRef :: Ref m (SamplingStats Double),
           -- ^ The statistics for the time spent to process the input and prepare the output.
           serverOutputWaitTimeRef :: Ref m (SamplingStats Double),
           -- ^ The statistics for the time spent for delivering the output.
           serverPreemptionTimeRef :: Ref m (SamplingStats Double),
           -- ^ The statistics for the time spent being preempted.
           serverInputReceivedSource :: SignalSource m a,
           -- ^ A signal raised when the server recieves a new input to process.
           serverTaskPreemptionBeginningSource :: SignalSource m a,
           -- ^ A signal raised when the task was preempted.
           serverTaskPreemptionEndingSource :: SignalSource m a,
           -- ^ A signal raised when the task was proceeded after it had been preempted earlier.
           serverTaskProcessedSource :: SignalSource m (a, b),
           -- ^ A signal raised when the input is processed and
           -- the output is prepared for deliverying.
           serverOutputProvidedSource :: SignalSource m (a, b)
           -- ^ A signal raised when the server has supplied the output.
         }

-- | Create a new server that can provide output @b@ by input @a@.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newServer :: MonadDES m
             => (a -> Process m b)
             -- ^ provide an output by the specified input
             -> Simulation m (Server m () a b)
{-# INLINABLE newServer #-}
newServer = newPreemptibleServer False

-- | Create a new server that can provide output @b@ by input @a@
-- starting from state @s@.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newStateServer :: MonadDES m
                  => (s -> a -> Process m (s, b))
                  -- ^ provide a new state and output by the specified 
                  -- old state and input
                  -> s
                  -- ^ the initial state
                  -> Simulation m (Server m s a b)
{-# INLINABLE newStateServer #-}
newStateServer = newPreemptibleStateServer False

-- | Create a new preemptible server that can provide output @b@ by input @a@.
newPreemptibleServer :: MonadDES m
                        => Bool
                        -- ^ whether the server process can be preempted
                        -> (a -> Process m b)
                        -- ^ provide an output by the specified input
                        -> Simulation m (Server m () a b)
{-# INLINABLE newPreemptibleServer #-}
newPreemptibleServer preemptible provide =
  flip (newPreemptibleStateServer preemptible) () $ \s a ->
  do b <- provide a
     return (s, b)

-- | Create a new preemptible server that can provide output @b@ by input @a@
-- starting from state @s@.
newPreemptibleStateServer :: MonadDES m
                             => Bool
                             -- ^ whether the server process can be preempted
                             -> (s -> a -> Process m (s, b))
                             -- ^ provide a new state and output by the specified 
                             -- old state and input
                             -> s
                             -- ^ the initial state
                             -> Simulation m (Server m s a b)
{-# INLINABLE newPreemptibleStateServer #-}
newPreemptibleStateServer preemptible provide state =
  do r0 <- newRef state
     r1 <- newRef 0
     r2 <- newRef 0
     r3 <- newRef 0
     r4 <- newRef 0
     r5 <- newRef emptySamplingStats
     r6 <- newRef emptySamplingStats
     r7 <- newRef emptySamplingStats
     r8 <- newRef emptySamplingStats
     s1 <- newSignalSource
     s2 <- newSignalSource
     s3 <- newSignalSource
     s4 <- newSignalSource
     s5 <- newSignalSource
     let server = Server { serverInitState = state,
                           serverStateRef = r0,
                           serverProcess = provide,
                           serverProcessPreemptible = preemptible,
                           serverTotalInputWaitTimeRef = r1,
                           serverTotalProcessingTimeRef = r2,
                           serverTotalOutputWaitTimeRef = r3,
                           serverTotalPreemptionTimeRef = r4,
                           serverInputWaitTimeRef = r5,
                           serverProcessingTimeRef = r6,
                           serverOutputWaitTimeRef = r7,
                           serverPreemptionTimeRef = r8,
                           serverInputReceivedSource = s1,
                           serverTaskPreemptionBeginningSource = s2,
                           serverTaskPreemptionEndingSource = s3,
                           serverTaskProcessedSource = s4,
                           serverOutputProvidedSource = s5 }
     return server

-- | Return a processor for the specified server.
--
-- The processor updates the internal state of the server. The usual case is when 
-- the processor is applied only once in a chain of data processing. Otherwise; 
-- every time the processor is used, the state of the server changes. Sometimes 
-- it can be indeed useful if you want to aggregate the statistics for different 
-- servers simultaneously, but it would be more preferable to avoid this.
--
-- If you connect different server processors returned by this function in a chain 
-- with help of '>>>' or other category combinator then this chain will act as one 
-- whole, where the first server will take a new task only after the last server 
-- finishes its current task and requests for the next one from the previous processor 
-- in the chain. This is not always that thing you might need.
--
-- To model a sequence of the server processors working independently, you
-- should use the 'processorSeq' function which separates the processors with help of
-- the 'prefetchProcessor' that plays a role of a small one-place buffer in that case.
--
-- The queue processors usually have the prefetching capabilities per se, where
-- the items are already stored in the queue. Therefore, the server processor
-- should not be prefetched if it is connected directly to the queue processor.
serverProcessor :: MonadDES m => Server m s a b -> Processor m a b
{-# INLINABLE serverProcessor #-}
serverProcessor server =
  Processor $ \xs -> loop (serverInitState server) Nothing xs
  where
    loop s r xs =
      Cons $
      do t0 <- liftDynamics time
         liftEvent $
           case r of
             Nothing -> return ()
             Just (t', a', b') ->
               do modifyRef (serverTotalOutputWaitTimeRef server) (+ (t0 - t'))
                  modifyRef (serverOutputWaitTimeRef server) $
                    addSamplingStats (t0 - t')
                  triggerSignal (serverOutputProvidedSource server) (a', b')
         -- get input
         (a, xs') <- runStream xs
         t1 <- liftDynamics time
         liftEvent $
           do modifyRef (serverTotalInputWaitTimeRef server) (+ (t1 - t0))
              modifyRef (serverInputWaitTimeRef server) $
                addSamplingStats (t1 - t0)
              triggerSignal (serverInputReceivedSource server) a
         -- provide the service
         (s', b, dt) <-
           if serverProcessPreemptible server
           then serverProcessPreempting server s a
           else do (s', b) <- serverProcess server s a
                   return (s', b, 0)
         t2 <- liftDynamics time
         liftEvent $
           do writeRef (serverStateRef server) $! s'
              modifyRef (serverTotalProcessingTimeRef server) (+ (t2 - t1 - dt))
              modifyRef (serverProcessingTimeRef server) $
                addSamplingStats (t2 - t1 - dt)
              triggerSignal (serverTaskProcessedSource server) (a, b)
         return (b, loop s' (Just (t2, a, b)) xs')

-- | Process the input with ability to handle a possible preemption.
serverProcessPreempting :: MonadDES m => Server m s a b -> s -> a -> Process m (s, b, Double)
{-# INLINABLE serverProcessPreempting #-}
serverProcessPreempting server s a =
  do pid <- processId
     t1  <- liftDynamics time
     rs  <- liftSimulation $ newRef 0
     r1  <- liftSimulation $ newRef t1
     h1  <- liftEvent $
            handleSignal (processPreemptionBeginning pid) $ \() ->
            do t1 <- liftDynamics time
               writeRef r1 t1
               triggerSignal (serverTaskPreemptionBeginningSource server) a
     h2  <- liftEvent $
            handleSignal (processPreemptionEnding pid) $ \() ->
            do t1 <- readRef r1
               t2 <- liftDynamics time
               let dt = t2 - t1
               modifyRef rs (+ dt)
               modifyRef (serverTotalPreemptionTimeRef server) (+ dt)
               modifyRef (serverPreemptionTimeRef server) $
                 addSamplingStats dt
               triggerSignal (serverTaskPreemptionEndingSource server) a 
     let m1 =
           do (s', b) <- serverProcess server s a
              dt <- liftEvent $ readRef rs
              return (s', b, dt)
         m2 =
           liftEvent $
           do disposeEvent h1
              disposeEvent h2
     finallyProcess m1 m2

-- | Return the current state of the server.
--
-- See also 'serverStateChanged' and 'serverStateChanged_'.
serverState :: MonadDES m => Server m s a b -> Event m s
{-# INLINABLE serverState #-}
serverState server =
  Event $ \p -> invokeEvent p $ readRef (serverStateRef server)
  
-- | Signal when the 'serverState' property value has changed.
serverStateChanged :: MonadDES m => Server m s a b -> Signal m s
{-# INLINABLE serverStateChanged #-}
serverStateChanged server =
  mapSignalM (const $ serverState server) (serverStateChanged_ server)
  
-- | Signal when the 'serverState' property value has changed.
serverStateChanged_ :: MonadDES m => Server m s a b -> Signal m ()
{-# INLINABLE serverStateChanged_ #-}
serverStateChanged_ server =
  mapSignal (const ()) (serverTaskProcessed server)

-- | Return the counted total time when the server was locked while awaiting the input.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'serverTotalInputWaitTimeChanged' and 'serverTotalInputWaitTimeChanged_'.
serverTotalInputWaitTime :: MonadDES m => Server m s a b -> Event m Double
{-# INLINABLE serverTotalInputWaitTime #-}
serverTotalInputWaitTime server =
  Event $ \p -> invokeEvent p $ readRef (serverTotalInputWaitTimeRef server)
  
-- | Signal when the 'serverTotalInputWaitTime' property value has changed.
serverTotalInputWaitTimeChanged :: MonadDES m => Server m s a b -> Signal m Double
{-# INLINABLE serverTotalInputWaitTimeChanged #-}
serverTotalInputWaitTimeChanged server =
  mapSignalM (const $ serverTotalInputWaitTime server) (serverTotalInputWaitTimeChanged_ server)
  
-- | Signal when the 'serverTotalInputWaitTime' property value has changed.
serverTotalInputWaitTimeChanged_ :: MonadDES m => Server m s a b -> Signal m ()
{-# INLINABLE serverTotalInputWaitTimeChanged_ #-}
serverTotalInputWaitTimeChanged_ server =
  mapSignal (const ()) (serverInputReceived server)

-- | Return the counted total time spent by the server while processing the tasks.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'serverTotalProcessingTimeChanged' and 'serverTotalProcessingTimeChanged_'.
serverTotalProcessingTime :: MonadDES m => Server m s a b -> Event m Double
{-# INLINABLE serverTotalProcessingTime #-}
serverTotalProcessingTime server =
  Event $ \p -> invokeEvent p $ readRef (serverTotalProcessingTimeRef server)
  
-- | Signal when the 'serverTotalProcessingTime' property value has changed.
serverTotalProcessingTimeChanged :: MonadDES m => Server m s a b -> Signal m Double
{-# INLINABLE serverTotalProcessingTimeChanged #-}
serverTotalProcessingTimeChanged server =
  mapSignalM (const $ serverTotalProcessingTime server) (serverTotalProcessingTimeChanged_ server)
  
-- | Signal when the 'serverTotalProcessingTime' property value has changed.
serverTotalProcessingTimeChanged_ :: MonadDES m => Server m s a b -> Signal m ()
{-# INLINABLE serverTotalProcessingTimeChanged_ #-}
serverTotalProcessingTimeChanged_ server =
  mapSignal (const ()) (serverTaskProcessed server)

-- | Return the counted total time when the server was locked while trying
-- to deliver the output.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'serverTotalOutputWaitTimeChanged' and 'serverTotalOutputWaitTimeChanged_'.
serverTotalOutputWaitTime :: MonadDES m => Server m s a b -> Event m Double
{-# INLINABLE serverTotalOutputWaitTime #-}
serverTotalOutputWaitTime server =
  Event $ \p -> invokeEvent p $ readRef (serverTotalOutputWaitTimeRef server)
  
-- | Signal when the 'serverTotalOutputWaitTime' property value has changed.
serverTotalOutputWaitTimeChanged :: MonadDES m => Server m s a b -> Signal m Double
{-# INLINABLE serverTotalOutputWaitTimeChanged #-}
serverTotalOutputWaitTimeChanged server =
  mapSignalM (const $ serverTotalOutputWaitTime server) (serverTotalOutputWaitTimeChanged_ server)
  
-- | Signal when the 'serverTotalOutputWaitTime' property value has changed.
serverTotalOutputWaitTimeChanged_ :: MonadDES m => Server m s a b -> Signal m ()
{-# INLINABLE serverTotalOutputWaitTimeChanged_ #-}
serverTotalOutputWaitTimeChanged_ server =
  mapSignal (const ()) (serverOutputProvided server)

-- | Return the counted total time spent by the server while it was preempted
-- waiting for the further proceeding.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'serverTotalPreemptionTimeChanged' and 'serverTotalPreemptionTimeChanged_'.
serverTotalPreemptionTime :: MonadDES m => Server m s a b -> Event m Double
{-# INLINABLE serverTotalPreemptionTime #-}
serverTotalPreemptionTime server =
  Event $ \p -> invokeEvent p $ readRef (serverTotalPreemptionTimeRef server)
  
-- | Signal when the 'serverTotalPreemptionTime' property value has changed.
serverTotalPreemptionTimeChanged :: MonadDES m => Server m s a b -> Signal m Double
{-# INLINABLE serverTotalPreemptionTimeChanged #-}
serverTotalPreemptionTimeChanged server =
  mapSignalM (const $ serverTotalPreemptionTime server) (serverTotalPreemptionTimeChanged_ server)
  
-- | Signal when the 'serverTotalPreemptionTime' property value has changed.
serverTotalPreemptionTimeChanged_ :: MonadDES m => Server m s a b -> Signal m ()
{-# INLINABLE serverTotalPreemptionTimeChanged_ #-}
serverTotalPreemptionTimeChanged_ server =
  mapSignal (const ()) (serverTaskPreemptionEnding server)

-- | Return the statistics of the time when the server was locked while awaiting the input.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'serverInputWaitTimeChanged' and 'serverInputWaitTimeChanged_'.
serverInputWaitTime :: MonadDES m => Server m s a b -> Event m (SamplingStats Double)
{-# INLINABLE serverInputWaitTime #-}
serverInputWaitTime server =
  Event $ \p -> invokeEvent p $ readRef (serverInputWaitTimeRef server)
  
-- | Signal when the 'serverInputWaitTime' property value has changed.
serverInputWaitTimeChanged :: MonadDES m => Server m s a b -> Signal m (SamplingStats Double)
{-# INLINABLE serverInputWaitTimeChanged #-}
serverInputWaitTimeChanged server =
  mapSignalM (const $ serverInputWaitTime server) (serverInputWaitTimeChanged_ server)
  
-- | Signal when the 'serverInputWaitTime' property value has changed.
serverInputWaitTimeChanged_ :: MonadDES m => Server m s a b -> Signal m ()
{-# INLINABLE serverInputWaitTimeChanged_ #-}
serverInputWaitTimeChanged_ server =
  mapSignal (const ()) (serverInputReceived server)

-- | Return the statistics of the time spent by the server while processing the tasks.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'serverProcessingTimeChanged' and 'serverProcessingTimeChanged_'.
serverProcessingTime :: MonadDES m => Server m s a b -> Event m (SamplingStats Double)
{-# INLINABLE serverProcessingTime #-}
serverProcessingTime server =
  Event $ \p -> invokeEvent p $ readRef (serverProcessingTimeRef server)
  
-- | Signal when the 'serverProcessingTime' property value has changed.
serverProcessingTimeChanged :: MonadDES m => Server m s a b -> Signal m (SamplingStats Double)
{-# INLINABLE serverProcessingTimeChanged #-}
serverProcessingTimeChanged server =
  mapSignalM (const $ serverProcessingTime server) (serverProcessingTimeChanged_ server)
  
-- | Signal when the 'serverProcessingTime' property value has changed.
serverProcessingTimeChanged_ :: MonadDES m => Server m s a b -> Signal m ()
{-# INLINABLE serverProcessingTimeChanged_ #-}
serverProcessingTimeChanged_ server =
  mapSignal (const ()) (serverTaskProcessed server)

-- | Return the statistics of the time when the server was locked while trying
-- to deliver the output. 
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'serverOutputWaitTimeChanged' and 'serverOutputWaitTimeChanged_'.
serverOutputWaitTime :: MonadDES m => Server m s a b -> Event m (SamplingStats Double)
{-# INLINABLE serverOutputWaitTime #-}
serverOutputWaitTime server =
  Event $ \p -> invokeEvent p $ readRef (serverOutputWaitTimeRef server)
  
-- | Signal when the 'serverOutputWaitTime' property value has changed.
serverOutputWaitTimeChanged :: MonadDES m => Server m s a b -> Signal m (SamplingStats Double)
{-# INLINABLE serverOutputWaitTimeChanged #-}
serverOutputWaitTimeChanged server =
  mapSignalM (const $ serverOutputWaitTime server) (serverOutputWaitTimeChanged_ server)
  
-- | Signal when the 'serverOutputWaitTime' property value has changed.
serverOutputWaitTimeChanged_ :: MonadDES m => Server m s a b -> Signal m ()
{-# INLINABLE serverOutputWaitTimeChanged_ #-}
serverOutputWaitTimeChanged_ server =
  mapSignal (const ()) (serverOutputProvided server)

-- | Return the statistics of the time spent by the server while it was preempted
-- waiting for the further proceeding.
--
-- The value returned changes discretely and it is usually delayed relative
-- to the current simulation time.
--
-- See also 'serverPreemptionTimeChanged' and 'serverPreemptionTimeChanged_'.
serverPreemptionTime :: MonadDES m => Server m s a b -> Event m (SamplingStats Double)
{-# INLINABLE serverPreemptionTime #-}
serverPreemptionTime server =
  Event $ \p -> invokeEvent p $ readRef (serverPreemptionTimeRef server)
  
-- | Signal when the 'serverPreemptionTime' property value has changed.
serverPreemptionTimeChanged :: MonadDES m => Server m s a b -> Signal m (SamplingStats Double)
{-# INLINABLE serverPreemptionTimeChanged #-}
serverPreemptionTimeChanged server =
  mapSignalM (const $ serverPreemptionTime server) (serverPreemptionTimeChanged_ server)
  
-- | Signal when the 'serverPreemptionTime' property value has changed.
serverPreemptionTimeChanged_ :: MonadDES m => Server m s a b -> Signal m ()
{-# INLINABLE serverPreemptionTimeChanged_ #-}
serverPreemptionTimeChanged_ server =
  mapSignal (const ()) (serverTaskPreemptionEnding server)

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the server was awaiting for the next input task.
--
-- This factor is calculated as
--
-- @
--   totalInputWaitTime \/ (totalInputWaitTime + totalProcessingTime + totalOutputWaitTime + totalPreemptionTime)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
--
-- See also 'serverInputWaitFactorChanged' and 'serverInputWaitFactorChanged_'.
serverInputWaitFactor :: MonadDES m => Server m s a b -> Event m Double
{-# INLINABLE serverInputWaitFactor #-}
serverInputWaitFactor server =
  Event $ \p ->
  do x1 <- invokeEvent p $ readRef (serverTotalInputWaitTimeRef server)
     x2 <- invokeEvent p $ readRef (serverTotalProcessingTimeRef server)
     x3 <- invokeEvent p $ readRef (serverTotalOutputWaitTimeRef server)
     x4 <- invokeEvent p $ readRef (serverTotalPreemptionTimeRef server)
     return (x1 / (x1 + x2 + x3 + x4))
  
-- | Signal when the 'serverInputWaitFactor' property value has changed.
serverInputWaitFactorChanged :: MonadDES m => Server m s a b -> Signal m Double
{-# INLINABLE serverInputWaitFactorChanged #-}
serverInputWaitFactorChanged server =
  mapSignalM (const $ serverInputWaitFactor server) (serverInputWaitFactorChanged_ server)
  
-- | Signal when the 'serverInputWaitFactor' property value has changed.
serverInputWaitFactorChanged_ :: MonadDES m => Server m s a b -> Signal m ()
{-# INLINABLE serverInputWaitFactorChanged_ #-}
serverInputWaitFactorChanged_ server =
  mapSignal (const ()) (serverInputReceived server) <>
  mapSignal (const ()) (serverTaskProcessed server) <>
  mapSignal (const ()) (serverOutputProvided server) <>
  mapSignal (const ()) (serverTaskPreemptionEnding server)

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the server was busy with direct processing its tasks.
--
-- This factor is calculated as
--
-- @
--   totalProcessingTime \/ (totalInputWaitTime + totalProcessingTime + totalOutputWaitTime + totalPreemptionTime)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
--
-- See also 'serverProcessingFactorChanged' and 'serverProcessingFactorChanged_'.
serverProcessingFactor :: MonadDES m => Server m s a b -> Event m Double
{-# INLINABLE serverProcessingFactor #-}
serverProcessingFactor server =
  Event $ \p ->
  do x1 <- invokeEvent p $ readRef (serverTotalInputWaitTimeRef server)
     x2 <- invokeEvent p $ readRef (serverTotalProcessingTimeRef server)
     x3 <- invokeEvent p $ readRef (serverTotalOutputWaitTimeRef server)
     x4 <- invokeEvent p $ readRef (serverTotalPreemptionTimeRef server)
     return (x2 / (x1 + x2 + x3 + x4))
  
-- | Signal when the 'serverProcessingFactor' property value has changed.
serverProcessingFactorChanged :: MonadDES m => Server m s a b -> Signal m Double
{-# INLINABLE serverProcessingFactorChanged #-}
serverProcessingFactorChanged server =
  mapSignalM (const $ serverProcessingFactor server) (serverProcessingFactorChanged_ server)
  
-- | Signal when the 'serverProcessingFactor' property value has changed.
serverProcessingFactorChanged_ :: MonadDES m => Server m s a b -> Signal m ()
{-# INLINABLE serverProcessingFactorChanged_ #-}
serverProcessingFactorChanged_ server =
  mapSignal (const ()) (serverInputReceived server) <>
  mapSignal (const ()) (serverTaskProcessed server) <>
  mapSignal (const ()) (serverOutputProvided server) <>
  mapSignal (const ()) (serverTaskPreemptionEnding server)

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the server was locked trying to deliver the output after the task is finished.
--
-- This factor is calculated as
--
-- @
--   totalOutputWaitTime \/ (totalInputWaitTime + totalProcessingTime + totalOutputWaitTime + totalPreemptionTime)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
--
-- See also 'serverOutputWaitFactorChanged' and 'serverOutputWaitFactorChanged_'.
serverOutputWaitFactor :: MonadDES m => Server m s a b -> Event m Double
{-# INLINABLE serverOutputWaitFactor #-}
serverOutputWaitFactor server =
  Event $ \p ->
  do x1 <- invokeEvent p $ readRef (serverTotalInputWaitTimeRef server)
     x2 <- invokeEvent p $ readRef (serverTotalProcessingTimeRef server)
     x3 <- invokeEvent p $ readRef (serverTotalOutputWaitTimeRef server)
     x4 <- invokeEvent p $ readRef (serverTotalPreemptionTimeRef server)
     return (x3 / (x1 + x2 + x3 + x4))
  
-- | Signal when the 'serverOutputWaitFactor' property value has changed.
serverOutputWaitFactorChanged :: MonadDES m => Server m s a b -> Signal m Double
{-# INLINABLE serverOutputWaitFactorChanged #-}
serverOutputWaitFactorChanged server =
  mapSignalM (const $ serverOutputWaitFactor server) (serverOutputWaitFactorChanged_ server)
  
-- | Signal when the 'serverOutputWaitFactor' property value has changed.
serverOutputWaitFactorChanged_ :: MonadDES m => Server m s a b -> Signal m ()
{-# INLINABLE serverOutputWaitFactorChanged_ #-}
serverOutputWaitFactorChanged_ server =
  mapSignal (const ()) (serverInputReceived server) <>
  mapSignal (const ()) (serverTaskProcessed server) <>
  mapSignal (const ()) (serverOutputProvided server) <>
  mapSignal (const ()) (serverTaskPreemptionEnding server)
  
-- | It returns the factor changing from 0 to 1, which estimates how often
-- the server was preempted waiting for the further proceeding.
--
-- This factor is calculated as
--
-- @
--   totalPreemptionTime \/ (totalInputWaitTime + totalProcessingTime + totalOutputWaitTime + totalPreemptionTime)
-- @
--
-- As before in this module, the value returned changes discretely and
-- it is usually delayed relative to the current simulation time.
--
-- See also 'serverPreemptionFactorChanged' and 'serverPreemptionFactorChanged_'.
serverPreemptionFactor :: MonadDES m => Server m s a b -> Event m Double
{-# INLINABLE serverPreemptionFactor #-}
serverPreemptionFactor server =
  Event $ \p ->
  do x1 <- invokeEvent p $ readRef (serverTotalInputWaitTimeRef server)
     x2 <- invokeEvent p $ readRef (serverTotalProcessingTimeRef server)
     x3 <- invokeEvent p $ readRef (serverTotalOutputWaitTimeRef server)
     x4 <- invokeEvent p $ readRef (serverTotalPreemptionTimeRef server)
     return (x4 / (x1 + x2 + x3 + x4))
  
-- | Signal when the 'serverPreemptionFactor' property value has changed.
serverPreemptionFactorChanged :: MonadDES m => Server m s a b -> Signal m Double
{-# INLINABLE serverPreemptionFactorChanged #-}
serverPreemptionFactorChanged server =
  mapSignalM (const $ serverPreemptionFactor server) (serverPreemptionFactorChanged_ server)
  
-- | Signal when the 'serverPreemptionFactor' property value has changed.
serverPreemptionFactorChanged_ :: MonadDES m => Server m s a b -> Signal m ()
{-# INLINABLE serverPreemptionFactorChanged_ #-}
serverPreemptionFactorChanged_ server =
  mapSignal (const ()) (serverInputReceived server) <>
  mapSignal (const ()) (serverTaskProcessed server) <>
  mapSignal (const ()) (serverOutputProvided server) <>
  mapSignal (const ()) (serverTaskPreemptionEnding server)

-- | Raised when the server receives a new input task.
serverInputReceived :: MonadDES m => Server m s a b -> Signal m a
{-# INLINABLE serverInputReceived #-}
serverInputReceived = publishSignal . serverInputReceivedSource

-- | Raised when the task processing was preempted.
serverTaskPreemptionBeginning :: MonadDES m => Server m s a b -> Signal m a
{-# INLINABLE serverTaskPreemptionBeginning #-}
serverTaskPreemptionBeginning = publishSignal . serverTaskPreemptionBeginningSource

-- | Raised when the task processing was proceeded after it had been preempeted earlier.
serverTaskPreemptionEnding :: MonadDES m => Server m s a b -> Signal m a
{-# INLINABLE serverTaskPreemptionEnding #-}
serverTaskPreemptionEnding = publishSignal . serverTaskPreemptionEndingSource

-- | Raised when the server has just processed the task.
serverTaskProcessed :: MonadDES m => Server m s a b -> Signal m (a, b)
{-# INLINABLE serverTaskProcessed #-}
serverTaskProcessed = publishSignal . serverTaskProcessedSource

-- | Raised when the server has just delivered the output.
serverOutputProvided :: MonadDES m => Server m s a b -> Signal m (a, b)
{-# INLINABLE serverOutputProvided #-}
serverOutputProvided = publishSignal . serverOutputProvidedSource

-- | Signal whenever any property of the server changes.
serverChanged_ :: MonadDES m => Server m s a b -> Signal m ()
{-# INLINABLE serverChanged_ #-}
serverChanged_ server =
  mapSignal (const ()) (serverInputReceived server) <>
  mapSignal (const ()) (serverTaskProcessed server) <>
  mapSignal (const ()) (serverOutputProvided server) <>
  mapSignal (const ()) (serverTaskPreemptionEnding server)

-- | Return the summary for the server with desciption of its
-- properties and activities using the specified indent.
serverSummary :: MonadDES m => Server m s a b -> Int -> Event m ShowS
{-# INLINABLE serverSummary #-}
serverSummary server indent =
  Event $ \p ->
  do tx1 <- invokeEvent p $ readRef (serverTotalInputWaitTimeRef server)
     tx2 <- invokeEvent p $ readRef (serverTotalProcessingTimeRef server)
     tx3 <- invokeEvent p $ readRef (serverTotalOutputWaitTimeRef server)
     tx4 <- invokeEvent p $ readRef (serverTotalPreemptionTimeRef server)
     let xf1 = tx1 / (tx1 + tx2 + tx3 + tx4)
         xf2 = tx2 / (tx1 + tx2 + tx3 + tx4)
         xf3 = tx3 / (tx1 + tx2 + tx3 + tx4)
         xf4 = tx4 / (tx1 + tx2 + tx3 + tx4)
     xs1 <- invokeEvent p $ readRef (serverInputWaitTimeRef server)
     xs2 <- invokeEvent p $ readRef (serverProcessingTimeRef server)
     xs3 <- invokeEvent p $ readRef (serverOutputWaitTimeRef server)
     xs4 <- invokeEvent p $ readRef (serverPreemptionTimeRef server)
     let tab = replicate indent ' '
     return $
       showString tab .
       showString "total input wait time (locked while awaiting the input) = " . shows tx1 .
       showString "\n" .
       showString tab .
       showString "total processing time = " . shows tx2 .
       showString "\n" .
       showString tab .
       showString "total output wait time (locked while delivering the output) = " . shows tx3 .
       showString "\n\n" .
       showString tab .
       showString "total preemption time = " . shows tx4 .
       showString "\n" .
       showString tab .
       showString "input wait factor (from 0 to 1) = " . shows xf1 .
       showString "\n" .
       showString tab .
       showString "processing factor (from 0 to 1) = " . shows xf2 .
       showString "\n" .
       showString tab .
       showString "output wait factor (from 0 to 1) = " . shows xf3 .
       showString "\n\n" .
       showString tab .
       showString "output preemption factor (from 0 to 1) = " . shows xf4 .
       showString "\n\n" .
       showString tab .
       showString "input wait time (locked while awaiting the input):\n\n" .
       samplingStatsSummary xs1 (2 + indent) .
       showString "\n\n" .
       showString tab .
       showString "processing time:\n\n" .
       samplingStatsSummary xs2 (2 + indent) .
       showString "\n\n" .
       showString tab .
       showString "output wait time (locked while delivering the output):\n\n" .
       samplingStatsSummary xs3 (2 + indent) .
       showString "\n\n" .
       showString tab .
       showString "preemption time (waiting for the proceeding after preemption):\n\n" .
       samplingStatsSummary xs4 (2 + indent)
