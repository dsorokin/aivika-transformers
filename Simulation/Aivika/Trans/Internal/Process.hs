
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- |
-- Module     : Simulation.Aivika.Trans.Internal.Process
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- A value in the 'Process' monad represents a discontinuous process that 
-- can suspend in any simulation time point and then resume later in the same 
-- or another time point. 
-- 
-- The process of this type can involve the 'Event', 'Dynamics' and 'Simulation'
-- computations. Moreover, a value in the @Process@ monad can be run within
-- the @Event@ computation.
--
-- A value of the 'ProcessId' type is just an identifier of such a process.
--
module Simulation.Aivika.Trans.Internal.Process
       (-- * Process Monad
        ProcessId,
        Process(..),
        ProcessLift(..),
        invokeProcess,
        -- * Running Process
        runProcess,
        runProcessUsingId,
        runProcessInStartTime,
        runProcessInStartTimeUsingId,
        runProcessInStopTime,
        runProcessInStopTimeUsingId,
        -- * Spawning Processes
        spawnProcess,
        spawnProcessUsingId,
        spawnProcessWith,
        spawnProcessUsingIdWith,
        -- * Enqueuing Process
        enqueueProcess,
        enqueueProcessUsingId,
        -- * Creating Process Identifier
        newProcessId,
        processId,
        processUsingId,
        -- * Holding, Interrupting, Passivating and Canceling Process
        holdProcess,
        interruptProcess,
        processInterrupted,
        processInterruptionTime,
        passivateProcess,
        passivateProcessBefore,
        processPassive,
        reactivateProcess,
        reactivateProcessImmediately,
        cancelProcessWithId,
        cancelProcess,
        processCancelled,
        processCancelling,
        whenCancellingProcess,
        -- * Awaiting Signal
        processAwait,
        -- * Preemption
        processPreemptionBegin,
        processPreemptionEnd,
        processPreemptionBeginning,
        processPreemptionEnding,
        -- * Yield of Process
        processYield,
        -- * Process Timeout
        timeoutProcess,
        timeoutProcessUsingId,
        -- * Parallelizing Processes
        processParallel,
        processParallelUsingIds,
        processParallel_,
        processParallelUsingIds_,
        -- * Exception Handling
        catchProcess,
        finallyProcess,
        throwProcess,
        -- * Utilities
        zipProcessParallel,
        zip3ProcessParallel,
        unzipProcess,
        -- * Memoizing Process
        memoProcess,
        -- * Never Ending Process
        neverProcess,
        -- * Retrying Computation
        retryProcess,
        -- * GoTo Statement
        transferProcess,
        -- * Debugging
        traceProcess) where

import Data.Maybe

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Catch as MC
import Control.Applicative

import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Cont
import Simulation.Aivika.Trans.Signal

-- | Represents a process identifier.
data ProcessId m = 
  ProcessId { processStarted :: Ref m Bool,
              processReactCont     :: Ref m (Maybe (ContParams m ())), 
              processContId  :: ContId m,
              processInterruptRef  :: Ref m Bool, 
              processInterruptCont :: Ref m (Maybe (ContParams m ())),
              processInterruptTime :: Ref m Double,
              processInterruptVersion :: Ref m Int }

-- | Specifies a discontinuous process that can suspend at any time
-- and then resume later.
newtype Process m a = Process (ProcessId m -> Cont m a)

-- | A type class to lift the 'Process' computation into other computations.
class ProcessLift t m where
  
  -- | Lift the specified 'Process' computation into another computation.
  liftProcess :: Process m a -> t m a

-- | Invoke the process computation.
invokeProcess :: ProcessId m -> Process m a -> Cont m a
{-# INLINE invokeProcess #-}
invokeProcess pid (Process m) = m pid

-- | Hold the process for the specified time period.
holdProcess :: MonadDES m => Double -> Process m ()
{-# INLINABLE holdProcess #-}
holdProcess dt =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do when (dt < 0) $
       error "Time period dt < 0: holdProcess"
     let x = processInterruptCont pid
         t = pointTime p + dt
     invokeEvent p $ writeRef x $ Just c
     invokeEvent p $ writeRef (processInterruptRef pid) False
     invokeEvent p $ writeRef (processInterruptTime pid) t
     v <- invokeEvent p $ readRef (processInterruptVersion pid)
     invokeEvent p $
       enqueueEvent t $
       Event $ \p ->
       do v' <- invokeEvent p $ readRef (processInterruptVersion pid)
          when (v == v') $ 
            do invokeEvent p $ writeRef x Nothing
               invokeEvent p $ resumeCont c ()

-- | Interrupt a process with the specified identifier if the process
-- is held by computation 'holdProcess'.
interruptProcess :: MonadDES m => ProcessId m -> Event m ()
{-# INLINABLE interruptProcess #-}
interruptProcess pid =
  Event $ \p ->
  do let x = processInterruptCont pid
     a <- invokeEvent p $ readRef x
     case a of
       Nothing -> return ()
       Just c ->
         do invokeEvent p $ writeRef x Nothing
            invokeEvent p $ writeRef (processInterruptRef pid) True
            invokeEvent p $ modifyRef (processInterruptVersion pid) $ (+) 1
            invokeEvent p $ enqueueEvent (pointTime p) $ resumeCont c ()
            
-- | Test whether the process with the specified identifier was interrupted.
processInterrupted :: MonadDES m => ProcessId m -> Event m Bool
{-# INLINABLE processInterrupted #-}
processInterrupted pid =
  Event $ \p ->
  invokeEvent p $ readRef (processInterruptRef pid)

-- | Return the expected interruption time after finishing the 'holdProcess' computation,
-- which value may change if the corresponding process is preempted.
processInterruptionTime :: MonadDES m => ProcessId m -> Event m (Maybe Double)
{-# INLINABLE processInterruptionTime #-}
processInterruptionTime pid =
  Event $ \p ->
  do let x = processInterruptCont pid
     a <- invokeEvent p $ readRef x
     case a of
       Just c  ->
         do t <- invokeEvent p $ readRef (processInterruptTime pid)
            return (Just t)
       Nothing ->
         return Nothing

-- | Define a reaction when the process with the specified identifier is preempted.
processPreempted :: MonadDES m => ProcessId m -> Event m ()
{-# INLINABLE processPreempted #-}
processPreempted pid =
  Event $ \p ->
  do let x = processInterruptCont pid
     a <- invokeEvent p $ readRef x
     case a of
       Just c ->
         do invokeEvent p $ writeRef x Nothing
            invokeEvent p $ writeRef (processInterruptRef pid) True
            invokeEvent p $ modifyRef (processInterruptVersion pid) $ (+) 1
            t <- invokeEvent p $ readRef (processInterruptTime pid)
            let dt = t - pointTime p
                c' = substituteCont c $ \a ->
                  Event $ \p ->
                  invokeEvent p $
                  invokeCont c $
                  invokeProcess pid $
                  holdProcess dt
            invokeEvent p $
              reenterCont c' ()
       Nothing ->
         do let x = processReactCont pid
            a <- invokeEvent p $ readRef x
            case a of
              Nothing ->
                return ()
              Just c ->
                do let c' = substituteCont c $ reenterCont c
                   invokeEvent p $ writeRef x $ Just c'

-- | Passivate the process.
passivateProcess :: MonadDES m => Process m ()
{-# INLINABLE passivateProcess #-}
passivateProcess =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do let x = processReactCont pid
     a <- invokeEvent p $ readRef x
     case a of
       Nothing -> invokeEvent p $ writeRef x $ Just c
       Just _  -> error "Cannot passivate the process twice: passivateProcess"

-- | Passivate the process before performing some action.
passivateProcessBefore :: MonadDES m => Event m () -> Process m ()
{-# INLINABLE passivateProcessBefore #-}
passivateProcessBefore m =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do let x = processReactCont pid
     a <- invokeEvent p $ readRef x
     case a of
       Nothing ->
         do invokeEvent p $ writeRef x $ Just c
            invokeEvent p m
       Just _  -> error "Cannot passivate the process twice: passivateProcessBefore"

-- | Test whether the process with the specified identifier is passivated.
processPassive :: MonadDES m => ProcessId m -> Event m Bool
{-# INLINABLE processPassive #-}
processPassive pid =
  Event $ \p ->
  do let x = processReactCont pid
     a <- invokeEvent p $ readRef x
     return $ isJust a

-- | Reactivate a process with the specified identifier.
reactivateProcess :: MonadDES m => ProcessId m -> Event m ()
{-# INLINABLE reactivateProcess #-}
reactivateProcess pid =
  Event $ \p ->
  do let x = processReactCont pid
     a <- invokeEvent p $ readRef x
     case a of
       Nothing -> 
         return ()
       Just c ->
         do invokeEvent p $ writeRef x Nothing
            invokeEvent p $ enqueueEvent (pointTime p) $ resumeCont c ()

-- | Reactivate a process with the specified identifier immediately.
reactivateProcessImmediately :: MonadDES m => ProcessId m -> Event m ()
{-# INLINABLE reactivateProcessImmediately #-}
reactivateProcessImmediately pid =
  Event $ \p ->
  do let x = processReactCont pid
     a <- invokeEvent p $ readRef x
     case a of
       Nothing -> 
         return ()
       Just c ->
         do invokeEvent p $ writeRef x Nothing
            invokeEvent p $ resumeCont c ()

-- | Prepare the processes identifier for running.
processIdPrepare :: MonadDES m => ProcessId m -> Event m ()
{-# INLINABLE processIdPrepare #-}
processIdPrepare pid =
  Event $ \p ->
  do y <- invokeEvent p $ readRef (processStarted pid)
     if y
       then error $
            "Another process with the specified identifier " ++
            "has been started already: processIdPrepare"
       else invokeEvent p $ writeRef (processStarted pid) True
     let signal = contSignal $ processContId pid
     invokeEvent p $
       handleSignal_ signal $ \e ->
       Event $ \p ->
       case e of
         ContCancellationInitiating ->
           do z <- invokeEvent p $ contCancellationActivated $ processContId pid
              when z $
                do invokeEvent p $ interruptProcess pid
                   invokeEvent p $ reactivateProcess pid
         ContPreemptionBeginning ->
           invokeEvent p $ processPreempted pid
         ContPreemptionEnding ->
           return ()

-- | Run immediately the process. A new 'ProcessId' identifier will be
-- assigned to the process.
--            
-- To run the process at the specified time, you can use
-- the 'enqueueProcess' function.
runProcess :: MonadDES m => Process m () -> Event m ()
{-# INLINABLE runProcess #-}
runProcess p =
  do pid <- liftSimulation newProcessId
     runProcessUsingId pid p
             
-- | Run immediately the process with the specified identifier.
-- It will be more efficient than as you would specify the process identifier
-- with help of the 'processUsingId' combinator and then would call 'runProcess'.
--            
-- To run the process at the specified time, you can use
-- the 'enqueueProcessUsingId' function.
runProcessUsingId :: MonadDES m => ProcessId m -> Process m () -> Event m ()
{-# INLINABLE runProcessUsingId #-}
runProcessUsingId pid p =
  do processIdPrepare pid
     runCont m cont econt ccont (processContId pid) False
       where cont  = return
             econt = throwEvent
             ccont = return
             m = invokeProcess pid p

-- | Run the process in the start time immediately involving all pending
-- 'CurrentEvents' in the computation too.
runProcessInStartTime :: MonadDES m => Process m () -> Simulation m ()
{-# INLINABLE runProcessInStartTime #-}
runProcessInStartTime = runEventInStartTime . runProcess

-- | Run the process in the start time immediately using the specified identifier
-- and involving all pending 'CurrentEvents' in the computation too.
runProcessInStartTimeUsingId :: MonadDES m => ProcessId m -> Process m () -> Simulation m ()
{-# INLINABLE runProcessInStartTimeUsingId #-}
runProcessInStartTimeUsingId pid p =
  runEventInStartTime $ runProcessUsingId pid p

-- | Run the process in the final simulation time immediately involving all
-- pending 'CurrentEvents' in the computation too.
runProcessInStopTime :: MonadDES m => Process m () -> Simulation m ()
{-# INLINABLE runProcessInStopTime #-}
runProcessInStopTime = runEventInStopTime . runProcess

-- | Run the process in the final simulation time immediately using 
-- the specified identifier and involving all pending 'CurrentEvents'
-- in the computation too.
runProcessInStopTimeUsingId :: MonadDES m => ProcessId m -> Process m () -> Simulation m ()
{-# INLINABLE runProcessInStopTimeUsingId #-}
runProcessInStopTimeUsingId pid p =
  runEventInStopTime $ runProcessUsingId pid p

-- | Enqueue the process that will be then started at the specified time
-- from the event queue.
enqueueProcess :: MonadDES m => Double -> Process m () -> Event m ()
{-# INLINABLE enqueueProcess #-}
enqueueProcess t p =
  enqueueEvent t $ runProcess p

-- | Enqueue the process that will be then started at the specified time
-- from the event queue.
enqueueProcessUsingId :: MonadDES m => Double -> ProcessId m -> Process m () -> Event m ()
{-# INLINABLE enqueueProcessUsingId #-}
enqueueProcessUsingId t pid p =
  enqueueEvent t $ runProcessUsingId pid p

-- | Return the current process identifier.
processId :: MonadDES m => Process m (ProcessId m)
{-# INLINABLE processId #-}
processId = Process return

-- | Create a new process identifier.
newProcessId :: MonadDES m => Simulation m (ProcessId m)
{-# INLINABLE newProcessId #-}
newProcessId =
  Simulation $ \r ->
  do x <- invokeSimulation r $ newRef Nothing
     y <- invokeSimulation r $ newRef False
     c <- invokeSimulation r $ newContId
     i <- invokeSimulation r $ newRef False
     z <- invokeSimulation r $ newRef Nothing
     t <- invokeSimulation r $ newRef 0
     v <- invokeSimulation r $ newRef 0
     return ProcessId { processStarted = y,
                        processReactCont     = x, 
                        processContId  = c, 
                        processInterruptRef  = i,
                        processInterruptCont = z,
                        processInterruptTime = t,
                        processInterruptVersion = v }

-- | Cancel a process with the specified identifier, interrupting it if needed.
cancelProcessWithId :: MonadDES m => ProcessId m -> Event m ()
{-# INLINABLE cancelProcessWithId #-}
cancelProcessWithId pid = contCancellationInitiate (processContId pid)

-- | The process cancels itself.
cancelProcess :: MonadDES m => Process m a
{-# INLINABLE cancelProcess #-}
cancelProcess =
  do pid <- processId
     liftEvent $ cancelProcessWithId pid
     throwProcess $ 
       (error "The process must be cancelled already: cancelProcess." :: SomeException)

-- | Test whether the process with the specified identifier was cancelled.
processCancelled :: MonadDES m => ProcessId m -> Event m Bool
{-# INLINABLE processCancelled #-}
processCancelled pid = contCancellationInitiated (processContId pid)

-- | Return a signal that notifies about cancelling the process with 
-- the specified identifier.
processCancelling :: MonadDES m => ProcessId m -> Signal m ()
{-# INLINABLE processCancelling #-}
processCancelling pid = contCancellationInitiating (processContId pid)

-- | Register a handler that will be invoked in case of cancelling the current process.
whenCancellingProcess :: MonadDES m => Event m () -> Process m ()
{-# INLINABLE whenCancellingProcess #-}
whenCancellingProcess h =
  Process $ \pid ->
  liftEvent $
  handleSignal_ (processCancelling pid) $ \() -> h

-- | Preempt a process with the specified identifier.
processPreemptionBegin :: MonadDES m => ProcessId m -> Event m ()
processPreemptionBegin pid = contPreemptionBegin (processContId pid)

-- | Proceed with the process with the specified identifier after it was preempted with help of 'preemptProcessBegin'.
processPreemptionEnd :: MonadDES m => ProcessId m -> Event m ()
processPreemptionEnd pid = contPreemptionEnd (processContId pid)

-- | Return a signal when the process is preempted.
processPreemptionBeginning :: MonadDES m => ProcessId m -> Signal m ()
processPreemptionBeginning pid = contPreemptionBeginning (processContId pid)

-- | Return a signal when the process is proceeded after it was preempted earlier.
processPreemptionEnding :: MonadDES m => ProcessId m -> Signal m ()
processPreemptionEnding pid = contPreemptionEnding (processContId pid)

instance MonadDES m => Eq (ProcessId m) where

  {-# INLINE (==) #-}
  x == y = processStarted x == processStarted y

instance MonadDES m => Monad (Process m) where

  {-# INLINE return #-}
  return a = Process $ \pid -> return a

  {-# INLINE (>>=) #-}
  (Process m) >>= k =
    Process $ \pid -> 
    do a <- m pid
       let Process m' = k a
       m' pid

instance MonadDES m => MonadCompTrans Process m where

  {-# INLINE liftComp #-}
  liftComp = Process . const . liftComp

instance MonadDES m => Functor (Process m) where
  
  {-# INLINE fmap #-}
  fmap f (Process x) = Process $ \pid -> fmap f $ x pid

instance MonadDES m => Applicative (Process m) where
  
  {-# INLINE pure #-}
  pure = Process . const . pure
  
  {-# INLINE (<*>) #-}
  (Process x) <*> (Process y) = Process $ \pid -> x pid <*> y pid

instance (MonadDES m, MonadIO m) => MonadIO (Process m) where
  
  {-# INLINE liftIO #-}
  liftIO = Process . const . liftIO

instance MonadDES m => ParameterLift Process m where

  {-# INLINE liftParameter #-}
  liftParameter = Process . const . liftParameter

instance MonadDES m => SimulationLift Process m where

  {-# INLINE liftSimulation #-}
  liftSimulation = Process . const . liftSimulation
  
instance MonadDES m => DynamicsLift Process m where

  {-# INLINE liftDynamics #-}
  liftDynamics = Process . const . liftDynamics
  
instance MonadDES m => EventLift Process m where

  {-# INLINE liftEvent #-}
  liftEvent = Process . const . liftEvent

instance MonadDES m => ProcessLift Process m where

  {-# INLINE liftProcess #-}
  liftProcess = id

instance MonadDES m => MC.MonadThrow (Process m) where

  {-# INLINE throwM #-}
  throwM = throwProcess

instance MonadDES m => MC.MonadCatch (Process m) where

  {-# INLINE catch #-}
  catch = catchProcess

-- | Exception handling within 'Process' computations.
catchProcess :: (MonadDES m, Exception e) => Process m a -> (e -> Process m a) -> Process m a
{-# INLINABLE catchProcess #-}
catchProcess (Process m) h =
  Process $ \pid ->
  catchCont (m pid) $ \e ->
  let Process m' = h e in m' pid
                           
-- | A computation with finalization part.
finallyProcess :: MonadDES m => Process m a -> Process m b -> Process m a
{-# INLINABLE finallyProcess #-}
finallyProcess (Process m) (Process m') =
  Process $ \pid ->
  finallyCont (m pid) (m' pid)

-- | Throw the exception with the further exception handling.
-- 
-- By some reason, an exception raised with help of the standard 'throw' function
-- is not handled properly within 'Process' computation, altough it will be still handled 
-- if it will be wrapped in the 'IO' monad. Therefore, you should use specialised
-- functions like the stated one that use the 'throw' function but within the 'IO' computation,
-- which allows already handling the exception.
throwProcess :: (MonadDES m, Exception e) => e -> Process m a
{-# INLINABLE throwProcess #-}
throwProcess = liftEvent . throwEvent

-- | Execute the specified computations in parallel within
-- the current computation and return their results. The cancellation
-- of any of the nested computations affects the current computation.
-- The exception raised in any of the nested computations is propagated
-- to the current computation as well.
--
-- Here word @parallel@ literally means that the computations are
-- actually executed on a single operating system thread but
-- they are processed simultaneously by the event queue.
--
-- New 'ProcessId' identifiers will be assigned to the started processes.
processParallel :: MonadDES m => [Process m a] -> Process m [a]
{-# INLINABLE processParallel #-}
processParallel xs =
  liftSimulation (processParallelCreateIds xs) >>= processParallelUsingIds 

-- | Like 'processParallel' but allows specifying the process identifiers.
-- It will be more efficient than as you would specify the process identifiers
-- with help of the 'processUsingId' combinator and then would call 'processParallel'.
processParallelUsingIds :: MonadDES m => [(ProcessId m, Process m a)] -> Process m [a]
{-# INLINABLE processParallelUsingIds #-}
processParallelUsingIds xs =
  Process $ \pid ->
  do liftEvent $ processParallelPrepare xs
     contParallel $
       flip map xs $ \(pid, m) ->
       (invokeProcess pid m, processContId pid)

-- | Like 'processParallel' but ignores the result.
processParallel_ :: MonadDES m => [Process m a] -> Process m ()
{-# INLINABLE processParallel_ #-}
processParallel_ xs =
  liftSimulation (processParallelCreateIds xs) >>= processParallelUsingIds_ 

-- | Like 'processParallelUsingIds' but ignores the result.
processParallelUsingIds_ :: MonadDES m => [(ProcessId m, Process m a)] -> Process m ()
{-# INLINABLE processParallelUsingIds_ #-}
processParallelUsingIds_ xs =
  Process $ \pid ->
  do liftEvent $ processParallelPrepare xs
     contParallel_ $
       flip map xs $ \(pid, m) ->
       (invokeProcess pid m, processContId pid)

-- | Create the new process identifiers.
processParallelCreateIds :: MonadDES m => [Process m a] -> Simulation m [(ProcessId m, Process m a)]
{-# INLINABLE processParallelCreateIds #-}
processParallelCreateIds xs =
  do pids <- liftSimulation $ forM xs $ const newProcessId
     return $ zip pids xs

-- | Prepare the processes for parallel execution.
processParallelPrepare :: MonadDES m => [(ProcessId m, Process m a)] -> Event m ()
{-# INLINABLE processParallelPrepare #-}
processParallelPrepare xs =
  Event $ \p ->
  forM_ xs $ invokeEvent p . processIdPrepare . fst

-- | Allow calling the process with the specified identifier.
-- It creates a nested process when canceling any of two, or raising an
-- @IO@ exception in any of the both, affects the 'Process' computation.
--
-- At the same time, the interruption has no such effect as it requires
-- explicit specifying the 'ProcessId' identifier of the nested process itself,
-- that is the nested process cannot be interrupted using only the parent
-- process identifier.
processUsingId :: MonadDES m => ProcessId m -> Process m a -> Process m a
{-# INLINABLE processUsingId #-}
processUsingId pid x =
  Process $ \pid' ->
  do liftEvent $ processIdPrepare pid
     rerunCont (invokeProcess pid x) (processContId pid)

-- | Spawn the child process. In case of cancelling one of the processes,
-- other process will be cancelled too.
spawnProcess :: MonadDES m => Process m () -> Process m ()
{-# INLINABLE spawnProcess #-}
spawnProcess = spawnProcessWith CancelTogether

-- | Spawn the child process specifying the process identifier.
-- In case of cancelling one of the processes, other process will be cancelled too.
spawnProcessUsingId :: MonadDES m => ProcessId m -> Process m () -> Process m ()
{-# INLINABLE spawnProcessUsingId #-}
spawnProcessUsingId = spawnProcessUsingIdWith CancelTogether

-- | Spawn the child process specifying how the child and parent processes
-- should be cancelled in case of need.
spawnProcessWith :: MonadDES m => ContCancellation -> Process m () -> Process m ()
{-# INLINABLE spawnProcessWith #-}
spawnProcessWith cancellation x =
  do pid <- liftSimulation newProcessId
     spawnProcessUsingIdWith cancellation pid x

-- | Spawn the child process specifying how the child and parent processes
-- should be cancelled in case of need.
spawnProcessUsingIdWith :: MonadDES m => ContCancellation -> ProcessId m -> Process m () -> Process m ()
{-# INLINABLE spawnProcessUsingIdWith #-}
spawnProcessUsingIdWith cancellation pid x =
  Process $ \pid' ->
  do liftEvent $ processIdPrepare pid
     spawnCont cancellation (invokeProcess pid x) (processContId pid)

-- | Await the signal.
processAwait :: MonadDES m => Signal m a -> Process m a
{-# INLINABLE processAwait #-}
processAwait signal =
  Process $ \pid -> contAwait signal

-- | The result of memoization.
data MemoResult a = MemoComputed a
                  | MemoError IOException
                  | MemoCancelled

-- | Memoize the process so that it would always return the same value
-- within the simulation run.
memoProcess :: MonadDES m => Process m a -> Simulation m (Process m a)
{-# INLINABLE memoProcess #-}
memoProcess x =
  Simulation $ \r ->
  do started  <- invokeSimulation r $ newRef False
     computed <- invokeSimulation r newSignalSource
     value    <- invokeSimulation r $ newRef Nothing
     let result =
           do Just x <- liftEvent $ readRef value
              case x of
                MemoComputed a -> return a
                MemoError e    -> throwProcess e
                MemoCancelled  -> cancelProcess
     return $
       do v <- liftEvent $ readRef value
          case v of
            Just _ -> result
            Nothing ->
              do f <- liftEvent $ readRef started
                 case f of
                   True ->
                     do processAwait $ publishSignal computed
                        result
                   False ->
                     do liftEvent $ writeRef started True
                        r <- liftSimulation $ newRef MemoCancelled
                        finallyProcess
                          (catchProcess
                           (do a <- x    -- compute only once!
                               liftEvent $ writeRef r (MemoComputed a))
                           (\e ->
                             liftEvent $ writeRef r (MemoError e)))
                          (liftEvent $
                           do x <- readRef r
                              writeRef value (Just x)
                              triggerSignal computed ())
                        result

-- | Zip two parallel processes waiting for the both.
zipProcessParallel :: MonadDES m => Process m a -> Process m b -> Process m (a, b)
{-# INLINABLE zipProcessParallel #-}
zipProcessParallel x y =
  do [Left a, Right b] <- processParallel [fmap Left x, fmap Right y]
     return (a, b)

-- | Zip three parallel processes waiting for their results.
zip3ProcessParallel :: MonadDES m => Process m a -> Process m b -> Process m c -> Process m (a, b, c)
{-# INLINABLE zip3ProcessParallel #-}
zip3ProcessParallel x y z =
  do [Left a,
      Right (Left b),
      Right (Right c)] <-
       processParallel [fmap Left x,
                        fmap (Right . Left) y,
                        fmap (Right . Right) z]
     return (a, b, c)

-- | Unzip the process using memoization so that the both returned
-- processes could be applied independently, although they will refer
-- to the same pair of values.
unzipProcess :: MonadDES m => Process m (a, b) -> Simulation m (Process m a, Process m b)
{-# INLINABLE unzipProcess #-}
unzipProcess xy =
  do xy' <- memoProcess xy
     return (fmap fst xy', fmap snd xy')

-- | Try to run the child process within the specified timeout.
-- If the process will finish successfully within this time interval then
-- the result wrapped in 'Just' will be returned; otherwise, the child process
-- will be cancelled and 'Nothing' will be returned.
--
-- If an exception is raised in the child process then it is propagated to
-- the parent computation as well.
--
-- A cancellation of the child process doesn't lead to cancelling the parent process.
-- Then 'Nothing' is returned within the computation.
--
-- This is a heavy-weight operation destined for working with arbitrary discontinuous
-- processes. Please consider using a more light-weight function 'interruptProcess' or else
-- 'cancelProcessWithId' whenever possible.
timeoutProcess :: MonadDES m => Double -> Process m a -> Process m (Maybe a)
{-# INLINABLE timeoutProcess #-}
timeoutProcess timeout p =
  do pid <- liftSimulation newProcessId
     timeoutProcessUsingId timeout pid p

-- | Try to run the child process with the given identifier within the specified timeout.
-- If the process will finish successfully within this time interval then
-- the result wrapped in 'Just' will be returned; otherwise, the child process
-- will be cancelled and 'Nothing' will be returned.
--
-- If an exception is raised in the child process then it is propagated to
-- the parent computation as well.
--
-- A cancellation of the child process doesn't lead to cancelling the parent process.
-- Then 'Nothing' is returned within the computation.
--
-- This is a heavy-weight operation destined for working with arbitrary discontinuous
-- processes. Please consider using a more light-weight function 'interruptProcess' or else
-- 'cancelProcessWithId' whenever possible.
timeoutProcessUsingId :: MonadDES m => Double -> ProcessId m -> Process m a -> Process m (Maybe a)
{-# INLINABLE timeoutProcessUsingId #-}
timeoutProcessUsingId timeout pid p =
  do s <- liftSimulation newSignalSource
     timeoutPid <- liftSimulation newProcessId
     spawnProcessUsingIdWith CancelChildAfterParent timeoutPid $
       do holdProcess timeout
          liftEvent $
            cancelProcessWithId pid
     spawnProcessUsingIdWith CancelChildAfterParent pid $
       do r <- liftSimulation $ newRef Nothing
          finallyProcess
            (catchProcess
             (do a <- p
                 liftEvent $ writeRef r $ Just (Right a))
             (\e ->
               liftEvent $ writeRef r $ Just (Left e)))
            (liftEvent $
             do cancelProcessWithId timeoutPid
                x <- readRef r
                triggerSignal s x)
     x <- processAwait $ publishSignal s
     case x of
       Nothing -> return Nothing
       Just (Right a) -> return (Just a)
       Just (Left (SomeException e)) -> throwProcess e

-- | Yield to allow other 'Process' and 'Event' computations to run
-- at the current simulation time point.
processYield :: MonadDES m => Process m ()
{-# INLINABLE processYield #-}
processYield =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  invokeEvent p $
  enqueueEvent (pointTime p) $
  resumeCont c ()

-- | A computation that never computes the result. It behaves like a black hole for
-- the discontinuous process, although such a process can still be canceled outside
-- (see 'cancelProcessWithId'), but then only its finalization parts (see 'finallyProcess')
-- will be called, usually, to release the resources acquired before.
neverProcess :: MonadDES m => Process m a
{-# INLINABLE neverProcess #-}
neverProcess =
  Process $ \pid ->
  Cont $ \c ->
  let signal = processCancelling pid
  in handleSignal_ signal $ \_ ->
     resumeCont c $ error "It must never be computed: neverProcess"

-- | Retry the current computation as possible, using the specified argument
-- as a 'SimulationRetry' exception message in case of failure.
retryProcess :: MonadDES m => String -> Process m a
{-# INLINABLE retryProcess #-}
retryProcess = liftEvent . retryEvent

-- | Like the GoTo statement it transfers the direction of computation,
-- but raises an exception when used within 'catchProcess' or 'finallyProcess'.
transferProcess :: MonadDES m => Process m () -> Process m a
{-# INLINABLE transferProcess #-}
transferProcess (Process m) =
  Process $ \pid -> transferCont (m pid)

-- | Show the debug message with the current simulation time.
traceProcess :: MonadDES m => String -> Process m a -> Process m a
{-# INLINABLE traceProcess #-}
traceProcess message m =
  Process $ \pid ->
  traceCont message $
  invokeProcess pid m
