
-- |
-- Module     : Simulation.Aivika.Trans.Task
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The 'Task' value represents a process that was already started in background.
-- We can check the completion of the task, receive notifications about changing
-- its state and even suspend an outer process awaiting the final result of the task.
-- It complements the 'Process' monad as it allows immediately continuing the main
-- computation without suspension.
--
module Simulation.Aivika.Trans.Task
       (-- * Task
        Task,
        TaskResult(..),
        taskId,
        tryGetTaskResult,
        taskResult,
        taskResultReceived,
        taskProcess,
        cancelTask,
        taskCancelled,
        -- * Running Task
        runTask,
        runTaskUsingId,
        -- * Spawning Tasks
        spawnTask,
        spawnTaskUsingId,
        spawnTaskWith,
        spawnTaskUsingIdWith,
        -- * Enqueueing Task
        enqueueTask,
        enqueueTaskUsingId,
        -- * Parallel Tasks
        taskParallelResult,
        taskParallelProcess) where

import Data.Monoid

import Control.Monad
import Control.Monad.Trans
import Control.Exception

import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Cont
import Simulation.Aivika.Trans.Internal.Process
import Simulation.Aivika.Trans.Signal

-- | The task represents a process that was already started in background.
data Task m a =
  Task { taskId :: ProcessId m,
         -- ^ Return an identifier for the process that was launched
         -- in background for this task.
         taskResultRef :: Ref m (Maybe (TaskResult a)),
         -- ^ It contains the result of the computation.
         taskResultReceived :: Signal m (TaskResult a)
         -- ^ Return a signal that notifies about receiving
         -- the result of the task.
       }

-- | Represents the result of the task.
data TaskResult a = TaskCompleted a
                    -- ^ the task was successfully completed and
                    -- it returned the specified result
                  | TaskError SomeException
                    -- ^ the specified exception was raised when performing the task.
                  | TaskCancelled
                    -- ^ the task was cancelled

-- | Try to get the task result immediately without suspension.
tryGetTaskResult :: MonadDES m => Task m a -> Event m (Maybe (TaskResult a))
{-# INLINABLE tryGetTaskResult #-}
tryGetTaskResult t = readRef (taskResultRef t)

-- | Return the task result suspending the outer process if required.
taskResult :: MonadDES m => Task m a -> Process m (TaskResult a)
{-# INLINABLE taskResult #-}
taskResult t =
  do x <- liftEvent $ readRef (taskResultRef t)
     case x of
       Just x -> return x
       Nothing -> processAwait (taskResultReceived t)

-- | Cancel the task.
cancelTask :: MonadDES m => Task m a -> Event m ()
{-# INLINABLE cancelTask #-}
cancelTask t =
  cancelProcessWithId (taskId t)

-- | Test whether the task was cancelled.
taskCancelled :: MonadDES m => Task m a -> Event m Bool
{-# INLINABLE taskCancelled #-}
taskCancelled t =
  processCancelled (taskId t)

-- | Create a task by the specified process and its identifier.
newTaskUsingId :: MonadDES m => ProcessId m -> Process m a -> Event m (Task m a, Process m ())
{-# INLINABLE newTaskUsingId #-}
newTaskUsingId pid p =
  do r <- liftSimulation $ newRef Nothing
     s <- liftSimulation newSignalSource
     let t = Task { taskId = pid,
                    taskResultRef = r,
                    taskResultReceived = publishSignal s }
     let m =
           do v <- liftSimulation $ newRef TaskCancelled
              finallyProcess
                (catchProcess
                 (do a <- p
                     liftEvent $ writeRef v (TaskCompleted a))
                 (\e ->
                   liftEvent $ writeRef v (TaskError e)))
                (liftEvent $
                 do x <- readRef v
                    writeRef r (Just x)
                    triggerSignal s x)
     return (t, m)

-- | Run the process with the specified identifier in background and
-- return the corresponding task immediately.
runTaskUsingId :: MonadDES m => ProcessId m -> Process m a -> Event m (Task m a)
{-# INLINABLE runTaskUsingId #-}
runTaskUsingId pid p =
  do (t, m) <- newTaskUsingId pid p
     runProcessUsingId pid m
     return t

-- | Run the process in background and return the corresponding task immediately.
runTask :: MonadDES m => Process m a -> Event m (Task m a)
{-# INLINABLE runTask #-}
runTask p =
  do pid <- liftSimulation newProcessId
     runTaskUsingId pid p

-- | Enqueue the process that will be started at the specified time with the given
-- identifier from the event queue. It returns the corresponding task immediately.
enqueueTaskUsingId :: MonadDES m => Double -> ProcessId m -> Process m a -> Event m (Task m a)
{-# INLINABLE enqueueTaskUsingId #-}
enqueueTaskUsingId time pid p =
  do (t, m) <- newTaskUsingId pid p
     enqueueProcessUsingId time pid m
     return t

-- | Enqueue the process that will be started at the specified time from the event queue.
-- It returns the corresponding task immediately.
enqueueTask :: MonadDES m => Double -> Process m a -> Event m (Task m a)
{-# INLINABLE enqueueTask #-}
enqueueTask time p =
  do pid <- liftSimulation newProcessId
     enqueueTaskUsingId time pid p

-- | Run using the specified identifier a child process in background and return
-- immediately the corresponding task.
spawnTaskUsingId :: MonadDES m => ProcessId m -> Process m a -> Process m (Task m a)
{-# INLINABLE spawnTaskUsingId #-}
spawnTaskUsingId = spawnTaskUsingIdWith CancelTogether

-- | Run a child process in background and return immediately the corresponding task.
spawnTask :: MonadDES m => Process m a -> Process m (Task m a)
{-# INLINABLE spawnTask #-}
spawnTask = spawnTaskWith CancelTogether

-- | Run using the specified identifier a child process in background and return
-- immediately the corresponding task.
spawnTaskUsingIdWith :: MonadDES m => ContCancellation -> ProcessId m -> Process m a -> Process m (Task m a)
{-# INLINABLE spawnTaskUsingIdWith #-}
spawnTaskUsingIdWith cancellation pid p =
  do (t, m) <- liftEvent $ newTaskUsingId pid p
     spawnProcessUsingIdWith cancellation pid m
     return t

-- | Run a child process in background and return immediately the corresponding task.
spawnTaskWith :: MonadDES m => ContCancellation -> Process m a -> Process m (Task m a)
{-# INLINABLE spawnTaskWith #-}
spawnTaskWith cancellation p =
  do pid <- liftSimulation newProcessId
     spawnTaskUsingIdWith cancellation pid p

-- | Return an outer process that behaves like the task itself, for example,
-- when the task is cancelled if the outer process is cancelled. 
taskProcess :: MonadDES m => Task m a -> Process m a
{-# INLINABLE taskProcess #-}
taskProcess t =
  do x <- finallyProcess
          (taskResult t)
          (do pid <- processId
              liftEvent $
                do cancelled <- processCancelled pid
                   when cancelled $
                     cancelTask t)
     case x of
       TaskCompleted a -> return a
       TaskError e -> throwProcess e
       TaskCancelled -> cancelProcess

-- | Return the result of two parallel tasks.
taskParallelResult :: MonadDES m => Task m a -> Task m a -> Process m (TaskResult a, Task m a)
{-# INLINABLE taskParallelResult #-}
taskParallelResult t1 t2 =
  do x1 <- liftEvent $ readRef (taskResultRef t1)
     case x1 of
       Just x1 -> return (x1, t2)
       Nothing ->
         do x2 <- liftEvent $ readRef (taskResultRef t2)
            case x2 of
              Just x2 -> return (x2, t1)
              Nothing ->
                do let s1 = fmap Left $ taskResultReceived t1
                       s2 = fmap Right $ taskResultReceived t2
                   x <- processAwait $ s1 <> s2
                   case x of
                     Left x1  -> return (x1, t2)
                     Right x2 -> return (x2, t1) 

-- | Return an outer process for two parallel tasks returning the result of
-- the first finished task and the rest task in pair. 
taskParallelProcess :: MonadDES m => Task m a -> Task m a -> Process m (a, Task m a)
{-# INLINABLE taskParallelProcess #-}
taskParallelProcess t1 t2 =
  do (x, t) <-
       finallyProcess
       (taskParallelResult t1 t2)
       (do pid <- processId
           liftEvent $
             do cancelled <- processCancelled pid
                when cancelled $
                  do cancelTask t1
                     cancelTask t2)
     case x of
       TaskCompleted a -> return (a, t)
       TaskError e ->
         do liftEvent $ cancelTask t
            throwProcess e
       TaskCancelled ->
         do liftEvent $ cancelTask t
            cancelProcess
