
-- |
-- Module     : Simulation.Aivika.Trans.Task
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
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
        -- * Enqueueing Task
        enqueueTask,
        enqueueTaskUsingId) where

import Data.Monoid

import Control.Monad
import Control.Monad.Trans
import Control.Exception

import Simulation.Aivika.Trans.Specs
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Cont
import Simulation.Aivika.Trans.Internal.Process
import Simulation.Aivika.Trans.Internal.Signal

-- | The task represents a process that was already started in background.
data Task m a =
  Task { taskId :: ProcessId m,
         -- ^ Return an identifier for the process that was launched
         -- in background for this task.
         taskResultRef :: ProtoRef m (Maybe (TaskResult a)),
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
tryGetTaskResult :: Comp m => Task m a -> Event m (Maybe (TaskResult a))
tryGetTaskResult t =
  Event $ \p -> readProtoRef (taskResultRef t)

-- | Return the task result suspending the outer process if required.
taskResult :: Comp m => Task m a -> Process m (TaskResult a)
taskResult t =
  do x <- liftComp $ readProtoRef (taskResultRef t)
     case x of
       Just x -> return x
       Nothing -> processAwait (taskResultReceived t)

-- | Cancel the task.
cancelTask :: Comp m => Task m a -> Event m ()
cancelTask t =
  cancelProcessWithId (taskId t)

-- | Test whether the task was cancelled.
taskCancelled :: Comp m => Task m a -> Event m Bool
taskCancelled t =
  processCancelled (taskId t)

-- | Create a task by the specified process and its identifier.
newTaskUsingId :: Comp m => ProcessId m -> Process m a -> Event m (Task m a, Process m ())
newTaskUsingId pid p =
  do sn <- liftParameter simulationSession
     r <- liftComp $ newProtoRef sn Nothing
     s <- liftSimulation newSignalSource
     let t = Task { taskId = pid,
                    taskResultRef = r,
                    taskResultReceived = publishSignal s }
     let m =
           do v <- liftComp $ newProtoRef sn TaskCancelled
              finallyProcess
                (catchProcess
                 (do a <- p
                     liftComp $ writeProtoRef v (TaskCompleted a))
                 (\e ->
                   liftComp $ writeProtoRef v (TaskError e)))
                (liftEvent $
                 do x <- liftComp $ readProtoRef v
                    liftComp $ writeProtoRef r (Just x)
                    triggerSignal s x)
     return (t, m)

-- | Run the process with the specified identifier in background and
-- return the corresponded task immediately.
runTaskUsingId :: Comp m => ProcessId m -> Process m a -> Event m (Task m a)
runTaskUsingId pid p =
  do (t, m) <- newTaskUsingId pid p
     runProcessUsingId pid m
     return t

-- | Run the process in background and return the corresponded task immediately.
runTask :: Comp m => Process m a -> Event m (Task m a)
runTask p =
  do pid <- liftSimulation newProcessId
     runTaskUsingId pid p

-- | Enqueue the process that will be started at the specified time with the given
-- identifier from the event queue. It returns the corresponded task immediately.
enqueueTaskUsingId :: Comp m => Double -> ProcessId m -> Process m a -> Event m (Task m a)
enqueueTaskUsingId time pid p =
  do (t, m) <- newTaskUsingId pid p
     enqueueProcessUsingId time pid m
     return t

-- | Enqueue the process that will be started at the specified time from the event queue.
-- It returns the corresponded task immediately.
enqueueTask :: Comp m => Double -> Process m a -> Event m (Task m a)
enqueueTask time p =
  do pid <- liftSimulation newProcessId
     enqueueTaskUsingId time pid p

-- | Run using the specified identifier a child process in background and return
-- immediately the corresponded task.
spawnTaskUsingId :: Comp m => ContCancellation -> ProcessId m -> Process m a -> Process m (Task m a)
spawnTaskUsingId cancellation pid p =
  do (t, m) <- liftEvent $ newTaskUsingId pid p
     spawnProcessUsingId cancellation pid m
     return t

-- | Run a child process in background and return immediately the corresponded task.
spawnTask :: Comp m => ContCancellation -> Process m a -> Process m (Task m a)
spawnTask cancellation p =
  do pid <- liftSimulation newProcessId
     spawnTaskUsingId cancellation pid p

-- | Return an outer process that behaves like the task itself except for one thing:
-- if the outer process is cancelled then it is not enough to cancel the task. 
taskProcess :: Comp m => Task m a -> Process m a
taskProcess t =
  do x <- taskResult t
     case x of
       TaskCompleted a -> return a
       TaskError e -> throwProcess e
       TaskCancelled -> cancelProcess
