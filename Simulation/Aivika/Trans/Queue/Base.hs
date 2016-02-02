
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.Queue.Base
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- This module defines an optimised finite queue, which has no counters nor signals.
--
module Simulation.Aivika.Trans.Queue.Base
       (-- * Queue Types
        FCFSQueue,
        LCFSQueue,
        SIROQueue,
        PriorityQueue,
        Queue,
        -- * Creating Queue
        newFCFSQueue,
        newLCFSQueue,
        newSIROQueue,
        newPriorityQueue,
        newQueue,
        -- * Queue Properties and Activities
        enqueueStrategy,
        enqueueStoringStrategy,
        dequeueStrategy,
        queueNull,
        queueFull,
        queueMaxCount,
        queueCount,
        -- * Dequeuing and Enqueuing
        dequeue,
        dequeueWithOutputPriority,
        tryDequeue,
        enqueue,
        enqueueWithInputPriority,
        enqueueWithStoringPriority,
        enqueueWithInputStoringPriorities,
        tryEnqueue,
        tryEnqueueWithStoringPriority,
        queueDelete,
        queueDelete_,
        queueDeleteBy,
        queueDeleteBy_,
        queueContains,
        queueContainsBy,
        clearQueue) where

import Data.Monoid
import Data.Maybe

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Process
import Simulation.Aivika.Trans.Resource.Base
import Simulation.Aivika.Trans.QueueStrategy

-- | A type synonym for the ordinary FIFO queue also known as the FCFS
-- (First Come - First Serviced) queue.
type FCFSQueue m a = Queue m FCFS FCFS FCFS a

-- | A type synonym for the ordinary LIFO queue also known as the LCFS
-- (Last Come - First Serviced) queue.
type LCFSQueue m a = Queue m FCFS LCFS FCFS a

-- | A type synonym for the SIRO (Serviced in Random Order) queue.
type SIROQueue m a = Queue m FCFS SIRO FCFS a

-- | A type synonym for the queue with static priorities applied when
-- storing the elements in the queue.
type PriorityQueue m a = Queue m FCFS StaticPriorities FCFS a

-- | Represents a queue using the specified strategies for enqueueing (input), @si@,
-- internal storing (in memory), @sm@, and dequeueing (output), @so@, where @a@ denotes
-- the type of items stored in the queue. Type @m@ denotes the underlying monad within
-- which the simulation executes.
data Queue m si sm so a =
  Queue { queueMaxCount :: Int,
          -- ^ The queue capacity.
          enqueueStrategy :: si,
          -- ^ The strategy applied to the enqueueing (input) processes when the queue is full.
          enqueueStoringStrategy :: sm,
          -- ^ The strategy applied when storing (in memory) items in the queue.
          dequeueStrategy :: so,
          -- ^ The strategy applied to the dequeueing (output) processes when the queue is empty.
          enqueueRes :: Resource m si,
          queueStore :: StrategyQueue m sm a,
          dequeueRes :: Resource m so,
          queueCountRef :: Ref m Int
        }

-- | Create a new FCFS queue with the specified capacity.  
newFCFSQueue :: MonadDES m => Int -> Simulation m (FCFSQueue m a)
{-# INLINABLE newFCFSQueue #-}
newFCFSQueue = newQueue FCFS FCFS FCFS
  
-- | Create a new LCFS queue with the specified capacity.  
newLCFSQueue :: MonadDES m => Int -> Simulation m (LCFSQueue m a)
{-# INLINABLE newLCFSQueue #-}
newLCFSQueue = newQueue FCFS LCFS FCFS
  
-- | Create a new SIRO queue with the specified capacity.  
newSIROQueue :: (MonadDES m, QueueStrategy m SIRO) => Int -> Simulation m (SIROQueue m a)
{-# INLINABLE newSIROQueue #-}
newSIROQueue = newQueue FCFS SIRO FCFS
  
-- | Create a new priority queue with the specified capacity.  
newPriorityQueue :: (MonadDES m, QueueStrategy m StaticPriorities) => Int -> Simulation m (PriorityQueue m a)
{-# INLINABLE newPriorityQueue #-}
newPriorityQueue = newQueue FCFS StaticPriorities FCFS
  
-- | Create a new queue with the specified strategies and capacity.  
newQueue :: (MonadDES m,
             QueueStrategy m si,
             QueueStrategy m sm,
             QueueStrategy m so) =>
            si
            -- ^ the strategy applied to the enqueueing (input) processes when the queue is full
            -> sm
            -- ^ the strategy applied when storing items in the queue
            -> so
            -- ^ the strategy applied to the dequeueing (output) processes when the queue is empty
            -> Int
            -- ^ the queue capacity
            -> Simulation m (Queue m si sm so a)
{-# INLINABLE newQueue #-}
newQueue si sm so count =
  do i  <- newRef 0
     ri <- newResourceWithMaxCount si count (Just count)
     qm <- newStrategyQueue sm
     ro <- newResourceWithMaxCount so 0 (Just count)
     return Queue { queueMaxCount = count,
                    enqueueStrategy = si,
                    enqueueStoringStrategy = sm,
                    dequeueStrategy = so,
                    enqueueRes = ri,
                    queueStore = qm,
                    dequeueRes = ro,
                    queueCountRef = i }
  
-- | Test whether the queue is empty.
--
-- See also 'queueNullChanged' and 'queueNullChanged_'.
queueNull :: MonadDES m => Queue m si sm so a -> Event m Bool
{-# INLINABLE queueNull #-}
queueNull q =
  Event $ \p ->
  do n <- invokeEvent p $ readRef (queueCountRef q)
     return (n == 0)
  
-- | Test whether the queue is full.
--
-- See also 'queueFullChanged' and 'queueFullChanged_'.
queueFull :: MonadDES m => Queue m si sm so a -> Event m Bool
{-# INLINABLE queueFull #-}
queueFull q =
  Event $ \p ->
  do n <- invokeEvent p $ readRef (queueCountRef q)
     return (n == queueMaxCount q)
  
-- | Return the current queue size.
--
-- See also 'queueCountStats', 'queueCountChanged' and 'queueCountChanged_'.
queueCount :: MonadDES m => Queue m si sm so a -> Event m Int
{-# INLINABLE queueCount #-}
queueCount q =
  Event $ \p -> invokeEvent p $ readRef (queueCountRef q)

-- | Dequeue suspending the process if the queue is empty.
dequeue :: (MonadDES m,
            DequeueStrategy m si,
            DequeueStrategy m sm,
            EnqueueStrategy m so)
           => Queue m si sm so a
           -- ^ the queue
           -> Process m a
           -- ^ the dequeued value
{-# INLINABLE dequeue #-}
dequeue q =
  do requestResource (dequeueRes q)
     liftEvent $ dequeueExtract q
  
-- | Dequeue with the output priority suspending the process if the queue is empty.
dequeueWithOutputPriority :: (MonadDES m,
                              DequeueStrategy m si,
                              DequeueStrategy m sm,
                              PriorityQueueStrategy m so po)
                             => Queue m si sm so a
                             -- ^ the queue
                             -> po
                             -- ^ the priority for output
                             -> Process m a
                             -- ^ the dequeued value
{-# INLINABLE dequeueWithOutputPriority #-}
dequeueWithOutputPriority q po =
  do requestResourceWithPriority (dequeueRes q) po
     liftEvent $ dequeueExtract q
  
-- | Try to dequeue immediately.
tryDequeue :: (MonadDES m,
               DequeueStrategy m si,
               DequeueStrategy m sm)
              => Queue m si sm so a
              -- ^ the queue
              -> Event m (Maybe a)
              -- ^ the dequeued value of 'Nothing'
{-# INLINABLE tryDequeue #-}
tryDequeue q =
  do x <- tryRequestResourceWithinEvent (dequeueRes q)
     if x 
       then fmap Just $ dequeueExtract q
       else return Nothing

-- | Remove the item from the queue and return a flag indicating
-- whether the item was found and actually removed.
queueDelete :: (MonadDES m,
                Eq a,
                DequeueStrategy m si,
                DeletingQueueStrategy m sm,
                DequeueStrategy m so)
               => Queue m si sm so a
               -- ^ the queue
               -> a
               -- ^ the item to remove from the queue
               -> Event m Bool
               -- ^ whether the item was found and removed
{-# INLINABLE queueDelete #-}
queueDelete q a = fmap isJust $ queueDeleteBy q (== a)

-- | Remove the specified item from the queue.
queueDelete_ :: (MonadDES m,
                 Eq a,
                 DequeueStrategy m si,
                 DeletingQueueStrategy m sm,
                 DequeueStrategy m so)
                => Queue m si sm so a
                -- ^ the queue
                -> a
                -- ^ the item to remove from the queue
                -> Event m ()
{-# INLINABLE queueDelete_ #-}
queueDelete_ q a = fmap (const ()) $ queueDeleteBy q (== a)

-- | Remove an item satisfying the specified predicate and return the item if found.
queueDeleteBy :: (MonadDES m,
                  DequeueStrategy m si,
                  DeletingQueueStrategy m sm,
                  DequeueStrategy m so)
                 => Queue m si sm so a
                 -- ^ the queue
                 -> (a -> Bool)
                 -- ^ the predicate
                 -> Event m (Maybe a)
{-# INLINABLE queueDeleteBy #-}
queueDeleteBy q pred =
  do x <- tryRequestResourceWithinEvent (dequeueRes q)
     if x
       then do i <- strategyQueueDeleteBy (queueStore q) pred
               case i of
                 Nothing ->
                   do releaseResourceWithinEvent (dequeueRes q)
                      return Nothing
                 Just i ->
                   fmap Just $ dequeuePostExtract q i
       else return Nothing
               
-- | Remove an item satisfying the specified predicate.
queueDeleteBy_ :: (MonadDES m,
                   DequeueStrategy m si,
                   DeletingQueueStrategy m sm,
                   DequeueStrategy m so)
                  => Queue m si sm so a
                  -- ^ the queue
                  -> (a -> Bool)
                  -- ^ the predicate
                  -> Event m ()
{-# INLINABLE queueDeleteBy_ #-}
queueDeleteBy_ q pred = fmap (const ()) $ queueDeleteBy q pred

-- | Detect whether the item is contained in the queue.
queueContains :: (MonadDES m,
                  Eq a,
                  DeletingQueueStrategy m sm)
                 => Queue m si sm so a
                 -- ^ the queue
                 -> a
                 -- ^ the item to search the queue for
                 -> Event m Bool
                 -- ^ whether the item was found
{-# INLINABLE queueContains #-}
queueContains q a = fmap isJust $ queueContainsBy q (== a)

-- | Detect whether an item satisfying the specified predicate is contained in the queue.
queueContainsBy :: (MonadDES m,
                    DeletingQueueStrategy m sm)
                   => Queue m si sm so a
                   -- ^ the queue
                   -> (a -> Bool)
                   -- ^ the predicate
                   -> Event m (Maybe a)
                   -- ^ the item if it was found
{-# INLINABLE queueContainsBy #-}
queueContainsBy q pred =
  strategyQueueContainsBy (queueStore q) pred

-- | Clear the queue immediately.
clearQueue :: (MonadDES m,
               DequeueStrategy m si,
               DequeueStrategy m sm)
              => Queue m si sm so a
              -- ^ the queue
              -> Event m ()
{-# INLINABLE clearQueue #-}
clearQueue q =
  do x <- tryDequeue q
     case x of
       Nothing -> return ()
       Just a  -> clearQueue q
              
-- | Enqueue the item suspending the process if the queue is full.  
enqueue :: (MonadDES m,
            EnqueueStrategy m si,
            EnqueueStrategy m sm,
            DequeueStrategy m so)
           => Queue m si sm so a
           -- ^ the queue
           -> a
           -- ^ the item to enqueue
           -> Process m ()
{-# INLINABLE enqueue #-}
enqueue q a =
  do requestResource (enqueueRes q)
     liftEvent $ enqueueStore q a
     
-- | Enqueue with the input priority the item suspending the process if the queue is full.  
enqueueWithInputPriority :: (MonadDES m,
                             PriorityQueueStrategy m si pi,
                             EnqueueStrategy m sm,
                             DequeueStrategy m so)
                            => Queue m si sm so a
                            -- ^ the queue
                            -> pi
                            -- ^ the priority for input
                            -> a
                            -- ^ the item to enqueue
                            -> Process m ()
{-# INLINABLE enqueueWithInputPriority #-}
enqueueWithInputPriority q pi a =
  do requestResourceWithPriority (enqueueRes q) pi
     liftEvent $ enqueueStore q a
     
-- | Enqueue with the storing priority the item suspending the process if the queue is full.  
enqueueWithStoringPriority :: (MonadDES m,
                               EnqueueStrategy m si,
                               PriorityQueueStrategy m sm pm,
                               DequeueStrategy m so)
                              => Queue m si sm so a
                              -- ^ the queue
                              -> pm
                              -- ^ the priority for storing
                              -> a
                              -- ^ the item to enqueue
                              -> Process m ()
{-# INLINABLE enqueueWithStoringPriority #-}
enqueueWithStoringPriority q pm a =
  do requestResource (enqueueRes q)
     liftEvent $ enqueueStoreWithPriority q pm a
     
-- | Enqueue with the input and storing priorities the item suspending the process if the queue is full.  
enqueueWithInputStoringPriorities :: (MonadDES m,
                                      PriorityQueueStrategy m si pi,
                                      PriorityQueueStrategy m sm pm,
                                      DequeueStrategy m so)
                                     => Queue m si sm so a
                                     -- ^ the queue
                                     -> pi
                                     -- ^ the priority for input
                                     -> pm
                                     -- ^ the priority for storing
                                     -> a
                                     -- ^ the item to enqueue
                                     -> Process m ()
{-# INLINABLE enqueueWithInputStoringPriorities #-}
enqueueWithInputStoringPriorities q pi pm a =
  do requestResourceWithPriority (enqueueRes q) pi
     liftEvent $ enqueueStoreWithPriority q pm a
     
-- | Try to enqueue the item. Return 'False' in the monad if the queue is full.
tryEnqueue :: (MonadDES m,
               EnqueueStrategy m sm,
               DequeueStrategy m so)
              => Queue m si sm so a
              -- ^ the queue
              -> a
              -- ^ the item which we try to enqueue
              -> Event m Bool
{-# INLINABLE tryEnqueue #-}
tryEnqueue q a =
  do x <- tryRequestResourceWithinEvent (enqueueRes q)
     if x 
       then do enqueueStore q a
               return True
       else return False

-- | Try to enqueue with the storing priority the item. Return 'False' in
-- the monad if the queue is full.
tryEnqueueWithStoringPriority :: (MonadDES m,
                                  PriorityQueueStrategy m sm pm,
                                  DequeueStrategy m so)
                                 => Queue m si sm so a
                                 -- ^ the queue
                                 -> pm
                                 -- ^ the priority for storing
                                 -> a
                                 -- ^ the item which we try to enqueue
                                 -> Event m Bool
{-# INLINABLE tryEnqueueWithStoringPriority #-}
tryEnqueueWithStoringPriority q pm a =
  do x <- tryRequestResourceWithinEvent (enqueueRes q)
     if x 
       then do enqueueStoreWithPriority q pm a
               return True
       else return False

-- | Store the item.
enqueueStore :: (MonadDES m,
                 EnqueueStrategy m sm,
                 DequeueStrategy m so)
                => Queue m si sm so a
                -- ^ the queue
                -> a
                -- ^ the item to be stored
                -> Event m ()
{-# INLINE enqueueStore #-}
enqueueStore q a =
  Event $ \p ->
  do invokeEvent p $
       strategyEnqueue (queueStore q) a
     c <- invokeEvent p $
          readRef (queueCountRef q)
     let c' = c + 1
     c' `seq` invokeEvent p $
       writeRef (queueCountRef q) c'
     invokeEvent p $
       releaseResourceWithinEvent (dequeueRes q)

-- | Store with the priority the item.
enqueueStoreWithPriority :: (MonadDES m,
                             PriorityQueueStrategy m sm pm,
                             DequeueStrategy m so)
                            => Queue m si sm so a
                            -- ^ the queue
                            -> pm
                            -- ^ the priority for storing
                            -> a
                            -- ^ the item to be enqueued
                            -> Event m ()
{-# INLINE enqueueStoreWithPriority #-}
enqueueStoreWithPriority q pm a =
  Event $ \p ->
  do invokeEvent p $
       strategyEnqueueWithPriority (queueStore q) pm a
     c <- invokeEvent p $
          readRef (queueCountRef q)
     let c' = c + 1
     c' `seq` invokeEvent p $
       writeRef (queueCountRef q) c'
     invokeEvent p $
       releaseResourceWithinEvent (dequeueRes q)

-- | Extract an item for the dequeuing request.  
dequeueExtract :: (MonadDES m,
                   DequeueStrategy m si,
                   DequeueStrategy m sm)
                  => Queue m si sm so a
                  -- ^ the queue
                  -> Event m a
                  -- ^ the dequeued value
{-# INLINE dequeueExtract #-}
dequeueExtract q =
  Event $ \p ->
  do a <- invokeEvent p $
          strategyDequeue (queueStore q)
     invokeEvent p $
       dequeuePostExtract q a

-- | A post action after extracting the item by the dequeuing request.  
dequeuePostExtract :: (MonadDES m,
                       DequeueStrategy m si,
                       DequeueStrategy m sm)
                      => Queue m si sm so a
                      -- ^ the queue
                      -> a
                      -- ^ the item to dequeue
                      -> Event m a
                      -- ^ the dequeued value
{-# INLINE dequeuePostExtract #-}
dequeuePostExtract q a =
  Event $ \p ->
  do c <- invokeEvent p $
          readRef (queueCountRef q)
     let c' = c - 1
     c' `seq` invokeEvent p $
       writeRef (queueCountRef q) c'
     invokeEvent p $
       releaseResourceWithinEvent (enqueueRes q)
     return a
