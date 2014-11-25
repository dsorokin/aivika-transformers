
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, FunctionalDependencies, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.Trans.QueueStrategy
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines the queue strategies.
--
module Simulation.Aivika.Trans.QueueStrategy where

import Control.Monad.Trans

import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Event

import qualified Simulation.Aivika.Trans.DoubleLinkedList as LL

-- | Defines the basic queue strategy.
class MonadDES m => QueueStrategy m s where

  -- | The strategy queue.
  data StrategyQueue m s :: * -> *

  -- | Create a new queue by the specified strategy.
  newStrategyQueue :: s
                      -- ^ the strategy
                      -> Simulation m (StrategyQueue m s a)
                      -- ^ a new queue

  -- | Test whether the queue is empty.
  strategyQueueNull :: StrategyQueue m s a
                       -- ^ the queue
                       -> Event m Bool
                       -- ^ the result of the test

-- | Defines a strategy with support of the dequeuing operation.
class QueueStrategy m s => DequeueStrategy m s where

  -- | Dequeue the front element and return it.
  strategyDequeue :: StrategyQueue m s a
                     -- ^ the queue
                     -> Event m a
                     -- ^ the dequeued element

-- | It defines a strategy when we can enqueue a single element.
class DequeueStrategy m s => EnqueueStrategy m s where

  -- | Enqueue an element.
  strategyEnqueue :: StrategyQueue m s a
                     -- ^ the queue
                     -> a
                     -- ^ the element to be enqueued
                     -> Event m ()
                     -- ^ the action of enqueuing

-- | It defines a strategy when we can enqueue an element with the specified priority.
class DequeueStrategy m s => PriorityQueueStrategy m s p | s -> p where

  -- | Enqueue an element with the specified priority.
  strategyEnqueueWithPriority :: StrategyQueue m s a
                                 -- ^ the queue
                                 -> p
                                 -- ^ the priority
                                 -> a
                                 -- ^ the element to be enqueued
                                 -> Event m ()
                                 -- ^ the action of enqueuing

-- | Strategy: First Come - First Served (FCFS).
data FCFS = FCFS deriving (Eq, Ord, Show)

-- | Strategy: Last Come - First Served (LCFS)
data LCFS = LCFS deriving (Eq, Ord, Show)

-- | Strategy: Service in Random Order (SIRO).
data SIRO = SIRO deriving (Eq, Ord, Show)

-- | Strategy: Static Priorities. It uses the priority queue.
data StaticPriorities = StaticPriorities deriving (Eq, Ord, Show)

-- | An implementation of the 'FCFS' queue strategy.
instance MonadDES m => QueueStrategy m FCFS where

  -- | A queue used by the 'FCFS' strategy.
  newtype StrategyQueue m FCFS a = FCFSQueue (LL.DoubleLinkedList m a)

  {-# INLINABLE newStrategyQueue #-}
  newStrategyQueue s = fmap FCFSQueue LL.newList

  {-# INLINABLE strategyQueueNull #-}
  strategyQueueNull (FCFSQueue q) = LL.listNull q

-- | An implementation of the 'FCFS' queue strategy.
instance QueueStrategy m FCFS => DequeueStrategy m FCFS where

  {-# INLINABLE strategyDequeue #-}
  strategyDequeue (FCFSQueue q) =
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

-- | An implementation of the 'FCFS' queue strategy.
instance DequeueStrategy m FCFS => EnqueueStrategy m FCFS where

  {-# INLINABLE strategyEnqueue #-}
  strategyEnqueue (FCFSQueue q) i = LL.listAddLast q i

-- | An implementation of the 'LCFS' queue strategy.
instance MonadDES m => QueueStrategy m LCFS where

  -- | A queue used by the 'LCFS' strategy.
  newtype StrategyQueue m LCFS a = LCFSQueue (LL.DoubleLinkedList m a)

  {-# INLINABLE newStrategyQueue #-}
  newStrategyQueue s = fmap LCFSQueue LL.newList
       
  {-# INLINABLE strategyQueueNull #-}
  strategyQueueNull (LCFSQueue q) = LL.listNull q

-- | An implementation of the 'LCFS' queue strategy.
instance QueueStrategy m LCFS => DequeueStrategy m LCFS where

  {-# INLINABLE strategyDequeue #-}
  strategyDequeue (LCFSQueue q) =
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

-- | An implementation of the 'LCFS' queue strategy.
instance DequeueStrategy m LCFS => EnqueueStrategy m LCFS where

  {-# INLINABLE strategyEnqueue #-}
  strategyEnqueue (LCFSQueue q) i = LL.listInsertFirst q i
