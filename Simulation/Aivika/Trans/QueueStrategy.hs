
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, FunctionalDependencies, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.Trans.QueueStrategy
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- This module defines the queue strategies.
--
module Simulation.Aivika.Trans.QueueStrategy where

import Control.Monad

import Data.Maybe

import Simulation.Aivika.Trans.Internal.Types

-- | Defines the basic queue strategy.
class Monad m => QueueStrategy m s where

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

-- | Defines a strategy with support of the deleting operation.
class DequeueStrategy m s => DeletingQueueStrategy m s where

  -- | Remove the element and return a flag indicating whether
  -- the element was found and removed.
  strategyQueueDelete :: Eq a
                         => StrategyQueue m s a
                         -- ^ the queue
                         -> a
                         -- ^ the element
                         -> Event m Bool
                         -- ^ whether the element was found and removed
  strategyQueueDelete s a =
    Event $ \p ->
    do x <- invokeEvent p $ strategyQueueDeleteBy s (== a)
       return (isJust x)

  -- | Remove an element satisfying the predicate and return the element if found.
  strategyQueueDeleteBy :: StrategyQueue m s a
                           -- ^ the queue
                           -> (a -> Bool)
                           -- ^ the predicate
                           -> Event m (Maybe a)
                           -- ^ the element if it was found and removed

  -- | Detect whether the specified element is contained in the queue.
  strategyQueueContains :: Eq a
                           => StrategyQueue m s a
                           -- ^ the queue
                           -> a
                           -- ^ the element to find
                           -> Event m Bool
                           -- ^ whether the element is contained in the queue
  strategyQueueContains s a =
    Event $ \p ->
    do x <- invokeEvent p $ strategyQueueContainsBy s (== a)
       return (isJust x)

  -- | Detect whether an element satifying the specified predicate is contained in the queue.
  strategyQueueContainsBy :: StrategyQueue m s a
                             -- ^ the queue
                             -> (a -> Bool)
                             -- ^ the predicate
                             -> Event m (Maybe a)
                             -- ^ the element if it was found

-- | Strategy: First Come - First Served (FCFS).
data FCFS = FCFS deriving (Eq, Ord, Show)

-- | Strategy: Last Come - First Served (LCFS)
data LCFS = LCFS deriving (Eq, Ord, Show)

-- | Strategy: Service in Random Order (SIRO).
data SIRO = SIRO deriving (Eq, Ord, Show)

-- | Strategy: Static Priorities. It uses the priority queue.
data StaticPriorities = StaticPriorities deriving (Eq, Ord, Show)
