
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, FunctionalDependencies, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.Trans.QueueStrategy
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines the queue strategies.
--
module Simulation.Aivika.Trans.QueueStrategy where

import Control.Monad.Trans

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Comp.Template
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Parameter.Random
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Event

import qualified Simulation.Aivika.Trans.DoubleLinkedList as LL
import qualified Simulation.Aivika.Trans.PriorityQueue as PQ
import qualified Simulation.Aivika.Trans.Vector as V

-- | Defines the basic queue strategy.
class Comp m => QueueStrategy m s where

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
instance Comp m => QueueStrategy m FCFS where

  -- | A queue used by the 'FCFS' strategy.
  newtype StrategyQueue m FCFS a = FCFSQueue (LL.DoubleLinkedList m a)

  newStrategyQueue s =
    fmap FCFSQueue $
    do session <- liftParameter simulationSession
       liftComp $ LL.newList session

  strategyQueueNull (FCFSQueue q) = liftComp $ LL.listNull q

-- | An implementation of the 'FCFS' queue strategy.
instance QueueStrategy m FCFS => DequeueStrategy m FCFS where

  strategyDequeue (FCFSQueue q) =
    liftComp $
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

-- | An implementation of the 'FCFS' queue strategy.
instance DequeueStrategy m FCFS => EnqueueStrategy m FCFS where

  strategyEnqueue (FCFSQueue q) i = liftComp $ LL.listAddLast q i

-- | An implementation of the 'LCFS' queue strategy.
instance Comp m => QueueStrategy m LCFS where

  -- | A queue used by the 'LCFS' strategy.
  newtype StrategyQueue m LCFS a = LCFSQueue (LL.DoubleLinkedList m a)

  newStrategyQueue s =
    fmap LCFSQueue $
    do session <- liftParameter simulationSession
       liftComp $ LL.newList session
       
  strategyQueueNull (LCFSQueue q) = liftComp $ LL.listNull q

-- | An implementation of the 'LCFS' queue strategy.
instance QueueStrategy m LCFS => DequeueStrategy m LCFS where

  strategyDequeue (LCFSQueue q) =
    liftComp $
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

-- | An implementation of the 'LCFS' queue strategy.
instance DequeueStrategy m LCFS => EnqueueStrategy m LCFS where

  strategyEnqueue (LCFSQueue q) i = liftComp $ LL.listInsertFirst q i

-- | An implementation of the 'StaticPriorities' queue strategy.
instance Comp m => QueueStrategy m StaticPriorities where

  -- | A queue used by the 'StaticPriorities' strategy.
  newtype StrategyQueue m StaticPriorities a = StaticPriorityQueue (PQ.PriorityQueue m a)

  newStrategyQueue s =
    fmap StaticPriorityQueue $
    do session <- liftParameter simulationSession
       liftComp $ PQ.newQueue session

  strategyQueueNull (StaticPriorityQueue q) = liftComp $ PQ.queueNull q

-- | An implementation of the 'StaticPriorities' queue strategy.
instance QueueStrategy m StaticPriorities => DequeueStrategy m StaticPriorities where

  strategyDequeue (StaticPriorityQueue q) =
    liftComp $
    do (_, i) <- PQ.queueFront q
       PQ.dequeue q
       return i

-- | An implementation of the 'StaticPriorities' queue strategy.
instance DequeueStrategy m StaticPriorities => PriorityQueueStrategy m StaticPriorities Double where

  strategyEnqueueWithPriority (StaticPriorityQueue q) p i = liftComp $ PQ.enqueue q p i

-- | An implementation of the 'SIRO' queue strategy.
instance Comp m => QueueStrategy m SIRO where

  -- | A queue used by the 'SIRO' strategy.
  newtype StrategyQueue m SIRO a = SIROQueue (V.Vector m a)
  
  newStrategyQueue s =
    fmap SIROQueue $
    do session <- liftParameter simulationSession
       liftComp $ V.newVector session

  strategyQueueNull (SIROQueue q) =
    liftComp $
    do n <- V.vectorCount q
       return (n == 0)

-- | An implementation of the 'SIRO' queue strategy.
instance QueueStrategy m SIRO => DequeueStrategy m SIRO where

  strategyDequeue (SIROQueue q) =
    do n <- liftComp $ V.vectorCount q
       i <- liftParameter $ randomUniformInt 0 (n - 1)
       x <- liftComp $ V.readVector q i
       liftComp $ V.vectorDeleteAt q i
       return x

-- | An implementation of the 'SIRO' queue strategy.
instance DequeueStrategy m SIRO => EnqueueStrategy m SIRO where

  strategyEnqueue (SIROQueue q) i = liftComp $ V.appendVector q i
