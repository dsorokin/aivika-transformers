
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.IO.QueueStrategy
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines some queue strategy instances
-- for the 'IO' computation.
--
module Simulation.Aivika.IO.QueueStrategy () where

import Control.Monad.Trans

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Parameter.Random
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.QueueStrategy

import Simulation.Aivika.IO.Comp

import qualified Simulation.Aivika.DoubleLinkedList as LL
import qualified Simulation.Aivika.PriorityQueue as PQ
import qualified Simulation.Aivika.Vector as V

-- | An implementation of the 'FCFS' queue strategy.
instance QueueStrategy IO FCFS where
-- instance (Monad m, MonadComp m, MonadIO m, MonadTemplate m)
--          => QueueStrategy m FCFS where

  {-# SPECIALISE instance QueueStrategy IO FCFS #-}

  -- | A queue used by the 'FCFS' strategy.
  newtype StrategyQueue IO FCFS a = FCFSQueue (LL.DoubleLinkedList a)

  {-# INLINABLE newStrategyQueue #-}
  newStrategyQueue s =
    fmap FCFSQueue $
    liftIO LL.newList

  {-# INLINABLE strategyQueueNull #-}
  strategyQueueNull (FCFSQueue q) =
    liftIO $ LL.listNull q

-- | An implementation of the 'FCFS' queue strategy.
instance DequeueStrategy IO FCFS where
-- instance (QueueStrategy m FCFS, MonadComp m, MonadIO m, MonadTemplate m)
--          => DequeueStrategy m FCFS where

  {-# SPECIALISE instance DequeueStrategy IO FCFS #-}

  {-# INLINABLE strategyDequeue #-}
  strategyDequeue (FCFSQueue q) =
    liftIO $
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

-- | An implementation of the 'FCFS' queue strategy.
instance EnqueueStrategy IO FCFS where
-- instance (DequeueStrategy m FCFS, MonadComp m, MonadIO m, MonadTemplate m)
--          => EnqueueStrategy m FCFS where

  {-# SPECIALISE instance EnqueueStrategy IO FCFS #-}

  {-# INLINABLE strategyEnqueue #-}
  strategyEnqueue (FCFSQueue q) i =
    liftIO $ LL.listAddLast q i

-- | An implementation of the 'FCFS' queue strategy.
instance DeletingQueueStrategy IO FCFS where
-- instance (DequeueStrategy m FCFS, MonadComp m, MonadIO m, MonadTemplate m)
--          => DeletingQueueStrategy m FCFS where

  {-# SPECIALISE instance DeletingQueueStrategy IO FCFS #-}

  {-# INLINABLE strategyQueueDeleteBy #-}
  strategyQueueDeleteBy (FCFSQueue q) p =
    liftIO $ LL.listRemoveBy q p

  {-# INLINABLE strategyQueueContainsBy #-}
  strategyQueueContainsBy (FCFSQueue q) p =
    liftIO $ LL.listContainsBy q p

-- | An implementation of the 'LCFS' queue strategy.
instance QueueStrategy IO LCFS where
-- instance (MonadComp m, MonadIO m, MonadTemplate m)
--          => QueueStrategy m LCFS where

  {-# SPECIALISE instance QueueStrategy IO LCFS #-}

  -- | A queue used by the 'LCFS' strategy.
  newtype StrategyQueue IO LCFS a = LCFSQueue (LL.DoubleLinkedList a)

  {-# INLINABLE newStrategyQueue #-}
  newStrategyQueue s =
    fmap LCFSQueue $
    liftIO LL.newList
       
  {-# INLINABLE strategyQueueNull #-}
  strategyQueueNull (LCFSQueue q) =
    liftIO $ LL.listNull q

-- | An implementation of the 'LCFS' queue strategy.
instance DequeueStrategy IO LCFS where
-- instance (QueueStrategy m LCFS, MonadComp m, MonadIO m, MonadTemplate m)
--          => DequeueStrategy m LCFS where

  {-# SPECIALISE instance DequeueStrategy IO LCFS #-}

  {-# INLINABLE strategyDequeue #-}
  strategyDequeue (LCFSQueue q) =
    liftIO $
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

-- | An implementation of the 'LCFS' queue strategy.
instance EnqueueStrategy IO LCFS where
-- instance (DequeueStrategy m LCFS, MonadComp m, MonadIO m, MonadTemplate m)
--          => EnqueueStrategy m LCFS where

  {-# SPECIALISE instance EnqueueStrategy IO LCFS #-}

  {-# INLINABLE strategyEnqueue #-}
  strategyEnqueue (LCFSQueue q) i =
    liftIO $ LL.listInsertFirst q i

-- | An implementation of the 'LCFS' queue strategy.
instance DeletingQueueStrategy IO LCFS where
-- instance (DequeueStrategy m LCFS, MonadComp m, MonadIO m, MonadTemplate m)
--          => DeletingQueueStrategy m LCFS where

  {-# SPECIALISE instance DeletingQueueStrategy IO LCFS #-}

  {-# INLINABLE strategyQueueDeleteBy #-}
  strategyQueueDeleteBy (LCFSQueue q) p =
    liftIO $ LL.listRemoveBy q p

  {-# INLINABLE strategyQueueContainsBy #-}
  strategyQueueContainsBy (LCFSQueue q) p =
    liftIO $ LL.listContainsBy q p

-- | An implementation of the 'StaticPriorities' queue strategy.
instance QueueStrategy IO StaticPriorities where
-- instance (MonadComp m, MonadIO m, MonadTemplate m)
--          => QueueStrategy m StaticPriorities where

  {-# SPECIALISE instance QueueStrategy IO StaticPriorities #-}

  -- | A queue used by the 'StaticPriorities' strategy.
  newtype StrategyQueue IO StaticPriorities a = StaticPriorityQueue (PQ.PriorityQueue a)

  {-# INLINABLE newStrategyQueue #-}
  newStrategyQueue s =
    fmap StaticPriorityQueue $
    liftIO $ PQ.newQueue

  {-# INLINABLE strategyQueueNull #-}
  strategyQueueNull (StaticPriorityQueue q) =
    liftIO $ PQ.queueNull q

-- | An implementation of the 'StaticPriorities' queue strategy.
instance DequeueStrategy IO StaticPriorities where
-- instance (QueueStrategy m StaticPriorities, MonadComp m, MonadIO m, MonadTemplate m)
--          => DequeueStrategy m StaticPriorities where

  {-# SPECIALISE instance DequeueStrategy IO StaticPriorities #-}

  {-# INLINABLE strategyDequeue #-}
  strategyDequeue (StaticPriorityQueue q) =
    liftIO $
    do (_, i) <- PQ.queueFront q
       PQ.dequeue q
       return i

-- | An implementation of the 'StaticPriorities' queue strategy.
instance PriorityQueueStrategy IO StaticPriorities Double where
-- instance (DequeueStrategy m StaticPriorities, MonadComp m, MonadIO m, MonadTemplate m)
--          => PriorityQueueStrategy m StaticPriorities Double where

  {-# SPECIALISE instance PriorityQueueStrategy IO StaticPriorities Double #-}

  {-# INLINABLE strategyEnqueueWithPriority #-}
  strategyEnqueueWithPriority (StaticPriorityQueue q) p i =
    liftIO $ PQ.enqueue q p i

-- | An implementation of the 'StaticPriorities' queue strategy.
instance DeletingQueueStrategy IO StaticPriorities where
-- instance (DequeueStrategy m StaticPriorities, MonadComp m, MonadIO m, MonadTemplate m)
--          => DeletingQueueStrategy m StaticPriorities where

  {-# SPECIALISE instance DeletingQueueStrategy IO StaticPriorities #-}

  {-# INLINABLE strategyQueueDeleteBy #-}
  strategyQueueDeleteBy (StaticPriorityQueue q) p =
    liftIO $ PQ.queueDeleteBy q p

  {-# INLINABLE strategyQueueContainsBy #-}
  strategyQueueContainsBy (StaticPriorityQueue q) p =
    liftIO $ PQ.queueContainsBy q p

-- | An implementation of the 'SIRO' queue strategy.
instance QueueStrategy IO SIRO where
-- instance (MonadComp m, MonadIO m, MonadTemplate m)
--          => QueueStrategy m SIRO where

  {-# SPECIALISE instance QueueStrategy IO SIRO #-}

  -- | A queue used by the 'SIRO' strategy.
  newtype StrategyQueue IO SIRO a = SIROQueue (V.Vector a)
  
  {-# INLINABLE newStrategyQueue #-}
  newStrategyQueue s =
    fmap SIROQueue $
    liftIO $ V.newVector

  {-# INLINABLE strategyQueueNull #-}
  strategyQueueNull (SIROQueue q) =
    liftIO $
    do n <- V.vectorCount q
       return (n == 0)

-- | An implementation of the 'SIRO' queue strategy.
instance DequeueStrategy IO SIRO where
-- instance (QueueStrategy m SIRO, MonadComp m, MonadIO m, MonadTemplate m)
--          => DequeueStrategy m SIRO where

  {-# SPECIALISE instance DequeueStrategy IO SIRO #-}

  {-# INLINABLE strategyDequeue #-}
  strategyDequeue (SIROQueue q) =
    do n <- liftIO $ V.vectorCount q
       i <- liftParameter $ randomUniformInt 0 (n - 1)
       x <- liftIO $ V.readVector q i
       liftIO $ V.vectorDeleteAt q i
       return x

-- | An implementation of the 'SIRO' queue strategy.
instance EnqueueStrategy IO SIRO where
-- instance (DequeueStrategy m SIRO, MonadComp m, MonadIO m, MonadTemplate m)
--          => EnqueueStrategy m SIRO where

  {-# SPECIALISE instance EnqueueStrategy IO SIRO #-}

  {-# INLINABLE strategyEnqueue #-}
  strategyEnqueue (SIROQueue q) i =
    liftIO $ V.appendVector q i

-- | An implementation of the 'SIRO' queue strategy.
instance DeletingQueueStrategy IO SIRO where
-- instance (DequeueStrategy m SIRO, MonadComp m, MonadIO m, MonadTemplate m)
--          => DeletingQueueStrategy m SIRO where

  {-# SPECIALISE instance DeletingQueueStrategy IO SIRO #-}

  {-# INLINABLE strategyQueueDeleteBy #-}
  strategyQueueDeleteBy (SIROQueue q) p =
    liftIO $ V.vectorDeleteBy q p

  {-# INLINABLE strategyQueueContainsBy #-}
  strategyQueueContainsBy (SIROQueue q) p =
    liftIO $ V.vectorContainsBy q p
