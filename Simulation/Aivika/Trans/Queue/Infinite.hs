
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.Queue.Infinite
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- This module defines an infinite queue that can use the specified strategies.
--
module Simulation.Aivika.Trans.Queue.Infinite
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
        enqueueStoringStrategy,
        dequeueStrategy,
        queueNull,
        queueCount,
        queueCountStats,
        enqueueStoreCount,
        dequeueCount,
        dequeueExtractCount,
        enqueueStoreRate,
        dequeueRate,
        dequeueExtractRate,
        queueWaitTime,
        dequeueWaitTime,
        queueRate,
        -- * Dequeuing and Enqueuing
        dequeue,
        dequeueWithOutputPriority,
        tryDequeue,
        enqueue,
        enqueueWithStoringPriority,
        -- * Summary
        queueSummary,
        -- * Derived Signals for Properties
        queueNullChanged,
        queueNullChanged_,
        queueCountChanged,
        queueCountChanged_,
        enqueueStoreCountChanged,
        enqueueStoreCountChanged_,
        dequeueCountChanged,
        dequeueCountChanged_,
        dequeueExtractCountChanged,
        dequeueExtractCountChanged_,
        queueWaitTimeChanged,
        queueWaitTimeChanged_,
        dequeueWaitTimeChanged,
        dequeueWaitTimeChanged_,
        queueRateChanged,
        queueRateChanged_,
        -- * Basic Signals
        enqueueStored,
        dequeueRequested,
        dequeueExtracted,
        -- * Overall Signal
        queueChanged_) where

import Data.Monoid

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
import Simulation.Aivika.Trans.Signal
import Simulation.Aivika.Trans.Resource
import Simulation.Aivika.Trans.QueueStrategy
import Simulation.Aivika.Trans.Statistics

-- | A type synonym for the ordinary FIFO queue also known as the FCFS
-- (First Come - First Serviced) queue.
type FCFSQueue m a = Queue m FCFS FCFS a

-- | A type synonym for the ordinary LIFO queue also known as the LCFS
-- (Last Come - First Serviced) queue.
type LCFSQueue m a = Queue m LCFS FCFS a

-- | A type synonym for the SIRO (Serviced in Random Order) queue.
type SIROQueue m a = Queue m SIRO FCFS a

-- | A type synonym for the queue with static priorities applied when
-- storing the elements in the queue.
type PriorityQueue m a = Queue m StaticPriorities FCFS a

-- | Represents an infinite queue using the specified strategies for
-- internal storing (in memory), @sm@, and dequeueing (output), @so@, where @a@ denotes
-- the type of items stored in the queue. As usual, type @m@ denotes
-- the underlying computation within which the simulation executes.
data Queue m sm so a =
  Queue { enqueueStoringStrategy :: sm,
          -- ^ The strategy applied when storing (in memory) items in the queue.
          dequeueStrategy :: so,
          -- ^ The strategy applied to the dequeueing (output) processes.
          queueStore :: StrategyQueue m sm (QueueItem a),
          dequeueRes :: Resource m so,
          queueCountRef :: Ref m Int,
          queueCountStatsRef :: Ref m (TimingStats Int),
          enqueueStoreCountRef :: Ref m Int,
          dequeueCountRef :: Ref m Int,
          dequeueExtractCountRef :: Ref m Int,
          queueWaitTimeRef :: Ref m (SamplingStats Double),
          dequeueWaitTimeRef :: Ref m (SamplingStats Double),
          enqueueStoredSource :: SignalSource m a,
          dequeueRequestedSource :: SignalSource m (),
          dequeueExtractedSource :: SignalSource m a }

-- | Stores the item and a time of its enqueuing. 
data QueueItem a =
  QueueItem { itemValue :: a,
              -- ^ Return the item value.
              itemStoringTime :: Double
              -- ^ Return the time of storing in the queue.
            }
  
-- | Create a new infinite FCFS queue.  
newFCFSQueue :: MonadDES m => Event m (FCFSQueue m a)
{-# INLINABLE newFCFSQueue #-}
newFCFSQueue = newQueue FCFS FCFS
  
-- | Create a new infinite LCFS queue.  
newLCFSQueue :: MonadDES m => Event m (LCFSQueue m a)  
{-# INLINABLE newLCFSQueue #-}
newLCFSQueue = newQueue LCFS FCFS
  
-- | Create a new infinite SIRO queue.  
newSIROQueue :: (MonadDES m, QueueStrategy m SIRO) => Event m (SIROQueue m a)  
{-# INLINABLE newSIROQueue #-}
newSIROQueue = newQueue SIRO FCFS
  
-- | Create a new infinite priority queue.
newPriorityQueue :: (MonadDES m, QueueStrategy m StaticPriorities) => Event m (PriorityQueue m a)  
{-# INLINABLE newPriorityQueue #-}
newPriorityQueue = newQueue StaticPriorities FCFS
  
-- | Create a new infinite queue with the specified strategies.  
newQueue :: (MonadDES m,
             QueueStrategy m sm,
             QueueStrategy m so) =>
            sm
            -- ^ the strategy applied when storing items in the queue
            -> so
            -- ^ the strategy applied to the dequeueing (output) processes when the queue is empty
            -> Event m (Queue m sm so a)  
{-# INLINABLE newQueue #-}
newQueue sm so =
  do t  <- liftDynamics time
     i  <- liftSimulation $ newRef 0
     is <- liftSimulation $ newRef $ returnTimingStats t 0
     cm <- liftSimulation $ newRef 0
     cr <- liftSimulation $ newRef 0
     co <- liftSimulation $ newRef 0
     qm <- liftSimulation $ newStrategyQueue sm
     ro <- liftSimulation $ newResourceWithMaxCount so 0 Nothing
     w  <- liftSimulation $ newRef mempty
     wo <- liftSimulation $ newRef mempty 
     s3 <- liftSimulation newSignalSource
     s4 <- liftSimulation newSignalSource
     s5 <- liftSimulation newSignalSource
     return Queue { enqueueStoringStrategy = sm,
                    dequeueStrategy = so,
                    queueStore = qm,
                    dequeueRes = ro,
                    queueCountRef = i,
                    queueCountStatsRef = is,
                    enqueueStoreCountRef = cm,
                    dequeueCountRef = cr,
                    dequeueExtractCountRef = co,
                    queueWaitTimeRef = w,
                    dequeueWaitTimeRef = wo,
                    enqueueStoredSource = s3,
                    dequeueRequestedSource = s4,
                    dequeueExtractedSource = s5 }

-- | Test whether the queue is empty.
--
-- See also 'queueNullChanged' and 'queueNullChanged_'.
queueNull :: MonadDES m => Queue m sm so a -> Event m Bool
{-# INLINABLE queueNull #-}
queueNull q =
  Event $ \p ->
  do n <- invokeEvent p $ readRef (queueCountRef q)
     return (n == 0)
  
-- | Signal when the 'queueNull' property value has changed.
queueNullChanged :: MonadDES m => Queue m sm so a -> Signal m Bool
{-# INLINABLE queueNullChanged #-}
queueNullChanged q =
  mapSignalM (const $ queueNull q) (queueNullChanged_ q)
  
-- | Signal when the 'queueNull' property value has changed.
queueNullChanged_ :: MonadDES m => Queue m sm so a -> Signal m ()
{-# INLINABLE queueNullChanged_ #-}
queueNullChanged_ = queueCountChanged_

-- | Return the current queue size.
--
-- See also 'queueCountStats', 'queueCountChanged' and 'queueCountChanged_'.
queueCount :: MonadDES m => Queue m sm so a -> Event m Int
{-# INLINABLE queueCount #-}
queueCount q =
  Event $ \p -> invokeEvent p $ readRef (queueCountRef q)

-- | Return the queue size statistics.
queueCountStats :: MonadDES m => Queue m sm so a -> Event m (TimingStats Int)
{-# INLINABLE queueCountStats #-}
queueCountStats q =
  Event $ \p -> invokeEvent p $ readRef (queueCountStatsRef q)
  
-- | Signal when the 'queueCount' property value has changed.
queueCountChanged :: MonadDES m => Queue m sm so a -> Signal m Int
{-# INLINABLE queueCountChanged #-}
queueCountChanged q =
  mapSignalM (const $ queueCount q) (queueCountChanged_ q)
  
-- | Signal when the 'queueCount' property value has changed.
queueCountChanged_ :: MonadDES m => Queue m sm so a -> Signal m ()
{-# INLINABLE queueCountChanged_ #-}
queueCountChanged_ q =
  mapSignal (const ()) (enqueueStored q) <>
  mapSignal (const ()) (dequeueExtracted q)
      
-- | Return the total number of input items that were stored.
--
-- See also 'enqueueStoreCountChanged' and 'enqueueStoreCountChanged_'.
enqueueStoreCount :: MonadDES m => Queue m sm so a -> Event m Int
{-# INLINABLE enqueueStoreCount #-}
enqueueStoreCount q =
  Event $ \p -> invokeEvent p $ readRef (enqueueStoreCountRef q)
  
-- | Signal when the 'enqueueStoreCount' property value has changed.
enqueueStoreCountChanged :: MonadDES m => Queue m sm so a -> Signal m Int
{-# INLINABLE enqueueStoreCountChanged #-}
enqueueStoreCountChanged q =
  mapSignalM (const $ enqueueStoreCount q) (enqueueStoreCountChanged_ q)
  
-- | Signal when the 'enqueueStoreCount' property value has changed.
enqueueStoreCountChanged_ :: MonadDES m => Queue m sm so a -> Signal m ()
{-# INLINABLE enqueueStoreCountChanged_ #-}
enqueueStoreCountChanged_ q =
  mapSignal (const ()) (enqueueStored q)
      
-- | Return the total number of requests for dequeueing the items,
-- not taking into account the failed attempts to dequeue immediately
-- without suspension.
--
-- See also 'dequeueCountChanged' and 'dequeueCountChanged_'.
dequeueCount :: MonadDES m => Queue m sm so a -> Event m Int
{-# INLINABLE dequeueCount #-}
dequeueCount q =
  Event $ \p -> invokeEvent p $ readRef (dequeueCountRef q)
      
-- | Signal when the 'dequeueCount' property value has changed.
dequeueCountChanged :: MonadDES m => Queue m sm so a -> Signal m Int
{-# INLINABLE dequeueCountChanged #-}
dequeueCountChanged q =
  mapSignalM (const $ dequeueCount q) (dequeueCountChanged_ q)
  
-- | Signal when the 'dequeueCount' property value has changed.
dequeueCountChanged_ :: MonadDES m => Queue m sm so a -> Signal m ()
{-# INLINABLE dequeueCountChanged_ #-}
dequeueCountChanged_ q =
  mapSignal (const ()) (dequeueRequested q)
      
-- | Return the total number of output items that were actually dequeued.
--
-- See also 'dequeueExtractCountChanged' and 'dequeueExtractCountChanged_'.
dequeueExtractCount :: MonadDES m => Queue m sm so a -> Event m Int
{-# INLINABLE dequeueExtractCount #-}
dequeueExtractCount q =
  Event $ \p -> invokeEvent p $ readRef (dequeueExtractCountRef q)
      
-- | Signal when the 'dequeueExtractCount' property value has changed.
dequeueExtractCountChanged :: MonadDES m => Queue m sm so a -> Signal m Int
{-# INLINABLE dequeueExtractCountChanged #-}
dequeueExtractCountChanged q =
  mapSignalM (const $ dequeueExtractCount q) (dequeueExtractCountChanged_ q)
  
-- | Signal when the 'dequeueExtractCount' property value has changed.
dequeueExtractCountChanged_ :: MonadDES m => Queue m sm so a -> Signal m ()
{-# INLINABLE dequeueExtractCountChanged_ #-}
dequeueExtractCountChanged_ q =
  mapSignal (const ()) (dequeueExtracted q)

-- | Return the rate of the items that were stored: how many items
-- per time.
enqueueStoreRate :: MonadDES m => Queue m sm so a -> Event m Double
{-# INLINABLE enqueueStoreRate #-}
enqueueStoreRate q =
  Event $ \p ->
  do x <- invokeEvent p $ readRef (enqueueStoreCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the rate of the requests for dequeueing the items: how many requests
-- per time. It does not include the failed attempts to dequeue immediately
-- without suspension.
dequeueRate :: MonadDES m => Queue m sm so a -> Event m Double
{-# INLINABLE dequeueRate #-}
dequeueRate q =
  Event $ \p ->
  do x <- invokeEvent p $ readRef (dequeueCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the rate of the output items that were dequeued: how many items
-- per time.
dequeueExtractRate :: MonadDES m => Queue m sm so a -> Event m Double
{-# INLINABLE dequeueExtractRate #-}
dequeueExtractRate q =
  Event $ \p ->
  do x <- invokeEvent p $ readRef (dequeueExtractCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the wait time from the time at which the item was stored in the queue to
-- the time at which it was dequeued.
--
-- See also 'queueWaitTimeChanged' and 'queueWaitTimeChanged_'.
queueWaitTime :: MonadDES m => Queue m sm so a -> Event m (SamplingStats Double)
{-# INLINABLE queueWaitTime #-}
queueWaitTime q =
  Event $ \p -> invokeEvent p $ readRef (queueWaitTimeRef q)
      
-- | Signal when the 'queueWaitTime' property value has changed.
queueWaitTimeChanged :: MonadDES m => Queue m sm so a -> Signal m (SamplingStats Double)
{-# INLINABLE queueWaitTimeChanged #-}
queueWaitTimeChanged q =
  mapSignalM (const $ queueWaitTime q) (queueWaitTimeChanged_ q)
  
-- | Signal when the 'queueWaitTime' property value has changed.
queueWaitTimeChanged_ :: MonadDES m => Queue m sm so a -> Signal m ()
{-# INLINABLE queueWaitTimeChanged_ #-}
queueWaitTimeChanged_ q =
  mapSignal (const ()) (dequeueExtracted q)
      
-- | Return the dequeue wait time from the time at which the item was requested
-- for dequeueing to the time at which it was actually dequeued.
--
-- See also 'dequeueWaitTimeChanged' and 'dequeueWaitTimeChanged_'.
dequeueWaitTime :: MonadDES m => Queue m sm so a -> Event m (SamplingStats Double)
{-# INLINABLE dequeueWaitTime #-}
dequeueWaitTime q =
  Event $ \p -> invokeEvent p $ readRef (dequeueWaitTimeRef q)
      
-- | Signal when the 'dequeueWaitTime' property value has changed.
dequeueWaitTimeChanged :: MonadDES m => Queue m sm so a -> Signal m (SamplingStats Double)
{-# INLINABLE dequeueWaitTimeChanged #-}
dequeueWaitTimeChanged q =
  mapSignalM (const $ dequeueWaitTime q) (dequeueWaitTimeChanged_ q)
  
-- | Signal when the 'dequeueWaitTime' property value has changed.
dequeueWaitTimeChanged_ :: MonadDES m => Queue m sm so a -> Signal m ()
{-# INLINABLE dequeueWaitTimeChanged_ #-}
dequeueWaitTimeChanged_ q =
  mapSignal (const ()) (dequeueExtracted q)

-- | Return a long-term average queue rate calculated as
-- the average queue size divided by the average wait time.
--
-- It should conform with Little's rule.
--
-- See also 'queueRateChanged' and 'queueRateChanged_'.
queueRate :: MonadDES m => Queue m sm so a -> Event m Double
{-# INLINABLE queueRate #-}
queueRate q =
  Event $ \p ->
  do x <- invokeEvent p $ readRef (queueCountStatsRef q)
     y <- invokeEvent p $ readRef (queueWaitTimeRef q)
     return (timingStatsMean x / samplingStatsMean y) 

-- | Signal when the 'queueRate' property value has changed.
queueRateChanged :: MonadDES m => Queue m sm so a -> Signal m Double
{-# INLINABLE queueRateChanged #-}
queueRateChanged q =
  mapSignalM (const $ queueRate q) (queueRateChanged_ q)

-- | Signal when the 'queueRate' property value has changed.
queueRateChanged_ :: MonadDES m => Queue m sm so a -> Signal m ()
{-# INLINABLE queueRateChanged_ #-}
queueRateChanged_ q =
  mapSignal (const ()) (enqueueStored q) <>
  mapSignal (const ()) (dequeueExtracted q)
  
-- | Dequeue suspending the process if the queue is empty.
dequeue :: (MonadDES m,
            DequeueStrategy m sm,
            EnqueueStrategy m so)
           => Queue m sm so a
           -- ^ the queue
           -> Process m a
           -- ^ the dequeued value
{-# INLINABLE dequeue #-}
dequeue q =
  do t <- liftEvent $ dequeueRequest q
     requestResource (dequeueRes q)
     liftEvent $ dequeueExtract q t
  
-- | Dequeue with the output priority suspending the process if the queue is empty.
dequeueWithOutputPriority :: (MonadDES m,
                              DequeueStrategy m sm,
                              PriorityQueueStrategy m so po)
                             => Queue m sm so a
                             -- ^ the queue
                             -> po
                             -- ^ the priority for output
                             -> Process m a
                             -- ^ the dequeued value
{-# INLINABLE dequeueWithOutputPriority #-}
dequeueWithOutputPriority q po =
  do t <- liftEvent $ dequeueRequest q
     requestResourceWithPriority (dequeueRes q) po
     liftEvent $ dequeueExtract q t
  
-- | Try to dequeue immediately.
tryDequeue :: (MonadDES m, DequeueStrategy m sm)
              => Queue m sm so a
              -- ^ the queue
              -> Event m (Maybe a)
              -- ^ the dequeued value of 'Nothing'
{-# INLINABLE tryDequeue #-}
tryDequeue q =
  do x <- tryRequestResourceWithinEvent (dequeueRes q)
     if x 
       then do t <- dequeueRequest q
               fmap Just $ dequeueExtract q t
       else return Nothing

-- | Enqueue the item.  
enqueue :: (MonadDES m,
            EnqueueStrategy m sm,
            DequeueStrategy m so)
           => Queue m sm so a
           -- ^ the queue
           -> a
           -- ^ the item to enqueue
           -> Event m ()
{-# INLINABLE enqueue #-}
enqueue = enqueueStore
     
-- | Enqueue with the storing priority the item.  
enqueueWithStoringPriority :: (MonadDES m,
                               PriorityQueueStrategy m sm pm,
                               DequeueStrategy m so)
                              => Queue m sm so a
                              -- ^ the queue
                              -> pm
                              -- ^ the priority for storing
                              -> a
                              -- ^ the item to enqueue
                              -> Event m ()
{-# INLINABLE enqueueWithStoringPriority #-}
enqueueWithStoringPriority = enqueueStoreWithPriority

-- | Return a signal that notifies when the enqueued item
-- is stored in the internal memory of the queue.
enqueueStored :: MonadDES m => Queue m sm so a -> Signal m a
{-# INLINABLE enqueueStored #-}
enqueueStored q = publishSignal (enqueueStoredSource q)

-- | Return a signal that notifies when the dequeuing operation was requested.
dequeueRequested :: MonadDES m => Queue m sm so a -> Signal m ()
{-# INLINABLE dequeueRequested #-}
dequeueRequested q = publishSignal (dequeueRequestedSource q)

-- | Return a signal that notifies when the item was extracted from the internal
-- storage of the queue and prepared for immediate receiving by the dequeuing process.
dequeueExtracted :: MonadDES m => Queue m sm so a -> Signal m a
{-# INLINABLE dequeueExtracted #-}
dequeueExtracted q = publishSignal (dequeueExtractedSource q)

-- | Store the item.
enqueueStore :: (MonadDES m,
                 EnqueueStrategy m sm,
                 DequeueStrategy m so)
                => Queue m sm so a
                -- ^ the queue
                -> a
                -- ^ the item to be stored
                -> Event m ()
{-# INLINE enqueueStore #-}
enqueueStore q a =
  Event $ \p ->
  do let i = QueueItem { itemValue = a,
                         itemStoringTime = pointTime p }
     invokeEvent p $
       strategyEnqueue (queueStore q) i
     c <- invokeEvent p $
          readRef (queueCountRef q)
     let c' = c + 1
         t  = pointTime p
     c' `seq` invokeEvent p $
       writeRef (queueCountRef q) c'
     invokeEvent p $
       modifyRef (queueCountStatsRef q) (addTimingStats t c')
     invokeEvent p $
       modifyRef (enqueueStoreCountRef q) (+ 1)
     invokeEvent p $
       releaseResourceWithinEvent (dequeueRes q)
     invokeEvent p $
       triggerSignal (enqueueStoredSource q) (itemValue i)

-- | Store with the priority the item.
enqueueStoreWithPriority :: (MonadDES m,
                             PriorityQueueStrategy m sm pm,
                             DequeueStrategy m so)
                            => Queue m sm so a
                            -- ^ the queue
                            -> pm
                            -- ^ the priority for storing
                            -> a
                            -- ^ the item to be enqueued
                            -> Event m ()
{-# INLINE enqueueStoreWithPriority #-}
enqueueStoreWithPriority q pm a =
  Event $ \p ->
  do let i = QueueItem { itemValue = a,
                         itemStoringTime = pointTime p }
     invokeEvent p $
       strategyEnqueueWithPriority (queueStore q) pm i
     c <- invokeEvent p $
          readRef (queueCountRef q)
     let c' = c + 1
         t  = pointTime p
     c' `seq` invokeEvent p $
       writeRef (queueCountRef q) c'
     invokeEvent p $
       modifyRef (queueCountStatsRef q) (addTimingStats t c')
     invokeEvent p $
       modifyRef (enqueueStoreCountRef q) (+ 1)
     invokeEvent p $
       releaseResourceWithinEvent (dequeueRes q)
     invokeEvent p $
       triggerSignal (enqueueStoredSource q) (itemValue i)

-- | Accept the dequeuing request and return the current simulation time.
dequeueRequest :: MonadDES m
                  => Queue m sm so a
                  -- ^ the queue
                  -> Event m Double
                  -- ^ the current time
{-# INLINE dequeueRequest #-}
dequeueRequest q =
  Event $ \p ->
  do invokeEvent p $
       modifyRef (dequeueCountRef q) (+ 1)
     invokeEvent p $
       triggerSignal (dequeueRequestedSource q) ()
     return $ pointTime p 

-- | Extract an item for the dequeuing request.  
dequeueExtract :: (MonadDES m, DequeueStrategy m sm)
                  => Queue m sm so a
                  -- ^ the queue
                  -> Double
                  -- ^ the time of the dequeuing request
                  -> Event m a
                  -- ^ the dequeued value
{-# INLINE dequeueExtract #-}
dequeueExtract q t' =
  Event $ \p ->
  do i <- invokeEvent p $
          strategyDequeue (queueStore q)
     c <- invokeEvent p $
          readRef (queueCountRef q)
     let c' = c - 1
         t  = pointTime p
     c' `seq` invokeEvent p $
       writeRef (queueCountRef q) c'
     invokeEvent p $
       modifyRef (queueCountStatsRef q) (addTimingStats t c')
     invokeEvent p $
       modifyRef (dequeueExtractCountRef q) (+ 1)
     invokeEvent p $
       dequeueStat q t' i
     invokeEvent p $
       triggerSignal (dequeueExtractedSource q) (itemValue i)
     return $ itemValue i

-- | Update the statistics for the output wait time of the dequeuing operation
-- and the wait time of storing in the queue.
dequeueStat :: MonadDES m
               => Queue m sm so a
               -- ^ the queue
               -> Double
               -- ^ the time of the dequeuing request
               -> QueueItem a
               -- ^ the item and its input time
               -> Event m ()
               -- ^ the action of updating the statistics
{-# INLINE dequeueStat #-}
dequeueStat q t' i =
  Event $ \p ->
  do let t1 = itemStoringTime i
         t  = pointTime p
     invokeEvent p $
       modifyRef (dequeueWaitTimeRef q) $
       addSamplingStats (t - t')
     invokeEvent p $
       modifyRef (queueWaitTimeRef q) $
       addSamplingStats (t - t1)

-- | Signal whenever any property of the queue changes.
--
-- The property must have the corresponded signal. There are also characteristics
-- similar to the properties but that have no signals. As a rule, such characteristics
-- already depend on the simulation time and therefore they may change at any
-- time point.
queueChanged_ :: MonadDES m => Queue m sm so a -> Signal m ()
{-# INLINABLE queueChanged_ #-}
queueChanged_ q =
  mapSignal (const ()) (enqueueStored q) <>
  dequeueRequested q <>
  mapSignal (const ()) (dequeueExtracted q)

-- | Return the summary for the queue with desciption of its
-- properties and activities using the specified indent.
queueSummary :: (MonadDES m, Show sm, Show so) => Queue m sm so a -> Int -> Event m ShowS
{-# INLINABLE queueSummary #-}
queueSummary q indent =
  do let sm = enqueueStoringStrategy q
         so = dequeueStrategy q
     null <- queueNull q
     count <- queueCount q
     countStats <- queueCountStats q
     enqueueStoreCount <- enqueueStoreCount q
     dequeueCount <- dequeueCount q
     dequeueExtractCount <- dequeueExtractCount q
     enqueueStoreRate <- enqueueStoreRate q
     dequeueRate <- dequeueRate q
     dequeueExtractRate <- dequeueExtractRate q
     waitTime <- queueWaitTime q
     dequeueWaitTime <- dequeueWaitTime q
     let tab = replicate indent ' '
     return $
       showString tab .
       showString "the storing (memory) strategy = " .
       shows sm .
       showString "\n" .
       showString tab .
       showString "the dequeueing (output) strategy = " .
       shows so .
       showString "\n" .
       showString tab .
       showString "empty? = " .
       shows null .
       showString "\n" .
       showString tab .
       showString "the current size = " .
       shows count .
       showString "\n" .
       showString tab .
       showString "the size statistics = \n\n" .
       timingStatsSummary countStats (2 + indent) .
       showString "\n\n" .
       showString tab .
       showString "the enqueue store count (number of the input items that were stored) = " .
       shows enqueueStoreCount .
       showString "\n" .
       showString tab .
       showString "the dequeue count (number of requests for dequeueing an item) = " .
       shows dequeueCount .
       showString "\n" .
       showString tab .
       showString "the dequeue extract count (number of the output items that were dequeued) = " .
       shows dequeueExtractCount .
       showString "\n" .
       showString tab .
       showString "the enqueue store rate (how many input items were stored per time) = " .
       shows enqueueStoreRate .
       showString "\n" .
       showString tab .
       showString "the dequeue rate (how many requests for dequeueing per time) = " .
       shows dequeueRate .
       showString "\n" .
       showString tab .
       showString "the dequeue extract rate (how many output items were dequeued per time) = " .
       shows dequeueExtractRate .
       showString "\n" .
       showString tab .
       showString "the wait time (when was stored -> when was dequeued) = \n\n" .
       samplingStatsSummary waitTime (2 + indent) .
       showString "\n\n" .
       showString tab .
       showString "the dequeue wait time (when was requested for dequeueing -> when was dequeued) = \n\n" .
       samplingStatsSummary dequeueWaitTime (2 + indent)
