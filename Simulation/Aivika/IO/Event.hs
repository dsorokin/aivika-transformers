
{-# LANGUAGE TypeFamilies, FlexibleInstances, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.IO.Event
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines a template-based event queue, where
-- the 'MonadIO'-based monad can be an instance of 'EventQueueing' and 'EventIOQueueing'.
--
module Simulation.Aivika.IO.Event () where

import Control.Monad
import Control.Monad.Trans

import Data.IORef

import qualified Simulation.Aivika.PriorityQueue as PQ

import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Template
import Simulation.Aivika.Trans.Internal.Types
import Simulation.Aivika.Trans.Event

-- | A template-based implementation of the 'EventQueueing' type class.
instance (Monad m, MonadIO m, MonadTemplate m) => EventQueueing m where

  {-# SPECIALISE instance EventQueueing IO #-}

  data EventQueue m =
    EventQueue { queuePQ :: PQ.PriorityQueue (Point m -> m ()),
                 -- ^ the underlying priority queue
                 queueBusy :: IORef Bool,
                 -- ^ whether the queue is currently processing events
                 queueTime :: IORef Double
                 -- ^ the actual time of the event queue
               }

  {-# INLINABLE newEventQueue #-}
  newEventQueue specs =
    liftIO $
    do f <- newIORef False
       t <- newIORef $ spcStartTime specs
       pq <- PQ.newQueue
       return EventQueue { queuePQ   = pq,
                           queueBusy = f,
                           queueTime = t }

  {-# INLINE enqueueEvent #-}
  enqueueEvent t (Event m) =
    Event $ \p ->
    let pq = queuePQ $ runEventQueue $ pointRun p
    in liftIO $ PQ.enqueue pq t m

  {-# INLINE runEventWith #-}
  runEventWith processing (Event e) =
    Dynamics $ \p ->
    do invokeDynamics p $ processEvents processing
       e p

  {-# INLINE eventQueueCount #-}
  eventQueueCount =
    Event $
    liftIO . PQ.queueCount . queuePQ . runEventQueue . pointRun

-- | Process the pending events.
processPendingEventsCore :: (MonadIO m, MonadTemplate m) => Bool -> Dynamics m ()
{-# INLINE processPendingEventsCore #-}
processPendingEventsCore includingCurrentEvents = Dynamics r where
  r p =
    do let q = runEventQueue $ pointRun p
           f = queueBusy q
       f' <- liftIO $ readIORef f
       unless f' $
         do liftIO $ writeIORef f True
            call q p
            liftIO $ writeIORef f False
  call q p =
    do let pq = queuePQ q
           r  = pointRun p
       f <- liftIO $ PQ.queueNull pq
       unless f $
         do (t2, c2) <- liftIO $ PQ.queueFront pq
            let t = queueTime q
            t' <- liftIO $ readIORef t
            when (t2 < t') $ 
              error "The time value is too small: processPendingEventsCore"
            when ((t2 < pointTime p) ||
                  (includingCurrentEvents && (t2 == pointTime p))) $
              do liftIO $ writeIORef t t2
                 liftIO $ PQ.dequeue pq
                 let sc = pointSpecs p
                     t0 = spcStartTime sc
                     dt = spcDT sc
                     n2 = fromIntegral $ floor ((t2 - t0) / dt)
                 c2 $ p { pointTime = t2,
                          pointIteration = n2,
                          pointPhase = -1 }
                 call q p

-- | Process the pending events synchronously, i.e. without past.
processPendingEvents :: (MonadIO m, MonadTemplate m) => Bool -> Dynamics m ()
{-# INLINE processPendingEvents #-}
processPendingEvents includingCurrentEvents = Dynamics r where
  r p =
    do let q = runEventQueue $ pointRun p
           t = queueTime q
       t' <- liftIO $ readIORef t
       if pointTime p < t'
         then error $
              "The current time is less than " ++
              "the time in the queue: processPendingEvents"
         else invokeDynamics p m
  m = processPendingEventsCore includingCurrentEvents

-- | A memoized value.
processEventsIncludingCurrent :: (MonadIO m, MonadTemplate m) => Dynamics m ()
{-# INLINE processEventsIncludingCurrent #-}
processEventsIncludingCurrent = processPendingEvents True

-- | A memoized value.
processEventsIncludingEarlier :: (MonadIO m, MonadTemplate m) => Dynamics m ()
{-# INLINE processEventsIncludingEarlier #-}
processEventsIncludingEarlier = processPendingEvents False

-- | A memoized value.
processEventsIncludingCurrentCore :: (MonadIO m, MonadTemplate m) => Dynamics m ()
{-# INLINE processEventsIncludingCurrentCore #-}
processEventsIncludingCurrentCore = processPendingEventsCore True

-- | A memoized value.
processEventsIncludingEarlierCore :: (MonadIO m, MonadTemplate m) => Dynamics m ()
{-# INLINE processEventsIncludingEarlierCore #-}
processEventsIncludingEarlierCore = processPendingEventsCore True

-- | Process the events.
processEvents :: (MonadIO m, MonadTemplate m) => EventProcessing -> Dynamics m ()
{-# INLINABLE processEvents #-}
processEvents CurrentEvents = processEventsIncludingCurrent
processEvents EarlierEvents = processEventsIncludingEarlier
processEvents CurrentEventsOrFromPast = processEventsIncludingCurrentCore
processEvents EarlierEventsOrFromPast = processEventsIncludingEarlierCore

-- | A template-based implementation of the 'EventIOQueueing' type class.
instance (Monad m, MonadIO m, MonadTemplate m, MonadDES m) => EventIOQueueing m where

  enqueueEventIO = enqueueEvent
  enqueueEventIOWithStartTime = enqueueEventWithStartTime
  enqueueEventIOWithStopTime = enqueueEventWithStopTime
  enqueueEventIOWithTimes = enqueueEventWithTimes
  enqueueEventIOWithIntegTimes = enqueueEventWithIntegTimes
