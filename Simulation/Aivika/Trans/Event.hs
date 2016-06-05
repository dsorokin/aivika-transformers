
-- |
-- Module     : Simulation.Aivika.Trans.Event
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines the 'Event' monad which is very similar to the 'Dynamics'
-- monad but only now the computation is strongly synchronized with the event queue.
--
module Simulation.Aivika.Trans.Event
       (-- * Event Monad
        Event,
        EventLift(..),
        EventProcessing(..),
        runEventInStartTime,
        runEventInStopTime,
        -- * Event Queue
        EventQueueing(..),
        enqueueEventWithCancellation,
        enqueueEventWithStartTime,
        enqueueEventWithStopTime,
        enqueueEventWithTimes,
        enqueueEventWithIntegTimes,
        yieldEvent,
        -- * Cancelling Event
        EventCancellation,
        cancelEvent,
        eventCancelled,
        eventFinished,
        -- * Error Handling
        catchEvent,
        finallyEvent,
        throwEvent,
        -- * Memoization
        memoEvent,
        memoEventInTime,
        -- * Disposable
        DisposableEvent(..),
        -- * Retrying Computation
        retryEvent,
        -- * Synchronizing IO Actions
        EventSync(..),
        EventSyncIO(..),
        -- * Debugging
        traceEvent) where

import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Internal.Event

