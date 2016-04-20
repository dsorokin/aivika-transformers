
{-# LANGUAGE TypeFamilies, FlexibleInstances, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.IO.Resource.Preemption
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- This module defines the preemptible resource, where
-- the 'MonadIO'-based monad can be an instance of 'MonadResource'.
--
module Simulation.Aivika.IO.Resource.Preemption () where

import Control.Monad
import Control.Monad.Trans

import Data.Maybe
import Data.IORef
import Data.Monoid

import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Template
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Cont
import Simulation.Aivika.Trans.Internal.Process
import Simulation.Aivika.Trans.Resource.Preemption
import Simulation.Aivika.Trans.Statistics
import Simulation.Aivika.Trans.Signal

import Simulation.Aivika.IO.DES

import qualified Simulation.Aivika.PriorityQueue as PQ

-- | The 'MonadIO' based monad is an instance of 'MonadResource'.
instance (MonadDES m, MonadIO m, MonadTemplate m) => MonadResource m where

  {-# SPECIALISE instance MonadResource IO #-}

  -- | A template-based implementation of the preemptible resource.
  data Resource m = 
    Resource { resourceMaxCount0 :: Maybe Int,
               -- ^ Return the maximum count of the resource, where 'Nothing'
               -- means that the resource has no upper bound.
               resourceCountRef :: IORef Int,
               resourceCountStatsRef :: IORef (TimingStats Int),
               resourceCountSource :: SignalSource m Int,
               resourceUtilisationCountRef :: IORef Int,
               resourceUtilisationCountStatsRef :: IORef (TimingStats Int),
               resourceUtilisationCountSource :: SignalSource m Int,
               resourceQueueCountRef :: IORef Int,
               resourceQueueCountStatsRef :: IORef (TimingStats Int),
               resourceQueueCountSource :: SignalSource m Int,
               resourceTotalWaitTimeRef :: IORef Double,
               resourceWaitTimeRef :: IORef (SamplingStats Double),
               resourceWaitTimeSource :: SignalSource m (),
               resourceActingQueue :: PQ.PriorityQueue (ResourceActingItem m),
               resourceWaitQueue :: PQ.PriorityQueue (ResourceAwaitingItem m) }

  {-# INLINABLE newResource #-}
  newResource count =
    newResourceWithMaxCount count (Just count)

  {-# INLINABLE newResourceWithMaxCount #-}
  newResourceWithMaxCount count maxCount =
    Event $ \p ->
    do let r = pointRun p
           t = pointTime p
       when (count < 0) $
         fail $
         "The resource count cannot be negative: " ++
         "newResourceWithMaxCount."
       case maxCount of
         Just maxCount | count > maxCount ->
           fail $
           "The resource count cannot be greater than " ++
           "its maximum value: newResourceWithMaxCount."
         _ ->
           return ()
       countRef <- liftIO $ newIORef count
       countStatsRef <- liftIO $ newIORef $ returnTimingStats t count
       countSource <- invokeSimulation r newSignalSource
       utilCountRef <- liftIO $ newIORef 0
       utilCountStatsRef <- liftIO $ newIORef $ returnTimingStats t 0
       utilCountSource <- invokeSimulation r newSignalSource
       queueCountRef <- liftIO $ newIORef 0
       queueCountStatsRef <- liftIO $ newIORef $ returnTimingStats t 0
       queueCountSource <- invokeSimulation r newSignalSource
       totalWaitTimeRef <- liftIO $ newIORef 0
       waitTimeRef <- liftIO $ newIORef emptySamplingStats
       waitTimeSource <- invokeSimulation r newSignalSource
       actingQueue <- liftIO PQ.newQueue
       waitQueue <- liftIO PQ.newQueue
       return Resource { resourceMaxCount0 = maxCount,
                         resourceCountRef = countRef,
                         resourceCountStatsRef = countStatsRef,
                         resourceCountSource = countSource,
                         resourceUtilisationCountRef = utilCountRef,
                         resourceUtilisationCountStatsRef = utilCountStatsRef,
                         resourceUtilisationCountSource = utilCountSource,
                         resourceQueueCountRef = queueCountRef,
                         resourceQueueCountStatsRef = queueCountStatsRef,
                         resourceQueueCountSource = queueCountSource,
                         resourceTotalWaitTimeRef = totalWaitTimeRef,
                         resourceWaitTimeRef = waitTimeRef,
                         resourceWaitTimeSource = waitTimeSource,
                         resourceActingQueue = actingQueue,
                         resourceWaitQueue = waitQueue }

  {-# INLINABLE resourceMaxCount #-}
  resourceMaxCount = resourceMaxCount0

  {-# INLINABLE resourceCount #-}
  resourceCount r =
    Event $ \p -> liftIO $ readIORef (resourceCountRef r)

  {-# INLINABLE resourceCountStats #-}
  resourceCountStats r =
    Event $ \p -> liftIO $ readIORef (resourceCountStatsRef r)

  {-# INLINABLE resourceCountChanged #-}
  resourceCountChanged r =
    publishSignal $ resourceCountSource r

  {-# INLINABLE resourceCountChanged_ #-}
  resourceCountChanged_ r =
    mapSignal (const ()) $ resourceCountChanged r

  {-# INLINABLE resourceUtilisationCount #-}
  resourceUtilisationCount r =
    Event $ \p -> liftIO $ readIORef (resourceUtilisationCountRef r)

  {-# INLINABLE resourceUtilisationCountStats #-}
  resourceUtilisationCountStats r =
    Event $ \p -> liftIO $ readIORef (resourceUtilisationCountStatsRef r)

  {-# INLINABLE resourceUtilisationCountChanged #-}
  resourceUtilisationCountChanged r =
    publishSignal $ resourceUtilisationCountSource r

  {-# INLINABLE resourceUtilisationCountChanged_ #-}
  resourceUtilisationCountChanged_ r =
    mapSignal (const ()) $ resourceUtilisationCountChanged r

  {-# INLINABLE resourceQueueCount #-}
  resourceQueueCount r =
    Event $ \p -> liftIO $ readIORef (resourceQueueCountRef r)

  {-# INLINABLE resourceQueueCountStats #-}
  resourceQueueCountStats r =
    Event $ \p -> liftIO $ readIORef (resourceQueueCountStatsRef r)

  {-# INLINABLE resourceQueueCountChanged #-}
  resourceQueueCountChanged r =
    publishSignal $ resourceQueueCountSource r

  {-# INLINABLE resourceQueueCountChanged_ #-}
  resourceQueueCountChanged_ r =
    mapSignal (const ()) $ resourceQueueCountChanged r

  {-# INLINABLE resourceTotalWaitTime #-}
  resourceTotalWaitTime r =
    Event $ \p -> liftIO $ readIORef (resourceTotalWaitTimeRef r)

  {-# INLINABLE resourceWaitTime #-}
  resourceWaitTime r =
    Event $ \p -> liftIO $ readIORef (resourceWaitTimeRef r)

  {-# INLINABLE resourceWaitTimeChanged #-}
  resourceWaitTimeChanged r =
    mapSignalM (\() -> resourceWaitTime r) $ resourceWaitTimeChanged_ r

  {-# INLINABLE resourceWaitTimeChanged_ #-}
  resourceWaitTimeChanged_ r =
    publishSignal $ resourceWaitTimeSource r

  {-# INLINABLE resourceChanged_ #-}
  resourceChanged_ r =
    resourceCountChanged_ r <>
    resourceUtilisationCountChanged_ r <>
    resourceQueueCountChanged_ r

  {-# INLINABLE requestResourceWithPriority #-}
  requestResourceWithPriority r priority =
    Process $ \pid ->
    Cont $ \c ->
    Event $ \p ->
    do let t = pointTime p
       a <- liftIO $ readIORef (resourceCountRef r)
       if a == 0
         then do f <- liftIO $ PQ.queueNull (resourceActingQueue r)
                 if f
                   then do c <- invokeEvent p $
                                freezeContReentering c () $
                                invokeCont c $
                                invokeProcess pid $
                                requestResourceWithPriority r priority
                           liftIO $ PQ.enqueue (resourceWaitQueue r) priority (Left $ ResourceRequestingItem priority t pid c)
                           invokeEvent p $ updateResourceQueueCount r 1
                   else do (p0', item0) <- liftIO $ PQ.queueFront (resourceActingQueue r)
                           let p0 = - p0'
                               pid0 = actingItemId item0
                           if priority < p0
                             then do liftIO $ PQ.dequeue (resourceActingQueue r)
                                     liftIO $ PQ.enqueue (resourceActingQueue r) (- priority) $ ResourceActingItem priority pid
                                     liftIO $ PQ.enqueue (resourceWaitQueue r) p0 (Right $ ResourcePreemptedItem p0 t pid0)
                                     invokeEvent p $ updateResourceQueueCount r 1
                                     invokeEvent p $ processPreemptionBegin pid0
                                     invokeEvent p $ resumeCont c ()
                             else do c <- invokeEvent p $
                                          freezeContReentering c () $
                                          invokeCont c $
                                          invokeProcess pid $
                                          requestResourceWithPriority r priority
                                     liftIO $ PQ.enqueue (resourceWaitQueue r) priority (Left $ ResourceRequestingItem priority t pid c)
                                     invokeEvent p $ updateResourceQueueCount r 1
         else do liftIO $ PQ.enqueue (resourceActingQueue r) (- priority) $ ResourceActingItem priority pid
                 invokeEvent p $ updateResourceWaitTime r 0
                 invokeEvent p $ updateResourceCount r (-1)
                 invokeEvent p $ updateResourceUtilisationCount r 1
                 invokeEvent p $ resumeCont c ()

  {-# INLINABLE releaseResource #-}
  releaseResource r = 
    Process $ \pid ->
    Cont $ \c ->
    Event $ \p ->
    do f <- liftIO $ fmap isJust $ PQ.queueDeleteBy (resourceActingQueue r) (\item -> actingItemId item == pid)
       if f
         then do invokeEvent p $ updateResourceUtilisationCount r (-1)
                 invokeEvent p $ releaseResource' r
                 invokeEvent p $ resumeCont c ()
         else fail $
              "The resource was not acquired by this process: releaseResource"

  {-# INLINABLE usingResourceWithPriority #-}
  usingResourceWithPriority r priority m =
    do requestResourceWithPriority r priority
       finallyProcess m $ releaseResource r

  {-# INLINABLE incResourceCount #-}
  incResourceCount r n
    | n < 0     = fail "The increment cannot be negative: incResourceCount"
    | n == 0    = return ()
    | otherwise =
      do releaseResource' r
         incResourceCount r (n - 1)

  {-# INLINABLE decResourceCount #-}
  decResourceCount r n
    | n < 0     = fail "The decrement cannot be negative: decResourceCount"
    | n == 0    = return ()
    | otherwise =
      do decResourceCount' r
         decResourceCount r (n - 1)

  {-# INLINABLE alterResourceCount #-}
  alterResourceCount r n
    | n < 0  = decResourceCount r (- n)
    | n > 0  = incResourceCount r n
    | n == 0 = return ()

-- | Identifies an acting item that acquired the resource.
data ResourceActingItem m =
  ResourceActingItem { actingItemPriority :: Double,
                       actingItemId :: ProcessId m }

-- | Idenitifies an awaiting item that waits for releasing of the resource to take it.
type ResourceAwaitingItem m = Either (ResourceRequestingItem m) (ResourcePreemptedItem m)

-- | Idenitifies an item that requests for the resource.
data ResourceRequestingItem m =
  ResourceRequestingItem { requestingItemPriority :: Double,
                           requestingItemTime :: Double,
                           requestingItemId :: ProcessId m,
                           requestingItemCont :: FrozenCont m () }

-- | Idenitifies an item that was preempted.
data ResourcePreemptedItem m =
  ResourcePreemptedItem { preemptedItemPriority :: Double,
                          preemptedItemTime :: Double,
                          preemptedItemId :: ProcessId m }

instance (MonadDES m, MonadIO m, MonadTemplate m) => Eq (Resource m) where

  {-# INLINABLE (==) #-}
  x == y = resourceCountRef x == resourceCountRef y  -- unique references

instance (MonadDES m, MonadIO m, MonadTemplate m) => Eq (ResourceActingItem m) where

  {-# INLINABLE (==) #-}
  x == y = actingItemId x == actingItemId y

-- | Release the resource increasing its count and resuming one of the
-- previously suspended or preempted processes as possible.
releaseResource' :: (MonadDES m, MonadIO m, MonadTemplate m)
                    => Resource m
                    -- ^ the resource to release
                    -> Event m ()
{-# INLINABLE releaseResource' #-}
releaseResource' r =
  Event $ \p ->
  do a <- liftIO $ readIORef (resourceCountRef r)
     let a' = a + 1
     case resourceMaxCount r of
       Just maxCount | a' > maxCount ->
         fail $
         "The resource count cannot be greater than " ++
         "its maximum value: releaseResource'."
       _ ->
         return ()
     f <- liftIO $ PQ.queueNull (resourceWaitQueue r)
     if f 
       then invokeEvent p $ updateResourceCount r 1
       else do (priority', item) <- liftIO $ PQ.queueFront (resourceWaitQueue r)
               liftIO $ PQ.dequeue (resourceWaitQueue r)
               invokeEvent p $ updateResourceQueueCount r (-1)
               case item of
                 Left (ResourceRequestingItem priority t pid c) ->
                   do c <- invokeEvent p $ unfreezeCont c
                      case c of
                        Nothing ->
                          invokeEvent p $ releaseResource' r
                        Just c ->
                          do liftIO $ PQ.enqueue (resourceActingQueue r) (- priority) $ ResourceActingItem priority pid
                             invokeEvent p $ updateResourceWaitTime r (pointTime p - t)
                             invokeEvent p $ updateResourceUtilisationCount r 1
                             invokeEvent p $ enqueueEvent (pointTime p) $ reenterCont c ()
                 Right (ResourcePreemptedItem priority t pid) ->
                   do f <- invokeEvent p $ processCancelled pid
                      case f of
                        True ->
                          invokeEvent p $ releaseResource' r
                        False ->
                          do liftIO $ PQ.enqueue (resourceActingQueue r) (- priority) $ ResourceActingItem priority pid
                             invokeEvent p $ updateResourceWaitTime r (pointTime p - t)
                             invokeEvent p $ updateResourceUtilisationCount r 1
                             invokeEvent p $ processPreemptionEnd pid

-- | Preempt a process with the lowest priority that acquires yet the resource
-- and decrease the count of available resource by 1. 
decResourceCount' :: (MonadDES m, MonadIO m, MonadTemplate m) => Resource m -> Event m ()
{-# INLINABLE decResourceCount' #-}
decResourceCount' r =
  Event $ \p ->
  do let t = pointTime p
     a <- liftIO $ readIORef (resourceCountRef r)
     when (a == 0) $
       fail $
       "The resource exceeded and its count is zero: decResourceCount'"
     f <- liftIO $ PQ.queueNull (resourceActingQueue r)
     unless f $
       do (p0', item0) <- liftIO $ PQ.queueFront (resourceActingQueue r)
          let p0 = - p0'
              pid0 = actingItemId item0
          liftIO $ PQ.dequeue (resourceActingQueue r)
          liftIO $ PQ.enqueue (resourceWaitQueue r) p0 (Right $ ResourcePreemptedItem p0 t pid0)
          invokeEvent p $ processPreemptionBegin pid0
          invokeEvent p $ updateResourceUtilisationCount r (-1)
          invokeEvent p $ updateResourceQueueCount r 1
     invokeEvent p $ updateResourceCount r (-1)

-- | Update the resource count and its statistics.
updateResourceCount :: (MonadDES m, MonadIO m, MonadTemplate m) => Resource m -> Int -> Event m ()
{-# INLINABLE updateResourceCount #-}
updateResourceCount r delta =
  Event $ \p ->
  do a <- liftIO $ readIORef (resourceCountRef r)
     let a' = a + delta
     a' `seq` liftIO $ writeIORef (resourceCountRef r) a'
     liftIO $
       modifyIORef' (resourceCountStatsRef r) $
       addTimingStats (pointTime p) a'
     invokeEvent p $
       triggerSignal (resourceCountSource r) a'

-- | Update the resource queue length and its statistics.
updateResourceQueueCount :: (MonadDES m, MonadIO m, MonadTemplate m) => Resource m -> Int -> Event m ()
{-# INLINABLE updateResourceQueueCount #-}
updateResourceQueueCount r delta =
  Event $ \p ->
  do a <- liftIO $ readIORef (resourceQueueCountRef r)
     let a' = a + delta
     a' `seq` liftIO $ writeIORef (resourceQueueCountRef r) a'
     liftIO $
       modifyIORef' (resourceQueueCountStatsRef r) $
       addTimingStats (pointTime p) a'
     invokeEvent p $
       triggerSignal (resourceQueueCountSource r) a'

-- | Update the resource utilisation count and its statistics.
updateResourceUtilisationCount :: (MonadDES m, MonadIO m, MonadTemplate m) => Resource m -> Int -> Event m ()
{-# INLINABLE updateResourceUtilisationCount #-}
updateResourceUtilisationCount r delta =
  Event $ \p ->
  do a <- liftIO $ readIORef (resourceUtilisationCountRef r)
     let a' = a + delta
     a' `seq` liftIO $ writeIORef (resourceUtilisationCountRef r) a'
     liftIO $
       modifyIORef' (resourceUtilisationCountStatsRef r) $
       addTimingStats (pointTime p) a'
     invokeEvent p $
       triggerSignal (resourceUtilisationCountSource r) a'

-- | Update the resource wait time and its statistics.
updateResourceWaitTime :: (MonadDES m, MonadIO m, MonadTemplate m) => Resource m -> Double -> Event m ()
{-# INLINABLE updateResourceWaitTime #-}
updateResourceWaitTime r delta =
  Event $ \p ->
  do a <- liftIO $ readIORef (resourceTotalWaitTimeRef r)
     let a' = a + delta
     a' `seq` liftIO $ writeIORef (resourceTotalWaitTimeRef r) a'
     liftIO $
       modifyIORef' (resourceWaitTimeRef r) $
       addSamplingStats delta
     invokeEvent p $
       triggerSignal (resourceWaitTimeSource r) ()
