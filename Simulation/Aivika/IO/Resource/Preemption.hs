
{-# LANGUAGE TypeFamilies, FlexibleInstances, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.IO.Resource.Preemption
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- This module defines the preemptible resource.
--
module Simulation.Aivika.IO.Resource.Preemption () where

import Control.Monad
import Control.Monad.Trans

import Data.Maybe
import Data.IORef

import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Template
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Cont
import Simulation.Aivika.Trans.Internal.Process
import Simulation.Aivika.Trans.Resource.Preemption

import Simulation.Aivika.IO.DES

import qualified Simulation.Aivika.PriorityQueue as PQ

-- | The 'MonadIO' based monad is an instance of 'MonadResource'.
instance (MonadDES m, MonadIO m, MonadTemplate m) => MonadResource m where

  {-# SPECIALISE instance MonadResource IO #-}

  -- | A template-based implementation of the preemptible resource.
  data Resource m = 
    Resource { resourceMaxCount0 :: Maybe Int,
               resourceCountRef :: IORef Int,
               resourceActingQueue :: PQ.PriorityQueue (ResourceActingItem m),
               resourceWaitQueue :: PQ.PriorityQueue (ResourceAwaitingItem m) }

  {-# INLINABLE newResource #-}
  newResource count =
    Simulation $ \r ->
    do when (count < 0) $
         error $
         "The resource count cannot be negative: " ++
         "newResource."
       countRef <- liftIO $ newIORef count
       actingQueue <- liftIO PQ.newQueue
       waitQueue <- liftIO PQ.newQueue
       return Resource { resourceMaxCount0 = Just count,
                         resourceCountRef = countRef,
                         resourceActingQueue = actingQueue,
                         resourceWaitQueue = waitQueue }

  {-# INLINABLE newResourceWithMaxCount #-}
  newResourceWithMaxCount count maxCount =
    Simulation $ \r ->
    do when (count < 0) $
         error $
         "The resource count cannot be negative: " ++
         "newResourceWithMaxCount."
       case maxCount of
         Just maxCount | count > maxCount ->
           error $
           "The resource count cannot be greater than " ++
           "its maximum value: newResourceWithMaxCount."
         _ ->
           return ()
       countRef <- liftIO $ newIORef count
       actingQueue <- liftIO PQ.newQueue
       waitQueue <- liftIO PQ.newQueue
       return Resource { resourceMaxCount0 = maxCount,
                         resourceCountRef = countRef,
                         resourceActingQueue = actingQueue,
                         resourceWaitQueue = waitQueue }

  {-# INLINABLE resourceCount #-}
  resourceCount r =
    Event $ \p -> liftIO $ readIORef (resourceCountRef r)

  {-# INLINABLE resourceMaxCount #-}
  resourceMaxCount = resourceMaxCount0

  {-# INLINABLE requestResourceWithPriority #-}
  requestResourceWithPriority r priority =
    Process $ \pid ->
    Cont $ \c ->
    Event $ \p ->
    do a <- liftIO $ readIORef (resourceCountRef r)
       if a == 0
         then do f <- liftIO $ PQ.queueNull (resourceActingQueue r)
                 if f
                   then do c <- invokeEvent p $
                                freezeContReentering c () $
                                invokeCont c $
                                invokeProcess pid $
                                requestResourceWithPriority r priority
                           liftIO $ PQ.enqueue (resourceWaitQueue r) priority (Left $ ResourceRequestingItem priority pid c)
                   else do (p0', item0) <- liftIO $ PQ.queueFront (resourceActingQueue r)
                           let p0 = - p0'
                               pid0 = actingItemId item0
                           if priority < p0
                             then do liftIO $ PQ.dequeue (resourceActingQueue r)
                                     liftIO $ PQ.enqueue (resourceActingQueue r) (- priority) $ ResourceActingItem priority pid
                                     liftIO $ PQ.enqueue (resourceWaitQueue r) p0 (Right $ ResourcePreemptedItem p0 pid0)
                                     invokeEvent p $ processPreemptionBegin pid0
                                     invokeEvent p $ resumeCont c ()
                             else do c <- invokeEvent p $
                                          freezeContReentering c () $
                                          invokeCont c $
                                          invokeProcess pid $
                                          requestResourceWithPriority r priority
                                     liftIO $ PQ.enqueue (resourceWaitQueue r) priority (Left $ ResourceRequestingItem priority pid c)
         else do let a' = a - 1
                 a' `seq` liftIO $ writeIORef (resourceCountRef r) a'
                 liftIO $ PQ.enqueue (resourceActingQueue r) (- priority) $ ResourceActingItem priority pid
                 invokeEvent p $ resumeCont c ()

  {-# INLINABLE releaseResource #-}
  releaseResource r = 
    Process $ \pid ->
    Cont $ \c ->
    Event $ \p ->
    do f <- liftIO $ fmap isJust $ PQ.queueDeleteBy (resourceActingQueue r) (\item -> actingItemId item == pid)
       if f
         then do invokeEvent p $ releaseResource' r
                 invokeEvent p $ resumeCont c ()
         else error $
              "The resource was not acquired by this process: releaseResource"
               
  {-# INLINABLE usingResourceWithPriority #-}
  usingResourceWithPriority r priority m =
    do requestResourceWithPriority r priority
       finallyProcess m $ releaseResource r

  {-# INLINABLE incResourceCount #-}
  incResourceCount r n
    | n < 0     = error "The increment cannot be negative: incResourceCount"
    | n == 0    = return ()
    | otherwise =
      do releaseResource' r
         incResourceCount r (n - 1)

  {-# INLINABLE decResourceCount #-}
  decResourceCount r n
    | n < 0     = error "The decrement cannot be negative: decResourceCount"
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

-- | Idenitifies an item that requests for the resource.
data ResourceRequestingItem m =
  ResourceRequestingItem { requestingItemPriority :: Double,
                           requestingItemId :: ProcessId m,
                           requestingItemCont :: FrozenCont m () }

-- | Idenitifies an item that was preempted.
data ResourcePreemptedItem m =
  ResourcePreemptedItem { preemptedItemPriority :: Double,
                          preemptedItemId :: ProcessId m }

-- | Idenitifies an awaiting item that waits for releasing of the resource to take it.
type ResourceAwaitingItem m =
  Either (ResourceRequestingItem m) (ResourcePreemptedItem m)

instance (MonadDES m, MonadIO m, MonadTemplate m) => Eq (Resource m) where

  {-# INLINABLE (==) #-}
  x == y = resourceCountRef x == resourceCountRef y  -- unique references

instance (MonadDES m, MonadIO m, MonadTemplate m) => Eq (ResourceActingItem m) where

  {-# INLINABLE (==) #-}
  x == y = actingItemId x == actingItemId y

releaseResource' :: (MonadDES m, MonadIO m, MonadTemplate m) => Resource m -> Event m ()
{-# INLINABLE releaseResource' #-}
releaseResource' r =
  Event $ \p ->
  do a <- liftIO $ readIORef (resourceCountRef r)
     let a' = a + 1
     case resourceMaxCount r of
       Just maxCount | a' > maxCount ->
         error $
         "The resource count cannot be greater than " ++
         "its maximum value: releaseResource'."
       _ ->
         return ()
     f <- liftIO $ PQ.queueNull (resourceWaitQueue r)
     if f 
       then a' `seq` liftIO $ writeIORef (resourceCountRef r) a'
       else do (priority', item) <- liftIO $ PQ.queueFront (resourceWaitQueue r)
               liftIO $ PQ.dequeue (resourceWaitQueue r)
               case item of
                 Left (ResourceRequestingItem priority pid c) ->
                   do c <- invokeEvent p $ unfreezeCont c
                      case c of
                        Nothing ->
                          invokeEvent p $ releaseResource' r
                        Just c ->
                          do liftIO $ PQ.enqueue (resourceActingQueue r) (- priority) $ ResourceActingItem priority pid
                             invokeEvent p $ enqueueEvent (pointTime p) $ resumeCont c ()
                 Right (ResourcePreemptedItem priority pid) ->
                   do f <- invokeEvent p $ processCancelled pid
                      case f of
                        True ->
                          invokeEvent p $ releaseResource' r
                        False ->
                          do liftIO $ PQ.enqueue (resourceActingQueue r) (- priority) $ ResourceActingItem priority pid
                             invokeEvent p $ processPreemptionEnd pid

decResourceCount' :: (MonadDES m, MonadIO m, MonadTemplate m) => Resource m -> Event m ()
{-# INLINABLE decResourceCount' #-}
decResourceCount' r =
  Event $ \p ->
  do a <- liftIO $ readIORef (resourceCountRef r)
     when (a == 0) $
       error $
       "The resource exceeded and its count is zero: decResourceCount'"
     f <- liftIO $ PQ.queueNull (resourceActingQueue r)
     unless f $
       do (p0', item0) <- liftIO $ PQ.queueFront (resourceActingQueue r)
          let p0 = - p0'
              pid0 = actingItemId item0
          liftIO $ PQ.dequeue (resourceActingQueue r)
          liftIO $ PQ.enqueue (resourceWaitQueue r) p0 (Right $ ResourcePreemptedItem p0 pid0)
          invokeEvent p $ processPreemptionBegin pid0
     let a' = a - 1
     a' `seq` liftIO $ writeIORef (resourceCountRef r) a'
