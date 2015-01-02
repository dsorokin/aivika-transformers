
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.Resource
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines the resource which can be acquired and 
-- then released by the discontinuous process 'Process'.
-- The resource can be either limited by the upper bound
-- (run-time check), or it can have no upper bound. The latter
-- is useful for modeling the infinite queue, for example.
--
module Simulation.Aivika.Trans.Resource
       (-- * Resource Types
        FCFSResource,
        LCFSResource,
        SIROResource,
        PriorityResource,
        Resource,
        -- * Creating Resource
        newFCFSResource,
        newFCFSResourceWithMaxCount,
        newLCFSResource,
        newLCFSResourceWithMaxCount,
        newSIROResource,
        newSIROResourceWithMaxCount,
        newPriorityResource,
        newPriorityResourceWithMaxCount,
        newResource,
        newResourceWithMaxCount,
        -- * Resource Properties
        resourceStrategy,
        resourceMaxCount,
        resourceCount,
        -- * Requesting for and Releasing Resource
        requestResource,
        requestResourceWithPriority,
        tryRequestResourceWithinEvent,
        releaseResource,
        releaseResourceWithinEvent,
        usingResource,
        usingResourceWithPriority) where

import Control.Monad
import Control.Monad.Trans
import Control.Exception

import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Cont
import Simulation.Aivika.Trans.Internal.Process
import Simulation.Aivika.Trans.QueueStrategy

-- | The ordinary FCFS (First Come - First Serviced) resource.
type FCFSResource m = Resource m FCFS

-- | The ordinary LCFS (Last Come - First Serviced) resource.
type LCFSResource m = Resource m LCFS

-- | The SIRO (Serviced in Random Order) resource.
type SIROResource m = Resource m SIRO

-- | The resource with static priorities.
type PriorityResource m = Resource m StaticPriorities

-- | Represents the resource with strategy @s@ applied for queuing the requests.
data Resource m s = 
  Resource { resourceStrategy :: s,
             -- ^ Return the strategy applied for queuing the requests.
             resourceMaxCount :: Maybe Int,
             -- ^ Return the maximum count of the resource, where 'Nothing'
             -- means that the resource has no upper bound.
             resourceCountRef :: Ref m Int, 
             resourceWaitList :: StrategyQueue m s (Event m (Maybe (ContParams m ()))) }

-- | Create a new FCFS resource with the specified initial count which value becomes
-- the upper bound as well.
newFCFSResource :: MonadDES m
                   => Int
                   -- ^ the initial count (and maximal count too) of the resource
                   -> Simulation m (FCFSResource m)
{-# INLINABLE newFCFSResource #-}
newFCFSResource = newResource FCFS

-- | Create a new FCFS resource with the specified initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newFCFSResourceWithMaxCount :: MonadDES m
                               => Int
                               -- ^ the initial count of the resource
                               -> Maybe Int
                               -- ^ the maximum count of the resource, which can be indefinite
                               -> Simulation m (FCFSResource m)
{-# INLINABLE newFCFSResourceWithMaxCount #-}
newFCFSResourceWithMaxCount = newResourceWithMaxCount FCFS

-- | Create a new LCFS resource with the specified initial count which value becomes
-- the upper bound as well.
newLCFSResource :: MonadDES m
                   => Int
                   -- ^ the initial count (and maximal count too) of the resource
                   -> Simulation m (LCFSResource m)
{-# INLINABLE newLCFSResource #-}
newLCFSResource = newResource LCFS

-- | Create a new LCFS resource with the specified initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newLCFSResourceWithMaxCount :: MonadDES m
                               => Int
                               -- ^ the initial count of the resource
                               -> Maybe Int
                               -- ^ the maximum count of the resource, which can be indefinite
                               -> Simulation m (LCFSResource m)
{-# INLINABLE newLCFSResourceWithMaxCount #-}
newLCFSResourceWithMaxCount = newResourceWithMaxCount LCFS

-- | Create a new SIRO resource with the specified initial count which value becomes
-- the upper bound as well.
newSIROResource :: (MonadDES m, QueueStrategy m SIRO)
                   => Int
                   -- ^ the initial count (and maximal count too) of the resource
                   -> Simulation m (SIROResource m)
{-# INLINABLE newSIROResource #-}
newSIROResource = newResource SIRO

-- | Create a new SIRO resource with the specified initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newSIROResourceWithMaxCount :: (MonadDES m, QueueStrategy m SIRO)
                               => Int
                               -- ^ the initial count of the resource
                               -> Maybe Int
                               -- ^ the maximum count of the resource, which can be indefinite
                               -> Simulation m (SIROResource m)
{-# INLINABLE newSIROResourceWithMaxCount #-}
newSIROResourceWithMaxCount = newResourceWithMaxCount SIRO

-- | Create a new priority resource with the specified initial count which value becomes
-- the upper bound as well.
newPriorityResource :: (MonadDES m, QueueStrategy m StaticPriorities)
                       => Int
                       -- ^ the initial count (and maximal count too) of the resource
                       -> Simulation m (PriorityResource m)
{-# INLINABLE newPriorityResource #-}
newPriorityResource = newResource StaticPriorities

-- | Create a new priority resource with the specified initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newPriorityResourceWithMaxCount :: (MonadDES m, QueueStrategy m StaticPriorities)
                                   => Int
                                   -- ^ the initial count of the resource
                                   -> Maybe Int
                                   -- ^ the maximum count of the resource, which can be indefinite
                                   -> Simulation m (PriorityResource m)
{-# INLINABLE newPriorityResourceWithMaxCount #-}
newPriorityResourceWithMaxCount = newResourceWithMaxCount StaticPriorities

-- | Create a new resource with the specified queue strategy and initial count.
-- The last value becomes the upper bound as well.
newResource :: (MonadDES m, QueueStrategy m s)
               => s
               -- ^ the strategy for managing the queuing requests
               -> Int
               -- ^ the initial count (and maximal count too) of the resource
               -> Simulation m (Resource m s)
{-# INLINABLE newResource #-}
newResource s count =
  Simulation $ \r ->
  do when (count < 0) $
       error $
       "The resource count cannot be negative: " ++
       "newResource."
     countRef <- invokeSimulation r $ newRef count
     waitList <- invokeSimulation r $ newStrategyQueue s
     return Resource { resourceStrategy = s,
                       resourceMaxCount = Just count,
                       resourceCountRef = countRef,
                       resourceWaitList = waitList }

-- | Create a new resource with the specified queue strategy, initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newResourceWithMaxCount :: (MonadDES m, QueueStrategy m s)
                           => s
                           -- ^ the strategy for managing the queuing requests
                           -> Int
                           -- ^ the initial count of the resource
                           -> Maybe Int
                           -- ^ the maximum count of the resource, which can be indefinite
                           -> Simulation m (Resource m s)
{-# INLINABLE newResourceWithMaxCount #-}
newResourceWithMaxCount s count maxCount =
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
     countRef <- invokeSimulation r $ newRef count
     waitList <- invokeSimulation r $ newStrategyQueue s
     return Resource { resourceStrategy = s,
                       resourceMaxCount = maxCount,
                       resourceCountRef = countRef,
                       resourceWaitList = waitList }

-- | Return the current count of the resource.
resourceCount :: MonadDES m => Resource m s -> Event m Int
{-# INLINABLE resourceCount #-}
resourceCount r =
  Event $ \p -> invokeEvent p $ readRef (resourceCountRef r)

-- | Request for the resource decreasing its count in case of success,
-- otherwise suspending the discontinuous process until some other 
-- process releases the resource.
requestResource :: (MonadDES m, EnqueueStrategy m s)
                   => Resource m s 
                   -- ^ the requested resource
                   -> Process m ()
{-# INLINABLE requestResource #-}
requestResource r =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do a <- invokeEvent p $ readRef (resourceCountRef r)
     if a == 0 
       then do c <- invokeEvent p $ contFreeze c
               invokeEvent p $
                 strategyEnqueue (resourceWaitList r) c
       else do let a' = a - 1
               a' `seq` invokeEvent p $ writeRef (resourceCountRef r) a'
               invokeEvent p $ resumeCont c ()

-- | Request with the priority for the resource decreasing its count
-- in case of success, otherwise suspending the discontinuous process
-- until some other process releases the resource.
requestResourceWithPriority :: (MonadDES m, PriorityQueueStrategy m s p)
                               => Resource m s
                               -- ^ the requested resource
                               -> p
                               -- ^ the priority
                               -> Process m ()
{-# INLINABLE requestResourceWithPriority #-}
requestResourceWithPriority r priority =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do a <- invokeEvent p $ readRef (resourceCountRef r)
     if a == 0 
       then do c <- invokeEvent p $ contFreeze c
               invokeEvent p $
                 strategyEnqueueWithPriority (resourceWaitList r) priority c
       else do let a' = a - 1
               a' `seq` invokeEvent p $ writeRef (resourceCountRef r) a'
               invokeEvent p $ resumeCont c ()

-- | Release the resource increasing its count and resuming one of the
-- previously suspended processes as possible.
releaseResource :: (MonadDES m, DequeueStrategy m s)
                   => Resource m s
                   -- ^ the resource to release
                   -> Process m ()
{-# INLINABLE releaseResource #-}
releaseResource r = 
  Process $ \_ ->
  Cont $ \c ->
  Event $ \p ->
  do invokeEvent p $ releaseResourceWithinEvent r
     invokeEvent p $ resumeCont c ()

-- | Release the resource increasing its count and resuming one of the
-- previously suspended processes as possible.
releaseResourceWithinEvent :: (MonadDES m, DequeueStrategy m s)
                              => Resource m s
                              -- ^ the resource to release
                              -> Event m ()
{-# INLINABLE releaseResourceWithinEvent #-}
releaseResourceWithinEvent r =
  Event $ \p ->
  do a <- invokeEvent p $ readRef (resourceCountRef r)
     let a' = a + 1
     case resourceMaxCount r of
       Just maxCount | a' > maxCount ->
         error $
         "The resource count cannot be greater than " ++
         "its maximum value: releaseResourceWithinEvent."
       _ ->
         return ()
     f <- invokeEvent p $
          strategyQueueNull (resourceWaitList r)
     if f 
       then a' `seq` invokeEvent p $ writeRef (resourceCountRef r) a'
       else do c <- invokeEvent p $
                    strategyDequeue (resourceWaitList r)
               c <- invokeEvent p c
               case c of
                 Nothing ->
                   invokeEvent p $ releaseResourceWithinEvent r
                 Just c  ->
                   invokeEvent p $ enqueueEvent (pointTime p) $ resumeCont c ()

-- | Try to request for the resource decreasing its count in case of success
-- and returning 'True' in the 'Event' monad; otherwise, returning 'False'.
tryRequestResourceWithinEvent :: MonadDES m
                                 => Resource m s
                                 -- ^ the resource which we try to request for
                                 -> Event m Bool
{-# INLINABLE tryRequestResourceWithinEvent #-}
tryRequestResourceWithinEvent r =
  Event $ \p ->
  do a <- invokeEvent p $ readRef (resourceCountRef r)
     if a == 0 
       then return False
       else do let a' = a - 1
               a' `seq` invokeEvent p $ writeRef (resourceCountRef r) a'
               return True
               
-- | Acquire the resource, perform some action and safely release the resource               
-- in the end, even if the 'IOException' was raised within the action. 
usingResource :: (MonadDES m, EnqueueStrategy m s)
                 => Resource m s
                 -- ^ the resource we are going to request for and then release in the end
                 -> Process m a
                 -- ^ the action we are going to apply having the resource
                 -> Process m a
                 -- ^ the result of the action
{-# INLINABLE usingResource #-}
usingResource r m =
  do requestResource r
     finallyProcess m $ releaseResource r

-- | Acquire the resource with the specified priority, perform some action and
-- safely release the resource in the end, even if the 'IOException' was raised
-- within the action.
usingResourceWithPriority :: (MonadDES m, PriorityQueueStrategy m s p)
                             => Resource m s
                             -- ^ the resource we are going to request for and then
                             -- release in the end
                             -> p
                             -- ^ the priority
                             -> Process m a
                             -- ^ the action we are going to apply having the resource
                             -> Process m a
                             -- ^ the result of the action
{-# INLINABLE usingResourceWithPriority #-}
usingResourceWithPriority r priority m =
  do requestResourceWithPriority r priority
     finallyProcess m $ releaseResource r
