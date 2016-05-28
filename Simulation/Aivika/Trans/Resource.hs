
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.Resource
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
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
        resourceCountStats,
        resourceUtilisationCount,
        resourceUtilisationCountStats,
        resourceQueueCount,
        resourceQueueCountStats,
        resourceTotalWaitTime,
        resourceWaitTime,
        -- * Requesting for and Releasing Resource
        requestResource,
        requestResourceWithPriority,
        tryRequestResourceWithinEvent,
        releaseResource,
        releaseResourceWithinEvent,
        usingResource,
        usingResourceWithPriority,
        -- * Altering Resource
        incResourceCount,
        decResourceCount,
        -- * Signals
        resourceCountChanged,
        resourceCountChanged_,
        resourceUtilisationCountChanged,
        resourceUtilisationCountChanged_,
        resourceQueueCountChanged,
        resourceQueueCountChanged_,
        resourceWaitTimeChanged,
        resourceWaitTimeChanged_,
        resourceChanged_) where

import Data.Monoid

import Control.Monad
import Control.Monad.Trans
import Control.Exception

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Cont
import Simulation.Aivika.Trans.Internal.Process
import Simulation.Aivika.Trans.QueueStrategy
import Simulation.Aivika.Trans.Statistics
import Simulation.Aivika.Trans.Signal

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
             resourceCountStatsRef :: Ref m (TimingStats Int),
             resourceCountSource :: SignalSource m Int,
             resourceUtilisationCountRef :: Ref m Int,
             resourceUtilisationCountStatsRef :: Ref m (TimingStats Int),
             resourceUtilisationCountSource :: SignalSource m Int,
             resourceQueueCountRef :: Ref m Int,
             resourceQueueCountStatsRef :: Ref m (TimingStats Int),
             resourceQueueCountSource :: SignalSource m Int,
             resourceTotalWaitTimeRef :: Ref m Double,
             resourceWaitTimeRef :: Ref m (SamplingStats Double),
             resourceWaitTimeSource :: SignalSource m (),
             resourceWaitList :: StrategyQueue m s (ResourceItem m) }

data ResourceItem m =
  ResourceItem { resourceItemTime :: Double,
                 resourceItemCont :: FrozenCont m () }

-- | Create a new FCFS resource with the specified initial count which value becomes
-- the upper bound as well.
newFCFSResource :: MonadDES m
                   => Int
                   -- ^ the initial count (and maximal count too) of the resource
                   -> Event m (FCFSResource m)
{-# INLINABLE newFCFSResource #-}
newFCFSResource = newResource FCFS

-- | Create a new FCFS resource with the specified initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newFCFSResourceWithMaxCount :: MonadDES m
                               => Int
                               -- ^ the initial count of the resource
                               -> Maybe Int
                               -- ^ the maximum count of the resource, which can be indefinite
                               -> Event m (FCFSResource m)
{-# INLINABLE newFCFSResourceWithMaxCount #-}
newFCFSResourceWithMaxCount = newResourceWithMaxCount FCFS

-- | Create a new LCFS resource with the specified initial count which value becomes
-- the upper bound as well.
newLCFSResource :: MonadDES m
                   => Int
                   -- ^ the initial count (and maximal count too) of the resource
                   -> Event m (LCFSResource m)
{-# INLINABLE newLCFSResource #-}
newLCFSResource = newResource LCFS

-- | Create a new LCFS resource with the specified initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newLCFSResourceWithMaxCount :: MonadDES m
                               => Int
                               -- ^ the initial count of the resource
                               -> Maybe Int
                               -- ^ the maximum count of the resource, which can be indefinite
                               -> Event m (LCFSResource m)
{-# INLINABLE newLCFSResourceWithMaxCount #-}
newLCFSResourceWithMaxCount = newResourceWithMaxCount LCFS

-- | Create a new SIRO resource with the specified initial count which value becomes
-- the upper bound as well.
newSIROResource :: (MonadDES m, QueueStrategy m SIRO)
                   => Int
                   -- ^ the initial count (and maximal count too) of the resource
                   -> Event m (SIROResource m)
{-# INLINABLE newSIROResource #-}
newSIROResource = newResource SIRO

-- | Create a new SIRO resource with the specified initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newSIROResourceWithMaxCount :: (MonadDES m, QueueStrategy m SIRO)
                               => Int
                               -- ^ the initial count of the resource
                               -> Maybe Int
                               -- ^ the maximum count of the resource, which can be indefinite
                               -> Event m (SIROResource m)
{-# INLINABLE newSIROResourceWithMaxCount #-}
newSIROResourceWithMaxCount = newResourceWithMaxCount SIRO

-- | Create a new priority resource with the specified initial count which value becomes
-- the upper bound as well.
newPriorityResource :: (MonadDES m, QueueStrategy m StaticPriorities)
                       => Int
                       -- ^ the initial count (and maximal count too) of the resource
                       -> Event m (PriorityResource m)
{-# INLINABLE newPriorityResource #-}
newPriorityResource = newResource StaticPriorities

-- | Create a new priority resource with the specified initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newPriorityResourceWithMaxCount :: (MonadDES m, QueueStrategy m StaticPriorities)
                                   => Int
                                   -- ^ the initial count of the resource
                                   -> Maybe Int
                                   -- ^ the maximum count of the resource, which can be indefinite
                                   -> Event m (PriorityResource m)
{-# INLINABLE newPriorityResourceWithMaxCount #-}
newPriorityResourceWithMaxCount = newResourceWithMaxCount StaticPriorities

-- | Create a new resource with the specified queue strategy and initial count.
-- The last value becomes the upper bound as well.
newResource :: (MonadDES m, QueueStrategy m s)
               => s
               -- ^ the strategy for managing the queuing requests
               -> Int
               -- ^ the initial count (and maximal count too) of the resource
               -> Event m (Resource m s)
{-# INLINABLE newResource #-}
newResource s count =
  newResourceWithMaxCount s count (Just count)

-- | Create a new resource with the specified queue strategy, initial and maximum counts,
-- where 'Nothing' means that the resource has no upper bound.
newResourceWithMaxCount :: (MonadDES m, QueueStrategy m s)
                           => s
                           -- ^ the strategy for managing the queuing requests
                           -> Int
                           -- ^ the initial count of the resource
                           -> Maybe Int
                           -- ^ the maximum count of the resource, which can be indefinite
                           -> Event m (Resource m s)
{-# INLINABLE newResourceWithMaxCount #-}
newResourceWithMaxCount s count maxCount =
  Event $ \p ->
  do let r = pointRun p
         t = pointTime p
     when (count < 0) $
       throwComp $
       SimulationRetry $
       "The resource count cannot be negative: " ++
       "newResourceWithMaxCount."
     case maxCount of
       Just maxCount | count > maxCount ->
         throwComp $
         SimulationRetry $
         "The resource count cannot be greater than " ++
         "its maximum value: newResourceWithMaxCount."
       _ ->
         return ()
     countRef <- invokeSimulation r $ newRef count
     countStatsRef <- invokeSimulation r $ newRef $ returnTimingStats t count
     countSource <- invokeSimulation r newSignalSource
     utilCountRef <- invokeSimulation r $ newRef 0
     utilCountStatsRef <- invokeSimulation r $ newRef $ returnTimingStats t 0
     utilCountSource <- invokeSimulation r newSignalSource
     queueCountRef <- invokeSimulation r $ newRef 0
     queueCountStatsRef <- invokeSimulation r $ newRef $ returnTimingStats t 0
     queueCountSource <- invokeSimulation r newSignalSource
     totalWaitTimeRef <- invokeSimulation r $ newRef 0
     waitTimeRef <- invokeSimulation r $ newRef emptySamplingStats
     waitTimeSource <- invokeSimulation r newSignalSource
     waitList <- invokeSimulation r $ newStrategyQueue s
     return Resource { resourceStrategy = s,
                       resourceMaxCount = maxCount,
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
                       resourceWaitList = waitList }

-- | Return the current available count of the resource.
resourceCount :: MonadDES m => Resource m s -> Event m Int
{-# INLINABLE resourceCount #-}
resourceCount r =
  Event $ \p -> invokeEvent p $ readRef (resourceCountRef r)

-- | Return the statistics for the available count of the resource.
resourceCountStats :: MonadDES m => Resource m s -> Event m (TimingStats Int)
{-# INLINABLE resourceCountStats #-}
resourceCountStats r =
  Event $ \p -> invokeEvent p $ readRef (resourceCountStatsRef r)

-- | Signal triggered when the 'resourceCount' property changes.
resourceCountChanged :: MonadDES m => Resource m s -> Signal m Int
{-# INLINABLE resourceCountChanged #-}
resourceCountChanged r =
  publishSignal $ resourceCountSource r

-- | Signal triggered when the 'resourceCount' property changes.
resourceCountChanged_ :: MonadDES m => Resource m s -> Signal m ()
{-# INLINABLE resourceCountChanged_ #-}
resourceCountChanged_ r =
  mapSignal (const ()) $ resourceCountChanged r

-- | Return the current utilisation count of the resource.
resourceUtilisationCount :: MonadDES m => Resource m s -> Event m Int
{-# INLINABLE resourceUtilisationCount #-}
resourceUtilisationCount r =
  Event $ \p -> invokeEvent p $ readRef (resourceUtilisationCountRef r)

-- | Return the statistics for the utilisation count of the resource.
resourceUtilisationCountStats :: MonadDES m => Resource m s -> Event m (TimingStats Int)
{-# INLINABLE resourceUtilisationCountStats #-}
resourceUtilisationCountStats r =
  Event $ \p -> invokeEvent p $ readRef (resourceUtilisationCountStatsRef r)

-- | Signal triggered when the 'resourceUtilisationCount' property changes.
resourceUtilisationCountChanged :: MonadDES m => Resource m s -> Signal m Int
{-# INLINABLE resourceUtilisationCountChanged #-}
resourceUtilisationCountChanged r =
  publishSignal $ resourceUtilisationCountSource r

-- | Signal triggered when the 'resourceUtilisationCount' property changes.
resourceUtilisationCountChanged_ :: MonadDES m => Resource m s -> Signal m ()
{-# INLINABLE resourceUtilisationCountChanged_ #-}
resourceUtilisationCountChanged_ r =
  mapSignal (const ()) $ resourceUtilisationCountChanged r

-- | Return the current queue length of the resource.
resourceQueueCount :: MonadDES m => Resource m s -> Event m Int
{-# INLINABLE resourceQueueCount #-}
resourceQueueCount r =
  Event $ \p -> invokeEvent p $ readRef (resourceQueueCountRef r)

-- | Return the statistics for the queue length of the resource.
resourceQueueCountStats :: MonadDES m => Resource m s -> Event m (TimingStats Int)
{-# INLINABLE resourceQueueCountStats #-}
resourceQueueCountStats r =
  Event $ \p -> invokeEvent p $ readRef (resourceQueueCountStatsRef r)

-- | Signal triggered when the 'resourceQueueCount' property changes.
resourceQueueCountChanged :: MonadDES m => Resource m s -> Signal m Int
{-# INLINABLE resourceQueueCountChanged #-}
resourceQueueCountChanged r =
  publishSignal $ resourceQueueCountSource r

-- | Signal triggered when the 'resourceQueueCount' property changes.
resourceQueueCountChanged_ :: MonadDES m => Resource m s -> Signal m ()
{-# INLINABLE resourceQueueCountChanged_ #-}
resourceQueueCountChanged_ r =
  mapSignal (const ()) $ resourceQueueCountChanged r

-- | Return the total wait time of the resource.
resourceTotalWaitTime :: MonadDES m => Resource m s -> Event m Double
{-# INLINABLE resourceTotalWaitTime #-}
resourceTotalWaitTime r =
  Event $ \p -> invokeEvent p $ readRef (resourceTotalWaitTimeRef r)

-- | Return the statistics for the wait time of the resource.
resourceWaitTime :: MonadDES m => Resource m s -> Event m (SamplingStats Double)
{-# INLINABLE resourceWaitTime #-}
resourceWaitTime r =
  Event $ \p -> invokeEvent p $ readRef (resourceWaitTimeRef r)

-- | Signal triggered when the 'resourceTotalWaitTime' and 'resourceWaitTime' properties change.
resourceWaitTimeChanged :: MonadDES m => Resource m s -> Signal m (SamplingStats Double)
{-# INLINABLE resourceWaitTimeChanged #-}
resourceWaitTimeChanged r =
  mapSignalM (\() -> resourceWaitTime r) $ resourceWaitTimeChanged_ r

-- | Signal triggered when the 'resourceTotalWaitTime' and 'resourceWaitTime' properties change.
resourceWaitTimeChanged_ :: MonadDES m => Resource m s -> Signal m ()
{-# INLINABLE resourceWaitTimeChanged_ #-}
resourceWaitTimeChanged_ r =
  publishSignal $ resourceWaitTimeSource r

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
       then do c <- invokeEvent p $
                    freezeContReentering c () $
                    invokeCont c $
                    invokeProcess pid $
                    requestResource r
               invokeEvent p $
                 strategyEnqueue (resourceWaitList r) $
                 ResourceItem (pointTime p) c
               invokeEvent p $ updateResourceQueueCount r 1
       else do invokeEvent p $ updateResourceWaitTime r 0
               invokeEvent p $ updateResourceCount r (-1)
               invokeEvent p $ updateResourceUtilisationCount r 1
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
       then do c <- invokeEvent p $
                    freezeContReentering c () $
                    invokeCont c $
                    invokeProcess pid $
                    requestResourceWithPriority r priority
               invokeEvent p $
                 strategyEnqueueWithPriority (resourceWaitList r) priority $
                 ResourceItem (pointTime p) c
               invokeEvent p $ updateResourceQueueCount r 1
       else do invokeEvent p $ updateResourceWaitTime r 0
               invokeEvent p $ updateResourceCount r (-1)
               invokeEvent p $ updateResourceUtilisationCount r 1
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
  do invokeEvent p $ updateResourceUtilisationCount r (-1)
     invokeEvent p $ releaseResource' r
  
-- | Release the resource without affecting its utilisation.
releaseResource' :: (MonadDES m, DequeueStrategy m s)
                    => Resource m s
                    -- ^ the resource to release
                    -> Event m ()
{-# INLINABLE releaseResource' #-}
releaseResource' r =
  Event $ \p ->
  do a <- invokeEvent p $ readRef (resourceCountRef r)
     let a' = a + 1
     case resourceMaxCount r of
       Just maxCount | a' > maxCount ->
         throwComp $
         SimulationRetry $
         "The resource count cannot be greater than " ++
         "its maximum value: releaseResource'."
       _ ->
         return ()
     f <- invokeEvent p $
          strategyQueueNull (resourceWaitList r)
     if f 
       then invokeEvent p $ updateResourceCount r 1
       else do x <- invokeEvent p $
                    strategyDequeue (resourceWaitList r)
               invokeEvent p $ updateResourceQueueCount r (-1)
               c <- invokeEvent p $ unfreezeCont (resourceItemCont x)
               case c of
                 Nothing ->
                   invokeEvent p $ releaseResource' r
                 Just c  ->
                   do invokeEvent p $ updateResourceWaitTime r (pointTime p - resourceItemTime x)
                      invokeEvent p $ updateResourceUtilisationCount r 1
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
       else do invokeEvent p $ updateResourceWaitTime r 0
               invokeEvent p $ updateResourceCount r (-1)
               invokeEvent p $ updateResourceUtilisationCount r 1
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

-- | Decrease the count of available resource.
decResourceCount' :: (MonadDES m, EnqueueStrategy m s)
                     => Resource m s
                     -- ^ the resource for which to decrease the count
                     -> Process m ()
{-# INLINABLE decResourceCount' #-}
decResourceCount' r =
  do liftEvent $
       updateResourceUtilisationCount r (-1)
     requestResource r
                   
-- | Increase the count of available resource by the specified number,
-- invoking the awaiting processes as needed.
incResourceCount :: (MonadDES m, DequeueStrategy m s)
                    => Resource m s
                    -- ^ the resource
                    -> Int
                    -- ^ the increment for the resource count
                    -> Event m ()
{-# INLINABLE incResourceCount #-}
incResourceCount r n
  | n < 0     = throwEvent $ SimulationRetry "The increment cannot be negative: incResourceCount"
  | n == 0    = return ()
  | otherwise =
    do releaseResource' r
       incResourceCount r (n - 1)

-- | Decrease the count of available resource by the specified number,
-- waiting for the processes capturing the resource as needed.
decResourceCount :: (MonadDES m, EnqueueStrategy m s)
                    => Resource m s
                    -- ^ the resource
                    -> Int
                    -- ^ the decrement for the resource count
                    -> Process m ()
{-# INLINABLE decResourceCount #-}
decResourceCount r n
  | n < 0     = throwProcess $ SimulationRetry "The decrement cannot be negative: decResourceCount"
  | n == 0    = return ()
  | otherwise =
    do decResourceCount' r
       decResourceCount r (n - 1)

-- | Signal triggered when one of the resource counters changes.
resourceChanged_ :: MonadDES m => Resource m s -> Signal m ()
{-# INLINABLE resourceChanged_ #-}
resourceChanged_ r =
  resourceCountChanged_ r <>
  resourceUtilisationCountChanged_ r <>
  resourceQueueCountChanged_ r

-- | Update the resource count and its statistics.
updateResourceCount :: MonadDES m => Resource m s -> Int -> Event m ()
{-# INLINABLE updateResourceCount #-}
updateResourceCount r delta =
  Event $ \p ->
  do a <- invokeEvent p $ readRef (resourceCountRef r)
     let a' = a + delta
     a' `seq` invokeEvent p $ writeRef (resourceCountRef r) a'
     invokeEvent p $
       modifyRef (resourceCountStatsRef r) $
       addTimingStats (pointTime p) a'
     invokeEvent p $
       triggerSignal (resourceCountSource r) a'

-- | Update the resource utilisation count and its statistics.
updateResourceUtilisationCount :: MonadDES m => Resource m s -> Int -> Event m ()
{-# INLINABLE updateResourceUtilisationCount #-}
updateResourceUtilisationCount r delta =
  Event $ \p ->
  do a <- invokeEvent p $ readRef (resourceUtilisationCountRef r)
     let a' = a + delta
     a' `seq` invokeEvent p $ writeRef (resourceUtilisationCountRef r) a'
     invokeEvent p $
       modifyRef (resourceUtilisationCountStatsRef r) $
       addTimingStats (pointTime p) a'
     invokeEvent p $
       triggerSignal (resourceUtilisationCountSource r) a'

-- | Update the resource queue length and its statistics.
updateResourceQueueCount :: MonadDES m => Resource m s -> Int -> Event m ()
{-# INLINABLE updateResourceQueueCount #-}
updateResourceQueueCount r delta =
  Event $ \p ->
  do a <- invokeEvent p $ readRef (resourceQueueCountRef r)
     let a' = a + delta
     a' `seq` invokeEvent p $ writeRef (resourceQueueCountRef r) a'
     invokeEvent p $
       modifyRef (resourceQueueCountStatsRef r) $
       addTimingStats (pointTime p) a'
     invokeEvent p $
       triggerSignal (resourceQueueCountSource r) a'

-- | Update the resource wait time and its statistics.
updateResourceWaitTime :: MonadDES m => Resource m s -> Double -> Event m ()
{-# INLINABLE updateResourceWaitTime #-}
updateResourceWaitTime r delta =
  Event $ \p ->
  do a <- invokeEvent p $ readRef (resourceTotalWaitTimeRef r)
     let a' = a + delta
     a' `seq` invokeEvent p $ writeRef (resourceTotalWaitTimeRef r) a'
     invokeEvent p $
       modifyRef (resourceWaitTimeRef r) $
       addSamplingStats delta
     invokeEvent p $
       triggerSignal (resourceWaitTimeSource r) ()
