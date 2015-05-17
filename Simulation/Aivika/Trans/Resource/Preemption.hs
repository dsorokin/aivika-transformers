
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Trans.Resource.Preemption
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- This module defines the preemptible resource.
--
module Simulation.Aivika.Trans.Resource.Preemption (MonadResource(..)) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Process

-- | A type class of monads whithin which we can create preemptible resources.
class MonadDES m => MonadResource m where

  -- | Represents a preemptible resource.
  data Resource m 

  -- | Create a new resource with the specified initial count that becomes the upper bound as well.
  newResource :: Int
                 -- ^ the initial count (and maximal count too) of the resource
                 -> Simulation m (Resource m)

  -- | Create a new resource with the specified initial and maximum counts,
  -- where 'Nothing' means that the resource has no upper bound.
  newResourceWithMaxCount :: Int
                             -- ^ the initial count of the resource
                             -> Maybe Int
                             -- ^ the maximum count of the resource, which can be indefinite
                             -> Simulation m (Resource m)

  -- | Return the current count of the resource.
  resourceCount :: Resource m -> Event m Int
  
  -- | Return the maximum count of the resource, where 'Nothing'
  -- means that the resource has no upper bound.
  resourceMaxCount :: Resource m -> Maybe Int
             
  -- | Request with the priority for the resource decreasing its count
  -- in case of success, otherwise suspending the discontinuous process
  -- until some other process releases the resource.
  --
  -- It may preempt another process if the latter aquired the resource before
  -- but had a lower priority. Then the current process takes an ownership of
  -- the resource.
  requestResourceWithPriority :: Resource m
                                 -- ^ the requested resource
                                 -> Double
                                 -- ^ the priority (the less value has a higher priority)
                                 -> Process m ()

  -- | Release the resource increasing its count and resuming one of the
  -- previously suspended or preempted processes as possible.
  releaseResource :: Resource m
                     -- ^ the resource to release
                     -> Process m ()

  -- | Acquire the resource with the specified priority, perform some action and
  -- safely release the resource in the end, even if the 'IOException' was raised
  -- within the action.
  usingResourceWithPriority :: Resource m
                               -- ^ the resource we are going to request for and then
                               -- release in the end
                               -> Double
                               -- ^ the priority (the less value has a higher priority)
                               -> Process m a
                               -- ^ the action we are going to apply having the resource
                               -> Process m a
                               -- ^ the result of the action

  -- | Increase the count of available resource by the specified number,
  -- invoking the awaiting and preempted processes according to their priorities
  -- as needed.
  incResourceCount :: Resource m
                      -- ^ the resource
                      -> Int
                      -- ^ the increment for the resource count
                      -> Event m ()

  -- | Decrease the count of available resource by the specified number,
  -- preempting the processes according to their priorities as needed.
  decResourceCount :: Resource m
                      -- ^ the resource
                      -> Int
                      -- ^ the decrement for the resource count
                      -> Event m ()

  -- | Alter the resource count either increasing or decreasing it by calling
  -- 'incResourceCount' or 'decResourceCount' respectively. 
  alterResourceCount :: Resource m
                        -- ^ the resource
                        -> Int
                        -- ^ a change of the resource count
                        -> Event m ()
