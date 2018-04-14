
-- |
-- Module     : Simulation.Aivika.Trans.Concurrent.MVar
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines helper functions for working with 'MVar'.
--
module Simulation.Aivika.Trans.Concurrent.MVar
       (withMVarComp,
        withMVarParameter,
        withMVarSimulation,
        withMVarDynamics,
        withMVarEvent) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Catch as MC

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Dynamics
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.Process

-- | Like 'withMVar' but operates within the specified computation.
withMVarComp :: (MonadComp m, MonadIO m, MC.MonadMask m) => MVar a -> (a -> m b) -> m b
withMVarComp v f =
  MC.mask $ \restore ->
  do a <- liftIO $ takeMVar v
     finallyComp
       (f a)
       (liftIO $ putMVar v a)

-- | Like 'withMVar' but operates within the 'Parameter' computation.
withMVarParameter :: (MonadComp m, MonadIO m, MC.MonadMask m) => MVar a -> (a -> Parameter m b) -> Parameter m b
withMVarParameter v f =
  MC.mask $ \restore ->
  do a <- liftIO $ takeMVar v
     finallyParameter
       (f a)
       (liftIO $ putMVar v a)

-- | Like 'withMVar' but operates within the 'Simulation' computation.
withMVarSimulation :: (MonadComp m, MonadIO m, MC.MonadMask m) => MVar a -> (a -> Simulation m b) -> Simulation m b
withMVarSimulation v f =
  MC.mask $ \restore ->
  do a <- liftIO $ takeMVar v
     finallySimulation
       (f a)
       (liftIO $ putMVar v a)

-- | Like 'withMVar' but operates within the 'Dynamics' computation.
withMVarDynamics :: (MonadComp m, MonadIO m, MC.MonadMask m) => MVar a -> (a -> Dynamics m b) -> Dynamics m b
withMVarDynamics v f =
  MC.mask $ \restore ->
  do a <- liftIO $ takeMVar v
     finallyDynamics
       (f a)
       (liftIO $ putMVar v a)

-- | Like 'withMVar' but operates within the 'Event' computation.
withMVarEvent :: (MonadComp m, MonadIO m, MC.MonadMask m) => MVar a -> (a -> Event m b) -> Event m b
withMVarEvent v f =
  MC.mask $ \restore ->
  do a <- liftIO $ takeMVar v
     finallyEvent
       (f a)
       (liftIO $ putMVar v a)
