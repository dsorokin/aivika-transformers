
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
       (withMVarComp) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Comp

withMVarComp :: (MonadComp m, MonadIO m) => MVar a -> (a -> m b) -> m b
