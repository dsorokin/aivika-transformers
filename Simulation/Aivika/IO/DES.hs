
-- |
-- Module     : Simulation.Aivika.IO.DES
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- It makes the 'IO' monad an instance of type class 'MonadDES'
-- used for Discrete Event Simulation (DES).
--
module Simulation.Aivika.IO.DES () where

import Control.Monad.Trans

import Simulation.Aivika.IO.Comp
import qualified Simulation.Aivika.IO.Ref.Base.Strict as StrictRef
import qualified Simulation.Aivika.IO.Ref.Base.Lazy as LazyRef
import Simulation.Aivika.IO.Event

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.DES

import Simulation.Aivika.IO.QueueStrategy

-- | An instantiation of the 'MonadDES' type class.
instance MonadDES IO where
-- instance (Monad m, MonadComp m, MonadIO m, MonadTemplate m, MonadEventQueueTemplate m) => MonadDES m where

  {-# SPECIALISE instance MonadDES IO #-}
