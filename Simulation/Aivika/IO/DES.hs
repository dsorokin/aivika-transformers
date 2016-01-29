
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.IO.DES
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- It allows making the 'MonadIO'-based monad an instance of type class 'MonadDES'
-- used for Discrete Event Simulation (DES).
--
module Simulation.Aivika.IO.DES () where

import Control.Monad.Trans

import Simulation.Aivika.IO.Comp
import Simulation.Aivika.IO.Ref.Base
import Simulation.Aivika.IO.Event

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Template

import Simulation.Aivika.IO.QueueStrategy

-- | A template-based instantiation of the 'MonadDES' type class.
instance (MonadComp m, MonadIO m, MonadTemplate m) => MonadDES m where

  {-# SPECIALISE instance MonadDES IO #-}
