
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.IO.DES
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It instantiates a class of monads for Discrete Event Simulation (DES).
--
module Simulation.Aivika.IO.DES where

import Control.Monad.Trans

import Simulation.Aivika.IO.Comp
import Simulation.Aivika.IO.Ref.Base
import Simulation.Aivika.IO.Event

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Template

-- | A template-based instantiation of the 'MonadDES' type class.
instance (MonadComp m, MonadIO m, MonadTemplate m) => MonadDES m
