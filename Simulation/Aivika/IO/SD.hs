
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.IO.SD
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- It allows making the 'MonadIO'-based monad an instance of type class 'MonadSD'
-- used for System Dynamics (SD).
--
module Simulation.Aivika.IO.SD () where

import Control.Monad.Trans

import Simulation.Aivika.IO.Comp
import qualified Simulation.Aivika.IO.Dynamics.Memo as M
import qualified Simulation.Aivika.IO.Dynamics.Memo.Unboxed as MU

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.SD
import Simulation.Aivika.Trans.Template

-- | A template-based instantiation of the 'MonadSD' type class.
instance (Monad m, MonadComp m, MonadIO m, MonadTemplate m) => MonadSD m where
  
  {-# SPECIALISE instance MonadSD IO #-}
