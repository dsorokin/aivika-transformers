
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.IO.Comp
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- It instantiates a class of 'IO'-based monads on top of which the simulation monads can be built.
--
module Simulation.Aivika.IO.Comp where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.IO.Exception
import Simulation.Aivika.IO.Generator

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Template

-- | A template-based instantiation of the 'MonadComp' type class. 
instance (Functor m, Monad m, MonadIO m, MonadException m, MonadTemplate m) => MonadComp m where

  {-# SPECIALISE instance MonadComp IO #-}
