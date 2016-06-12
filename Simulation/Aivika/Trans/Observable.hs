
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Trans.Observable
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines an observable entity, which value can be read within some computation.
--
module Simulation.Aivika.Trans.Observable
       (Observable(..)) where

import Control.Monad
import Control.Monad.Trans

-- | A class of observable entities.
class Observable o m where

  -- | Read the observable entity value within the specified computation.
  readObservable :: o a -> m a
