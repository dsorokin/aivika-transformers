
-- |
-- Module     : Simulation.Aivika.IO.Exception
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- It instantiates a class of 'IO'-based monads with exception handling capabilities.
--
module Simulation.Aivika.IO.Exception where

import Control.Exception

import Simulation.Aivika.Trans.Exception

-- | An instance of the type class.
instance MonadException IO where

  {-# INLINE catchComp #-}
  catchComp = catch

  {-# INLINE finallyComp #-}
  finallyComp = finally

  {-# INLINE throwComp #-}
  throwComp = throw
