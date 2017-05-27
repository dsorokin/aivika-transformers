
-- |
-- Module     : Simulation.Aivika.IO.Exception
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- It provides with exception handling capabilities,
-- where 'IO' is an instance of 'MonadException'.
--
module Simulation.Aivika.IO.Exception () where

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
