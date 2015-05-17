
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.Array
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- The module defines helper functions for creating mutable arrays.
--
module Simulation.Aivika.Trans.Array
       (newIOArray_,
        newIOUArray_) where

import Data.Array.IO.Safe
import Data.Array.MArray.Safe

-- | Create a new 'IOArray'.
newIOArray_ :: Ix i => (i, i) -> IO (IOArray i e)
newIOArray_ = newArray_

-- | Create a new 'IOUArray'.
newIOUArray_ :: (Ix i, MArray IOUArray e IO) => (i, i) -> IO (IOUArray i e)
newIOUArray_ = newArray_
