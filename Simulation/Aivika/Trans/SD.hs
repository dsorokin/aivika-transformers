
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.SD
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- It defines a type class of monads for System Dynamics (SD).
--
module Simulation.Aivika.Trans.SD (MonadSD) where

import Simulation.Aivika.Trans.Comp
import qualified Simulation.Aivika.Trans.Dynamics.Memo as M
import qualified Simulation.Aivika.Trans.Dynamics.Memo.Unboxed as MU

-- | A type class of monads for System Dynamics (SD). 
class (MonadComp m,
       M.MonadMemo m,
       MU.MonadMemo m Double,
       MU.MonadMemo m Float,
       MU.MonadMemo m Int) => MonadSD m
