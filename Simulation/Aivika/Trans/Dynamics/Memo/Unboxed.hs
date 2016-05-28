
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Trans.Dynamics.Memo.Unboxed
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- This module defines the unboxed memo functions. The memoization creates such 'Dynamics'
-- computations, which values are cached in the integration time points. Then
-- these values are interpolated in all other time points.
--

module Simulation.Aivika.Trans.Dynamics.Memo.Unboxed
       (MonadMemo(..)) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics

-- | A monad with the support of unboxed memoisation.
class Monad m => MonadMemo m e where

  -- | Memoize and order the computation in the integration time points using 
  -- the interpolation that knows of the Runge-Kutta method. The values are
  -- calculated sequentially starting from 'starttime'.
  memoDynamics :: Dynamics m e -> Simulation m (Dynamics m e)

  -- | Memoize and order the computation in the integration time points using 
  -- the 'discreteDynamics' interpolation. It consumes less memory than the 'memoDynamics'
  -- function but it is not aware of the Runge-Kutta method. There is a subtle
  -- difference when we request for values in the intermediate time points
  -- that are used by this method to integrate. In general case you should 
  -- prefer the 'memo0Dynamics' function above 'memoDynamics'.
  memo0Dynamics :: Dynamics m e -> Simulation m (Dynamics m e)
