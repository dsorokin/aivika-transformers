
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.Transform.Memo.Unboxed
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- This module defines the unboxed memoization transforms. The memoization creates such 'Dynamics'
-- computations, which values are cached in the integration time points. Then
-- these values are interpolated in all other time points.
--

module Simulation.Aivika.Trans.Transform.Memo.Unboxed
       (memoTransform,
        memo0Transform) where

import Simulation.Aivika.Trans.Dynamics
import Simulation.Aivika.Trans.Dynamics.Memo.Unboxed
import Simulation.Aivika.Trans.Transform
import Simulation.Aivika.Trans.SD

-- | A transform that memoizes and order the computation in the integration time points
-- using the interpolation that knows of the Runge-Kutta method. The values are
-- calculated sequentially starting from 'starttime'.
memoTransform :: (MonadSD m, MonadMemo m e) => Transform m e e
{-# INLINE memoTransform #-}
memoTransform = Transform memoDynamics 

-- | A transform that memoizes and order the computation in the integration time points using 
-- the 'discreteDynamics' interpolation. It consumes less memory than the 'memoTransform'
-- computation but it is not aware of the Runge-Kutta method. There is a subtle
-- difference when we request for values in the intermediate time points
-- that are used by this method to integrate. In general case you should 
-- prefer the 'memo0Transform' computation above 'memoTransform'.
memo0Transform :: (MonadSD m, MonadMemo m e) => Transform m e e
{-# INLINE memo0Transform #-}
memo0Transform =  Transform memo0Dynamics
