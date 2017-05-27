
-- |
-- Module     : Simulation.Aivika.Trans.Dynamics.Memo
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines memo functions. The memoization creates such 'Dynamics'
-- computations, which values are cached in the integration time points. Then
-- these values are interpolated in all other time points.
--

module Simulation.Aivika.Trans.Dynamics.Memo
       (MonadMemo(..),
        unzipDynamics,
        unzip0Dynamics) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics

-- | A monad with the support of memoisation.
class Monad m => MonadMemo m where 

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

  -- | Iterate sequentially the dynamic process with side effects in 
  -- the integration time points. It is equivalent to a call of the
  -- 'memo0Dynamics' function but significantly more efficient, for the array 
  -- is not created.
  iterateDynamics :: Dynamics m () -> Simulation m (Dynamics m ())

-- | Memoize and unzip the computation of pairs, applying the 'memoDynamics' function.
unzipDynamics :: MonadMemo m => Dynamics m (a, b) -> Simulation m (Dynamics m a, Dynamics m b)
{-# INLINABLE unzipDynamics #-}
unzipDynamics m =
  Simulation $ \r ->
  do m' <- invokeSimulation r (memoDynamics m)
     let ma =
           Dynamics $ \p ->
           do (a, _) <- invokeDynamics p m'
              return a
         mb =
           Dynamics $ \p ->
           do (_, b) <- invokeDynamics p m'
              return b
     return (ma, mb)

-- | Memoize and unzip the computation of pairs, applying the 'memo0Dynamics' function.
unzip0Dynamics :: MonadMemo m => Dynamics m (a, b) -> Simulation m (Dynamics m a, Dynamics m b)
{-# INLINABLE unzip0Dynamics #-}
unzip0Dynamics m =
  Simulation $ \r ->
  do m' <- invokeSimulation r (memo0Dynamics m)
     let ma =
           Dynamics $ \p ->
           do (a, _) <- invokeDynamics p m'
              return a
         mb =
           Dynamics $ \p ->
           do (_, b) <- invokeDynamics p m'
              return b
     return (ma, mb)
