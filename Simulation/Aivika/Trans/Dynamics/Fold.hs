
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Trans.Dynamics.Fold
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines the fold functions that allows traversing the values of
-- any 'Dynamics' computation in the integration time points.
--
module Simulation.Aivika.Trans.Dynamics.Fold
       (foldDynamics1,
        foldDynamics) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Dynamics.Memo

-- | Like the standard 'foldl1' function but applied to values in 
-- the integration time points. The accumulator values are transformed
-- according to the first argument, which should be either function 
-- 'memo0Dynamics' or its unboxed version.
foldDynamics1 :: (Comp m, MonadFix m)
                 => (Dynamics m a -> Simulation m (Dynamics m a))
                 -> (a -> a -> a) 
                 -> Dynamics m a 
                 -> Simulation m (Dynamics m a)
foldDynamics1 tr f m =
  mdo y <- tr $ Dynamics $ \p ->
        case pointIteration p of
          0 -> 
            invokeDynamics p m
          n -> do 
            let sc = pointSpecs p
                ty = basicTime sc (n - 1) 0
                py = p { pointTime = ty, pointIteration = n - 1, pointPhase = 0 }
            s <- invokeDynamics py y
            x <- invokeDynamics p m
            return $! f s x
      return y

-- | Like the standard 'foldl' function but applied to values in 
-- the integration time points. The accumulator values are transformed
-- according to the first argument, which should be either function
-- 'memo0Dynamics' or its unboxed version.
foldDynamics :: (Comp m, MonadFix m)
                => (Dynamics m a -> Simulation m (Dynamics m a))
                -> (a -> b -> a) 
                -> a
                -> Dynamics m b 
                -> Simulation m (Dynamics m a)
foldDynamics tr f acc m =
  mdo y <- tr $ Dynamics $ \p ->
        case pointIteration p of
          0 -> do
            x <- invokeDynamics p m
            return $! f acc x
          n -> do 
            let sc = pointSpecs p
                ty = basicTime sc (n - 1) 0
                py = p { pointTime = ty, pointIteration = n - 1, pointPhase = 0 }
            s <- invokeDynamics py y
            x <- invokeDynamics p m
            return $! f s x
      return y
