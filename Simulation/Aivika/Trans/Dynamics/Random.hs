
-- |
-- Module     : Simulation.Aivika.Trans.Dynamics.Random
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines the random functions that always return the same values
-- in the integration time points within a single simulation run. The values
-- for another simulation run will be regenerated anew.
--
-- For example, the computations returned by these functions can be used in
-- the equations of System Dynamics.
--
-- Also it is worth noting that the values are generated in a strong order starting
-- from 'starttime' with step 'dt'. This is how the 'memo0Dynamics' function
-- actually works.
--

module Simulation.Aivika.Trans.Dynamics.Random
       (memoRandomUniformDynamics,
        memoRandomUniformIntDynamics,
        memoRandomNormalDynamics,
        memoRandomExponentialDynamics,
        memoRandomErlangDynamics,
        memoRandomPoissonDynamics,
        memoRandomBinomialDynamics) where

import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Dynamics.Memo.Unboxed
import Simulation.Aivika.Trans.SD

-- | Computation that generates random numbers distributed uniformly and
-- memoizes them in the integration time points.
memoRandomUniformDynamics :: MonadSD m
                             => Dynamics m Double     -- ^ minimum
                             -> Dynamics m Double     -- ^ maximum
                             -> Simulation m (Dynamics m Double)
{-# INLINABLE memoRandomUniformDynamics #-}
memoRandomUniformDynamics min max =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     min' <- invokeDynamics p min
     max' <- invokeDynamics p max
     generateUniform g min' max'

-- | Computation that generates random integer numbers distributed uniformly and
-- memoizes them in the integration time points.
memoRandomUniformIntDynamics :: MonadSD m
                                => Dynamics m Int     -- ^ minimum
                                -> Dynamics m Int     -- ^ maximum
                                -> Simulation m (Dynamics m Int)
{-# INLINABLE memoRandomUniformIntDynamics #-}
memoRandomUniformIntDynamics min max =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     min' <- invokeDynamics p min
     max' <- invokeDynamics p max
     generateUniformInt g min' max'

-- | Computation that generates random numbers distributed normally and
-- memoizes them in the integration time points.
memoRandomNormalDynamics :: MonadSD m
                            => Dynamics m Double     -- ^ mean
                            -> Dynamics m Double     -- ^ deviation
                            -> Simulation m (Dynamics m Double)
{-# INLINABLE memoRandomNormalDynamics #-}
memoRandomNormalDynamics mu nu =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     mu' <- invokeDynamics p mu
     nu' <- invokeDynamics p nu
     generateNormal g mu' nu'

-- | Computation that generates exponential random numbers with the specified mean
-- (the reciprocal of the rate) and memoizes them in the integration time points.
memoRandomExponentialDynamics :: MonadSD m
                                 => Dynamics m Double
                                 -- ^ the mean (the reciprocal of the rate)
                                 -> Simulation m (Dynamics m Double)
{-# INLINABLE memoRandomExponentialDynamics #-}
memoRandomExponentialDynamics mu =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     mu' <- invokeDynamics p mu
     generateExponential g mu'

-- | Computation that generates the Erlang random numbers with the specified scale
-- (the reciprocal of the rate) and integer shape but memoizes them in the integration
-- time points.
memoRandomErlangDynamics :: MonadSD m
                            => Dynamics m Double
                            -- ^ the scale (the reciprocal of the rate)
                            -> Dynamics m Int
                            -- ^ the shape
                            -> Simulation m (Dynamics m Double)
{-# INLINABLE memoRandomErlangDynamics #-}
memoRandomErlangDynamics beta m =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     beta' <- invokeDynamics p beta
     m' <- invokeDynamics p m
     generateErlang g beta' m'

-- | Computation that generats the Poisson random numbers with the specified mean
-- and memoizes them in the integration time points.
memoRandomPoissonDynamics :: MonadSD m
                             => Dynamics m Double
                             -- ^ the mean
                             -> Simulation m (Dynamics m Int)
{-# INLINABLE memoRandomPoissonDynamics #-}
memoRandomPoissonDynamics mu =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     mu' <- invokeDynamics p mu
     generatePoisson g mu'

-- | Computation that generates binomial random numbers with the specified
-- probability and trials but memoizes them in the integration time points.
memoRandomBinomialDynamics :: MonadSD m
                              => Dynamics m Double  -- ^ the probability
                              -> Dynamics m Int  -- ^ the number of trials
                              -> Simulation m (Dynamics m Int)
{-# INLINABLE memoRandomBinomialDynamics #-}
memoRandomBinomialDynamics prob trials =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     prob' <- invokeDynamics p prob
     trials' <- invokeDynamics p trials
     generateBinomial g prob' trials'
