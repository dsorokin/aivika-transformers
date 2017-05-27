
-- |
-- Module     : Simulation.Aivika.Trans.Dynamics.Random
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
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
        memoRandomTriangularDynamics,
        memoRandomNormalDynamics,
        memoRandomLogNormalDynamics,
        memoRandomExponentialDynamics,
        memoRandomErlangDynamics,
        memoRandomPoissonDynamics,
        memoRandomBinomialDynamics,
        memoRandomGammaDynamics,
        memoRandomBetaDynamics,
        memoRandomWeibullDynamics,
        memoRandomDiscreteDynamics) where

import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
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

-- | Computation that generates random numbers from the triangular distribution
-- and memoizes the numbers in the integration time points.
memoRandomTriangularDynamics :: MonadSD m
                                => Dynamics m Double  -- ^ minimum
                                -> Dynamics m Double  -- ^ median
                                -> Dynamics m Double  -- ^ maximum
                                -> Simulation m (Dynamics m Double)
{-# INLINABLE memoRandomTriangularDynamics #-}
memoRandomTriangularDynamics min median max =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     min' <- invokeDynamics p min
     median' <- invokeDynamics p median
     max' <- invokeDynamics p max
     generateTriangular g min' median' max'

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

-- | Computation that generates random numbers from the lognormal distribution
-- and memoizes the numbers in the integration time points.
memoRandomLogNormalDynamics :: MonadSD m
                               => Dynamics m Double
                               -- ^ the mean of a normal distribution which
                               -- this distribution is derived from
                               -> Dynamics m Double
                               -- ^ the deviation of a normal distribution which
                               -- this distribution is derived from
                               -> Simulation m (Dynamics m Double)
{-# INLINABLE memoRandomLogNormalDynamics #-}
memoRandomLogNormalDynamics mu nu =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     mu' <- invokeDynamics p mu
     nu' <- invokeDynamics p nu
     generateLogNormal g mu' nu'

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

-- | Computation that generates random numbers from the Gamma distribution
-- with the specified shape and scale but memoizes the numbers in
-- the integration time points.
memoRandomGammaDynamics :: MonadSD m
                           => Dynamics m Double  -- ^ shape
                           -> Dynamics m Double  -- ^ scale (a reciprocal of the rate)
                           -> Simulation m (Dynamics m Double)
{-# INLINABLE memoRandomGammaDynamics #-}
memoRandomGammaDynamics kappa theta =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     kappa' <- invokeDynamics p kappa
     theta' <- invokeDynamics p theta
     generateGamma g kappa' theta'

-- | Computation that generates random numbers from the Beta distribution
-- by the specified shape parameters and memoizes the numbers in
-- the integration time points.
memoRandomBetaDynamics :: MonadSD m
                          => Dynamics m Double  -- ^ shape (alpha)
                          -> Dynamics m Double  -- ^ shape (beta)
                          -> Simulation m (Dynamics m Double)
{-# INLINABLE memoRandomBetaDynamics #-}
memoRandomBetaDynamics alpha beta =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     alpha' <- invokeDynamics p alpha
     beta'  <- invokeDynamics p beta
     generateBeta g alpha' beta'

-- | Computation that generates random numbers from the Weibull distribution
-- with the specified shape and scale but memoizes the numbers in
-- the integration time points.
memoRandomWeibullDynamics :: MonadSD m
                             => Dynamics m Double  -- ^ shape
                             -> Dynamics m Double  -- ^ scale
                             -> Simulation m (Dynamics m Double)
{-# INLINABLE memoRandomWeibullDynamics #-}
memoRandomWeibullDynamics alpha beta =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     alpha' <- invokeDynamics p alpha
     beta'  <- invokeDynamics p beta
     generateWeibull g alpha' beta'

-- | Computation that generates random values from the specified discrete
-- distribution and memoizes the values in the integration time points.
memoRandomDiscreteDynamics :: (MonadSD m, MonadMemo m a) => Dynamics m (DiscretePDF a) -> Simulation m (Dynamics m a)
{-# INLINABLE memoRandomDiscreteDynamics #-}
memoRandomDiscreteDynamics dpdf =
  memo0Dynamics $
  Dynamics $ \p ->
  do let g = runGenerator $ pointRun p
     dpdf' <- invokeDynamics p dpdf
     generateDiscrete g dpdf'
