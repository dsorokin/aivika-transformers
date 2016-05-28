
-- |
-- Module     : Simulation.Aivika.Trans.Net.Random
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- This module defines some useful random network computations that
-- hold the current process for the corresponding time interval,
-- when processing every input element.
--

module Simulation.Aivika.Trans.Net.Random
       (randomUniformNet,
        randomUniformIntNet,
        randomTriangularNet,
        randomNormalNet,
        randomLogNormalNet,
        randomExponentialNet,
        randomErlangNet,
        randomPoissonNet,
        randomBinomialNet,
        randomGammaNet,
        randomBetaNet,
        randomWeibullNet,
        randomDiscreteNet) where

import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Process
import Simulation.Aivika.Trans.Process.Random
import Simulation.Aivika.Trans.Net

-- | When processing every input element, hold the process
-- for a random time interval distributed uniformly.
randomUniformNet :: MonadDES m
                    => Double
                    -- ^ the minimum time interval
                    -> Double
                    -- ^ the maximum time interval
                    -> Net m a a
{-# INLINABLE randomUniformNet #-}
randomUniformNet min max =
  withinNet $
  randomUniformProcess_ min max

-- | When processing every input element, hold the process
-- for a random time interval distributed uniformly.
randomUniformIntNet :: MonadDES m
                       => Int
                       -- ^ the minimum time interval
                       -> Int
                       -- ^ the maximum time interval
                       -> Net m a a
{-# INLINABLE randomUniformIntNet #-}
randomUniformIntNet min max =
  withinNet $
  randomUniformIntProcess_ min max

-- | When processing every input element, hold the process
-- for a random time interval having the triangular distribution.
randomTriangularNet :: MonadDES m
                       => Double
                       -- ^ the minimum time interval
                       -> Double
                       -- ^ the median of the time interval
                       -> Double
                       -- ^ the maximum time interval
                       -> Net m a a
{-# INLINABLE randomTriangularNet #-}
randomTriangularNet min median max =
  withinNet $
  randomTriangularProcess_ min median max

-- | When processing every input element, hold the process
-- for a random time interval distributed normally.
randomNormalNet :: MonadDES m
                   => Double
                   -- ^ the mean time interval
                   -> Double
                   -- ^ the time interval deviation
                   -> Net m a a
{-# INLINABLE randomNormalNet #-}
randomNormalNet mu nu =
  withinNet $
  randomNormalProcess_ mu nu
         
-- | When processing every input element, hold the process
-- for a random time interval having the lognormal distribution.
randomLogNormalNet :: MonadDES m
                      => Double
                      -- ^ the mean of a normal distribution which
                      -- this distribution is derived from
                      -> Double
                      -- ^ the deviation of a normal distribution which
                      -- this distribution is derived from
                      -> Net m a a
{-# INLINABLE randomLogNormalNet #-}
randomLogNormalNet mu nu =
  withinNet $
  randomLogNormalProcess_ mu nu
         
-- | When processing every input element, hold the process
-- for a random time interval distributed exponentially
-- with the specified mean (the reciprocal of the rate).
randomExponentialNet :: MonadDES m
                        => Double
                        -- ^ the mean time interval (the reciprocal of the rate)
                        -> Net m a a
{-# INLINABLE randomExponentialNet #-}
randomExponentialNet mu =
  withinNet $
  randomExponentialProcess_ mu
         
-- | When processing every input element, hold the process
-- for a random time interval having the Erlang distribution with
-- the specified scale (the reciprocal of the rate) and shape parameters.
randomErlangNet :: MonadDES m
                   => Double
                   -- ^ the scale (the reciprocal of the rate)
                   -> Int
                   -- ^ the shape
                   -> Net m a a
{-# INLINABLE randomErlangNet #-}
randomErlangNet beta m =
  withinNet $
  randomErlangProcess_ beta m

-- | When processing every input element, hold the process
-- for a random time interval having the Poisson distribution
-- with the specified mean.
randomPoissonNet :: MonadDES m
                    => Double
                    -- ^ the mean time interval
                    -> Net m a a
{-# INLINABLE randomPoissonNet #-}
randomPoissonNet mu =
  withinNet $
  randomPoissonProcess_ mu

-- | When processing every input element, hold the process
-- for a random time interval having the binomial distribution
-- with the specified probability and trials.
randomBinomialNet :: MonadDES m
                     => Double
                     -- ^ the probability
                     -> Int
                     -- ^ the number of trials
                     -> Net m a a
{-# INLINABLE randomBinomialNet #-}
randomBinomialNet prob trials =
  withinNet $
  randomBinomialProcess_ prob trials

-- | When processing every input element, hold the process
-- for a random time interval having the Gamma distribution
-- with the specified shape and scale.
randomGammaNet :: MonadDES m
                  => Double
                  -- ^ the shape
                  -> Double
                  -- ^ the scale (a reciprocal of the rate)
                  -> Net m a a
{-# INLINABLE randomGammaNet #-}
randomGammaNet kappa theta =
  withinNet $
  randomGammaProcess_ kappa theta

-- | When processing every input element, hold the process
-- for a random time interval having the Beta distribution
-- with the specified shape parameters (alpha and beta).
randomBetaNet :: MonadDES m
                 => Double
                 -- ^ shape (alpha)
                 -> Double
                 -- ^ shape (beta)
                 -> Net m a a
{-# INLINABLE randomBetaNet #-}
randomBetaNet alpha beta =
  withinNet $
  randomBetaProcess_ alpha beta

-- | When processing every input element, hold the process
-- for a random time interval having the Weibull distribution
-- with the specified shape and scale.
randomWeibullNet :: MonadDES m
                    => Double
                    -- ^ shape
                    -> Double
                    -- ^ scale
                    -> Net m a a
{-# INLINABLE randomWeibullNet #-}
randomWeibullNet alpha beta =
  withinNet $
  randomWeibullProcess_ alpha beta

-- | When processing every input element, hold the process
-- for a random time interval having the specified discrete distribution.
randomDiscreteNet :: MonadDES m
                     => DiscretePDF Double
                     -- ^ the discrete probability density function
                     -> Net m a a
{-# INLINABLE randomDiscreteNet #-}
randomDiscreteNet dpdf =
  withinNet $
  randomDiscreteProcess_ dpdf
