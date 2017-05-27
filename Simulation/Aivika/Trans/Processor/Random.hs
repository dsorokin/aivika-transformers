
-- |
-- Module     : Simulation.Aivika.Trans.Processor.Random
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines some useful random processors that
-- hold the current process for the corresponding time interval,
-- when processing every input element.
--

module Simulation.Aivika.Trans.Processor.Random
       (randomUniformProcessor,
        randomUniformIntProcessor,
        randomTriangularProcessor,
        randomNormalProcessor,
        randomLogNormalProcessor,
        randomExponentialProcessor,
        randomErlangProcessor,
        randomPoissonProcessor,
        randomBinomialProcessor,
        randomGammaProcessor,
        randomBetaProcessor,
        randomWeibullProcessor,
        randomDiscreteProcessor) where

import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Process
import Simulation.Aivika.Trans.Process.Random
import Simulation.Aivika.Trans.Processor

-- | When processing every input element, hold the process
-- for a random time interval distributed uniformly.
randomUniformProcessor :: MonadDES m
                          => Double
                          -- ^ the minimum time interval
                          -> Double
                          -- ^ the maximum time interval
                          -> Processor m a a
{-# INLINABLE randomUniformProcessor #-}
randomUniformProcessor min max =
  withinProcessor $
  randomUniformProcess_ min max

-- | When processing every input element, hold the process
-- for a random time interval distributed uniformly.
randomUniformIntProcessor :: MonadDES m
                             => Int
                             -- ^ the minimum time interval
                             -> Int
                             -- ^ the maximum time interval
                             -> Processor m a a
{-# INLINABLE randomUniformIntProcessor #-}
randomUniformIntProcessor min max =
  withinProcessor $
  randomUniformIntProcess_ min max

-- | When processing every input element, hold the process
-- for a random time interval having the triangular distribution.
randomTriangularProcessor :: MonadDES m
                             => Double
                             -- ^ the minimum time interval
                             -> Double
                             -- ^ the median of the time interval
                             -> Double
                             -- ^ the maximum time interval
                             -> Processor m a a
{-# INLINABLE randomTriangularProcessor #-}
randomTriangularProcessor min median max =
  withinProcessor $
  randomTriangularProcess_ min median max

-- | When processing every input element, hold the process
-- for a random time interval distributed normally.
randomNormalProcessor :: MonadDES m
                         => Double
                         -- ^ the mean time interval
                         -> Double
                         -- ^ the time interval deviation
                         -> Processor m a a
{-# INLINABLE randomNormalProcessor #-}
randomNormalProcessor mu nu =
  withinProcessor $
  randomNormalProcess_ mu nu
         
-- | When processing every input element, hold the process
-- for a random time interval having the lognormal distribution.
randomLogNormalProcessor :: MonadDES m
                            => Double
                            -- ^ the mean for a normal distribution
                            -- which this distribution is derived from
                            -> Double
                            -- ^ the deviation for a normal distribution
                            -- which this distribution is derived from
                            -> Processor m a a
{-# INLINABLE randomLogNormalProcessor #-}
randomLogNormalProcessor mu nu =
  withinProcessor $
  randomLogNormalProcess_ mu nu
         
-- | When processing every input element, hold the process
-- for a random time interval distributed exponentially
-- with the specified mean (the reciprocal of the rate).
randomExponentialProcessor :: MonadDES m
                              => Double
                              -- ^ the mean time interval (the reciprocal of the rate)
                              -> Processor m a a
{-# INLINABLE randomExponentialProcessor #-}
randomExponentialProcessor mu =
  withinProcessor $
  randomExponentialProcess_ mu
         
-- | When processing every input element, hold the process
-- for a random time interval having the Erlang distribution with
-- the specified scale (the reciprocal of the rate) and shape parameters.
randomErlangProcessor :: MonadDES m
                         => Double
                         -- ^ the scale (the reciprocal of the rate)
                         -> Int
                         -- ^ the shape
                         -> Processor m a a
{-# INLINABLE randomErlangProcessor #-}
randomErlangProcessor beta m =
  withinProcessor $
  randomErlangProcess_ beta m

-- | When processing every input element, hold the process
-- for a random time interval having the Poisson distribution
-- with the specified mean.
randomPoissonProcessor :: MonadDES m
                          => Double
                          -- ^ the mean time interval
                          -> Processor m a a
{-# INLINABLE randomPoissonProcessor #-}
randomPoissonProcessor mu =
  withinProcessor $
  randomPoissonProcess_ mu

-- | When processing every input element, hold the process
-- for a random time interval having the binomial distribution
-- with the specified probability and trials.
randomBinomialProcessor :: MonadDES m
                           => Double
                           -- ^ the probability
                           -> Int
                           -- ^ the number of trials
                           -> Processor m a a
{-# INLINABLE randomBinomialProcessor #-}
randomBinomialProcessor prob trials =
  withinProcessor $
  randomBinomialProcess_ prob trials

-- | When processing every input element, hold the process
-- for a random time interval having the Gamma distribution
-- with the specified shape and scale.
randomGammaProcessor :: MonadDES m
                        => Double
                        -- ^ the shape
                        -> Double
                        -- ^ the scale (a reciprocal of the rate)
                        -> Processor m a a
{-# INLINABLE randomGammaProcessor #-}
randomGammaProcessor kappa theta =
  withinProcessor $
  randomGammaProcess_ kappa theta

-- | When processing every input element, hold the process
-- for a random time interval having the Beta distribution
-- with the specified shape parameters (alpha and beta).
randomBetaProcessor :: MonadDES m
                       => Double
                       -- ^ shape (alpha)
                       -> Double
                       -- ^ shape (beta)
                       -> Processor m a a
{-# INLINABLE randomBetaProcessor #-}
randomBetaProcessor alpha beta =
  withinProcessor $
  randomBetaProcess_ alpha beta

-- | When processing every input element, hold the process
-- for a random time interval having the Weibull distribution
-- with the specified shape and scale.
randomWeibullProcessor :: MonadDES m
                          => Double
                          -- ^ shape
                          -> Double
                          -- ^ scale
                          -> Processor m a a
{-# INLINABLE randomWeibullProcessor #-}
randomWeibullProcessor alpha beta =
  withinProcessor $
  randomWeibullProcess_ alpha beta

-- | When processing every input element, hold the process
-- for a random time interval having the specified discrete distribution.
randomDiscreteProcessor :: MonadDES m
                           => DiscretePDF Double
                           -- ^ the discrete probability density function
                           -> Processor m a a
{-# INLINABLE randomDiscreteProcessor #-}
randomDiscreteProcessor dpdf =
  withinProcessor $
  randomDiscreteProcess_ dpdf
