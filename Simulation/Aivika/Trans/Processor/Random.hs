
-- |
-- Module     : Simulation.Aivika.Trans.Processor.Random
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines some useful random processors that
-- hold the current process for the corresponding time interval,
-- when processing every input element.
--

module Simulation.Aivika.Trans.Processor.Random
       (randomUniformProcessor,
        randomUniformIntProcessor,
        randomNormalProcessor,
        randomExponentialProcessor,
        randomErlangProcessor,
        randomPoissonProcessor,
        randomBinomialProcessor) where

import Simulation.Aivika.Trans.DES
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
