
-- |
-- Module     : Simulation.Aivika.Trans.Process.Random
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- This module defines helper functions, which are useful to hold 
-- the 'Process' computation for a time interval according to some
-- random distribution.
--

module Simulation.Aivika.Trans.Process.Random
       (randomUniformProcess,
        randomUniformProcess_,
        randomUniformIntProcess,
        randomUniformIntProcess_,
        randomTriangularProcess,
        randomTriangularProcess_,
        randomNormalProcess,
        randomNormalProcess_,
        randomLogNormalProcess,
        randomLogNormalProcess_,
        randomExponentialProcess,
        randomExponentialProcess_,
        randomErlangProcess,
        randomErlangProcess_,
        randomPoissonProcess,
        randomPoissonProcess_,
        randomBinomialProcess,
        randomBinomialProcess_,
        randomGammaProcess,
        randomGammaProcess_,
        randomBetaProcess,
        randomBetaProcess_,
        randomWeibullProcess,
        randomWeibullProcess_,
        randomDiscreteProcess,
        randomDiscreteProcess_) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Parameter.Random
import Simulation.Aivika.Trans.Process

-- | Hold the process for a random time interval distributed uniformly.
randomUniformProcess :: MonadDES m
                        => Double
                        -- ^ the minimum time interval
                        -> Double
                        -- ^ the maximum time interval
                        -> Process m Double
                        -- ^ a computation of the time interval
                        -- for which the process was actually held
{-# INLINABLE randomUniformProcess #-}
randomUniformProcess min max =
  do t <- liftParameter $ randomUniform min max
     holdProcess t
     return t

-- | Hold the process for a random time interval distributed uniformly.
randomUniformProcess_ :: MonadDES m
                         => Double
                         -- ^ the minimum time interval
                         -> Double
                         -- ^ the maximum time interval
                         -> Process m ()
{-# INLINABLE randomUniformProcess_ #-}
randomUniformProcess_ min max =
  do t <- liftParameter $ randomUniform min max
     holdProcess t

-- | Hold the process for a random time interval distributed uniformly.
randomUniformIntProcess :: MonadDES m
                           => Int
                           -- ^ the minimum time interval
                           -> Int
                           -- ^ the maximum time interval
                           -> Process m Int
                           -- ^ a computation of the time interval
                           -- for which the process was actually held
{-# INLINABLE randomUniformIntProcess #-}
randomUniformIntProcess min max =
  do t <- liftParameter $ randomUniformInt min max
     holdProcess $ fromIntegral t
     return t

-- | Hold the process for a random time interval distributed uniformly.
randomUniformIntProcess_ :: MonadDES m
                            => Int
                            -- ^ the minimum time interval
                            -> Int
                            -- ^ the maximum time interval
                            -> Process m ()
{-# INLINABLE randomUniformIntProcess_ #-}
randomUniformIntProcess_ min max =
  do t <- liftParameter $ randomUniformInt min max
     holdProcess $ fromIntegral t

-- | Hold the process for a random time interval having the triangular distribution.
randomTriangularProcess :: MonadDES m
                           => Double
                           -- ^ the minimum time interval
                           -> Double
                           -- ^ a median of the time interval
                           -> Double
                           -- ^ the maximum time interval
                           -> Process m Double
                           -- ^ a computation of the time interval
                           -- for which the process was actually held
{-# INLINABLE randomTriangularProcess #-}
randomTriangularProcess min median max =
  do t <- liftParameter $ randomTriangular min median max
     holdProcess t
     return t

-- | Hold the process for a random time interval having the triangular distribution.
randomTriangularProcess_ :: MonadDES m
                            => Double
                            -- ^ the minimum time interval
                            -> Double
                            -- ^ a median of the time interval
                            -> Double
                            -- ^ the maximum time interval
                            -> Process m ()
{-# INLINABLE randomTriangularProcess_ #-}
randomTriangularProcess_ min median max =
  do t <- liftParameter $ randomTriangular min median max
     holdProcess t

-- | Hold the process for a random time interval distributed normally.
randomNormalProcess :: MonadDES m
                       => Double
                       -- ^ the mean time interval
                       -> Double
                       -- ^ the time interval deviation
                       -> Process m Double
                       -- ^ a computation of the time interval
                       -- for which the process was actually held
{-# INLINABLE randomNormalProcess #-}
randomNormalProcess mu nu =
  do t <- liftParameter $ randomNormal mu nu
     when (t > 0) $
       holdProcess t
     return t
         
-- | Hold the process for a random time interval distributed normally.
randomNormalProcess_ :: MonadDES m
                        => Double
                        -- ^ the mean time interval
                        -> Double
                        -- ^ the time interval deviation
                        -> Process m ()
{-# INLINABLE randomNormalProcess_ #-}
randomNormalProcess_ mu nu =
  do t <- liftParameter $ randomNormal mu nu
     when (t > 0) $
       holdProcess t

-- | Hold the process for a random time interval having the lognormal distribution.
randomLogNormalProcess :: MonadDES m
                          => Double
                          -- ^ the mean for a normal distribution
                          -- which this distribution is derived from
                          -> Double
                          -- ^ the deviation for a normal distribution
                          -- which this distribution is derived from
                          -> Process m Double
                          -- ^ a computation of the time interval
                          -- for which the process was actually held
{-# INLINABLE randomLogNormalProcess #-}
randomLogNormalProcess mu nu =
  do t <- liftParameter $ randomLogNormal mu nu
     holdProcess t
     return t

-- | Hold the process for a random time interval having the lognormal distribution.
randomLogNormalProcess_ :: MonadDES m
                           => Double
                           -- ^ the mean for a normal distribution
                           -- which this distribution is derived from
                           -> Double
                           -- ^ the deviation for a normal distribution
                           -- which this distribution is derived from
                           -> Process m ()
{-# INLINABLE randomLogNormalProcess_ #-}
randomLogNormalProcess_ mu nu =
  do t <- liftParameter $ randomLogNormal mu nu
     holdProcess t
         
-- | Hold the process for a random time interval distributed exponentially
-- with the specified mean (the reciprocal of the rate).
randomExponentialProcess :: MonadDES m
                            => Double
                            -- ^ the mean time interval (the reciprocal of the rate)
                            -> Process m Double
                            -- ^ a computation of the time interval
                            -- for which the process was actually held
{-# INLINABLE randomExponentialProcess #-}
randomExponentialProcess mu =
  do t <- liftParameter $ randomExponential mu
     holdProcess t
     return t
         
-- | Hold the process for a random time interval distributed exponentially
-- with the specified mean (the reciprocal of the rate).
randomExponentialProcess_ :: MonadDES m
                             => Double
                             -- ^ the mean time interval (the reciprocal of the rate)
                             -> Process m ()
{-# INLINABLE randomExponentialProcess_ #-}
randomExponentialProcess_ mu =
  do t <- liftParameter $ randomExponential mu
     holdProcess t
         
-- | Hold the process for a random time interval having the Erlang distribution with
-- the specified scale (the reciprocal of the rate) and shape parameters.
randomErlangProcess :: MonadDES m
                       => Double
                       -- ^ the scale (the reciprocal of the rate)
                       -> Int
                       -- ^ the shape
                       -> Process m Double
                       -- ^ a computation of the time interval
                       -- for which the process was actually held
{-# INLINABLE randomErlangProcess #-}
randomErlangProcess beta m =
  do t <- liftParameter $ randomErlang beta m
     holdProcess t
     return t

-- | Hold the process for a random time interval having the Erlang distribution with
-- the specified scale (the reciprocal of the rate) and shape parameters.
randomErlangProcess_ :: MonadDES m
                        => Double
                        -- ^ the scale (the reciprocal of the rate)
                        -> Int
                        -- ^ the shape
                        -> Process m ()
{-# INLINABLE randomErlangProcess_ #-}
randomErlangProcess_ beta m =
  do t <- liftParameter $ randomErlang beta m
     holdProcess t

-- | Hold the process for a random time interval having the Poisson distribution with
-- the specified mean.
randomPoissonProcess :: MonadDES m
                        => Double
                        -- ^ the mean time interval
                        -> Process m Int
                        -- ^ a computation of the time interval
                        -- for which the process was actually held
{-# INLINABLE randomPoissonProcess #-}
randomPoissonProcess mu =
  do t <- liftParameter $ randomPoisson mu
     holdProcess $ fromIntegral t
     return t

-- | Hold the process for a random time interval having the Poisson distribution with
-- the specified mean.
randomPoissonProcess_ :: MonadDES m
                         => Double
                         -- ^ the mean time interval
                         -> Process m ()
{-# INLINABLE randomPoissonProcess_ #-}
randomPoissonProcess_ mu =
  do t <- liftParameter $ randomPoisson mu
     holdProcess $ fromIntegral t

-- | Hold the process for a random time interval having the binomial distribution
-- with the specified probability and trials.
randomBinomialProcess :: MonadDES m
                         => Double
                         -- ^ the probability
                         -> Int
                         -- ^ the number of trials
                         -> Process m Int
                         -- ^ a computation of the time interval
                         -- for which the process was actually held
{-# INLINABLE randomBinomialProcess #-}
randomBinomialProcess prob trials =
  do t <- liftParameter $ randomBinomial prob trials
     holdProcess $ fromIntegral t
     return t

-- | Hold the process for a random time interval having the binomial distribution
-- with the specified probability and trials.
randomBinomialProcess_ :: MonadDES m
                          =>Double
                          -- ^ the probability
                          -> Int
                          -- ^ the number of trials
                          -> Process m ()
{-# INLINABLE randomBinomialProcess_ #-}
randomBinomialProcess_ prob trials =
  do t <- liftParameter $ randomBinomial prob trials
     holdProcess $ fromIntegral t

-- | Hold the process for a random time interval having the Gamma distribution
-- with the specified shape and scale.
randomGammaProcess :: MonadDES m
                      => Double
                      -- ^ the shape
                      -> Double
                      -- ^ the scale (a reciprocal of the rate)
                      -> Process m Double
                      -- ^ a computation of the time interval
                      -- for which the process was actually held
{-# INLINABLE randomGammaProcess #-}
randomGammaProcess kappa theta =
  do t <- liftParameter $ randomGamma kappa theta
     holdProcess t
     return t

-- | Hold the process for a random time interval having the Gamma distribution
-- with the specified shape and scale.
randomGammaProcess_ :: MonadDES m
                       => Double
                       -- ^ the shape
                       -> Double
                       -- ^ the scale (a reciprocal of the rate)
                       -> Process m ()
{-# INLINABLE randomGammaProcess_ #-}
randomGammaProcess_ kappa theta =
  do t <- liftParameter $ randomGamma kappa theta
     holdProcess t

-- | Hold the process for a random time interval having the Beta distribution
-- with the specified shape parameters (alpha and beta).
randomBetaProcess :: MonadDES m
                     => Double
                     -- ^ the shape (alpha)
                     -> Double
                     -- ^ the shape (beta)
                     -> Process m Double
                     -- ^ a computation of the time interval
                     -- for which the process was actually held
{-# INLINABLE randomBetaProcess #-}
randomBetaProcess alpha beta =
  do t <- liftParameter $ randomBeta alpha beta
     holdProcess t
     return t

-- | Hold the process for a random time interval having the Beta distribution
-- with the specified shape parameters (alpha and beta).
randomBetaProcess_ :: MonadDES m
                      => Double
                      -- ^ the shape (alpha)
                      -> Double
                      -- ^ the shape (beta)
                      -> Process m ()
{-# INLINABLE randomBetaProcess_ #-}
randomBetaProcess_ alpha beta =
  do t <- liftParameter $ randomBeta alpha beta
     holdProcess t

-- | Hold the process for a random time interval having the Weibull distribution
-- with the specified shape and scale.
randomWeibullProcess :: MonadDES m
                        => Double
                        -- ^ the shape
                        -> Double
                        -- ^ the scale
                        -> Process m Double
                        -- ^ a computation of the time interval
                        -- for which the process was actually held
{-# INLINABLE randomWeibullProcess #-}
randomWeibullProcess alpha beta =
  do t <- liftParameter $ randomWeibull alpha beta
     holdProcess t
     return t

-- | Hold the process for a random time interval having the Weibull distribution
-- with the specified shape and scale.
randomWeibullProcess_ :: MonadDES m
                         => Double
                         -- ^ the shape
                         -> Double
                         -- ^ the scale
                         -> Process m ()
{-# INLINABLE randomWeibullProcess_ #-}
randomWeibullProcess_ alpha beta =
  do t <- liftParameter $ randomWeibull alpha beta
     holdProcess t

-- | Hold the process for a random time interval having the specified discrete distribution.
randomDiscreteProcess :: MonadDES m
                         => DiscretePDF Double
                         -- ^ the discrete probability density function
                         -> Process m Double
                         -- ^ a computation of the time interval
                         -- for which the process was actually held
{-# INLINABLE randomDiscreteProcess #-}
randomDiscreteProcess dpdf =
  do t <- liftParameter $ randomDiscrete dpdf
     holdProcess t
     return t

-- | Hold the process for a random time interval having the specified discrete distribution.
randomDiscreteProcess_ :: MonadDES m
                          => DiscretePDF Double
                          -- ^ the discrete probability density function
                          -> Process m ()
{-# INLINABLE randomDiscreteProcess_ #-}
randomDiscreteProcess_ dpdf =
  do t <- liftParameter $ randomDiscrete dpdf
     holdProcess t
