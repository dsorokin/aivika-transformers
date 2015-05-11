
-- |
-- Module     : Simulation.Aivika.Trans.Process.Random
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
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
        randomNormalProcess,
        randomNormalProcess_,
        randomExponentialProcess,
        randomExponentialProcess_,
        randomErlangProcess,
        randomErlangProcess_,
        randomPoissonProcess,
        randomPoissonProcess_,
        randomBinomialProcess,
        randomBinomialProcess_) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.DES
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
