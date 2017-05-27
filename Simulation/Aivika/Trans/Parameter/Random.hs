
-- |
-- Module     : Simulation.Aivika.Trans.Parameter.Random
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines the random parameters of simulation experiments.
--
-- To create a parameter that would return the same value within the simulation run,
-- you should memoize the computation with help of 'memoParameter', which is important
-- for the Monte-Carlo simulation.
--
-- To create a random function that would return the same values in the integration
-- time points within the simulation run, you should either lift the computation to
-- the 'Dynamics' computation and then memoize it too but using the 'memo0Dynamics'
-- function for that computation, or just take the predefined function that does
-- namely this.

module Simulation.Aivika.Trans.Parameter.Random
       (randomUniform,
        randomUniformInt,
        randomTriangular,
        randomNormal,
        randomLogNormal,
        randomExponential,
        randomErlang,
        randomPoisson,
        randomBinomial,
        randomGamma,
        randomBeta,
        randomWeibull,
        randomDiscrete,
        randomTrue,
        randomFalse) where

import System.Random

import Control.Monad.Trans

import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Dynamics
import Simulation.Aivika.Trans.Dynamics.Memo.Unboxed

-- | Computation that generates a new random number distributed uniformly.
randomUniform :: MonadComp m
                 => Double     -- ^ minimum
                 -> Double  -- ^ maximum
                 -> Parameter m Double
{-# INLINE randomUniform #-}
randomUniform min max =
  Parameter $ \r ->
  let g = runGenerator r
  in generateUniform g min max

-- | Computation that generates a new random integer number distributed uniformly.
randomUniformInt :: MonadComp m
                    => Int     -- ^ minimum
                    -> Int  -- ^ maximum
                    -> Parameter m Int
{-# INLINE randomUniformInt #-}
randomUniformInt min max =
  Parameter $ \r ->
  let g = runGenerator r
  in generateUniformInt g min max

-- | Computation that generates a new random number from the triangular distribution.
randomTriangular :: MonadComp m
                    => Double  -- ^ minimum
                    -> Double  -- ^ median
                    -> Double  -- ^ maximum
                    -> Parameter m Double
{-# INLINE randomTriangular #-}
randomTriangular min median max =
  Parameter $ \r ->
  let g = runGenerator r
  in generateTriangular g min median max

-- | Computation that generates a new random number distributed normally.
randomNormal :: MonadComp m
                => Double     -- ^ mean
                -> Double  -- ^ deviation
                -> Parameter m Double
{-# INLINE randomNormal #-}
randomNormal mu nu =
  Parameter $ \r ->
  let g = runGenerator r
  in generateNormal g mu nu

-- | Computation that generates a new random number from the lognormal distribution.
randomLogNormal :: MonadComp m
                   => Double
                   -- ^ the mean of a normal distribution
                   -- which this distribution is derived from
                   -> Double
                   -- ^ the deviation of a normal distribution
                   -- which this distribution is derived from
                   -> Parameter m Double
{-# INLINE randomLogNormal #-}
randomLogNormal mu nu =
  Parameter $ \r ->
  let g = runGenerator r
  in generateLogNormal g mu nu

-- | Computation that returns a new exponential random number with the specified mean
-- (the reciprocal of the rate).
randomExponential :: MonadComp m
                     => Double
                     -- ^ the mean (the reciprocal of the rate)
                     -> Parameter m Double
{-# INLINE randomExponential #-}
randomExponential mu =
  Parameter $ \r ->
  let g = runGenerator r
  in generateExponential g mu

-- | Computation that returns a new Erlang random number with the specified scale
-- (the reciprocal of the rate) and integer shape.
randomErlang :: MonadComp m
                => Double
                -- ^ the scale (the reciprocal of the rate)
                -> Int
                -- ^ the shape
                -> Parameter m Double
{-# INLINE randomErlang #-}
randomErlang beta m =
  Parameter $ \r ->
  let g = runGenerator r
  in generateErlang g beta m

-- | Computation that returns a new Poisson random number with the specified mean.
randomPoisson :: MonadComp m
                 => Double
                 -- ^ the mean
                 -> Parameter m Int
{-# INLINE randomPoisson #-}
randomPoisson mu =
  Parameter $ \r ->
  let g = runGenerator r
  in generatePoisson g mu

-- | Computation that returns a new binomial random number with the specified
-- probability and trials.
randomBinomial :: MonadComp m
                  => Double  -- ^ the probability
                  -> Int  -- ^ the number of trials
                  -> Parameter m Int
{-# INLINE randomBinomial #-}
randomBinomial prob trials =
  Parameter $ \r ->
  let g = runGenerator r
  in generateBinomial g prob trials

-- | Computation that returns a new random number from the Gamma distribution.
randomGamma :: MonadComp m
               => Double  -- ^ the shape
               -> Double  -- ^ the scale (a reciprocal of the rate)
               -> Parameter m Double
{-# INLINE randomGamma #-}
randomGamma kappa theta =
  Parameter $ \r ->
  let g = runGenerator r
  in generateGamma g kappa theta

-- | Computation that returns a new random number from the Beta distribution.
randomBeta :: MonadComp m
              => Double  -- ^ the shape (alpha)
              -> Double  -- ^ the shape (beta)
              -> Parameter m Double
{-# INLINE randomBeta #-}
randomBeta alpha beta =
  Parameter $ \r ->
  let g = runGenerator r
  in generateBeta g alpha beta

-- | Computation that returns a new random number from the Weibull distribution.
randomWeibull :: MonadComp m
                 => Double  -- ^ shape
                 -> Double  -- ^ scale
                 -> Parameter m Double
{-# INLINE randomWeibull #-}
randomWeibull alpha beta =
  Parameter $ \r ->
  let g = runGenerator r
  in generateWeibull g alpha beta

-- | Computation that returns a new random value from the specified discrete distribution.
randomDiscrete :: MonadComp m => DiscretePDF a -> Parameter m a
{-# INLINE randomDiscrete #-}
randomDiscrete dpdf =
  Parameter $ \r ->
  let g = runGenerator r
  in generateDiscrete g dpdf

-- | Computation that returns 'True' in case of success.
randomTrue :: MonadComp m
              => Double      -- ^ the probability of the success
              -> Parameter m Bool
{-# INLINE randomTrue #-}              
randomTrue p =
  do x <- randomUniform 0 1
     return (x <= p)

-- | Computation that returns 'False' in case of success.
randomFalse :: MonadComp m
               => Double      -- ^ the probability of the success
               -> Parameter m Bool
{-# INLINE randomFalse #-}
randomFalse p =
  do x <- randomUniform 0 1
     return (x > p)     
