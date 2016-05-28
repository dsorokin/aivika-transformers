
-- |
-- Module     : Simulation.Aivika.Trans.Activity.Random
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- This module defines some useful predefined activities that
-- hold the current process for the corresponding random time
-- interval, when processing every input element.
--

module Simulation.Aivika.Trans.Activity.Random
       (newRandomUniformActivity,
        newRandomUniformIntActivity,
        newRandomTriangularActivity,
        newRandomNormalActivity,
        newRandomLogNormalActivity,
        newRandomExponentialActivity,
        newRandomErlangActivity,
        newRandomPoissonActivity,
        newRandomBinomialActivity,
        newRandomGammaActivity,
        newRandomBetaActivity,
        newRandomWeibullActivity,
        newRandomDiscreteActivity,
        newPreemptibleRandomUniformActivity,
        newPreemptibleRandomUniformIntActivity,
        newPreemptibleRandomTriangularActivity,
        newPreemptibleRandomNormalActivity,
        newPreemptibleRandomLogNormalActivity,
        newPreemptibleRandomExponentialActivity,
        newPreemptibleRandomErlangActivity,
        newPreemptibleRandomPoissonActivity,
        newPreemptibleRandomBinomialActivity,
        newPreemptibleRandomGammaActivity,
        newPreemptibleRandomBetaActivity,
        newPreemptibleRandomWeibullActivity,
        newPreemptibleRandomDiscreteActivity) where

import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Process
import Simulation.Aivika.Trans.Process.Random
import Simulation.Aivika.Trans.Activity

-- | Create a new activity that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomUniformActivity :: MonadDES m
                            => Double
                            -- ^ the minimum time interval
                            -> Double
                            -- ^ the maximum time interval
                            -> Simulation m (Activity m () a a)
{-# INLINABLE newRandomUniformActivity #-}
newRandomUniformActivity =
  newPreemptibleRandomUniformActivity False

-- | Create a new activity that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomUniformIntActivity :: MonadDES m
                               => Int
                               -- ^ the minimum time interval
                               -> Int
                               -- ^ the maximum time interval
                               -> Simulation m (Activity m () a a)
{-# INLINABLE newRandomUniformIntActivity #-}
newRandomUniformIntActivity =
  newPreemptibleRandomUniformIntActivity False

-- | Create a new activity that holds the process for a random time interval
-- having the triangular distribution, when processing every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomTriangularActivity :: MonadDES m
                               => Double
                               -- ^ the minimum time interval
                               -> Double
                               -- ^ the median of the time interval
                               -> Double
                               -- ^ the maximum time interval
                               -> Simulation m (Activity m () a a)
{-# INLINABLE newRandomTriangularActivity #-}
newRandomTriangularActivity =
  newPreemptibleRandomTriangularActivity False

-- | Create a new activity that holds the process for a random time interval
-- distributed normally, when processing every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomNormalActivity :: MonadDES m
                           => Double
                           -- ^ the mean time interval
                           -> Double
                           -- ^ the time interval deviation
                           -> Simulation m (Activity m () a a)
{-# INLINABLE newRandomNormalActivity #-}
newRandomNormalActivity =
  newPreemptibleRandomNormalActivity False
         
-- | Create a new activity that holds the process for a random time interval
-- having the lognormal distribution, when processing every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomLogNormalActivity :: MonadDES m
                              => Double
                              -- ^ the mean of a normal distribution which
                              -- this distribution is derived from
                              -> Double
                              -- ^ the deviation of a normal distribution which
                              -- this distribution is derived from
                              -> Simulation m (Activity m () a a)
{-# INLINABLE newRandomLogNormalActivity #-}
newRandomLogNormalActivity =
  newPreemptibleRandomLogNormalActivity False
         
-- | Create a new activity that holds the process for a random time interval
-- distributed exponentially with the specified mean (the reciprocal of the rate),
-- when processing every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomExponentialActivity :: MonadDES m
                                => Double
                                -- ^ the mean time interval (the reciprocal of the rate)
                                -> Simulation m (Activity m () a a)
{-# INLINABLE newRandomExponentialActivity #-}
newRandomExponentialActivity =
  newPreemptibleRandomExponentialActivity False
         
-- | Create a new activity that holds the process for a random time interval
-- having the Erlang distribution with the specified scale (the reciprocal of the rate)
-- and shape parameters, when processing every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomErlangActivity :: MonadDES m
                           => Double
                           -- ^ the scale (the reciprocal of the rate)
                           -> Int
                           -- ^ the shape
                           -> Simulation m (Activity m () a a)
{-# INLINABLE newRandomErlangActivity #-}
newRandomErlangActivity =
  newPreemptibleRandomErlangActivity False

-- | Create a new activity that holds the process for a random time interval
-- having the Poisson distribution with the specified mean, when processing
-- every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomPoissonActivity :: MonadDES m
                            => Double
                            -- ^ the mean time interval
                            -> Simulation m (Activity m () a a)
{-# INLINABLE newRandomPoissonActivity #-}
newRandomPoissonActivity =
  newPreemptibleRandomPoissonActivity False

-- | Create a new activity that holds the process for a random time interval
-- having the binomial distribution with the specified probability and trials,
-- when processing every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomBinomialActivity :: MonadDES m
                             => Double
                             -- ^ the probability
                             -> Int
                             -- ^ the number of trials
                             -> Simulation m (Activity m () a a)
{-# INLINABLE newRandomBinomialActivity #-}
newRandomBinomialActivity =
  newPreemptibleRandomBinomialActivity False

-- | Create a new activity that holds the process for a random time interval
-- having the Gamma distribution with the specified shape and scale,
-- when processing every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomGammaActivity :: MonadDES m
                          => Double
                          -- ^ the shape
                          -> Double
                          -- ^ the scale (a reciprocal of the rate)
                          -> Simulation m (Activity m () a a)
{-# INLINABLE newRandomGammaActivity #-}
newRandomGammaActivity =
  newPreemptibleRandomGammaActivity False

-- | Create a new activity that holds the process for a random time interval
-- having the Beta distribution with the specified shape parameters (alpha and beta),
-- when processing every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomBetaActivity :: MonadDES m
                         => Double
                         -- ^ shape (alpha)
                         -> Double
                         -- ^ shape (beta)
                         -> Simulation m (Activity m () a a)
{-# INLINABLE newRandomBetaActivity #-}
newRandomBetaActivity =
  newPreemptibleRandomBetaActivity False

-- | Create a new activity that holds the process for a random time interval
-- having the Weibull distribution with the specified shape and scale,
-- when processing every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomWeibullActivity :: MonadDES m
                            => Double
                            -- ^ shape
                            -> Double
                            -- ^ scale
                            -> Simulation m (Activity m () a a)
{-# INLINABLE newRandomWeibullActivity #-}
newRandomWeibullActivity =
  newPreemptibleRandomWeibullActivity False

-- | Create a new activity that holds the process for a random time interval
-- having the specified discrete distribution, when processing every input element.
--
-- By default, it is assumed that the activity process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomDiscreteActivity :: MonadDES m
                             => DiscretePDF Double
                             -- ^ the discrete probability density function
                             -> Simulation m (Activity m () a a)
{-# INLINABLE newRandomDiscreteActivity #-}
newRandomDiscreteActivity =
  newPreemptibleRandomDiscreteActivity False

-- | Create a new activity that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
newPreemptibleRandomUniformActivity :: MonadDES m
                                       => Bool
                                       -- ^ whether the activity process can be preempted
                                       -> Double
                                       -- ^ the minimum time interval
                                       -> Double
                                       -- ^ the maximum time interval
                                       -> Simulation m (Activity m () a a)
{-# INLINABLE newPreemptibleRandomUniformActivity #-}
newPreemptibleRandomUniformActivity preemptible min max =
  newPreemptibleActivity preemptible $ \a ->
  do randomUniformProcess_ min max
     return a

-- | Create a new activity that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
newPreemptibleRandomUniformIntActivity :: MonadDES m
                                          => Bool
                                          -- ^ whether the activity process can be preempted
                                          -> Int
                                          -- ^ the minimum time interval
                                          -> Int
                                          -- ^ the maximum time interval
                                          -> Simulation m (Activity m () a a)
{-# INLINABLE newPreemptibleRandomUniformIntActivity #-}
newPreemptibleRandomUniformIntActivity preemptible min max =
  newPreemptibleActivity preemptible $ \a ->
  do randomUniformIntProcess_ min max
     return a

-- | Create a new activity that holds the process for a random time interval
-- having the triangular distribution, when processing every input element.
newPreemptibleRandomTriangularActivity :: MonadDES m
                                          => Bool
                                          -- ^ whether the activity process can be preempted
                                          -> Double
                                          -- ^ the minimum time interval
                                          -> Double
                                          -- ^ the median of the time interval
                                          -> Double
                                          -- ^ the maximum time interval
                                          -> Simulation m (Activity m () a a)
{-# INLINABLE newPreemptibleRandomTriangularActivity #-}
newPreemptibleRandomTriangularActivity preemptible min median max =
  newPreemptibleActivity preemptible $ \a ->
  do randomTriangularProcess_ min median max
     return a

-- | Create a new activity that holds the process for a random time interval
-- distributed normally, when processing every input element.
newPreemptibleRandomNormalActivity :: MonadDES m
                                      => Bool
                                      -- ^ whether the activity process can be preempted
                                      -> Double
                                      -- ^ the mean time interval
                                      -> Double
                                      -- ^ the time interval deviation
                                      -> Simulation m (Activity m () a a)
{-# INLINABLE newPreemptibleRandomNormalActivity #-}
newPreemptibleRandomNormalActivity preemptible mu nu =
  newPreemptibleActivity preemptible $ \a ->
  do randomNormalProcess_ mu nu
     return a

-- | Create a new activity that holds the process for a random time interval
-- having the lognormal distribution, when processing every input element.
newPreemptibleRandomLogNormalActivity :: MonadDES m
                                         => Bool
                                         -- ^ whether the activity process can be preempted
                                         -> Double
                                         -- ^ the mean of a normal distribution which
                                         -- this distribution is derived from
                                         -> Double
                                         -- ^ the deviation of a normal distribution which
                                         -- this distribution is derived from
                                         -> Simulation m (Activity m () a a)
{-# INLINABLE newPreemptibleRandomLogNormalActivity #-}
newPreemptibleRandomLogNormalActivity preemptible mu nu =
  newPreemptibleActivity preemptible $ \a ->
  do randomLogNormalProcess_ mu nu
     return a
         
-- | Create a new activity that holds the process for a random time interval
-- distributed exponentially with the specified mean (the reciprocal of the rate),
-- when processing every input element.
newPreemptibleRandomExponentialActivity :: MonadDES m
                                           => Bool
                                           -- ^ whether the activity process can be preempted
                                           -> Double
                                           -- ^ the mean time interval (the reciprocal of the rate)
                                           -> Simulation m (Activity m () a a)
{-# INLINABLE newPreemptibleRandomExponentialActivity #-}
newPreemptibleRandomExponentialActivity preemptible mu =
  newPreemptibleActivity preemptible $ \a ->
  do randomExponentialProcess_ mu
     return a
         
-- | Create a new activity that holds the process for a random time interval
-- having the Erlang distribution with the specified scale (the reciprocal of the rate)
-- and shape parameters, when processing every input element.
newPreemptibleRandomErlangActivity :: MonadDES m
                                      => Bool
                                      -- ^ whether the activity process can be preempted
                                      -> Double
                                      -- ^ the scale (the reciprocal of the rate)
                                      -> Int
                                      -- ^ the shape
                                      -> Simulation m (Activity m () a a)
{-# INLINABLE newPreemptibleRandomErlangActivity #-}
newPreemptibleRandomErlangActivity preemptible beta m =
  newPreemptibleActivity preemptible $ \a ->
  do randomErlangProcess_ beta m
     return a

-- | Create a new activity that holds the process for a random time interval
-- having the Poisson distribution with the specified mean, when processing
-- every input element.
newPreemptibleRandomPoissonActivity :: MonadDES m
                                       => Bool
                                       -- ^ whether the activity process can be preempted
                                       -> Double
                                       -- ^ the mean time interval
                                       -> Simulation m (Activity m () a a)
{-# INLINABLE newPreemptibleRandomPoissonActivity #-}
newPreemptibleRandomPoissonActivity preemptible mu =
  newPreemptibleActivity preemptible $ \a ->
  do randomPoissonProcess_ mu
     return a

-- | Create a new activity that holds the process for a random time interval
-- having the binomial distribution with the specified probability and trials,
-- when processing every input element.
newPreemptibleRandomBinomialActivity :: MonadDES m
                                        => Bool
                                        -- ^ whether the activity process can be preempted
                                        -> Double
                                        -- ^ the probability
                                        -> Int
                                        -- ^ the number of trials
                                        -> Simulation m (Activity m () a a)
{-# INLINABLE newPreemptibleRandomBinomialActivity #-}
newPreemptibleRandomBinomialActivity preemptible prob trials =
  newPreemptibleActivity preemptible $ \a ->
  do randomBinomialProcess_ prob trials
     return a

-- | Create a new activity that holds the process for a random time interval
-- having the Gamma distribution with the specified shape and scale,
-- when processing every input element.
newPreemptibleRandomGammaActivity :: MonadDES m
                                     => Bool
                                     -- ^ whether the activity process can be preempted
                                     -> Double
                                     -- ^ the shape
                                     -> Double
                                     -- ^ the scale
                                     -> Simulation m (Activity m () a a)
{-# INLINABLE newPreemptibleRandomGammaActivity #-}
newPreemptibleRandomGammaActivity preemptible kappa theta =
  newPreemptibleActivity preemptible $ \a ->
  do randomGammaProcess_ kappa theta
     return a

-- | Create a new activity that holds the process for a random time interval
-- having the Beta distribution with the specified shape parameters (alpha and beta),
-- when processing every input element.
newPreemptibleRandomBetaActivity :: MonadDES m
                                    => Bool
                                    -- ^ whether the activity process can be preempted
                                    -> Double
                                    -- ^ shape (alpha)
                                    -> Double
                                    -- ^ shape (beta)
                                    -> Simulation m (Activity m () a a)
{-# INLINABLE newPreemptibleRandomBetaActivity #-}
newPreemptibleRandomBetaActivity preemptible alpha beta =
  newPreemptibleActivity preemptible $ \a ->
  do randomBetaProcess_ alpha beta
     return a

-- | Create a new activity that holds the process for a random time interval
-- having the Weibull distribution with the specified shape and scale,
-- when processing every input element.
newPreemptibleRandomWeibullActivity :: MonadDES m
                                       => Bool
                                       -- ^ whether the activity process can be preempted
                                       -> Double
                                       -- ^ shape
                                       -> Double
                                       -- ^ scale
                                       -> Simulation m (Activity m () a a)
{-# INLINABLE newPreemptibleRandomWeibullActivity #-}
newPreemptibleRandomWeibullActivity preemptible alpha beta =
  newPreemptibleActivity preemptible $ \a ->
  do randomWeibullProcess_ alpha beta
     return a

-- | Create a new activity that holds the process for a random time interval
-- having the specified discrete distribution, when processing every input element.
newPreemptibleRandomDiscreteActivity :: MonadDES m
                                        => Bool
                                        -- ^ whether the activity process can be preempted
                                        -> DiscretePDF Double
                                        -- ^ the discrete probability density function
                                        -> Simulation m (Activity m () a a)
{-# INLINABLE newPreemptibleRandomDiscreteActivity #-}
newPreemptibleRandomDiscreteActivity preemptible dpdf =
  newPreemptibleActivity preemptible $ \a ->
  do randomDiscreteProcess_ dpdf
     return a
