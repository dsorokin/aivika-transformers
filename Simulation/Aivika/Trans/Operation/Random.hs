
-- |
-- Module     : Simulation.Aivika.Trans.Operation.Random
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines some useful predefined operations that
-- hold the current process for the corresponding random time
-- interval, when processing every input element.
--

module Simulation.Aivika.Trans.Operation.Random
       (newRandomUniformOperation,
        newRandomUniformIntOperation,
        newRandomTriangularOperation,
        newRandomNormalOperation,
        newRandomLogNormalOperation,
        newRandomExponentialOperation,
        newRandomErlangOperation,
        newRandomPoissonOperation,
        newRandomBinomialOperation,
        newRandomGammaOperation,
        newRandomBetaOperation,
        newRandomWeibullOperation,
        newRandomDiscreteOperation,
        newPreemptibleRandomUniformOperation,
        newPreemptibleRandomUniformIntOperation,
        newPreemptibleRandomTriangularOperation,
        newPreemptibleRandomNormalOperation,
        newPreemptibleRandomLogNormalOperation,
        newPreemptibleRandomExponentialOperation,
        newPreemptibleRandomErlangOperation,
        newPreemptibleRandomPoissonOperation,
        newPreemptibleRandomBinomialOperation,
        newPreemptibleRandomGammaOperation,
        newPreemptibleRandomBetaOperation,
        newPreemptibleRandomWeibullOperation,
        newPreemptibleRandomDiscreteOperation) where

import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.Process
import Simulation.Aivika.Trans.Process.Random
import Simulation.Aivika.Trans.Operation

-- | Create a new operation that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomUniformOperation :: MonadDES m
                             => Double
                             -- ^ the minimum time interval
                             -> Double
                             -- ^ the maximum time interval
                             -> Event m (Operation m a a)
{-# INLINABLE newRandomUniformOperation #-}
newRandomUniformOperation =
  newPreemptibleRandomUniformOperation False

-- | Create a new operation that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomUniformIntOperation :: MonadDES m
                                => Int
                                -- ^ the minimum time interval
                                -> Int
                                -- ^ the maximum time interval
                                -> Event m (Operation m a a)
{-# INLINABLE newRandomUniformIntOperation #-}
newRandomUniformIntOperation =
  newPreemptibleRandomUniformIntOperation False

-- | Create a new operation that holds the process for a random time interval
-- having the triangular distribution, when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomTriangularOperation :: MonadDES m
                                => Double
                                -- ^ the minimum time interval
                                -> Double
                                -- ^ the median of the time interval
                                -> Double
                                -- ^ the maximum time interval
                                -> Event m (Operation m a a)
{-# INLINABLE newRandomTriangularOperation #-}
newRandomTriangularOperation =
  newPreemptibleRandomTriangularOperation False

-- | Create a new operation that holds the process for a random time interval
-- distributed normally, when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomNormalOperation :: MonadDES m
                            => Double
                            -- ^ the mean time interval
                            -> Double
                            -- ^ the time interval deviation
                            -> Event m (Operation m a a)
{-# INLINABLE newRandomNormalOperation #-}
newRandomNormalOperation =
  newPreemptibleRandomNormalOperation False
         
-- | Create a new operation that holds the process for a random time interval
-- having the lognormal distribution, when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomLogNormalOperation :: MonadDES m
                               => Double
                               -- ^ the mean of a normal distribution which
                               -- this distribution is derived from
                               -> Double
                               -- ^ the deviation of a normal distribution which
                               -- this distribution is derived from
                               -> Event m (Operation m a a)
{-# INLINABLE newRandomLogNormalOperation #-}
newRandomLogNormalOperation =
  newPreemptibleRandomLogNormalOperation False
         
-- | Create a new operation that holds the process for a random time interval
-- distributed exponentially with the specified mean (the reciprocal of the rate),
-- when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomExponentialOperation :: MonadDES m
                                 => Double
                                 -- ^ the mean time interval (the reciprocal of the rate)
                                 -> Event m (Operation m a a)
{-# INLINABLE newRandomExponentialOperation #-}
newRandomExponentialOperation =
  newPreemptibleRandomExponentialOperation False
         
-- | Create a new operation that holds the process for a random time interval
-- having the Erlang distribution with the specified scale (the reciprocal of the rate)
-- and shape parameters, when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomErlangOperation :: MonadDES m
                            => Double
                            -- ^ the scale (the reciprocal of the rate)
                            -> Int
                            -- ^ the shape
                            -> Event m (Operation m a a)
{-# INLINABLE newRandomErlangOperation #-}
newRandomErlangOperation =
  newPreemptibleRandomErlangOperation False

-- | Create a new operation that holds the process for a random time interval
-- having the Poisson distribution with the specified mean, when processing
-- every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomPoissonOperation :: MonadDES m
                             => Double
                             -- ^ the mean time interval
                             -> Event m (Operation m a a)
{-# INLINABLE newRandomPoissonOperation #-}
newRandomPoissonOperation =
  newPreemptibleRandomPoissonOperation False

-- | Create a new operation that holds the process for a random time interval
-- having the binomial distribution with the specified probability and trials,
-- when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomBinomialOperation :: MonadDES m
                              => Double
                              -- ^ the probability
                              -> Int
                              -- ^ the number of trials
                              -> Event m (Operation m a a)
{-# INLINABLE newRandomBinomialOperation #-}
newRandomBinomialOperation =
  newPreemptibleRandomBinomialOperation False

-- | Create a new operation that holds the process for a random time interval
-- having the Gamma distribution with the specified shape and scale,
-- when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomGammaOperation :: MonadDES m
                           => Double
                           -- ^ the shape
                           -> Double
                           -- ^ the scale (a reciprocal of the rate)
                           -> Event m (Operation m a a)
{-# INLINABLE newRandomGammaOperation #-}
newRandomGammaOperation =
  newPreemptibleRandomGammaOperation False

-- | Create a new operation that holds the process for a random time interval
-- having the Beta distribution with the specified shape parameters (alpha and beta),
-- when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomBetaOperation :: MonadDES m
                          => Double
                          -- ^ shape (alpha)
                          -> Double
                          -- ^ shape (beta)
                          -> Event m (Operation m a a)
{-# INLINABLE newRandomBetaOperation #-}
newRandomBetaOperation =
  newPreemptibleRandomBetaOperation False

-- | Create a new operation that holds the process for a random time interval
-- having the Weibull distribution with the specified shape and scale,
-- when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomWeibullOperation :: MonadDES m
                             => Double
                             -- ^ shape
                             -> Double
                             -- ^ scale
                             -> Event m (Operation m a a)
{-# INLINABLE newRandomWeibullOperation #-}
newRandomWeibullOperation =
  newPreemptibleRandomWeibullOperation False

-- | Create a new operation that holds the process for a random time interval
-- having the specified discrete distribution, when processing every input element.
--
-- By default, it is assumed that the operation process cannot be preempted,
-- because the handling of possible task preemption is rather costly.
newRandomDiscreteOperation :: MonadDES m
                              => DiscretePDF Double
                              -- ^ the discrete probability density function
                              -> Event m (Operation m a a)
{-# INLINABLE newRandomDiscreteOperation #-}
newRandomDiscreteOperation =
  newPreemptibleRandomDiscreteOperation False

-- | Create a new operation that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
newPreemptibleRandomUniformOperation :: MonadDES m
                                        => Bool
                                        -- ^ whether the operation process can be preempted
                                        -> Double
                                        -- ^ the minimum time interval
                                        -> Double
                                        -- ^ the maximum time interval
                                        -> Event m (Operation m a a)
{-# INLINABLE newPreemptibleRandomUniformOperation #-}
newPreemptibleRandomUniformOperation preemptible min max =
  newPreemptibleOperation preemptible $ \a ->
  do randomUniformProcess_ min max
     return a

-- | Create a new operation that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
newPreemptibleRandomUniformIntOperation :: MonadDES m
                                           => Bool
                                           -- ^ whether the operation process can be preempted
                                           -> Int
                                           -- ^ the minimum time interval
                                           -> Int
                                           -- ^ the maximum time interval
                                           -> Event m (Operation m a a)
{-# INLINABLE newPreemptibleRandomUniformIntOperation #-}
newPreemptibleRandomUniformIntOperation preemptible min max =
  newPreemptibleOperation preemptible $ \a ->
  do randomUniformIntProcess_ min max
     return a

-- | Create a new operation that holds the process for a random time interval
-- having the triangular distribution, when processing every input element.
newPreemptibleRandomTriangularOperation :: MonadDES m
                                           => Bool
                                           -- ^ whether the operation process can be preempted
                                           -> Double
                                           -- ^ the minimum time interval
                                           -> Double
                                           -- ^ the median of the time interval
                                           -> Double
                                           -- ^ the maximum time interval
                                           -> Event m (Operation m a a)
{-# INLINABLE newPreemptibleRandomTriangularOperation #-}
newPreemptibleRandomTriangularOperation preemptible min median max =
  newPreemptibleOperation preemptible $ \a ->
  do randomTriangularProcess_ min median max
     return a

-- | Create a new operation that holds the process for a random time interval
-- distributed normally, when processing every input element.
newPreemptibleRandomNormalOperation :: MonadDES m
                                       => Bool
                                       -- ^ whether the operation process can be preempted
                                       -> Double
                                       -- ^ the mean time interval
                                       -> Double
                                       -- ^ the time interval deviation
                                       -> Event m (Operation m a a)
{-# INLINABLE newPreemptibleRandomNormalOperation #-}
newPreemptibleRandomNormalOperation preemptible mu nu =
  newPreemptibleOperation preemptible $ \a ->
  do randomNormalProcess_ mu nu
     return a

-- | Create a new operation that holds the process for a random time interval
-- having the lognormal distribution, when processing every input element.
newPreemptibleRandomLogNormalOperation :: MonadDES m
                                          => Bool
                                          -- ^ whether the operation process can be preempted
                                          -> Double
                                          -- ^ the mean of a normal distribution which
                                          -- this distribution is derived from
                                          -> Double
                                          -- ^ the deviation of a normal distribution which
                                          -- this distribution is derived from
                                          -> Event m (Operation m a a)
{-# INLINABLE newPreemptibleRandomLogNormalOperation #-}
newPreemptibleRandomLogNormalOperation preemptible mu nu =
  newPreemptibleOperation preemptible $ \a ->
  do randomLogNormalProcess_ mu nu
     return a

-- | Create a new operation that holds the process for a random time interval
-- distributed exponentially with the specified mean (the reciprocal of the rate),
-- when processing every input element.
newPreemptibleRandomExponentialOperation :: MonadDES m
                                            => Bool
                                            -- ^ whether the operation process can be preempted
                                            -> Double
                                            -- ^ the mean time interval (the reciprocal of the rate)
                                            -> Event m (Operation m a a)
{-# INLINABLE newPreemptibleRandomExponentialOperation #-}
newPreemptibleRandomExponentialOperation preemptible mu =
  newPreemptibleOperation preemptible $ \a ->
  do randomExponentialProcess_ mu
     return a
         
-- | Create a new operation that holds the process for a random time interval
-- having the Erlang distribution with the specified scale (the reciprocal of the rate)
-- and shape parameters, when processing every input element.
newPreemptibleRandomErlangOperation :: MonadDES m
                                       => Bool
                                       -- ^ whether the operation process can be preempted
                                       -> Double
                                       -- ^ the scale (the reciprocal of the rate)
                                       -> Int
                                       -- ^ the shape
                                       -> Event m (Operation m a a)
{-# INLINABLE newPreemptibleRandomErlangOperation #-}
newPreemptibleRandomErlangOperation preemptible beta m =
  newPreemptibleOperation preemptible $ \a ->
  do randomErlangProcess_ beta m
     return a

-- | Create a new operation that holds the process for a random time interval
-- having the Poisson distribution with the specified mean, when processing
-- every input element.
newPreemptibleRandomPoissonOperation :: MonadDES m
                                        => Bool
                                        -- ^ whether the operation process can be preempted
                                        -> Double
                                        -- ^ the mean time interval
                                        -> Event m (Operation m a a)
{-# INLINABLE newPreemptibleRandomPoissonOperation #-}
newPreemptibleRandomPoissonOperation preemptible mu =
  newPreemptibleOperation preemptible $ \a ->
  do randomPoissonProcess_ mu
     return a

-- | Create a new operation that holds the process for a random time interval
-- having the binomial distribution with the specified probability and trials,
-- when processing every input element.
newPreemptibleRandomBinomialOperation :: MonadDES m
                                         => Bool
                                         -- ^ whether the operation process can be preempted
                                         -> Double
                                         -- ^ the probability
                                         -> Int
                                         -- ^ the number of trials
                                         -> Event m (Operation m a a)
{-# INLINABLE newPreemptibleRandomBinomialOperation #-}
newPreemptibleRandomBinomialOperation preemptible prob trials =
  newPreemptibleOperation preemptible $ \a ->
  do randomBinomialProcess_ prob trials
     return a

-- | Create a new operation that holds the process for a random time interval
-- having the Gamma distribution with the specified shape and scale,
-- when processing every input element.
newPreemptibleRandomGammaOperation :: MonadDES m
                                      => Bool
                                      -- ^ whether the operation process can be preempted
                                      -> Double
                                      -- ^ the shape
                                      -> Double
                                      -- ^ the scale
                                      -> Event m (Operation m a a)
{-# INLINABLE newPreemptibleRandomGammaOperation #-}
newPreemptibleRandomGammaOperation preemptible kappa theta =
  newPreemptibleOperation preemptible $ \a ->
  do randomGammaProcess_ kappa theta
     return a

-- | Create a new operation that holds the process for a random time interval
-- having the Beta distribution with the specified shape parameters (alpha and beta),
-- when processing every input element.
newPreemptibleRandomBetaOperation :: MonadDES m
                                     => Bool
                                     -- ^ whether the operation process can be preempted
                                     -> Double
                                     -- ^ shape (alpha)
                                     -> Double
                                     -- ^ shape (beta)
                                     -> Event m (Operation m a a)
{-# INLINABLE newPreemptibleRandomBetaOperation #-}
newPreemptibleRandomBetaOperation preemptible alpha beta =
  newPreemptibleOperation preemptible $ \a ->
  do randomBetaProcess_ alpha beta
     return a

-- | Create a new operation that holds the process for a random time interval
-- having the Weibull distribution with the specified shape and scale,
-- when processing every input element.
newPreemptibleRandomWeibullOperation :: MonadDES m
                                        => Bool
                                        -- ^ whether the operation process can be preempted
                                        -> Double
                                        -- ^ shape
                                        -> Double
                                        -- ^ scale
                                        -> Event m (Operation m a a)
{-# INLINABLE newPreemptibleRandomWeibullOperation #-}
newPreemptibleRandomWeibullOperation preemptible alpha beta =
  newPreemptibleOperation preemptible $ \a ->
  do randomWeibullProcess_ alpha beta
     return a

-- | Create a new operation that holds the process for a random time interval
-- having the specified discrete distribution, when processing every input element.
newPreemptibleRandomDiscreteOperation :: MonadDES m
                                         => Bool
                                         -- ^ whether the operation process can be preempted
                                         -> DiscretePDF Double
                                         -- ^ the discrete probability density function
                                         -> Event m (Operation m a a)
{-# INLINABLE newPreemptibleRandomDiscreteOperation #-}
newPreemptibleRandomDiscreteOperation preemptible dpdf =
  newPreemptibleOperation preemptible $ \a ->
  do randomDiscreteProcess_ dpdf
     return a
