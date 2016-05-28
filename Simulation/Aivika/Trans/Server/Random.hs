
-- |
-- Module     : Simulation.Aivika.Trans.Server.Random
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines some useful predefined servers that
-- hold the current process for the corresponding random time
-- interval, when processing every input element.
--

module Simulation.Aivika.Trans.Server.Random
       (newRandomUniformServer,
        newRandomUniformIntServer,
        newRandomTriangularServer,
        newRandomNormalServer,
        newRandomLogNormalServer,
        newRandomExponentialServer,
        newRandomErlangServer,
        newRandomPoissonServer,
        newRandomBinomialServer,
        newRandomGammaServer,
        newRandomBetaServer,
        newRandomWeibullServer,
        newRandomDiscreteServer,
        newPreemptibleRandomUniformServer,
        newPreemptibleRandomUniformIntServer,
        newPreemptibleRandomTriangularServer,
        newPreemptibleRandomNormalServer,
        newPreemptibleRandomLogNormalServer,
        newPreemptibleRandomExponentialServer,
        newPreemptibleRandomErlangServer,
        newPreemptibleRandomPoissonServer,
        newPreemptibleRandomBinomialServer,
        newPreemptibleRandomGammaServer,
        newPreemptibleRandomBetaServer,
        newPreemptibleRandomWeibullServer,
        newPreemptibleRandomDiscreteServer) where

import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Process
import Simulation.Aivika.Trans.Process.Random
import Simulation.Aivika.Trans.Server

-- | Create a new server that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomUniformServer :: MonadDES m
                          => Double
                          -- ^ the minimum time interval
                          -> Double
                          -- ^ the maximum time interval
                          -> Simulation m (Server m () a a)
{-# INLINABLE newRandomUniformServer #-}
newRandomUniformServer =
  newPreemptibleRandomUniformServer False

-- | Create a new server that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomUniformIntServer :: MonadDES m
                             => Int
                             -- ^ the minimum time interval
                             -> Int
                             -- ^ the maximum time interval
                             -> Simulation m (Server m () a a)
{-# INLINABLE newRandomUniformIntServer #-}
newRandomUniformIntServer =
  newPreemptibleRandomUniformIntServer False

-- | Create a new server that holds the process for a random time interval
-- having the triangular distribution, when processing every input element.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomTriangularServer :: MonadDES m
                             => Double
                             -- ^ the minimum time interval
                             -> Double
                             -- ^ the median of the time interval
                             -> Double
                             -- ^ the maximum time interval
                             -> Simulation m (Server m () a a)
{-# INLINABLE newRandomTriangularServer #-}
newRandomTriangularServer =
  newPreemptibleRandomTriangularServer False

-- | Create a new server that holds the process for a random time interval
-- distributed normally, when processing every input element.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomNormalServer :: MonadDES m
                         => Double
                         -- ^ the mean time interval
                         -> Double
                         -- ^ the time interval deviation
                         -> Simulation m (Server m () a a)
{-# INLINABLE newRandomNormalServer #-}
newRandomNormalServer =
  newPreemptibleRandomNormalServer False
         
-- | Create a new server that holds the process for a random time interval
-- having the lognormal distribution, when processing every input element.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomLogNormalServer :: MonadDES m
                            => Double
                            -- ^ the mean of a normal distribution which
                            -- this distribution is derived from
                            -> Double
                            -- ^ the deviation of a normal distribution which
                            -- this distribution is derived from
                            -> Simulation m (Server m () a a)
{-# INLINABLE newRandomLogNormalServer #-}
newRandomLogNormalServer =
  newPreemptibleRandomLogNormalServer False
         
-- | Create a new server that holds the process for a random time interval
-- distributed exponentially with the specified mean (the reciprocal of the rate),
-- when processing every input element.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomExponentialServer :: MonadDES m
                              => Double
                              -- ^ the mean time interval (the reciprocal of the rate)
                              -> Simulation m (Server m () a a)
{-# INLINABLE newRandomExponentialServer #-}
newRandomExponentialServer =
  newPreemptibleRandomExponentialServer False
         
-- | Create a new server that holds the process for a random time interval
-- having the Erlang distribution with the specified scale (the reciprocal of the rate)
-- and shape parameters, when processing every input element.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomErlangServer :: MonadDES m
                         => Double
                         -- ^ the scale (the reciprocal of the rate)
                         -> Int
                         -- ^ the shape
                         -> Simulation m (Server m () a a)
{-# INLINABLE newRandomErlangServer #-}
newRandomErlangServer =
  newPreemptibleRandomErlangServer False

-- | Create a new server that holds the process for a random time interval
-- having the Poisson distribution with the specified mean, when processing
-- every input element.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomPoissonServer :: MonadDES m
                          => Double
                          -- ^ the mean time interval
                          -> Simulation m (Server m () a a)
{-# INLINABLE newRandomPoissonServer #-}
newRandomPoissonServer =
  newPreemptibleRandomPoissonServer False

-- | Create a new server that holds the process for a random time interval
-- having the binomial distribution with the specified probability and trials,
-- when processing every input element.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomBinomialServer :: MonadDES m
                           => Double
                           -- ^ the probability
                           -> Int
                           -- ^ the number of trials
                           -> Simulation m (Server m () a a)
{-# INLINABLE newRandomBinomialServer #-}
newRandomBinomialServer =
  newPreemptibleRandomBinomialServer False

-- | Create a new server that holds the process for a random time interval
-- having the Gamma distribution with the specified shape and scale,
-- when processing every input element.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomGammaServer :: MonadDES m
                        => Double
                        -- ^ the shape
                        -> Double
                        -- ^ the scale (a reciprocal of the rate)
                        -> Simulation m (Server m () a a)
{-# INLINABLE newRandomGammaServer #-}
newRandomGammaServer =
  newPreemptibleRandomGammaServer False

-- | Create a new server that holds the process for a random time interval
-- having the Beta distribution with the specified shape parameters (alpha and beta),
-- when processing every input element.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomBetaServer :: MonadDES m
                       => Double
                       -- ^ shape (alpha)
                       -> Double
                       -- ^ shape (beta)
                       -> Simulation m (Server m () a a)
{-# INLINABLE newRandomBetaServer #-}
newRandomBetaServer =
  newPreemptibleRandomBetaServer False

-- | Create a new server that holds the process for a random time interval
-- having the Weibull distribution with the specified shape and scale,
-- when processing every input element.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomWeibullServer :: MonadDES m
                          => Double
                          -- ^ shape
                          -> Double
                          -- ^ scale
                          -> Simulation m (Server m () a a)
{-# INLINABLE newRandomWeibullServer #-}
newRandomWeibullServer =
  newPreemptibleRandomWeibullServer False

-- | Create a new server that holds the process for a random time interval
-- having the specified discrete distribution, when processing every input element.
--
-- By default, it is assumed that the server process cannot be preempted,
-- because the handling of possible task preemption is rather costly
-- operation.
newRandomDiscreteServer :: MonadDES m
                           => DiscretePDF Double
                           -- ^ the discrete probability density function
                           -> Simulation m (Server m () a a)
{-# INLINABLE newRandomDiscreteServer #-}
newRandomDiscreteServer =
  newPreemptibleRandomDiscreteServer False

-- | Create a new server that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
newPreemptibleRandomUniformServer :: MonadDES m
                                     => Bool
                                     -- ^ whether the server process can be preempted
                                     -> Double
                                     -- ^ the minimum time interval
                                     -> Double
                                     -- ^ the maximum time interval
                                     -> Simulation m (Server m () a a)
{-# INLINABLE newPreemptibleRandomUniformServer #-}
newPreemptibleRandomUniformServer preemptible min max =
  newPreemptibleServer preemptible $ \a ->
  do randomUniformProcess_ min max
     return a

-- | Create a new server that holds the process for a random time interval
-- distributed uniformly, when processing every input element.
newPreemptibleRandomUniformIntServer :: MonadDES m
                                        => Bool
                                        -- ^ whether the server process can be preempted
                                        -> Int
                                        -- ^ the minimum time interval
                                        -> Int
                                        -- ^ the maximum time interval
                                        -> Simulation m (Server m () a a)
{-# INLINABLE newPreemptibleRandomUniformIntServer #-}
newPreemptibleRandomUniformIntServer preemptible min max =
  newPreemptibleServer preemptible $ \a ->
  do randomUniformIntProcess_ min max
     return a

-- | Create a new server that holds the process for a random time interval
-- having the triangular distribution, when processing every input element.
newPreemptibleRandomTriangularServer :: MonadDES m
                                        => Bool
                                        -- ^ whether the server process can be preempted
                                        -> Double
                                        -- ^ the minimum time interval
                                        -> Double
                                        -- ^ the median of the time interval
                                        -> Double
                                        -- ^ the maximum time interval
                                        -> Simulation m (Server m () a a)
{-# INLINABLE newPreemptibleRandomTriangularServer #-}
newPreemptibleRandomTriangularServer preemptible min median max =
  newPreemptibleServer preemptible $ \a ->
  do randomTriangularProcess_ min median max
     return a

-- | Create a new server that holds the process for a random time interval
-- distributed normally, when processing every input element.
newPreemptibleRandomNormalServer :: MonadDES m
                                    => Bool
                                    -- ^ whether the server process can be preempted
                                    -> Double
                                    -- ^ the mean time interval
                                    -> Double
                                    -- ^ the time interval deviation
                                    -> Simulation m (Server m () a a)
{-# INLINABLE newPreemptibleRandomNormalServer #-}
newPreemptibleRandomNormalServer preemptible mu nu =
  newPreemptibleServer preemptible $ \a ->
  do randomNormalProcess_ mu nu
     return a

-- | Create a new server that holds the process for a random time interval
-- having the lognormal distribution, when processing every input element.
newPreemptibleRandomLogNormalServer :: MonadDES m
                                       => Bool
                                       -- ^ whether the server process can be preempted
                                       -> Double
                                       -- ^ the mean of a normal distribution which
                                       -- this distribution is derived from
                                       -> Double
                                       -- ^ the deviation of a normal distribution which
                                       -- this distribution is derived from
                                       -> Simulation m (Server m () a a)
{-# INLINABLE newPreemptibleRandomLogNormalServer #-}
newPreemptibleRandomLogNormalServer preemptible mu nu =
  newPreemptibleServer preemptible $ \a ->
  do randomLogNormalProcess_ mu nu
     return a
         
-- | Create a new server that holds the process for a random time interval
-- distributed exponentially with the specified mean (the reciprocal of the rate),
-- when processing every input element.
newPreemptibleRandomExponentialServer :: MonadDES m
                                         => Bool
                                         -- ^ whether the server process can be preempted
                                         -> Double
                                         -- ^ the mean time interval (the reciprocal of the rate)
                                         -> Simulation m (Server m () a a)
{-# INLINABLE newPreemptibleRandomExponentialServer #-}
newPreemptibleRandomExponentialServer preemptible mu =
  newPreemptibleServer preemptible $ \a ->
  do randomExponentialProcess_ mu
     return a
         
-- | Create a new server that holds the process for a random time interval
-- having the Erlang distribution with the specified scale (the reciprocal of the rate)
-- and shape parameters, when processing every input element.
newPreemptibleRandomErlangServer :: MonadDES m
                                    => Bool
                                    -- ^ whether the server process can be preempted
                                    -> Double
                                    -- ^ the scale (the reciprocal of the rate)
                                    -> Int
                                    -- ^ the shape
                                    -> Simulation m (Server m () a a)
{-# INLINABLE newPreemptibleRandomErlangServer #-}
newPreemptibleRandomErlangServer preemptible beta m =
  newPreemptibleServer preemptible $ \a ->
  do randomErlangProcess_ beta m
     return a

-- | Create a new server that holds the process for a random time interval
-- having the Poisson distribution with the specified mean, when processing
-- every input element.
newPreemptibleRandomPoissonServer :: MonadDES m
                                     => Bool
                                     -- ^ whether the server process can be preempted
                                     -> Double
                                     -- ^ the mean time interval
                                     -> Simulation m (Server m () a a)
{-# INLINABLE newPreemptibleRandomPoissonServer #-}
newPreemptibleRandomPoissonServer preemptible mu =
  newPreemptibleServer preemptible $ \a ->
  do randomPoissonProcess_ mu
     return a

-- | Create a new server that holds the process for a random time interval
-- having the binomial distribution with the specified probability and trials,
-- when processing every input element.
newPreemptibleRandomBinomialServer :: MonadDES m
                                      => Bool
                                      -- ^ whether the server process can be preempted
                                      -> Double
                                      -- ^ the probability
                                      -> Int
                                      -- ^ the number of trials
                                      -> Simulation m (Server m () a a)
{-# INLINABLE newPreemptibleRandomBinomialServer #-}
newPreemptibleRandomBinomialServer preemptible prob trials =
  newPreemptibleServer preemptible $ \a ->
  do randomBinomialProcess_ prob trials
     return a

-- | Create a new server that holds the process for a random time interval
-- having the Gamma distribution with the specified shape and scale,
-- when processing every input element.
newPreemptibleRandomGammaServer :: MonadDES m
                                   => Bool
                                   -- ^ whether the server process can be preempted
                                   -> Double
                                   -- ^ the shape
                                   -> Double
                                   -- ^ the scale (a reciprocal of the rate)
                                   -> Simulation m (Server m () a a)
{-# INLINABLE newPreemptibleRandomGammaServer #-}
newPreemptibleRandomGammaServer preemptible kappa theta =
  newPreemptibleServer preemptible $ \a ->
  do randomGammaProcess_ kappa theta
     return a

-- | Create a new server that holds the process for a random time interval
-- having the Beta distribution with the specified shape parameters (alpha and beta),
-- when processing every input element.
newPreemptibleRandomBetaServer :: MonadDES m
                                  => Bool
                                  -- ^ whether the server process can be preempted
                                  -> Double
                                  -- ^ shape (alpha)
                                  -> Double
                                  -- ^ shape (beta)
                                  -> Simulation m (Server m () a a)
{-# INLINABLE newPreemptibleRandomBetaServer #-}
newPreemptibleRandomBetaServer preemptible alpha beta =
  newPreemptibleServer preemptible $ \a ->
  do randomBetaProcess_ alpha beta
     return a

-- | Create a new server that holds the process for a random time interval
-- having the Weibull distribution with the specified shape and scale,
-- when processing every input element.
newPreemptibleRandomWeibullServer :: MonadDES m
                                     => Bool
                                     -- ^ whether the server process can be preempted
                                     -> Double
                                     -- ^ shape
                                     -> Double
                                     -- ^ scale
                                     -> Simulation m (Server m () a a)
{-# INLINABLE newPreemptibleRandomWeibullServer #-}
newPreemptibleRandomWeibullServer preemptible alpha beta =
  newPreemptibleServer preemptible $ \a ->
  do randomWeibullProcess_ alpha beta
     return a

-- | Create a new server that holds the process for a random time interval
-- having the specified discrete distribution, when processing every input element.
newPreemptibleRandomDiscreteServer :: MonadDES m
                                      => Bool
                                      -- ^ whether the server process can be preempted
                                      -> DiscretePDF Double
                                      -- ^ the discrete probability density function
                                      -> Simulation m (Server m () a a)
{-# INLINABLE newPreemptibleRandomDiscreteServer #-}
newPreemptibleRandomDiscreteServer preemptible dpdf =
  newPreemptibleServer preemptible $ \a ->
  do randomDiscreteProcess_ dpdf
     return a
