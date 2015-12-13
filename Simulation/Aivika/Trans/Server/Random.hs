
-- |
-- Module     : Simulation.Aivika.Trans.Server.Random
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- This module defines some useful predefined servers that
-- hold the current process for the corresponding random time
-- interval, when processing every input element.
--

module Simulation.Aivika.Trans.Server.Random
       (newRandomUniformServer,
        newRandomUniformIntServer,
        newRandomNormalServer,
        newRandomExponentialServer,
        newRandomErlangServer,
        newRandomPoissonServer,
        newRandomBinomialServer,
        newPreemptibleRandomUniformServer,
        newPreemptibleRandomUniformIntServer,
        newPreemptibleRandomNormalServer,
        newPreemptibleRandomExponentialServer,
        newPreemptibleRandomErlangServer,
        newPreemptibleRandomPoissonServer,
        newPreemptibleRandomBinomialServer) where

import Simulation.Aivika.Trans.DES
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
