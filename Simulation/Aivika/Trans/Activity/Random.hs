
-- |
-- Module     : Simulation.Aivika.Trans.Activity.Random
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
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
        newRandomNormalActivity,
        newRandomExponentialActivity,
        newRandomErlangActivity,
        newRandomPoissonActivity,
        newRandomBinomialActivity,
        newPreemptibleRandomUniformActivity,
        newPreemptibleRandomUniformIntActivity,
        newPreemptibleRandomNormalActivity,
        newPreemptibleRandomExponentialActivity,
        newPreemptibleRandomErlangActivity,
        newPreemptibleRandomPoissonActivity,
        newPreemptibleRandomBinomialActivity) where

import Simulation.Aivika.Trans.DES
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
