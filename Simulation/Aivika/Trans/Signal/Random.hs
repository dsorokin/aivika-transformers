
-- |
-- Module     : Simulation.Aivika.Trans.Signal.Random
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines random signals of events, which are useful
-- for describing the input of the model.
--

module Simulation.Aivika.Trans.Signal.Random
       (-- * Signal of Random Events
        newRandomSignal,
        newRandomUniformSignal,
        newRandomUniformIntSignal,
        newRandomTriangularSignal,
        newRandomNormalSignal,
        newRandomLogNormalSignal,
        newRandomExponentialSignal,
        newRandomErlangSignal,
        newRandomPoissonSignal,
        newRandomBinomialSignal,
        newRandomGammaSignal,
        newRandomBetaSignal,
        newRandomWeibullSignal,
        newRandomDiscreteSignal) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Parameter.Random
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Dynamics
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.Composite
import Simulation.Aivika.Trans.Process
import Simulation.Aivika.Trans.Signal
import Simulation.Aivika.Trans.Statistics
import Simulation.Aivika.Trans.Arrival

-- | Return a signal of random events that arrive with the specified delay.
newRandomSignal :: MonadDES m
                   => Parameter m (Double, a)
                   -- ^ compute a pair of the delay and event of type @a@
                   -> Composite m (Signal m (Arrival a))
                   -- ^ the computation that returns a signal emitting the delayed events
{-# INLINABLE newRandomSignal #-}
newRandomSignal delay =
  do source <- liftSimulation newSignalSource
     let loop t0 =
           do (delay, a) <- liftParameter delay
              when (delay > 0) $
                holdProcess delay
              t2 <- liftDynamics time
              let arrival = Arrival { arrivalValue = a,
                                      arrivalTime  = t2,
                                      arrivalDelay =
                                        case t0 of
                                          Nothing -> Nothing
                                          Just t0 -> Just delay }
              liftEvent $
                triggerSignal source arrival
              loop (Just t2)
     pid <- liftSimulation newProcessId
     liftEvent $
       runProcessUsingId pid $
       loop Nothing
     disposableComposite $
       DisposableEvent $
       cancelProcessWithId pid
     return $ publishSignal source

-- | Create a new signal with random delays distributed uniformly.
newRandomUniformSignal :: MonadDES m
                          => Double
                          -- ^ the minimum delay
                          -> Double
                          -- ^ the maximum delay
                          -> Composite m (Signal m (Arrival Double))
                          -- ^ the computation of signal emitting random events with the delays generated
{-# INLINABLE newRandomUniformSignal #-}
newRandomUniformSignal min max =
  newRandomSignal $
  randomUniform min max >>= \x ->
  return (x, x)

-- | Create a new signal with integer random delays distributed uniformly.
newRandomUniformIntSignal :: MonadDES m
                             => Int
                             -- ^ the minimum delay
                             -> Int
                             -- ^ the maximum delay
                             -> Composite m (Signal m (Arrival Int))
                             -- ^ the computation of signal emitting random events with the delays generated
{-# INLINABLE newRandomUniformIntSignal #-}
newRandomUniformIntSignal min max =
  newRandomSignal $
  randomUniformInt min max >>= \x ->
  return (fromIntegral x, x)

-- | Create a new signal with random delays having the triangular distribution.
newRandomTriangularSignal :: MonadDES m
                             => Double
                             -- ^ the minimum delay
                             -> Double
                             -- ^ the median of the delay
                             -> Double
                             -- ^ the maximum delay
                             -> Composite m (Signal m (Arrival Double))
                             -- ^ the computation of signal emitting random events with the delays generated
{-# INLINABLE newRandomTriangularSignal #-}
newRandomTriangularSignal min median max =
  newRandomSignal $
  randomTriangular min median max >>= \x ->
  return (x, x)

-- | Create a new signal with random delays distributed normally.
newRandomNormalSignal :: MonadDES m
                         => Double
                         -- ^ the mean delay
                         -> Double
                         -- ^ the delay deviation
                         -> Composite m (Signal m (Arrival Double))
                         -- ^ the computation of signal emitting random events with the delays generated
{-# INLINABLE newRandomNormalSignal #-}
newRandomNormalSignal mu nu =
  newRandomSignal $
  randomNormal mu nu >>= \x ->
  return (x, x)

-- | Create a new signal with random delays having the lognormal distribution.
newRandomLogNormalSignal :: MonadDES m
                            => Double
                            -- ^ the mean of a normal distribution which
                            -- this distribution is derived from
                            -> Double
                            -- ^ the deviation of a normal distribution which
                            -- this distribution is derived from
                            -> Composite m (Signal m (Arrival Double))
                            -- ^ the computation of signal emitting random events with the delays generated
{-# INLINABLE newRandomLogNormalSignal #-}
newRandomLogNormalSignal mu nu =
  newRandomSignal $
  randomLogNormal mu nu >>= \x ->
  return (x, x)

-- | Return a new signal with random delays distibuted exponentially with the specified mean
-- (the reciprocal of the rate).
newRandomExponentialSignal :: MonadDES m
                              => Double
                              -- ^ the mean delay (the reciprocal of the rate)
                              -> Composite m (Signal m (Arrival Double))
                              -- ^ the computation of signal emitting random events with the delays generated
{-# INLINABLE newRandomExponentialSignal #-}
newRandomExponentialSignal mu =
  newRandomSignal $
  randomExponential mu >>= \x ->
  return (x, x)
         
-- | Return a new signal with random delays having the Erlang distribution with the specified
-- scale (the reciprocal of the rate) and shape parameters.
newRandomErlangSignal :: MonadDES m
                         => Double
                         -- ^ the scale (the reciprocal of the rate)
                         -> Int
                         -- ^ the shape
                         -> Composite m (Signal m (Arrival Double))
                         -- ^ the computation of signal emitting random events with the delays generated
{-# INLINABLE newRandomErlangSignal #-}
newRandomErlangSignal beta m =
  newRandomSignal $
  randomErlang beta m >>= \x ->
  return (x, x)

-- | Return a new signal with random delays having the Poisson distribution with
-- the specified mean.
newRandomPoissonSignal :: MonadDES m
                          => Double
                          -- ^ the mean delay
                          -> Composite m (Signal m (Arrival Int))
                          -- ^ the computation of signal emitting random events with the delays generated
{-# INLINABLE newRandomPoissonSignal #-}
newRandomPoissonSignal mu =
  newRandomSignal $
  randomPoisson mu >>= \x ->
  return (fromIntegral x, x)

-- | Return a new signal with random delays having the binomial distribution with the specified
-- probability and trials.
newRandomBinomialSignal :: MonadDES m
                           => Double
                           -- ^ the probability
                           -> Int
                           -- ^ the number of trials
                           -> Composite m (Signal m (Arrival Int))
                           -- ^ the computation of signal emitting random events with the delays generated
{-# INLINABLE newRandomBinomialSignal #-}
newRandomBinomialSignal prob trials =
  newRandomSignal $
  randomBinomial prob trials >>= \x ->
  return (fromIntegral x, x)

-- | Return a new signal with random delays having the Gamma distribution by the specified
-- shape and scale.
newRandomGammaSignal :: MonadDES m
                        => Double
                        -- ^ the shape
                        -> Double
                        -- ^ the scale (a reciprocal of the rate)
                        -> Composite m (Signal m (Arrival Double))
                        -- ^ the computation of signal emitting random events with the delays generated
{-# INLINABLE newRandomGammaSignal #-}
newRandomGammaSignal kappa theta =
  newRandomSignal $
  randomGamma kappa theta >>= \x ->
  return (x, x)

-- | Return a new signal with random delays having the Beta distribution by the specified
-- shape parameters (alpha and beta).
newRandomBetaSignal :: MonadDES m
                       => Double
                       -- ^ the shape (alpha)
                       -> Double
                       -- ^ the shape (beta)
                       -> Composite m (Signal m (Arrival Double))
                       -- ^ the computation of signal emitting random events with the delays generated
{-# INLINABLE newRandomBetaSignal #-}
newRandomBetaSignal alpha beta =
  newRandomSignal $
  randomBeta alpha beta >>= \x ->
  return (x, x)

-- | Return a new signal with random delays having the Weibull distribution by the specified
-- shape and scale.
newRandomWeibullSignal :: MonadDES m
                          => Double
                          -- ^ shape
                          -> Double
                          -- ^ scale
                          -> Composite m (Signal m (Arrival Double))
                          -- ^ the computation of signal emitting random events with the delays generated
{-# INLINABLE newRandomWeibullSignal #-}
newRandomWeibullSignal alpha beta =
  newRandomSignal $
  randomWeibull alpha beta >>= \x ->
  return (x, x)

-- | Return a new signal with random delays having the specified discrete distribution.
newRandomDiscreteSignal :: MonadDES m
                           => DiscretePDF Double
                           -- ^ the discrete probability density function
                           -> Composite m (Signal m (Arrival Double))
                           -- ^ the computation of signal emitting random events with the delays generated
{-# INLINABLE newRandomDiscreteSignal #-}
newRandomDiscreteSignal dpdf =
  newRandomSignal $
  randomDiscrete dpdf >>= \x ->
  return (x, x)
