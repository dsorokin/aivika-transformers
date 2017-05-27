
-- |
-- Module     : Simulation.Aivika.Trans.Stream.Random
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines random streams of events, which are useful
-- for describing the input of the model.
--

module Simulation.Aivika.Trans.Stream.Random
       (-- * Stream of Random Events
        randomStream,
        randomUniformStream,
        randomUniformIntStream,
        randomTriangularStream,
        randomNormalStream,
        randomLogNormalStream,
        randomExponentialStream,
        randomErlangStream,
        randomPoissonStream,
        randomBinomialStream,
        randomGammaStream,
        randomBetaStream,
        randomWeibullStream,
        randomDiscreteStream) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Parameter.Random
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Dynamics
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.Process
import Simulation.Aivika.Trans.Processor
import Simulation.Aivika.Trans.Stream
import Simulation.Aivika.Trans.Statistics
import Simulation.Aivika.Trans.Ref
import Simulation.Aivika.Trans.Arrival

-- | Return a sream of random events that arrive with the specified delay.
randomStream :: MonadDES m
                => Parameter m (Double, a)
                -- ^ compute a pair of the delay and event of type @a@
                -> Stream m (Arrival a)
                -- ^ a stream of delayed events
{-# INLINE randomStream #-}
randomStream delay = Cons $ loop Nothing where
  loop t0 =
    do t1 <- liftDynamics time
       case t0 of
         Nothing -> return ()
         Just t0 ->
           when (t1 /= t0) $
           error $
           "The time of requesting for a new random event is different from " ++
           "the time when the previous event has arrived. Probably, your model " ++
           "contains a logical error. The random events should be requested permanently. " ++
           "At least, they can be lost, for example, when trying to enqueue them, but " ++
           "the random stream itself must always execute: randomStream."
       (delay, a) <- liftParameter delay
       when (delay > 0) $
         holdProcess delay
       t2 <- liftDynamics time
       let arrival = Arrival { arrivalValue = a,
                               arrivalTime  = t2,
                               arrivalDelay =
                                 case t0 of
                                   Nothing -> Nothing
                                   Just t0 -> Just delay }
       return (arrival, Cons $ loop (Just t2))

-- | Create a new stream with delays distributed uniformly.
randomUniformStream :: MonadDES m
                       => Double
                       -- ^ the minimum delay
                       -> Double
                       -- ^ the maximum delay
                       -> Stream m (Arrival Double)
                       -- ^ the stream of random events with the delays generated
{-# INLINABLE randomUniformStream #-}
randomUniformStream min max =
  randomStream $
  randomUniform min max >>= \x ->
  return (x, x)

-- | Create a new stream with integer delays distributed uniformly.
randomUniformIntStream :: MonadDES m
                          => Int
                          -- ^ the minimum delay
                          -> Int
                          -- ^ the maximum delay
                          -> Stream m (Arrival Int)
                          -- ^ the stream of random events with the delays generated
{-# INLINABLE randomUniformIntStream #-}
randomUniformIntStream min max =
  randomStream $
  randomUniformInt min max >>= \x ->
  return (fromIntegral x, x)

-- | Create a new stream with random delays having the triangular distribution.
randomTriangularStream :: MonadDES m
                          => Double
                          -- ^ the minimum delay
                          -> Double
                          -- ^ the median of the delay
                          -> Double
                          -- ^ the maximum delay
                          -> Stream m (Arrival Double)
                          -- ^ the stream of random events with the delays generated
{-# INLINABLE randomTriangularStream #-}
randomTriangularStream min median max =
  randomStream $
  randomTriangular min median max >>= \x ->
  return (x, x)

-- | Create a new stream with delays distributed normally.
randomNormalStream :: MonadDES m
                      => Double
                      -- ^ the mean delay
                      -> Double
                      -- ^ the delay deviation
                      -> Stream m (Arrival Double)
                      -- ^ the stream of random events with the delays generated
{-# INLINABLE randomNormalStream #-}
randomNormalStream mu nu =
  randomStream $
  randomNormal mu nu >>= \x ->
  return (x, x)

-- | Create a new stream with random delays having the lognormal distribution.
randomLogNormalStream :: MonadDES m
                         => Double
                         -- ^ the mean of a normal distribution which
                         -- this distribution is derived from
                         -> Double
                         -- ^ the deviation of a normal distribution which
                         -- this distribution is derived from
                         -> Stream m (Arrival Double)
                         -- ^ the stream of random events with the delays generated
{-# INLINABLE randomLogNormalStream #-}
randomLogNormalStream mu nu =
  randomStream $
  randomLogNormal mu nu >>= \x ->
  return (x, x)
         
-- | Return a new stream with delays distibuted exponentially with the specified mean
-- (the reciprocal of the rate).
randomExponentialStream :: MonadDES m
                           => Double
                           -- ^ the mean delay (the reciprocal of the rate)
                           -> Stream m (Arrival Double)
                           -- ^ the stream of random events with the delays generated
{-# INLINABLE randomExponentialStream #-}
randomExponentialStream mu =
  randomStream $
  randomExponential mu >>= \x ->
  return (x, x)
         
-- | Return a new stream with delays having the Erlang distribution with the specified
-- scale (the reciprocal of the rate) and shape parameters.
randomErlangStream :: MonadDES m
                      => Double
                      -- ^ the scale (the reciprocal of the rate)
                      -> Int
                      -- ^ the shape
                      -> Stream m (Arrival Double)
                      -- ^ the stream of random events with the delays generated
{-# INLINABLE randomErlangStream #-}
randomErlangStream beta m =
  randomStream $
  randomErlang beta m >>= \x ->
  return (x, x)

-- | Return a new stream with delays having the Poisson distribution with
-- the specified mean.
randomPoissonStream :: MonadDES m
                       => Double
                       -- ^ the mean delay
                       -> Stream m (Arrival Int)
                       -- ^ the stream of random events with the delays generated
{-# INLINABLE randomPoissonStream #-}
randomPoissonStream mu =
  randomStream $
  randomPoisson mu >>= \x ->
  return (fromIntegral x, x)

-- | Return a new stream with delays having the binomial distribution with the specified
-- probability and trials.
randomBinomialStream :: MonadDES m
                        => Double
                        -- ^ the probability
                        -> Int
                        -- ^ the number of trials
                        -> Stream m (Arrival Int)
                        -- ^ the stream of random events with the delays generated
{-# INLINABLE randomBinomialStream #-}
randomBinomialStream prob trials =
  randomStream $
  randomBinomial prob trials >>= \x ->
  return (fromIntegral x, x)

-- | Return a new stream with random delays having the Gamma distribution by the specified
-- shape and scale.
randomGammaStream :: MonadDES m
                     => Double
                     -- ^ the shape
                     -> Double
                     -- ^ the scale (a reciprocal of the rate)
                     -> Stream m (Arrival Double)
                     -- ^ the stream of random events with the delays generated
{-# INLINABLE randomGammaStream #-}
randomGammaStream kappa theta =
  randomStream $
  randomGamma kappa theta >>= \x ->
  return (x, x)

-- | Return a new stream with random delays having the Beta distribution by the specified
-- shape parameters (alpha and beta).
randomBetaStream :: MonadDES m
                    => Double
                    -- ^ the shape (alpha)
                    -> Double
                    -- ^ the shape (beta)
                    -> Stream m (Arrival Double)
                    -- ^ the stream of random events with the delays generated
{-# INLINABLE randomBetaStream #-}
randomBetaStream alpha beta =
  randomStream $
  randomBeta alpha beta >>= \x ->
  return (x, x)

-- | Return a new stream with random delays having the Weibull distribution by the specified
-- shape and scale.
randomWeibullStream :: MonadDES m
                       => Double
                       -- ^ shape
                       -> Double
                       -- ^ scale
                       -> Stream m (Arrival Double)
                       -- ^ the stream of random events with the delays generated
{-# INLINABLE randomWeibullStream #-}
randomWeibullStream alpha beta =
  randomStream $
  randomWeibull alpha beta >>= \x ->
  return (x, x)

-- | Return a new stream with random delays having the specified discrete distribution.
randomDiscreteStream :: MonadDES m
                        => DiscretePDF Double
                        -- ^ the discrete probability density function
                        -> Stream m (Arrival Double)
                        -- ^ the stream of random events with the delays generated
{-# INLINABLE randomDiscreteStream #-}
randomDiscreteStream dpdf =
  randomStream $
  randomDiscrete dpdf >>= \x ->
  return (x, x)
