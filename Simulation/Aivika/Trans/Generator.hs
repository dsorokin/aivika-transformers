
{-# LANGUAGE TypeFamilies, RankNTypes #-}

-- |
-- Module     : Simulation.Aivika.Trans.Generator
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- Below is defined a random number generator.
--
module Simulation.Aivika.Trans.Generator 
       (MonadGenerator(..),
        GeneratorType(..),
        DiscretePDF(..)) where

import System.Random
import Data.Word

import Simulation.Aivika.Generator (DiscretePDF)

-- | Defines a monad whithin which computation the random number generator can work.
class (Functor m, Monad m) => MonadGenerator m where

  -- | Defines a random number generator.
  data Generator m :: *

  -- | Generate an uniform random number
  -- with the specified minimum and maximum.
  generateUniform :: Generator m -> Double -> Double -> m Double
  
  -- | Generate an uniform integer random number
  -- with the specified minimum and maximum.
  generateUniformInt :: Generator m -> Int -> Int -> m Int
  
  -- | Generate a triangular random number
  -- by the specified minimum, median and maximum.
  generateTriangular :: Generator m -> Double -> Double -> Double -> m Double

  -- | Generate a normal random number
  -- with the specified mean and deviation.
  generateNormal :: Generator m -> Double -> Double -> m Double
  
  -- | Generate a random number from the lognormal distribution derived
  -- from a normal distribution with the specified mean and deviation.
  generateLogNormal :: Generator m -> Double -> Double -> m Double

  -- | Generate a random number distributed exponentially
  -- with the specified mean (the reciprocal of the rate).
  generateExponential :: Generator m -> Double -> m Double

  -- | Generate the Erlang random number
  -- with the specified scale (the reciprocal of the rate)
  -- and integer shape.
  generateErlang :: Generator m -> Double -> Int -> m Double
  
  -- | Generate the Poisson random number with the specified mean.
  generatePoisson :: Generator m -> Double -> m Int

  -- | Generate the binomial random number
  -- with the specified probability and number of trials.
  generateBinomial :: Generator m -> Double -> Int -> m Int

  -- | Generate a random number from the Gamma distribution with
  -- the specified shape (kappa) and scale (theta, a reciprocal of the rate).
  --
  -- The probability density for the Gamma distribution is
  --
  -- @f x = x ** (kappa - 1) * exp (- x \/ theta) \/ theta ** kappa * Gamma kappa@
  generateGamma :: Generator m -> Double -> Double -> m Double

  -- | Generate a random number from the Beta distribution by
  -- the specified shape parameters (alpha and beta).
  --
  -- The probability density for the Beta distribution is
  --
  -- @f x = x ** (alpha - 1) * (1 - x) ** (beta - 1) \/ B alpha beta@
  generateBeta :: Generator m -> Double -> Double -> m Double

  -- | Generate a random number from the Weibull distribution by
  -- the specified shape and scale.
  generateWeibull :: Generator m -> Double -> Double -> m Double

  -- | Generate a random value from the specified discrete distribution.
  generateDiscrete :: forall a. Generator m -> DiscretePDF a -> m a

  -- | Generate a sequence number which can be considered quite unique.
  generateSequenceNo :: Generator m -> m Int
              
  -- | Create a new random number generator.
  newGenerator :: GeneratorType m -> m (Generator m)

  -- | Create a new random generator by the specified standard generator.
  newRandomGenerator :: RandomGen g => g -> m (Generator m)

  -- | Create a new random generator by the specified uniform generator of numbers from 0 to 1.
  newRandomGenerator01 :: m Double -> m (Generator m)

-- | Defines a type of the random number generator.
data GeneratorType m = SimpleGenerator
                       -- ^ The simple random number generator.
                     | SimpleGeneratorWithSeed Word32
                       -- ^ The simple random number generator with the specified seed.
                     | CustomGenerator (m (Generator m))
                       -- ^ The custom random number generator.
                     | CustomGenerator01 (m Double)
                       -- ^ The custom random number generator by the specified uniform
                       -- generator of numbers from 0 to 1.
