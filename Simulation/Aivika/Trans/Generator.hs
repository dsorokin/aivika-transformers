
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Trans.Generator
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- Below is defined a random number generator.
--
module Simulation.Aivika.Trans.Generator 
       (MonadGenerator(..),
        GeneratorType(..)) where

import System.Random

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

  -- | Generate a normal random number
  -- with the specified mean and deviation.
  generateNormal :: Generator m -> Double -> Double -> m Double

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
  
  -- | Create a new random number generator.
  newGenerator :: GeneratorType m -> m (Generator m)

  -- | Create a new random generator by the specified standard generator.
  newRandomGenerator :: RandomGen g => g -> m (Generator m)

  -- | Create a new random generator by the specified uniform generator of numbers from 0 to 1.
  newRandomGenerator01 :: m Double -> m (Generator m)

-- | Defines a type of the random number generator.
data GeneratorType m = SimpleGenerator
                       -- ^ The simple random number generator.
                     | SimpleGeneratorWithSeed Int
                       -- ^ The simple random number generator with the specified seed.
                     | CustomGenerator (m (Generator m))
                       -- ^ The custom random number generator.
                     | CustomGenerator01 (m Double)
                       -- ^ The custom random number generator by the specified uniform
                       -- generator of numbers from 0 to 1.
