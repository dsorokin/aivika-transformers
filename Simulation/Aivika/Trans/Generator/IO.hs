
{-# LANGUAGE TypeFamilies, FlexibleInstances, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.Trans.Generator.IO
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- Below is defined a random number generator.
--
module Simulation.Aivika.Trans.Generator.IO (Generator(..)) where

import Control.Monad
import Control.Monad.Trans

import System.Random

import Data.IORef

import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Template

instance (Functor m, MonadIO m, MonadTemplate m) => MonadGenerator m where

  data Generator m =
    Generator { generator01 :: m Double,
                -- ^ the generator of uniform numbers from 0 to 1
                generatorNormal01 :: m Double
                -- ^ the generator of normal numbers with mean 0 and variance 1
              }

  {-# SPECIALISE INLINE generateUniform :: Generator IO -> Double -> Double -> IO Double #-}
  generateUniform = generateUniform01 . generator01

  {-# SPECIALISE INLINE generateUniformInt :: Generator IO -> Int -> Int -> IO Int #-}
  generateUniformInt = generateUniformInt01 . generator01

  {-# SPECIALISE INLINE generateUniform :: Generator IO -> Double -> Double -> IO Double #-}
  generateNormal = generateNormal01 . generatorNormal01

  {-# SPECIALISE INLINE generateExponential :: Generator IO -> Double -> IO Double #-}
  generateExponential = generateExponential01 . generator01

  {-# SPECIALISE INLINE generateErlang :: Generator IO -> Double -> Int -> IO Double #-}
  generateErlang = generateErlang01 . generator01

  {-# SPECIALISE INLINE generatePoisson :: Generator IO -> Double -> IO Int #-}
  generatePoisson = generatePoisson01 . generator01

  {-# SPECIALISE INLINE generateBinomial :: Generator IO -> Double -> Int -> IO Int #-}
  generateBinomial = generateBinomial01 . generator01

  {-# SPECIALISE INLINE newGenerator :: GeneratorType IO -> IO (Generator IO) #-}
  newGenerator tp =
    case tp of
      SimpleGenerator ->
        liftIO newStdGen >>= newRandomGenerator
      SimpleGeneratorWithSeed x ->
        newRandomGenerator $ mkStdGen x
      CustomGenerator g ->
        g
      CustomGenerator01 g ->
        newRandomGenerator01 g

  {-# SPECIALISE INLINE newRandomGenerator :: RandomGen g => g -> IO (Generator IO) #-}
  newRandomGenerator g = 
    do r <- liftIO $ newIORef g
       let g01 = do g <- liftIO $ readIORef r
                    let (x, g') = random g
                    liftIO $ writeIORef r g'
                    return x
       newRandomGenerator01 g01

  {-# SPECIALISE INLINE newRandomGenerator01 :: IO Double -> IO (Generator IO) #-}
  newRandomGenerator01 g01 =
    do gNormal01 <- newNormalGenerator01 g01
       return Generator { generator01 = g01,
                          generatorNormal01 = gNormal01 }

-- | Generate an uniform random number with the specified minimum and maximum.
generateUniform01 :: Monad m
                     => m Double
                     -- ^ the generator
                     -> Double
                     -- ^ minimum
                     -> Double
                     -- ^ maximum
                     -> m Double
{-# SPECIALISE INLINE generateUniform01 :: IO Double -> Double -> Double -> IO Double #-}
generateUniform01 g min max =
  do x <- g
     return $ min + x * (max - min)

-- | Generate an uniform random number with the specified minimum and maximum.
generateUniformInt01 :: Monad m
                        => m Double
                        -- ^ the generator
                        -> Int
                        -- ^ minimum
                        -> Int
                        -- ^ maximum
                        -> m Int
{-# SPECIALISE INLINE generateUniformInt01 :: IO Double -> Int -> Int -> IO Int #-}
generateUniformInt01 g min max =
  do x <- g
     let min' = fromIntegral min
         max' = fromIntegral max
     return $ round (min' + x * (max' - min'))

-- | Generate a normal random number by the specified generator, mean and variance.
generateNormal01 :: Monad m
                    => m Double
                    -- ^ normal random numbers with mean 0 and variance 1
                    -> Double
                    -- ^ mean
                    -> Double
                    -- ^ variance
                    -> m Double
{-# SPECIALISE INLINE generateNormal01 :: IO Double -> Double -> Double -> IO Double #-}
generateNormal01 g mu nu =
  do x <- g
     return $ mu + nu * x

-- | Create a normal random number generator with mean 0 and variance 1
-- by the specified generator of uniform random numbers from 0 to 1.
newNormalGenerator01 :: MonadIO m
                        => m Double
                        -- ^ the generator
                        -> m (m Double)
{-# SPECIALISE INLINE newNormalGenerator01 :: IO Double -> IO (IO Double) #-}
newNormalGenerator01 g =
  do nextRef <- liftIO $ newIORef 0.0
     flagRef <- liftIO $ newIORef False
     xi1Ref  <- liftIO $ newIORef 0.0
     xi2Ref  <- liftIO $ newIORef 0.0
     psiRef  <- liftIO $ newIORef 0.0
     let loop =
           do psi <- liftIO $ readIORef psiRef
              if (psi >= 1.0) || (psi == 0.0)
                then do g1 <- g
                        g2 <- g
                        let xi1 = 2.0 * g1 - 1.0
                            xi2 = 2.0 * g2 - 1.0
                            psi = xi1 * xi1 + xi2 * xi2
                        liftIO $ writeIORef xi1Ref xi1
                        liftIO $ writeIORef xi2Ref xi2
                        liftIO $ writeIORef psiRef psi
                        loop
                else liftIO $ writeIORef psiRef $ sqrt (- 2.0 * log psi / psi)
     return $
       do flag <- liftIO $ readIORef flagRef
          if flag
            then do liftIO $ writeIORef flagRef False
                    liftIO $ readIORef nextRef
            else do liftIO $ writeIORef xi1Ref 0.0
                    liftIO $ writeIORef xi2Ref 0.0
                    liftIO $ writeIORef psiRef 0.0
                    loop
                    xi1 <- liftIO $ readIORef xi1Ref
                    xi2 <- liftIO $ readIORef xi2Ref
                    psi <- liftIO $ readIORef psiRef
                    liftIO $ writeIORef flagRef True
                    liftIO $ writeIORef nextRef $ xi2 * psi
                    return $ xi1 * psi

-- | Return the exponential random number with the specified mean.
generateExponential01 :: Monad m
                         => m Double
                         -- ^ the generator
                         -> Double
                         -- ^ the mean
                         -> m Double
{-# SPECIALISE INLINE generateExponential01 :: IO Double -> Double -> IO Double #-}
generateExponential01 g mu =
  do x <- g
     return (- log x * mu)

-- | Return the Erlang random number.
generateErlang01 :: Monad m
                    => m Double
                    -- ^ the generator
                    -> Double
                    -- ^ the scale
                    -> Int
                    -- ^ the shape
                    -> m Double
{-# SPECIALISE INLINE generateErlang01 :: IO Double -> Double -> Int -> IO Double #-}
generateErlang01 g beta m =
  do x <- loop m 1
     return (- log x * beta)
       where loop m acc
               | m < 0     = error "Negative shape: generateErlang."
               | m == 0    = return acc
               | otherwise = do x <- g
                                loop (m - 1) (x * acc)

-- | Generate the Poisson random number with the specified mean.
generatePoisson01 :: Monad m
                     => m Double
                     -- ^ the generator
                     -> Double
                     -- ^ the mean
                     -> m Int
{-# SPECIALISE INLINE generatePoisson01 :: IO Double -> Double -> IO Int #-}
generatePoisson01 g mu =
  do prob0 <- g
     let loop prob prod acc
           | prob <= prod = return acc
           | otherwise    = loop
                            (prob - prod)
                            (prod * mu / fromIntegral (acc + 1))
                            (acc + 1)
     loop prob0 (exp (- mu)) 0

-- | Generate a binomial random number with the specified probability and number of trials. 
generateBinomial01 :: Monad m
                      => m Double
                      -- ^ the generator
                      -> Double 
                      -- ^ the probability
                      -> Int
                      -- ^ the number of trials
                      -> m Int
{-# SPECIALISE INLINE generateBinomial01 :: IO Double -> Double -> Int -> IO Int #-}
generateBinomial01 g prob trials = loop trials 0 where
  loop n acc
    | n < 0     = error "Negative number of trials: generateBinomial."
    | n == 0    = return acc
    | otherwise = do x <- g
                     if x <= prob
                       then loop (n - 1) (acc + 1)
                       else loop (n - 1) acc
