
{-# LANGUAGE TypeFamilies, FlexibleInstances, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.IO.Generator
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- Here is defined a random number generator, where
-- the 'MonadIO'-based monad can be an instance of 'MonadGenerator'.
--
module Simulation.Aivika.IO.Generator () where

import Control.Monad
import Control.Monad.Trans

import System.Random

import Data.IORef

import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Template

instance (Functor m, MonadIO m, MonadTemplate m) => MonadGenerator m where

  {-# SPECIALISE instance MonadGenerator IO #-}

  data Generator m =
    Generator { generator01 :: m Double,
                -- ^ the generator of uniform numbers from 0 to 1
                generatorNormal01 :: m Double
                -- ^ the generator of normal numbers with mean 0 and variance 1
              }

  {-# INLINE generateUniform #-}
  generateUniform = generateUniform01 . generator01

  {-# INLINE generateUniformInt #-}
  generateUniformInt = generateUniformInt01 . generator01

  {-# INLINE generateTriangular #-}
  generateTriangular = generateTriangular01 . generator01

  {-# INLINE generateNormal #-}
  generateNormal = generateNormal01 . generatorNormal01

  {-# INLINE generateLogNormal #-}
  generateLogNormal = generateLogNormal01 . generatorNormal01

  {-# INLINE generateExponential #-}
  generateExponential = generateExponential01 . generator01

  {-# INLINE generateErlang #-}
  generateErlang = generateErlang01 . generator01

  {-# INLINE generatePoisson #-}
  generatePoisson = generatePoisson01 . generator01

  {-# INLINE generateBinomial #-}
  generateBinomial = generateBinomial01 . generator01

  {-# INLINE generateGamma #-}
  generateGamma g = generateGamma01 (generatorNormal01 g) (generator01 g)

  {-# INLINE generateBeta #-}
  generateBeta g = generateBeta01 (generatorNormal01 g) (generator01 g)

  {-# INLINE generateWeibull #-}
  generateWeibull = generateWeibull01 . generator01

  {-# INLINE generateDiscrete #-}
  generateDiscrete = generateDiscrete01 . generator01

  {-# INLINABLE newGenerator #-}
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

  {-# INLINABLE newRandomGenerator #-}
  newRandomGenerator g = 
    do r <- liftIO $ newIORef g
       let g01 = do g <- liftIO $ readIORef r
                    let (x, g') = random g
                    liftIO $ writeIORef r g'
                    return x
       newRandomGenerator01 g01

  {-# INLINABLE newRandomGenerator01 #-}
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
{-# INLINE generateUniform01 #-}
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
{-# INLINE generateUniformInt01 #-}
generateUniformInt01 g min max =
  do x <- g
     let min' = fromIntegral min
         max' = fromIntegral max
     return $ round (min' + x * (max' - min'))

-- | Generate the triangular random number by the specified minimum, median and maximum.
generateTriangular01 :: Monad m
                        => m Double
                        -- ^ the generator
                        -> Double
                        -- ^ minimum
                        -> Double
                        -- ^ median
                        -> Double
                        -- ^ maximum
                        -> m Double
{-# INLINE generateTriangular01 #-}
generateTriangular01 g min median max =
  do x <- g
     if x <= (median - min) / (max - min)
       then return $ min + sqrt ((median - min) * (max - min) * x)
       else return $ max - sqrt ((max - median) * (max - min) * (1 - x))

-- | Generate a normal random number by the specified generator, mean and variance.
generateNormal01 :: Monad m
                    => m Double
                    -- ^ normal random numbers with mean 0 and variance 1
                    -> Double
                    -- ^ mean
                    -> Double
                    -- ^ variance
                    -> m Double
{-# INLINE generateNormal01 #-}
generateNormal01 g mu nu =
  do x <- g
     return $ mu + nu * x

-- | Generate the lognormal random number derived from a normal distribution with
-- the specified generator, mean and variance.
generateLogNormal01 :: Monad m
                       => m Double
                       -- ^ normal random numbers with mean 0 and variance 1
                       -> Double
                       -- ^ mean
                       -> Double
                       -- ^ variance
                       -> m Double
{-# INLINE generateLogNormal01 #-}
generateLogNormal01 g mu nu =
  do x <- g
     return $ exp (mu + nu * x)

-- | Create a normal random number generator with mean 0 and variance 1
-- by the specified generator of uniform random numbers from 0 to 1.
newNormalGenerator01 :: MonadIO m
                        => m Double
                        -- ^ the generator
                        -> m (m Double)
{-# INLINABLE newNormalGenerator01 #-}
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
{-# INLINE generateExponential01 #-}
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
{-# INLINABLE generateErlang01 #-}
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
{-# INLINABLE generatePoisson01 #-}
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
{-# INLINABLE generateBinomial01 #-}
generateBinomial01 g prob trials = loop trials 0 where
  loop n acc
    | n < 0     = error "Negative number of trials: generateBinomial."
    | n == 0    = return acc
    | otherwise = do x <- g
                     if x <= prob
                       then loop (n - 1) (acc + 1)
                       else loop (n - 1) acc

-- | Generate a random number from the Gamma distribution using Marsaglia and Tsang method.
generateGamma01 :: Monad m
                   => m Double
                   -- ^ a normal random number ~ N (0,1)
                   -> m Double
                   -- ^ an uniform random number ~ U (0, 1)
                   -> Double
                   -- ^ the shape parameter (kappa) 
                   -> Double
                   -- ^ the scale parameter (theta)
                   -> m Double
{-# INLINABLE generateGamma01 #-}
generateGamma01 gn gu kappa theta
  | kappa <= 0 = error "The shape parameter (kappa) must be positive: generateGamma01"
  | kappa > 1  =
    let d = kappa - 1 / 3
        c = 1 / sqrt (9 * d)
        loop =
          do z <- gn
             if z <= - (1 / c)
               then loop
               else do let v = (1 + c * z) ** 3
                       u <- gu
                       if log u > 0.5 * z * z + d - d * v + d * log v
                         then loop
                         else return $ d * v * theta
    in loop
  | otherwise  =
    do x <- generateGamma01 gn gu (1 + kappa) theta
       u <- gu
       return $ x * u ** (1 / kappa)

-- | Generate a random number from the Beta distribution.
generateBeta01 :: Monad m
                  => m Double
                  -- ^ a normal random number ~ N (0, 1)
                  -> m Double
                  -- ^ an uniform random number ~ U (0, 1)
                  -> Double
                  -- ^ the shape parameter alpha
                  -> Double
                  -- ^ the shape parameter beta
                  -> m Double
{-# INLINABLE generateBeta01 #-}
generateBeta01 gn gu alpha beta =
  do g1 <- generateGamma01 gn gu alpha 1
     g2 <- generateGamma01 gn gu beta 1
     return $ g1 / (g1 + g2)

-- | Generate a random number from the Weibull distribution.
generateWeibull01 :: Monad m
                     => m Double
                     -- ^ an uniform random number ~ U (0, 1)
                     -> Double
                     -- ^ shape
                     -> Double
                     -- ^ scale
                     -> m Double
{-# INLINABLE generateWeibull01 #-}
generateWeibull01 g alpha beta =
  do x <- g
     return $ beta * (- log x) ** (1 / alpha)

-- | Generate a random value from the specified discrete distribution.
generateDiscrete01 :: Monad m
                      => m Double
                      -- ^ an uniform random number ~ U (0, 1)
                      -> DiscretePDF a
                      -- ^ a discrete probability density function
                      -> m a
{-# INLINABLE generateDiscrete01 #-}
generateDiscrete01 g []   = error "Empty PDF: generateDiscrete01"
generateDiscrete01 g dpdf =
  do x <- g
     let loop acc [(a, p)] = a
         loop acc ((a, p) : dpdf) =
           if x <= acc + p
           then a
           else loop (acc + p) dpdf
     return $ loop 0 dpdf
