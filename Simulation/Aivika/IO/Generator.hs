
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.IO.Generator
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- Here is defined a random number generator, where
-- the 'IO' monad is an instance of 'MonadGenerator'.
--
module Simulation.Aivika.IO.Generator () where

import System.Random
import qualified System.Random.MWC as MWC

import Control.Monad
import Control.Monad.Trans

import Data.IORef
import Data.Vector
import Data.Functor

import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Generator.Primitive

instance MonadGenerator IO where
-- instance (Functor m, Monad m, MonadIO m, MonadTemplate m) => MonadGenerator m where

  {-# SPECIALISE instance MonadGenerator IO #-}

  data Generator IO =
    Generator { generator01 :: IO Double,
                -- ^ the generator of uniform numbers from 0 to 1
                generatorNormal01 :: IO Double,
                -- ^ the generator of normal numbers with mean 0 and variance 1
                generatorSequenceNo :: IO Int
                -- ^ the generator of sequence numbers
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

  {-# INLINE generateSequenceNo #-}
  generateSequenceNo = generatorSequenceNo 

  {-# INLINABLE newGenerator #-}
  newGenerator tp =
    case tp of
      SimpleGenerator ->
        MWC.uniform <$> MWC.createSystemRandom >>= newRandomGenerator01
      SimpleGeneratorWithSeed x ->
        MWC.uniform <$> MWC.initialize (singleton x) >>= newRandomGenerator01
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
       gSeqNoRef <- newIORef 0
       let gSeqNo = do { x <- readIORef gSeqNoRef; modifyIORef' gSeqNoRef (+1); return x }
       return Generator { generator01 = g01,
                          generatorNormal01 = gNormal01,
                          generatorSequenceNo = gSeqNo }

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
