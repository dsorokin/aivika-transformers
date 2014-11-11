
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Trans.Transform
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines something which is most close to the notion of
-- analogous circuit as an opposite to the digital one.
--
module Simulation.Aivika.Trans.Transform
       (-- * The Transform Arrow
        Transform(..),
        -- * Delaying the Transform
        delayTransform,
        -- * The Time Transform
        timeTransform,
        -- * Differential and Difference Equations
        integTransform,
        integTransformEither,
        sumTransform,
        sumTransformEither) where

import qualified Control.Category as C
import Control.Arrow
import Control.Monad
import Control.Monad.Fix

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Dynamics
import Simulation.Aivika.Trans.Dynamics.Memo
import Simulation.Aivika.Trans.Unboxed
import Simulation.Aivika.Trans.SystemDynamics

-- | It allows representing an analogous circuit as an opposite to
-- the digital one.
--
-- This is a transform of one time varying function to another usually
-- specified in the integration time points and then interpolated in
-- other time points with help of one of the memoization functions
-- like 'memo0Dynamics'.
--
newtype Transform m a b =
  Transform { runTransform :: Dynamics m a -> Simulation m (Dynamics m b)
              -- ^ Run the transform.
            }

instance MonadComp m => C.Category (Transform m) where

  id = Transform return
  
  (Transform g) . (Transform f) =
    Transform $ \a -> f a >>= g

instance MonadComp m => Arrow (Transform m) where

  arr f = Transform $ return . fmap f

  first (Transform f) =
    Transform $ \bd ->
    do (b, d) <- unzip0Dynamics bd
       c <- f b
       return $ liftM2 (,) c d 

  second (Transform f) =
    Transform $ \db ->
    do (d, b) <- unzip0Dynamics db
       c <- f b
       return $ liftM2 (,) d c

  (Transform f) *** (Transform g) =
    Transform $ \bb' ->
    do (b, b') <- unzip0Dynamics bb'
       c  <- f b
       c' <- g b'
       return $ liftM2 (,) c c'

  (Transform f) &&& (Transform g) =
    Transform $ \b ->
    do c  <- f b
       c' <- g b
       return $ liftM2 (,) c c'

instance (MonadComp m, MonadFix m) => ArrowLoop (Transform m) where

  loop (Transform f) =
    Transform $ \b ->
    mdo let bd = liftM2 (,) b d
        cd <- f bd
        (c, d) <- unzip0Dynamics cd
        return c

-- | A transform that returns the current modeling time.
timeTransform :: MonadComp m => Transform m a Double
timeTransform = Transform $ const $ return time

-- | Return a delayed transform by the specified lag time and initial value.
--
-- This is actually the 'delayI' function wrapped in the 'Transform' type. 
delayTransform :: MonadComp m
                  => Dynamics m Double     -- ^ the lag time
                  -> Dynamics m a       -- ^ the initial value
                  -> Transform m a a    -- ^ the delayed transform
delayTransform lagTime init =
  Transform $ \a -> delayI a lagTime init
  
-- | Return a transform that maps the derivative to an integral
-- by the specified initial value.
--
-- This is actually the 'integ' function wrapped in the 'Transform' type. 
integTransform :: (MonadComp m, MonadFix m)
                  => Dynamics m Double
                  -- ^ the initial value
                  -> Transform m Double Double
                  -- ^ map the derivative to an integral
integTransform init = Transform $ \diff -> integ diff init
  
-- | Like 'integTransform' but allows either setting a new 'Left' value of the integral,
-- or updating it by the specified 'Right' derivative.
integTransformEither :: (MonadComp m, MonadFix m)
                        => Dynamics m Double
                        -- ^ the initial value
                        -> Transform m (Either Double Double) Double
                        -- ^ map either a new 'Left' value or the 'Right' derivative to an integral
integTransformEither init = Transform $ \diff -> integEither diff init

-- | Return a transform that maps the difference to a sum
-- by the specified initial value.
--
-- This is actually the 'diffsum' function wrapped in the 'Transform' type. 
sumTransform :: (MonadComp m, MonadFix m, Num a, Unboxed m a)
                => Dynamics m a
                -- ^ the initial value
                -> Transform m a a
                -- ^ map the difference to a sum
sumTransform init = Transform $ \diff -> diffsum diff init

-- | Like 'sumTransform' but allows either setting a new 'Left' value of the sum,
-- or updating it by the specified 'Right' difference.
sumTransformEither :: (MonadComp m, MonadFix m, Num a, Unboxed m a)
                      => Dynamics m a
                      -- ^ the initial value
                      -> Transform m (Either a a) a
                      -- ^ map either a new 'Left' value or the 'Right' difference to a sum
sumTransformEither init = Transform $ \diff -> diffsumEither diff init
