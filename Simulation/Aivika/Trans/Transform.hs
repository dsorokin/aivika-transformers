
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Trans.Transform
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
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

import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Dynamics
import qualified Simulation.Aivika.Trans.Dynamics.Memo as M
import qualified Simulation.Aivika.Trans.Dynamics.Memo.Unboxed as MU
import Simulation.Aivika.Trans.SystemDynamics
import Simulation.Aivika.Trans.SD

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

instance Monad m => C.Category (Transform m) where

  {-# INLINE id #-}
  id = Transform return
  
  {-# INLINE (.) #-}
  (Transform g) . (Transform f) =
    Transform $ \a -> f a >>= g

instance MonadSD m => Arrow (Transform m) where

  {-# INLINE arr #-}
  arr f = Transform $ return . fmap f

  {-# INLINABLE first #-}
  first (Transform f) =
    Transform $ \bd ->
    do (b, d) <- M.unzip0Dynamics bd
       c <- f b
       return $ liftM2 (,) c d 

  {-# INLINABLE second #-}
  second (Transform f) =
    Transform $ \db ->
    do (d, b) <- M.unzip0Dynamics db
       c <- f b
       return $ liftM2 (,) d c

  {-# INLINABLE (***) #-}
  (Transform f) *** (Transform g) =
    Transform $ \bb' ->
    do (b, b') <- M.unzip0Dynamics bb'
       c  <- f b
       c' <- g b'
       return $ liftM2 (,) c c'

  {-# INLINABLE (&&&) #-}
  (Transform f) &&& (Transform g) =
    Transform $ \b ->
    do c  <- f b
       c' <- g b
       return $ liftM2 (,) c c'

-- instance (MonadSD m, MonadFix m) => ArrowLoop (Transform m) where
-- 
--   {-# INLINABLE loop #-}
--   loop (Transform f) =
--     Transform $ \b ->
--     mdo let bd = liftM2 (,) b d
--         cd <- f bd
--         (c, d) <- M.unzip0Dynamics cd
--         return c

-- | A transform that returns the current modeling time.
timeTransform :: Monad m => Transform m a Double
{-# INLINE timeTransform #-}
timeTransform = Transform $ const $ return time

-- | Return a delayed transform by the specified lag time and initial value.
--
-- This is actually the 'delayI' function wrapped in the 'Transform' type. 
delayTransform :: MonadSD m
                  => Dynamics m Double     -- ^ the lag time
                  -> Dynamics m a       -- ^ the initial value
                  -> Transform m a a    -- ^ the delayed transform
{-# INLINE delayTransform #-}
delayTransform lagTime init =
  Transform $ \a -> delayI a lagTime init
  
-- | Return a transform that maps the derivative to an integral
-- by the specified initial value.
--
-- This is actually the 'integ' function wrapped in the 'Transform' type. 
integTransform :: (MonadSD m, MonadFix m)
                  => Dynamics m Double
                  -- ^ the initial value
                  -> Transform m Double Double
                  -- ^ map the derivative to an integral
{-# INLINE integTransform #-}
integTransform init = Transform $ \diff -> integ diff init
  
-- | Like 'integTransform' but allows either setting a new 'Left' value of the integral,
-- or updating it by the specified 'Right' derivative.
integTransformEither :: (MonadSD m, MonadFix m)
                        => Dynamics m Double
                        -- ^ the initial value
                        -> Transform m (Either Double Double) Double
                        -- ^ map either a new 'Left' value or the 'Right' derivative to an integral
{-# INLINE integTransformEither #-}
integTransformEither init = Transform $ \diff -> integEither diff init

-- | Return a transform that maps the difference to a sum
-- by the specified initial value.
--
-- This is actually the 'diffsum' function wrapped in the 'Transform' type. 
sumTransform :: (MonadSD m, MonadFix m, Num a, MU.MonadMemo m a)
                => Dynamics m a
                -- ^ the initial value
                -> Transform m a a
                -- ^ map the difference to a sum
{-# INLINE sumTransform #-}
sumTransform init = Transform $ \diff -> diffsum diff init

-- | Like 'sumTransform' but allows either setting a new 'Left' value of the sum,
-- or updating it by the specified 'Right' difference.
sumTransformEither :: (MonadSD m, MonadFix m, Num a, MU.MonadMemo m a)
                      => Dynamics m a
                      -- ^ the initial value
                      -> Transform m (Either a a) a
                      -- ^ map either a new 'Left' value or the 'Right' difference to a sum
{-# INLINE sumTransformEither #-}
sumTransformEither init = Transform $ \diff -> diffsumEither diff init
