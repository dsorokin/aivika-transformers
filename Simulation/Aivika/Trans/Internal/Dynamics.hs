
{-# LANGUAGE RecursiveDo, MultiParamTypeClasses, FlexibleInstances #-}

-- |
-- Module     : Simulation.Aivika.Trans.Internal.Dynamics
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines the 'Dynamics' monad transformer representing a time varying polymorphic function. 
--
module Simulation.Aivika.Trans.Internal.Dynamics
       (-- * Dynamics
        Dynamics(..),
        DynamicsLift(..),
        invokeDynamics,
        runDynamicsInStartTime,
        runDynamicsInStopTime,
        runDynamicsInIntegTimes,
        runDynamicsInTime,
        runDynamicsInTimes,
        -- * Error Handling
        catchDynamics,
        finallyDynamics,
        throwDynamics,
        -- * Simulation Time
        time,
        isTimeInteg,
        integIteration,
        integPhase,
        -- * Debugging
        traceDynamics) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Applicative

import Debug.Trace (trace)

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Types
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation

instance Monad m => Monad (Dynamics m) where

  {-# INLINE return #-}
  return a = Dynamics $ \p -> return a

  {-# INLINE (>>=) #-}
  (Dynamics m) >>= k =
    Dynamics $ \p -> 
    do a <- m p
       let Dynamics m' = k a
       m' p

-- | Run the 'Dynamics' computation in the initial time point.
runDynamicsInStartTime :: Dynamics m a -> Simulation m a
runDynamicsInStartTime (Dynamics m) =
  Simulation $ m . integStartPoint

-- | Run the 'Dynamics' computation in the final time point.
runDynamicsInStopTime :: Dynamics m a -> Simulation m a
runDynamicsInStopTime (Dynamics m) =
  Simulation $ m . integStopPoint

-- | Run the 'Dynamics' computation in all integration time points.
runDynamicsInIntegTimes :: Monad m => Dynamics m a -> Simulation m [m a]
runDynamicsInIntegTimes (Dynamics m) =
  Simulation $ return . map m . integPoints

-- | Run the 'Dynamics' computation in the specified time point.
runDynamicsInTime :: Double -> Dynamics m a -> Simulation m a
runDynamicsInTime t (Dynamics m) =
  Simulation $ \r -> m $ pointAt r t

-- | Run the 'Dynamics' computation in the specified time points.
runDynamicsInTimes :: Monad m => [Double] -> Dynamics m a -> Simulation m [m a]
runDynamicsInTimes ts (Dynamics m) =
  Simulation $ \r -> return $ map (m . pointAt r) ts 

instance Functor m => Functor (Dynamics m) where
  
  {-# INLINE fmap #-}
  fmap f (Dynamics x) = Dynamics $ \p -> fmap f $ x p

instance Applicative m => Applicative (Dynamics m) where
  
  {-# INLINE pure #-}
  pure = Dynamics . const . pure
  
  {-# INLINE (<*>) #-}
  (Dynamics x) <*> (Dynamics y) = Dynamics $ \p -> x p <*> y p

liftMD :: Monad m => (a -> b) -> Dynamics m a -> Dynamics m b
{-# INLINE liftMD #-}
liftMD f (Dynamics x) =
  Dynamics $ \p -> do { a <- x p; return $ f a }

liftM2D :: Monad m => (a -> b -> c) -> Dynamics m a -> Dynamics m b -> Dynamics m c
{-# INLINE liftM2D #-}
liftM2D f (Dynamics x) (Dynamics y) =
  Dynamics $ \p -> do { a <- x p; b <- y p; return $ f a b }

instance (Num a, Monad m) => Num (Dynamics m a) where

  {-# INLINE (+) #-}
  x + y = liftM2D (+) x y

  {-# INLINE (-) #-}
  x - y = liftM2D (-) x y

  {-# INLINE (*) #-}
  x * y = liftM2D (*) x y

  {-# INLINE negate #-}
  negate = liftMD negate

  {-# INLINE abs #-}
  abs = liftMD abs

  {-# INLINE signum #-}
  signum = liftMD signum

  {-# INLINE fromInteger #-}
  fromInteger i = return $ fromInteger i

instance (Fractional a, Monad m) => Fractional (Dynamics m a) where

  {-# INLINE (/) #-}
  x / y = liftM2D (/) x y

  {-# INLINE recip #-}
  recip = liftMD recip

  {-# INLINE fromRational #-}
  fromRational t = return $ fromRational t

instance (Floating a, Monad m) => Floating (Dynamics m a) where

  {-# INLINE pi #-}
  pi = return pi

  {-# INLINE exp #-}
  exp = liftMD exp

  {-# INLINE log #-}
  log = liftMD log

  {-# INLINE sqrt #-}
  sqrt = liftMD sqrt

  {-# INLINE (**) #-}
  x ** y = liftM2D (**) x y

  {-# INLINE sin #-}
  sin = liftMD sin

  {-# INLINE cos #-}
  cos = liftMD cos

  {-# INLINE tan #-}
  tan = liftMD tan

  {-# INLINE asin #-}
  asin = liftMD asin

  {-# INLINE acos #-}
  acos = liftMD acos

  {-# INLINE atan #-}
  atan = liftMD atan

  {-# INLINE sinh #-}
  sinh = liftMD sinh

  {-# INLINE cosh #-}
  cosh = liftMD cosh

  {-# INLINE tanh #-}
  tanh = liftMD tanh

  {-# INLINE asinh #-}
  asinh = liftMD asinh

  {-# INLINE acosh #-}
  acosh = liftMD acosh

  {-# INLINE atanh #-}
  atanh = liftMD atanh

instance MonadTrans Dynamics where

  {-# INLINE lift #-}
  lift = Dynamics . const

instance MonadIO m => MonadIO (Dynamics m) where
  
  {-# INLINE liftIO #-}
  liftIO = Dynamics . const . liftIO

instance Monad m => MonadCompTrans Dynamics m where

  {-# INLINE liftComp #-}
  liftComp = Dynamics . const

-- | A type class to lift the 'Dynamics' computations into other computations.
class DynamicsLift t m where
  
  -- | Lift the specified 'Dynamics' computation into another computation.
  liftDynamics :: Dynamics m a -> t m a

instance Monad m => DynamicsLift Dynamics m where
  
  {-# INLINE liftDynamics #-}
  liftDynamics = id

instance Monad m => SimulationLift Dynamics m where

  {-# INLINE liftSimulation #-}
  liftSimulation (Simulation x) = Dynamics $ x . pointRun 

instance Monad m => ParameterLift Dynamics m where

  {-# INLINE liftParameter #-}
  liftParameter (Parameter x) = Dynamics $ x . pointRun
  
-- | Exception handling within 'Dynamics' computations.
catchDynamics :: (MonadComp m, Exception e) => Dynamics m a -> (e -> Dynamics m a) -> Dynamics m a
catchDynamics (Dynamics m) h =
  Dynamics $ \p -> 
  catchComp (m p) $ \e ->
  let Dynamics m' = h e in m' p
                           
-- | A computation with finalization part like the 'finally' function.
finallyDynamics :: MonadComp m => Dynamics m a -> Dynamics m b -> Dynamics m a
finallyDynamics (Dynamics m) (Dynamics m') =
  Dynamics $ \p ->
  finallyComp (m p) (m' p)

-- | Like the standard 'throw' function.
throwDynamics :: (MonadComp m, Exception e) => e -> Dynamics m a
throwDynamics = throw

instance MonadFix m => MonadFix (Dynamics m) where

  {-# INLINE mfix #-}
  mfix f = 
    Dynamics $ \p ->
    do { rec { a <- invokeDynamics p (f a) }; return a }

-- | Computation that returns the current simulation time.
time :: Monad m => Dynamics m Double
{-# INLINE time #-}
time = Dynamics $ return . pointTime 

-- | Whether the current time is an integration time.
isTimeInteg :: Monad m => Dynamics m Bool
{-# INLINE isTimeInteg #-}
isTimeInteg = Dynamics $ \p -> return $ pointPhase p >= 0

-- | Return the integration iteration closest to the current simulation time.
integIteration :: Monad m => Dynamics m Int
{-# INLINE integIteration #-}
integIteration = Dynamics $ return . pointIteration

-- | Return the integration phase for the current simulation time.
-- It is @(-1)@ for non-integration time points.
integPhase :: Monad m => Dynamics m Int
{-# INLINE integPhase #-}
integPhase = Dynamics $ return . pointPhase

-- | Show the debug message with the current simulation time.
traceDynamics :: Monad m => String -> Dynamics m a -> Dynamics m a
traceDynamics message m =
  Dynamics $ \p ->
  trace ("t = " ++ show (pointTime p) ++ ": " ++ message) $
  invokeDynamics p m
