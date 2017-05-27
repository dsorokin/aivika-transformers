
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances, RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Trans.Composite
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- It defines the 'Composite' monad transformer that allows constructing components which
-- can be then destroyed in case of need.
--
module Simulation.Aivika.Trans.Composite
       (-- * Composite Monad
        Composite,
        CompositeLift(..),
        runComposite,
        runComposite_,
        runCompositeInStartTime_,
        runCompositeInStopTime_,
        disposableComposite) where

import Data.Monoid

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Applicative

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Dynamics
import Simulation.Aivika.Trans.Event

-- | It represents a composite which can be then destroyed in case of need.
newtype Composite m a = Composite { runComposite :: DisposableEvent m -> Event m (a, DisposableEvent m)
                                    -- ^ Run the computation returning the result
                                    -- and some 'DisposableEvent' that being applied
                                    -- destroys the composite, for example, unsubscribes
                                    -- from signals or cancels the processes.
                                    --
                                  }

-- | Like 'runComposite' but retains the composite parts during the simulation.
runComposite_ :: Monad m => Composite m a -> Event m a
{-# INLINABLE runComposite_ #-}
runComposite_ m =
  do (a, _) <- runComposite m mempty
     return a

-- | Like 'runComposite_' but runs the computation in the start time.
runCompositeInStartTime_ :: MonadDES m => Composite m a -> Simulation m a
{-# INLINABLE runCompositeInStartTime_ #-}
runCompositeInStartTime_ = runEventInStartTime . runComposite_

-- | Like 'runComposite_' but runs the computation in the stop time.
runCompositeInStopTime_ :: MonadDES m => Composite m a -> Simulation m a
{-# INLINABLE runCompositeInStopTime_ #-}
runCompositeInStopTime_ = runEventInStopTime . runComposite_

-- | When destroying the composite, the specified action will be applied.
disposableComposite :: Monad m => DisposableEvent m -> Composite m ()
{-# INLINABLE disposableComposite #-}
disposableComposite h = Composite $ \h0 -> return ((), h0 <> h)

instance Monad m => Functor (Composite m) where

  {-# INLINE fmap #-}
  fmap f (Composite m) =
    Composite $ \h0 ->
    do (a, h) <- m h0
       return (f a, h)

instance Monad m => Applicative (Composite m) where

  {-# INLINE pure #-}
  pure = return

  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad m => Monad (Composite m) where

  {-# INLINE return #-}
  return a = Composite $ \h0 -> return (a, h0)

  {-# INLINE (>>=) #-}
  (Composite m) >>= k =
    Composite $ \h0 ->
    do (a, h) <- m h0
       let Composite m' = k a
       (b, h') <- m' h
       return (b, h')

instance (Monad m, MonadIO (Event m)) => MonadIO (Composite m) where

  {-# INLINE liftIO #-}
  liftIO m =
    Composite $ \h0 ->
    do a <- liftIO m
       return (a, h0)

instance (Monad m, MonadFix (Event m)) => MonadFix (Composite m) where

  {-# INLINABLE mfix #-}
  mfix f =
    Composite $ \h0 ->
    do rec (a, h) <- runComposite (f a) h0
       return (a, h)

instance MonadTrans Composite where

  {-# INLINE lift #-}
  lift m =
    Composite $ \h0 ->
    do a <- lift m
       return (a, h0)

instance Monad m => MonadCompTrans Composite m where

  {-# INLINE liftComp #-}
  liftComp m =
    Composite $ \h0 ->
    do a <- liftComp m
       return (a, h0)

instance Monad m => ParameterLift Composite m where

  {-# INLINE liftParameter #-}
  liftParameter m =
    Composite $ \h0 ->
    do a <- liftParameter m
       return (a, h0)

instance Monad m => SimulationLift Composite m where

  {-# INLINE liftSimulation #-}
  liftSimulation m =
    Composite $ \h0 ->
    do a <- liftSimulation m
       return (a, h0)

instance Monad m => DynamicsLift Composite m where

  {-# INLINE liftDynamics #-}
  liftDynamics m =
    Composite $ \h0 ->
    do a <- liftDynamics m
       return (a, h0)

instance Monad m => EventLift Composite m where

  {-# INLINE liftEvent #-}
  liftEvent m =
    Composite $ \h0 ->
    do a <- liftEvent m
       return (a, h0)

-- | A type class to lift the 'Composite' computation to other computations.
class CompositeLift t m where
  
  -- | Lift the specified 'Composite' computation to another computation.
  liftComposite :: Composite m a -> t m a

instance Monad m => CompositeLift Composite m where

  {-# INLINE liftComposite #-}
  liftComposite = id
