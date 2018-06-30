
{-# LANGUAGE RecursiveDo, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, RankNTypes #-}

-- |
-- Module     : Simulation.Aivika.Trans.Internal.Simulation
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines the 'Simulation' monad transformer that represents a computation
-- within the simulation run.
-- 
module Simulation.Aivika.Trans.Internal.Simulation
       (-- * Simulation
        Simulation(..),
        SimulationLift(..),
        invokeSimulation,
        runSimulation,
        runSimulations,
        runSimulationByIndex,
        -- * Error Handling
        catchSimulation,
        finallySimulation,
        throwSimulation,
        -- * Exceptions
        SimulationException(..),
        SimulationAbort(..),
        SimulationRetry(..)) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import qualified Control.Monad.Catch as MC
import Control.Applicative

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Internal.Types
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter

import Simulation.Aivika.Simulation (SimulationException(..), SimulationAbort(..), SimulationRetry(..))

instance Monad m => Monad (Simulation m) where

  {-# INLINE return #-}
  return a = Simulation $ \r -> return a

  {-# INLINE (>>=) #-}
  (Simulation m) >>= k =
    Simulation $ \r -> 
    do a <- m r
       let Simulation m' = k a
       m' r

-- | Run the simulation using the specified specs.
runSimulation :: MonadDES m => Simulation m a -> Specs m -> m a
{-# INLINABLE runSimulation #-}
runSimulation (Simulation m) sc =
  do q <- newEventQueue sc
     g <- newGenerator $ spcGeneratorType sc
     m Run { runSpecs = sc,
             runIndex = 1,
             runCount = 1,
             runEventQueue = q,
             runGenerator = g }

-- | Run the simulation by the specified specs and run index in series.
runSimulationByIndex :: MonadDES m
                        => Simulation m a
                        -- ^ the simulation model
                        -> Specs m
                        -- ^ the simulation specs
                        -> Int
                        -- ^ the number of runs in series
                        -> Int
                        -- ^ the index of the current run (started from 1)
                        -> m a
{-# INLINABLE runSimulationByIndex #-}
runSimulationByIndex (Simulation m) sc runs index =
  do q <- newEventQueue sc
     g <- newGenerator $ spcGeneratorType sc
     m Run { runSpecs = sc,
             runIndex = index,
             runCount = runs,
             runEventQueue = q,
             runGenerator = g }

-- | Run the given number of simulations using the specified specs, 
--   where each simulation is distinguished by its index 'simulationIndex'.
runSimulations :: MonadDES m => Simulation m a -> Specs m -> Int -> [m a]
{-# INLINABLE runSimulations #-}
runSimulations (Simulation m) sc runs = map f [1 .. runs]
  where f i = do q <- newEventQueue sc
                 g <- newGenerator $ spcGeneratorType sc
                 m Run { runSpecs = sc,
                         runIndex = i,
                         runCount = runs,
                         runEventQueue = q,
                         runGenerator = g }

instance Functor m => Functor (Simulation m) where
  
  {-# INLINE fmap #-}
  fmap f (Simulation x) = Simulation $ \r -> fmap f $ x r

instance Applicative m => Applicative (Simulation m) where
  
  {-# INLINE pure #-}
  pure = Simulation . const . pure
  
  {-# INLINE (<*>) #-}
  (Simulation x) <*> (Simulation y) = Simulation $ \r -> x r <*> y r

liftMS :: Monad m => (a -> b) -> Simulation m a -> Simulation m b
{-# INLINE liftMS #-}
liftMS f (Simulation x) =
  Simulation $ \r -> do { a <- x r; return $ f a }

instance MonadTrans Simulation where

  {-# INLINE lift #-}
  lift = Simulation . const

instance Monad m => MonadCompTrans Simulation m where

  {-# INLINE liftComp #-}
  liftComp = Simulation . const

instance MonadIO m => MonadIO (Simulation m) where
  
  {-# INLINE liftIO #-}
  liftIO = Simulation . const . liftIO

-- | A type class to lift the simulation computations into other computations.
class SimulationLift t m where
  
  -- | Lift the specified 'Simulation' computation into another computation.
  liftSimulation :: Simulation m a -> t m a

instance Monad m => SimulationLift Simulation m where
  
  {-# INLINE liftSimulation #-}
  liftSimulation = id

instance Monad m => ParameterLift Simulation m where

  {-# INLINE liftParameter #-}
  liftParameter (Parameter x) = Simulation x
    
-- | Exception handling within 'Simulation' computations.
catchSimulation :: (MonadException m, Exception e) => Simulation m a -> (e -> Simulation m a) -> Simulation m a
{-# INLINABLE catchSimulation #-}
catchSimulation (Simulation m) h =
  Simulation $ \r -> 
  catchComp (m r) $ \e ->
  let Simulation m' = h e in m' r
                           
-- | A computation with finalization part like the 'finally' function.
finallySimulation :: MonadException m => Simulation m a -> Simulation m b -> Simulation m a
{-# INLINABLE finallySimulation #-}
finallySimulation (Simulation m) (Simulation m') =
  Simulation $ \r ->
  finallyComp (m r) (m' r)

-- | Like the standard 'throw' function.
throwSimulation :: (MonadException m, Exception e) => e -> Simulation m a
{-# INLINABLE throwSimulation #-}
throwSimulation e =
  Simulation $ \r ->
  throwComp e

-- | Runs an action with asynchronous exceptions disabled.
maskSimulation :: MC.MonadMask m => ((forall a. Simulation m a -> Simulation m a) -> Simulation m b) -> Simulation m b
{-# INLINABLE maskSimulation #-}
maskSimulation a =
  Simulation $ \r ->
  MC.mask $ \u ->
  invokeSimulation r (a $ q u)
  where q u (Simulation b) = Simulation (u . b)

-- | Like 'maskSimulation', but the masked computation is not interruptible.
uninterruptibleMaskSimulation :: MC.MonadMask m => ((forall a. Simulation m a -> Simulation m a) -> Simulation m b) -> Simulation m b
{-# INLINABLE uninterruptibleMaskSimulation #-}
uninterruptibleMaskSimulation a =
  Simulation $ \r ->
  MC.uninterruptibleMask $ \u ->
  invokeSimulation r (a $ q u)
  where q u (Simulation b) = Simulation (u . b)

-- | An implementation of 'generalBracket'.
generalBracketSimulation :: MC.MonadMask m
                            => Simulation m a
                            -> (a -> MC.ExitCase b -> Simulation m c)
                            -> (a -> Simulation m b)
                            -> Simulation m (b, c)
{-# INLINABLE generalBracketSimulation #-}
generalBracketSimulation acquire release use =
  Simulation $ \r -> do
    MC.generalBracket
      (invokeSimulation r acquire)
      (\resource e -> invokeSimulation r $ release resource e)
      (\resource -> invokeSimulation r $ use resource)

instance MonadFix m => MonadFix (Simulation m) where

  {-# INLINE mfix #-}
  mfix f = 
    Simulation $ \r ->
    do { rec { a <- invokeSimulation r (f a) }; return a }

instance MonadException m => MC.MonadThrow (Simulation m) where

  {-# INLINE throwM #-}
  throwM = throwSimulation

instance MonadException m => MC.MonadCatch (Simulation m) where

  {-# INLINE catch #-}
  catch = catchSimulation

instance (MonadException m, MC.MonadMask m) => MC.MonadMask (Simulation m) where

  {-# INLINE mask #-}
  mask = maskSimulation
  
  {-# INLINE uninterruptibleMask #-}
  uninterruptibleMask = uninterruptibleMaskSimulation
  
  {-# INLINE generalBracket #-}
  generalBracket = generalBracketSimulation
