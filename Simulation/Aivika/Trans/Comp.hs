
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.Comp
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It defines a type class of monads based on which the simulation monads can be built.
--
module Simulation.Aivika.Trans.Comp
       (ProtoMonadComp(..),
        MonadComp(..),
        MonadCompTrans(..)) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.ProtoArray
import Simulation.Aivika.Trans.Unboxed
import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Internal.Types

-- | A prototype of the type class of monads based on which the simulation monads can be built. 
class (Monad m,
       MonadException m,
       SessionMonad m,
       ProtoRefMonad m,
       ProtoArrayMonad m,
       Unboxed m Double,
       Unboxed m Float,
       Unboxed m Int,
       GeneratorMonad m) => ProtoMonadComp m

-- | Such a prototype monad that allows enqueueing events.
class (ProtoMonadComp m, EventQueueing m) => MonadComp m

-- | A variant of the standard 'MonadTrans' type class with one difference:
-- the computation that will be lifted into another must be 'MonadComp' instead of
-- more general and less restricted 'Monad'.
class MonadCompTrans t m where

  -- | Lift the underlying computation into another within simulation.
  liftComp :: m a -> t m a

instance ProtoMonadComp IO
