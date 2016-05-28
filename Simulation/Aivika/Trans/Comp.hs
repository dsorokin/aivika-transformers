
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Trans.Comp
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- It defines a type class of monads based on which the simulation monads can be built.
--
module Simulation.Aivika.Trans.Comp
       (MonadComp(..),
        MonadCompTrans(..)) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Internal.Types

-- | A type class of monads based on which the simulation monads can be built. 
class (Monad m, MonadException m, MonadGenerator m) => MonadComp m

-- | A variant of the standard 'MonadTrans' type class with one difference:
-- the computation that will be lifted into another must be 'MonadComp' instead of
-- more general and less restricted 'Monad'.
class MonadCompTrans t m where

  -- | Lift the underlying computation into another within simulation.
  liftComp :: m a -> t m a
