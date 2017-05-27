
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Trans.Ref.Base.Strict
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines a plain and more fast version of an updatable and strict reference
-- that depends on the event queue but that doesn't supply with the signal notification.
--
module Simulation.Aivika.Trans.Ref.Base.Strict
       (MonadRef(..),
        MonadRef0(..)) where

import Data.IORef

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Internal.Types
import Simulation.Aivika.Trans.Observable

-- | A monad within which we can create mutable references.
class Monad m => MonadRef m where

  -- | The 'Ref' type represents a mutable variable similar to the 'IORef' variable 
  -- but only dependent on the event queue, which allows synchronizing the reference
  -- with the model explicitly through the 'Event' monad.
  data Ref m a

  -- | Create a new reference.
  newRef :: a -> Simulation m (Ref m a)
     
  -- | Read the value of a reference.
  readRef :: Ref m a -> Event m a

  -- | Write a new value into the reference.
  writeRef :: Ref m a -> a -> Event m ()

  -- | Mutate the contents of the reference.
  modifyRef :: Ref m a -> (a -> a) -> Event m ()

  -- | Compare two references for equality.
  equalRef :: Ref m a -> Ref m a -> Bool

instance MonadRef m => Eq (Ref m a) where

  {-# INLINE (==) #-}
  (==) = equalRef

-- | A kind of reference that can be created within more low level computation than 'Simulation'.
class MonadRef m => MonadRef0 m where

  -- | Create a new reference within more low level computation than 'Simulation'.
  newRef0 :: a -> m (Ref m a)

instance (Monad m, MonadRef m) => Observable (Ref m) (Event m) where

  {-# INLINE readObservable #-}
  readObservable = readRef
