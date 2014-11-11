
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Trans.Ref.Base
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines a plain and more fast version of an updatable reference
-- that depends on the event queue but that doesn't supply with the signal notification.
--
module Simulation.Aivika.Trans.Ref.Base
       (MonadRef(..)) where

import Data.IORef

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Internal.Types

-- | A monad within which we can create mutable references.
class Monad m => MonadRef m where

  -- | The 'ProtoRef' type represents a mutable variable similar to the 'IORef' variable 
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
