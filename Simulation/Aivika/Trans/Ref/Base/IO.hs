
{-# LANGUAGE TypeFamilies, FlexibleInstances, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.Trans.Ref.Base.IO
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The 'MonadIO' based monad is an instance of 'MonadRef'.
--
module Simulation.Aivika.Trans.Ref.Base.IO where

import Data.IORef

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Internal.Types
import Simulation.Aivika.Trans.Ref.Base

-- | The 'MonadIO' based monad is an instance of 'MonadRef'.
instance MonadIO m => MonadRef m where

  -- | A type safe wrapper for the 'IORef' reference.
  newtype Ref m a = Ref { refValue :: IORef a }

  newRef a =
    Simulation $ \r ->
    do x <- liftIO $ newIORef a
       return Ref { refValue = x }
     
  readRef r = Event $ \p ->
    liftIO $ readIORef (refValue r)

  writeRef r a = Event $ \p -> 
    a `seq` liftIO $ writeIORef (refValue r) a

  modifyRef r f = Event $ \p -> 
    do a <- liftIO $ readIORef (refValue r)
       let b = f a
       b `seq` liftIO $ writeIORef (refValue r) b
