
{-# LANGUAGE TypeFamilies, FlexibleInstances, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.IO.Ref.Base
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The 'MonadIO'-based monad can be an instance of 'MonadRef'.
--
module Simulation.Aivika.IO.Ref.Base () where

import Data.IORef

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Internal.Types
import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.Template

-- | The 'MonadIO' based monad is an instance of 'MonadRef'.
instance (Monad m, MonadIO m, MonadTemplate m) => MonadRef m where

  {-# SPECIALISE instance MonadRef IO #-}

  -- | A type safe wrapper for the 'IORef' reference.
  newtype Ref m a = Ref { refValue :: IORef a }

  {-# INLINE newRef #-}
  newRef a =
    Simulation $ \r ->
    do x <- liftIO $ newIORef a
       return Ref { refValue = x }
     
  {-# INLINE readRef #-}
  readRef r = Event $ \p ->
    liftIO $ readIORef (refValue r)

  {-# INLINE writeRef #-}
  writeRef r a = Event $ \p -> 
    a `seq` liftIO $ writeIORef (refValue r) a

  {-# INLINE modifyRef #-}
  modifyRef r f = Event $ \p -> 
    do a <- liftIO $ readIORef (refValue r)
       let b = f a
       b `seq` liftIO $ writeIORef (refValue r) b

  {-# INLINE equalRef #-}
  equalRef (Ref r1) (Ref r2) = (r1 == r2)

-- | The 'MonadIO' based monad is an instance of 'MonadRef0'.
instance (MonadIO m, MonadTemplate m) => MonadRef0 m where

  {-# SPECIALISE instance MonadRef0 IO #-}

  {-# INLINE newRef0 #-}
  newRef0 a =
    do x <- liftIO $ newIORef a
       return Ref { refValue = x }
     
