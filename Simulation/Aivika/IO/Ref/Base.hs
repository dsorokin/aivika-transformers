
{-# LANGUAGE TypeFamilies, FlexibleInstances, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.IO.Ref.Base
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The 'MonadIO' based monad is an instance of 'MonadRef'.
--
module Simulation.Aivika.IO.Ref.Base where

import Data.IORef

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Internal.Types
import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.Template

-- | The 'MonadIO' based monad is an instance of 'MonadRef'.
instance (MonadIO m, MonadTemplate m) => MonadRef m where

  {-# SPECIALISE instance MonadRef IO #-}

  -- | A type safe wrapper for the 'IORef' reference.
  newtype Ref m a = Ref { refValue :: IORef a }

  {-# SPECIALISE INLINE newRef :: a -> Simulation IO (Ref IO a) #-}
  newRef a =
    Simulation $ \r ->
    do x <- liftIO $ newIORef a
       return Ref { refValue = x }
     
  {-# SPECIALISE INLINE readRef :: Ref IO a -> Event IO a #-}
  readRef r = Event $ \p ->
    liftIO $ readIORef (refValue r)

  {-# SPECIALISE INLINE writeRef :: Ref IO a -> a -> Event IO () #-}
  writeRef r a = Event $ \p -> 
    a `seq` liftIO $ writeIORef (refValue r) a

  {-# SPECIALISE INLINE modifyRef :: Ref IO a -> (a -> a) -> Event IO () #-}
  modifyRef r f = Event $ \p -> 
    do a <- liftIO $ readIORef (refValue r)
       let b = f a
       b `seq` liftIO $ writeIORef (refValue r) b

  {-# SPECIALISE INLINE equalRef :: Ref IO a -> Ref IO a -> Bool #-}
  equalRef (Ref r1) (Ref r2) = (r1 == r2)
