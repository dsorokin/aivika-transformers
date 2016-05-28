
{-# LANGUAGE TypeFamilies, FlexibleInstances, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.IO.Var
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The 'MonadIO'-based monad can be an instance 'MonadVar'.
--
module Simulation.Aivika.IO.Var () where

import Control.Monad.Trans

import Data.Array

import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Ref
import Simulation.Aivika.Trans.Signal
import Simulation.Aivika.Trans.Template
import Simulation.Aivika.Trans.Var

import Simulation.Aivika.IO.DES

import qualified Simulation.Aivika.Vector as V
import qualified Simulation.Aivika.Vector.Unboxed as UV

-- | The 'MonadIO' based monad is an instance of 'MonadVar'.
instance (Monad m, MonadDES m, MonadIO m, MonadTemplate m) => MonadVar m where

  {-# SPECIALISE instance MonadVar IO #-}

  -- | A template-based implementation of the variable.
  data Var m a = 
    Var { varXS    :: UV.Vector Double,
          varMS    :: V.Vector a,
          varYS    :: V.Vector a,
          varChangedSource :: SignalSource m a }

  {-# INLINABLE newVar #-}
  newVar a =
    Simulation $ \r ->
    do xs <- liftIO UV.newVector
       ms <- liftIO V.newVector
       ys <- liftIO V.newVector
       liftIO $ UV.appendVector xs $ spcStartTime $ runSpecs r
       liftIO $ V.appendVector ms a
       liftIO $ V.appendVector ys a
       s  <- invokeSimulation r newSignalSource
       return Var { varXS = xs,
                    varMS = ms,
                    varYS = ms,
                    varChangedSource = s }

  {-# INLINABLE varMemo #-}
  varMemo v =
    runEventWith CurrentEventsOrFromPast $
    Event $ \p ->
    liftIO $
    do let xs = varXS v
           ms = varMS v
           ys = varYS v
           t  = pointTime p
       count <- UV.vectorCount xs
       let i = count - 1
       x <- UV.readVector xs i
       if x < t
         then do a <- V.readVector ys i
                 UV.appendVector xs t
                 V.appendVector ms a
                 V.appendVector ys a
                 return a
         else if x == t
              then V.readVector ms i
              else do i <- UV.vectorBinarySearch xs t
                      if i >= 0
                        then V.readVector ms i
                        else V.readVector ms $ - (i + 1) - 1

  {-# INLINABLE readVar #-}
  readVar v = 
    Event $ \p ->
    liftIO $
    do let xs = varXS v
           ys = varYS v
           t  = pointTime p
       count <- UV.vectorCount xs
       let i = count - 1
       x <- UV.readVector xs i
       if x <= t 
         then V.readVector ys i
         else do i <- UV.vectorBinarySearch xs t
                 if i >= 0
                   then V.readVector ys i
                   else V.readVector ys $ - (i + 1) - 1

  {-# INLINABLE writeVar #-}
  writeVar v a =
    Event $ \p ->
    do let xs = varXS v
           ms = varMS v
           ys = varYS v
           t  = pointTime p
           s  = varChangedSource v
       count <- liftIO $ UV.vectorCount xs
       let i = count - 1
       x <- liftIO $ UV.readVector xs i
       if t < x 
         then error "Cannot update the past data: writeVar."
         else if t == x
              then liftIO $ V.writeVector ys i $! a
              else liftIO $
                   do UV.appendVector xs t
                      V.appendVector ms $! a
                      V.appendVector ys $! a
       invokeEvent p $ triggerSignal s a

  {-# INLINABLE modifyVar #-}
  modifyVar v f =
    Event $ \p ->
    do let xs = varXS v
           ms = varMS v
           ys = varYS v
           t  = pointTime p
           s  = varChangedSource v
       count <- liftIO $ UV.vectorCount xs
       let i = count - 1
       x <- liftIO $ UV.readVector xs i
       if t < x
         then error "Cannot update the past data: modifyVar."
         else if t == x
              then do a <- liftIO $ V.readVector ys i
                      let b = f a
                      liftIO $ V.writeVector ys i $! b
                      invokeEvent p $ triggerSignal s b
              else do a <- liftIO $ V.readVector ys i
                      let b = f a
                      liftIO $ UV.appendVector xs t
                      liftIO $ V.appendVector ms $! b
                      liftIO $ V.appendVector ys $! b
                      invokeEvent p $ triggerSignal s b

  {-# INLINABLE freezeVar #-}
  freezeVar v =
    Event $ \p ->
    liftIO $
    do xs <- UV.freezeVector (varXS v)
       ms <- V.freezeVector (varMS v)
       ys <- V.freezeVector (varYS v)
       return (xs, ms, ys)
     
  {-# INLINE varChanged #-}
  varChanged v = publishSignal (varChangedSource v)

  {-# INLINE varChanged_ #-}
  varChanged_ v = mapSignal (const ()) $ varChanged v     
