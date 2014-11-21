
-- |
-- Module     : Simulation.Aivika.IO.Signal
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module allows collecting the signal history.
--

module Simulation.Aivika.IO.Signal
       (-- * Signal History
        SignalHistory,
        signalHistorySignal,
        newSignalHistory,
        newSignalHistoryStartingWith,
        readSignalHistory) where

import Data.Monoid
import Data.List
import Data.Array
import Data.Array.MArray.Safe

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Template
import Simulation.Aivika.Trans.Signal

import qualified Simulation.Aivika.Vector as V
import qualified Simulation.Aivika.Vector.Unboxed as UV
                                    
-- | Represents the history of the signal values.
data SignalHistory m a =
  SignalHistory { signalHistorySignal :: Signal m a,  
                  -- ^ The signal for which the history is created.
                  signalHistoryTimes  :: UV.Vector Double,
                  signalHistoryValues :: V.Vector a }

-- | Create a history of the signal values.
newSignalHistory :: (MonadDES m, MonadIO m, MonadTemplate m)
                    => Signal m a -> Event m (SignalHistory m a)
newSignalHistory =
  newSignalHistoryStartingWith Nothing

-- | Create a history of the signal values starting with
-- the optional initial value.
newSignalHistoryStartingWith :: (MonadDES m, MonadIO m, MonadTemplate m)
                                => Maybe a -> Signal m a -> Event m (SignalHistory m a)
newSignalHistoryStartingWith init signal =
  Event $ \p ->
  do ts <- liftIO UV.newVector
     xs <- liftIO V.newVector
     case init of
       Nothing -> return ()
       Just a ->
         liftIO $
         do UV.appendVector ts (pointTime p)
            V.appendVector xs a
     invokeEvent p $
       handleSignal_ signal $ \a ->
       Event $ \p ->
       liftIO $
       do UV.appendVector ts (pointTime p)
          V.appendVector xs a
     return SignalHistory { signalHistorySignal = signal,
                            signalHistoryTimes  = ts,
                            signalHistoryValues = xs }
       
-- | Read the history of signal values.
readSignalHistory :: (MonadDES m, MonadIO m, MonadTemplate m)
                     => SignalHistory m a -> Event m (Array Int Double, Array Int a)
readSignalHistory history =
  Event $ \p ->
  liftIO $
  do xs <- UV.freezeVector (signalHistoryTimes history)
     ys <- V.freezeVector (signalHistoryValues history)
     return (xs, ys)     
