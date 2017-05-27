
-- |
-- Module     : Simulation.Aivika.IO.Signal
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module allows collecting the signal history in a more
-- optimal way than it suggests the general implementation.
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
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Composite
import Simulation.Aivika.Trans.Signal hiding (SignalHistory,
                                              signalHistorySignal,
                                              newSignalHistory,
                                              newSignalHistoryStartingWith,
                                              readSignalHistory)

import Simulation.Aivika.IO.DES

import qualified Simulation.Aivika.Vector as V
import qualified Simulation.Aivika.Vector.Unboxed as UV
                                    
-- | Represents the history of the signal values.
data SignalHistory m a =
  SignalHistory { signalHistorySignal :: Signal m a,  
                  -- ^ The signal for which the history is created.
                  signalHistoryTimes  :: UV.Vector Double,
                  signalHistoryValues :: V.Vector a }

-- | Create a history of the signal values.
newSignalHistory :: Signal IO a -> Composite IO (SignalHistory IO a)
{-# INLINABLE newSignalHistory #-}
newSignalHistory =
  newSignalHistoryStartingWith Nothing

-- | Create a history of the signal values starting with
-- the optional initial value.
newSignalHistoryStartingWith :: Maybe a -> Signal IO a -> Composite IO (SignalHistory IO a)
{-# INLINABLE newSignalHistoryStartingWith #-}
newSignalHistoryStartingWith init signal =
  do ts <- liftIO UV.newVector
     xs <- liftIO V.newVector
     case init of
       Nothing -> return ()
       Just a ->
         do t <- liftDynamics time
            liftIO $
              do UV.appendVector ts t
                 V.appendVector xs a
     handleSignalComposite signal $ \a ->
       Event $ \p ->
       liftIO $
       do UV.appendVector ts (pointTime p)
          V.appendVector xs a
     return SignalHistory { signalHistorySignal = signal,
                            signalHistoryTimes  = ts,
                            signalHistoryValues = xs }
       
-- | Read the history of signal values.
readSignalHistory :: SignalHistory IO a -> Event IO (Array Int Double, Array Int a)
-- readSignalHistory :: (MonadDES m, MonadIO m, MonadTemplate m)
--                      => SignalHistory m a -> Event m (Array Int Double, Array Int a)
{-# INLINABLE readSignalHistory #-}
readSignalHistory history =
  Event $ \p ->
  liftIO $
  do xs <- UV.freezeVector (signalHistoryTimes history)
     ys <- V.freezeVector (signalHistoryValues history)
     return (xs, ys)     
