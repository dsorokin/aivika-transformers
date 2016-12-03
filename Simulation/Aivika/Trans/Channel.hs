
-- |
-- Module     : Simulation.Aivika.Trans.Channel
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines a channel that transforms one 'Signal' to another
-- within the 'Composite' computation.
--
module Simulation.Aivika.Trans.Channel
       (-- * Channel Computation
        Channel(..),
        -- * Delay Channel
        delayChannel,
        delayChannelM,
        -- * Debugging
        traceChannel) where

import qualified Control.Category as C
import Control.Monad

import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Dynamics
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.Signal
import Simulation.Aivika.Trans.Composite

-- | It allows representing a signal transformation.
newtype Channel m a b =
  Channel { runChannel :: Signal m a -> Composite m (Signal m b)
            -- ^ Run the channel transform.
          }

instance MonadDES m => C.Category (Channel m) where

  {-# INLINE id #-}
  id = Channel return

  {-# INLINE (.) #-}
  (Channel g) . (Channel f) =
    Channel $ \a -> f a >>= g

-- | Return a delayed signal.
--
-- This is actually the 'delaySignal' function wrapped in the 'Channel' type. 
delayChannel :: MonadDES m
                => Double           -- ^ the delay
                -> Channel m a a    -- ^ the delay channel
{-# INLINABLE delayChannel #-}
delayChannel delay =
  Channel $ \a -> return $ delaySignal delay a

-- | Like 'delayChannel', but it re-computes the delay each time.
--
-- This is actually the 'delaySignalM' function wrapped in the 'Channel' type. 
delayChannelM :: MonadDES m
                 => Event m Double    -- ^ the delay
                 -> Channel m a a     -- ^ the delay channel
{-# INLINABLE delayChannelM #-}
delayChannelM delay =
  Channel $ \a -> return $ delaySignalM delay a
                                 
-- | Show the debug message with the current simulation time,
-- when emitting the output signal.
traceChannel :: MonadDES m => String -> Channel m a b -> Channel m a b
{-# INLINABLE traceChannel #-}
traceChannel message (Channel f) =
  Channel $ \a ->
  do b <- f a
     return $
       traceSignal message b
