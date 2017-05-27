
-- |
-- Module     : Simulation.Aivika.Trans.Gate
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines a gate which can be either opened or closed.
--
module Simulation.Aivika.Trans.Gate
       (Gate,
        newGate,
        newGateOpened,
        newGateClosed,
        openGate,
        closeGate,
        invertGate,
        gateOpened,
        gateClosed,
        awaitGateOpened,
        awaitGateClosed,
        gateChanged_) where

import Control.Monad

import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.Process
import Simulation.Aivika.Trans.Signal
import Simulation.Aivika.Trans.Ref

-- | Represents a gate, which can be either opened or closed.
data Gate m = Gate { gateRef :: Ref m Bool }

-- | Create a new gate, specifying whether the gate is initially open.
newGate :: MonadDES m => Bool -> Simulation m (Gate m)
{-# INLINE newGate #-}
newGate opened =
  do r <- newRef opened
     return Gate { gateRef = r }

-- | Create a new initially open gate.
newGateOpened :: MonadDES m => Simulation m (Gate m)
{-# INLINE newGateOpened #-}
newGateOpened = newGate True

-- | Create a new initially close gate.
newGateClosed :: MonadDES m => Simulation m (Gate m)
{-# INLINE newGateClosed #-}
newGateClosed = newGate False

-- | Open the gate if it was closed.
openGate :: MonadDES m => Gate m -> Event m ()
{-# INLINE openGate #-}
openGate gate =
  writeRef (gateRef gate) True

-- | Close the gate if it was open.
closeGate :: MonadDES m => Gate m -> Event m ()
{-# INLINE closeGate #-}
closeGate gate =
  writeRef (gateRef gate) False

-- | Invert the gate.
invertGate :: MonadDES m => Gate m -> Event m ()
{-# INLINABLE invertGate #-}
invertGate gate =
  modifyRef (gateRef gate) not

-- | Test whether the gate is open.
gateOpened :: MonadDES m => Gate m -> Event m Bool
{-# INLINE gateOpened #-}
gateOpened gate =
  readRef (gateRef gate)

-- | Test whether the gate is closed.
gateClosed :: MonadDES m => Gate m -> Event m Bool
{-# INLINE gateClosed #-}
gateClosed gate =
  fmap not $ readRef (gateRef gate)

-- | Await the gate to be opened if required. If the gate is already open
-- then the computation returns immediately.
awaitGateOpened :: MonadDES m => Gate m -> Process m ()
{-# INLINABLE awaitGateOpened #-}
awaitGateOpened gate =
  do f <- liftEvent $ readRef (gateRef gate)
     unless f $
       do processAwait $ refChanged_ (gateRef gate)
          awaitGateOpened gate

-- | Await the gate to be closed if required. If the gate is already closed
-- then the computation returns immediately.
awaitGateClosed :: MonadDES m => Gate m -> Process m ()
{-# INLINABLE awaitGateClosed #-}
awaitGateClosed gate =
  do f <- liftEvent $ readRef (gateRef gate)
     when f $
       do processAwait $ refChanged_ (gateRef gate)
          awaitGateClosed gate

-- | Signal triggered when the state of the gate changes.
gateChanged_ :: MonadDES m => Gate m -> Signal m ()
{-# INLINE gateChanged_ #-}
gateChanged_ gate =
  refChanged_ (gateRef gate)
