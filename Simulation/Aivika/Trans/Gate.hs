
-- |
-- Module     : Simulation.Aivika.Trans.Gate
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- The module defines a gate which can be either opened or closed.
--
module Simulation.Aivika.Trans.Gate
       (Gate,
        createGate,
        createGateOpened,
        createGateClosed,
        openGate,
        closeGate,
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
createGate :: MonadDES m => Bool -> Simulation m (Gate m)
{-# INLINE createGate #-}
createGate opened =
  do r <- newRef opened
     return Gate { gateRef = r }

-- | Create a new initially open gate.
createGateOpened :: MonadDES m => Simulation m (Gate m)
{-# INLINE createGateOpened #-}
createGateOpened = createGate True

-- | Create a new initially close gate.
createGateClosed :: MonadDES m => Simulation m (Gate m)
{-# INLINE createGateClosed #-}
createGateClosed = createGate False

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
{-# INLINE awaitGateOpened #-}
awaitGateOpened gate =
  do f <- liftEvent $ readRef (gateRef gate)
     unless f $
       do processAwait $ refChanged_ (gateRef gate)
          awaitGateOpened gate

-- | Await the gate to be closed if required. If the gate is already closed
-- then the computation returns immediately.
awaitGateClosed :: MonadDES m => Gate m -> Process m ()
{-# INLINE awaitGateClosed #-}
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
