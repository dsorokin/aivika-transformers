
-- |
-- Module     : Simulation.Aivika.Trans.Ref
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines an updatable reference that depends on the event queue.
--
module Simulation.Aivika.Trans.Ref
       (Ref,
        refChanged,
        refChanged_,
        newRef,
        readRef,
        writeRef,
        modifyRef) where

import Data.IORef

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Signal
import qualified Simulation.Aivika.Trans.Ref.Base as B
import Simulation.Aivika.Trans.DES

-- | The 'Ref' type represents a mutable variable similar to the 'IORef' variable 
-- but only dependent on the event queue, which allows synchronizing the reference
-- with the model explicitly through the 'Event' monad.
data Ref m a = 
  Ref { refValue :: B.Ref m a, 
        refChangedSource :: SignalSource m a }

-- | Create a new reference.
newRef :: MonadDES m => a -> Simulation m (Ref m a)
newRef a =
  Simulation $ \r ->
  do x <- invokeSimulation r $ B.newRef a
     s <- invokeSimulation r newSignalSource
     return Ref { refValue = x, 
                  refChangedSource = s }
     
-- | Read the value of a reference.
readRef :: MonadDES m => Ref m a -> Event m a
readRef r = B.readRef (refValue r)

-- | Write a new value into the reference.
writeRef :: MonadDES m => Ref m a -> a -> Event m ()
writeRef r a = Event $ \p -> 
  do a `seq` invokeEvent p $ B.writeRef (refValue r) a
     invokeEvent p $ triggerSignal (refChangedSource r) a

-- | Mutate the contents of the reference.
modifyRef :: MonadDES m => Ref m a -> (a -> a) -> Event m ()
modifyRef r f = Event $ \p -> 
  do a <- invokeEvent p $ B.readRef (refValue r)
     let b = f a
     b `seq` invokeEvent p $ B.writeRef (refValue r) b
     invokeEvent p $ triggerSignal (refChangedSource r) b

-- | Return a signal that notifies about every change of the reference state.
refChanged :: Ref m a -> Signal m a
refChanged r = publishSignal (refChangedSource r)

-- | Return a signal that notifies about every change of the reference state.
refChanged_ :: MonadDES m => Ref m a -> Signal m ()
refChanged_ r = mapSignal (const ()) $ refChanged r

instance MonadDES m => Eq (Ref m a) where
  r1 == r2 = (refValue r1) == (refValue r2)
