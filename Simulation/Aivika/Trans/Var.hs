
{-# LANGUAGE TypeFamilies, ConstrainedClassMethods #-}

-- |
-- Module     : Simulation.Aivika.Trans.Var
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- This module defines a variable that is bound up with the event queue and 
-- that keeps the history of changes storing the values in arrays, which
-- allows using the variable in differential and difference equations of
-- System Dynamics within hybrid discrete-continuous simulation.
--
module Simulation.Aivika.Trans.Var (MonadVar(..)) where

import Data.Array

import Simulation.Aivika.Trans.Ref
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Signal

-- | A type class of monads within which we can create mutable variables.
class MonadDES m => MonadVar m where

  -- | Like the 'Ref' reference but keeps the history of changes in 
  -- different time points. The 'Var' variable is safe to be used in
  -- the hybrid discrete-continuous simulation.
  --
  -- For example, the memoised values of a variable can be used in
  -- the differential or difference equations of System Dynamics, while
  -- the variable iself can be updated wihin the discrete event simulation.
  --
  -- Only this variable is much slower than the reference.
  data Var m a
     
  -- | Create a new variable.
  newVar :: a -> Simulation m (Var m a)

  -- | Read the first actual, i.e. memoised, value of a variable for the requested time
  -- actuating the current events from the queue if needed.
  --
  -- This computation can be used in the ordinary differential and
  -- difference equations of System Dynamics.
  varMemo :: Var m a -> Dynamics m a
  
  -- | Read the recent actual value of a variable for the requested time.
  --
  -- This computation is destined for using within discrete event simulation.
  readVar :: Var m a -> Event m a
  
  -- | Write a new value into the variable.
  writeVar :: Var m a -> a -> Event m ()

  -- | Mutate the contents of the variable.
  modifyVar :: Var m a -> (a -> a) -> Event m ()

  -- | Freeze the variable and return in arrays the time points and corresponded 
  -- first and last values when the variable had changed or had been memoised in
  -- different time points: (1) the time points are sorted in ascending order;
  -- (2) the first and last actual values per each time point are provided.
  --
  -- If you need to get all changes including those ones that correspond to the same
  -- simulation time points then you can use the 'newSignalHistory' function passing
  -- in the 'varChanged' signal to it and then call function 'readSignalHistory'.
  freezeVar :: Var m a -> Event m (Array Int Double, Array Int a, Array Int a)
     
  -- | Return a signal that notifies about every change of the variable state.
  varChanged :: Var m a -> Signal m a

  -- | Return a signal that notifies about every change of the variable state.
  varChanged_ :: MonadDES m => Var m a -> Signal m ()
