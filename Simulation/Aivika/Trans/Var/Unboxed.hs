
-- |
-- Module     : Simulation.Aivika.Trans.Var.Unboxed
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines an unboxed variable that is bound up with the event queue and 
-- that keeps the history of changes storing the values in unboxed arrays, which
-- allows using the variable in differential and difference equations of
-- System Dynamics within hybrid discrete-continuous simulation.
--
module Simulation.Aivika.Trans.Var.Unboxed
       (Var,
        varChanged,
        varChanged_,
        newVar,
        readVar,
        writeVar,
        modifyVar,
        freezeVar) where

import Data.Array

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Signal
import Simulation.Aivika.Trans.Ref
import Simulation.Aivika.Trans.Signal
import Simulation.Aivika.Trans.Unboxed

import qualified Simulation.Aivika.Trans.Vector.Unboxed as UV

-- | Like the 'Ref' reference but keeps the history of changes in 
-- different time points. The 'Var' variable is safe to be used in
-- the hybrid discrete-continuous simulation.
--
-- For example, the memoised values of a variable can be used in
-- the differential or difference equations of System Dynamics, while
-- the variable iself can be updated wihin the discrete event simulation.
--
-- Only this variable is much slower than the reference.
data Var m a = 
  Var { varXS    :: UV.Vector m Double,
        varMS    :: UV.Vector m a,
        varYS    :: UV.Vector m a,
        varChangedSource :: SignalSource m a }

-- | Create a new variable.
newVar :: (MonadComp m, Unboxed m a) => a -> Simulation m (Var m a)
newVar a =
  Simulation $ \r ->
  do let sn = runSession r
     xs <- UV.newVector sn
     ms <- UV.newVector sn
     ys <- UV.newVector sn
     UV.appendVector xs $ spcStartTime $ runSpecs r
     UV.appendVector ms a
     UV.appendVector ys a
     s  <- invokeSimulation r newSignalSource
     return Var { varXS = xs,
                  varMS = ms,
                  varYS = ms,
                  varChangedSource = s }

-- | Read the first actual, i.e. memoised, value of a variable for the requested time
-- actuating the current events from the queue if needed.
--
-- This computation can be used in the ordinary differential and
-- difference equations of System Dynamics.
varMemo :: (MonadComp m, Unboxed m a) => Var m a -> Dynamics m a
varMemo v =
  runEventWith CurrentEventsOrFromPast $
  Event $ \p ->
  do let xs = varXS v
         ms = varMS v
         ys = varYS v
         t  = pointTime p
     count <- UV.vectorCount xs
     let i = count - 1
     x <- UV.readVector xs i
     if x < t
       then do a <- UV.readVector ys i
               UV.appendVector xs t
               UV.appendVector ms a
               UV.appendVector ys a
               return a
       else if x == t
            then UV.readVector ms i
            else do i <- UV.vectorBinarySearch xs t
                    if i >= 0
                      then UV.readVector ms i
                      else UV.readVector ms $ - (i + 1) - 1

-- | Read the recent actual value of a variable for the requested time.
--
-- This computation is destined for using within discrete event simulation.
readVar :: (MonadComp m, Unboxed m a) => Var m a -> Event m a
readVar v = 
  Event $ \p ->
  do let xs = varXS v
         ys = varYS v
         t  = pointTime p
     count <- UV.vectorCount xs
     let i = count - 1
     x <- UV.readVector xs i
     if x <= t 
       then UV.readVector ys i
       else do i <- UV.vectorBinarySearch xs t
               if i >= 0
                 then UV.readVector ys i
                 else UV.readVector ys $ - (i + 1) - 1

-- | Write a new value into the variable.
writeVar :: (MonadComp m, Unboxed m a) => Var m a -> a -> Event m ()
writeVar v a =
  Event $ \p ->
  do let xs = varXS v
         ms = varMS v
         ys = varYS v
         t  = pointTime p
         s  = varChangedSource v
     count <- UV.vectorCount xs
     let i = count - 1
     x <- UV.readVector xs i
     if t < x 
       then error "Cannot update the past data: writeVar."
       else if t == x
            then UV.writeVector ys i $! a
            else do UV.appendVector xs t
                    UV.appendVector ms $! a
                    UV.appendVector ys $! a
     invokeEvent p $ triggerSignal s a

-- | Mutate the contents of the variable.
modifyVar :: (MonadComp m, Unboxed m a) => Var m a -> (a -> a) -> Event m ()
modifyVar v f =
  Event $ \p ->
  do let xs = varXS v
         ms = varMS v
         ys = varYS v
         t  = pointTime p
         s  = varChangedSource v
     count <- UV.vectorCount xs
     let i = count - 1
     x <- UV.readVector xs i
     if t < x
       then error "Cannot update the past data: modifyVar."
       else if t == x
            then do a <- UV.readVector ys i
                    let b = f a
                    UV.writeVector ys i $! b
                    invokeEvent p $ triggerSignal s b
            else do a <- UV.readVector ys i
                    let b = f a
                    UV.appendVector xs t
                    UV.appendVector ms $! b
                    UV.appendVector ys $! b
                    invokeEvent p $ triggerSignal s b

-- | Freeze the variable and return in arrays the time points and corresponded 
-- first and last values when the variable had changed or had been memoised in
-- different time points: (1) the time points are sorted in ascending order;
-- (2) the first and last actual values per each time point are provided.
--
-- If you need to get all changes including those ones that correspond to the same
-- simulation time points then you can use the 'newSignalHistory' function passing
-- in the 'varChanged' signal to it and then call function 'readSignalHistory'.
freezeVar :: (MonadComp m, Unboxed m a) => Var m a -> Event m (Array Int Double, Array Int a, Array Int a)
freezeVar v =
  Event $ \p ->
  do xs <- UV.freezeVector (varXS v)
     ms <- UV.freezeVector (varMS v)
     ys <- UV.freezeVector (varYS v)
     return (xs, ms, ys)
     
-- | Return a signal that notifies about every change of the variable state.
varChanged :: Var m a -> Signal m a
varChanged v = publishSignal (varChangedSource v)

-- | Return a signal that notifies about every change of the variable state.
varChanged_ :: MonadComp m => Var m a -> Signal m ()
varChanged_ v = mapSignal (const ()) $ varChanged v     
