
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Trans.Internal.Event
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines the 'Event' monad transformer which is very similar to the 'Dynamics'
-- monad transformer but only now the computation is strongly synchronized with the event queue.
--
module Simulation.Aivika.Trans.Internal.Event
       (-- * Event Monad
        EventLift(..),
        runEventInStartTime,
        runEventInStopTime,
        -- * Event Queue
        enqueueEventWithCancellation,
        enqueueEventWithTimes,
        enqueueEventWithPoints,
        enqueueEventWithIntegTimes,
        yieldEvent,
        -- * Cancelling Event
        EventCancellation,
        cancelEvent,
        eventCancelled,
        eventFinished,
        -- * Error Handling
        catchEvent,
        finallyEvent,
        throwEvent,
        -- * Memoization
        memoEvent,
        memoEventInTime,
        -- * Disposable
        DisposableEvent(..),
        -- * Debugging
        traceEvent) where

import Data.Monoid

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Applicative

import Debug.Trace (trace)

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics

instance Monad m => Monad (Event m) where

  {-# INLINE return #-}
  return a = Event $ \p -> return a

  {-# INLINE (>>=) #-}
  (Event m) >>= k =
    Event $ \p -> 
    do a <- m p
       let Event m' = k a
       m' p

instance Functor m => Functor (Event m) where
  
  {-# INLINE fmap #-}
  fmap f (Event x) = Event $ \p -> fmap f $ x p

instance Applicative m => Applicative (Event m) where
  
  {-# INLINE pure #-}
  pure = Event . const . pure
  
  {-# INLINE (<*>) #-}
  (Event x) <*> (Event y) = Event $ \p -> x p <*> y p

instance MonadTrans Event where

  {-# INLINE lift #-}
  lift = Event . const

instance MonadIO m => MonadIO (Event m) where
  
  {-# INLINE liftIO #-}
  liftIO = Event . const . liftIO

instance MonadCompTrans Event where

  {-# INLINE liftComp #-}
  liftComp = Event . const

-- | A type class to lift the 'Event' computations into other computations.
class EventLift t where
  
  -- | Lift the specified 'Event' computation into another computation.
  liftEvent :: MonadComp m => Event m a -> t m a

instance EventLift Event where
  
  {-# INLINE liftEvent #-}
  liftEvent = id

instance DynamicsLift Event where
  
  {-# INLINE liftDynamics #-}
  liftDynamics (Dynamics x) = Event x

instance SimulationLift Event where

  {-# INLINE liftSimulation #-}
  liftSimulation (Simulation x) = Event $ x . pointRun 

instance ParameterLift Event where

  {-# INLINE liftParameter #-}
  liftParameter (Parameter x) = Event $ x . pointRun

-- | Exception handling within 'Event' computations.
catchEvent :: (MonadComp m, Exception e) => Event m a -> (e -> Event m a) -> Event m a
catchEvent (Event m) h =
  Event $ \p -> 
  catchComp (m p) $ \e ->
  let Event m' = h e in m' p
                           
-- | A computation with finalization part like the 'finally' function.
finallyEvent :: MonadComp m => Event m a -> Event m b -> Event m a
finallyEvent (Event m) (Event m') =
  Event $ \p ->
  finallyComp (m p) (m' p)

-- | Like the standard 'throw' function.
throwEvent :: (MonadComp m, Exception e) => e -> Event m a
throwEvent = throw

instance MonadFix m => MonadFix (Event m) where

  {-# INLINE mfix #-}
  mfix f = 
    Event $ \p ->
    do { rec { a <- invokeEvent p (f a) }; return a }

-- | Run the 'Event' computation in the start time involving all
-- pending 'CurrentEvents' in the processing too.
runEventInStartTime :: MonadComp m => Event m a -> Simulation m a
runEventInStartTime = runDynamicsInStartTime . runEvent

-- | Run the 'Event' computation in the stop time involving all
-- pending 'CurrentEvents' in the processing too.
runEventInStopTime :: MonadComp m => Event m a -> Simulation m a
runEventInStopTime = runDynamicsInStopTime . runEvent

-- | Actuate the event handler in the specified time points.
enqueueEventWithTimes :: MonadComp m => [Double] -> Event m () -> Event m ()
enqueueEventWithTimes ts e = loop ts
  where loop []       = return ()
        loop (t : ts) = enqueueEvent t $ e >> loop ts
       
-- | Actuate the event handler in the specified time points.
enqueueEventWithPoints :: MonadComp m => [Point m] -> Event m () -> Event m ()
enqueueEventWithPoints xs (Event e) = loop xs
  where loop []       = return ()
        loop (x : xs) = enqueueEvent (pointTime x) $ 
                        Event $ \p ->
                        do e x    -- N.B. we substitute the time point!
                           invokeEvent p $ loop xs
                           
-- | Actuate the event handler in the integration time points.
enqueueEventWithIntegTimes :: MonadComp m => Event m () -> Event m ()
enqueueEventWithIntegTimes e =
  Event $ \p ->
  let points = integPointsStartingFrom p
  in invokeEvent p $ enqueueEventWithPoints points e

-- | It allows cancelling the event.
data EventCancellation m =
  EventCancellation { cancelEvent :: Event m (),
                      -- ^ Cancel the event.
                      eventCancelled :: Event m Bool,
                      -- ^ Test whether the event was cancelled.
                      eventFinished :: Event m Bool
                      -- ^ Test whether the event was processed and finished.
                    }

-- | Enqueue the event with an ability to cancel it.
enqueueEventWithCancellation :: MonadComp m => Double -> Event m () -> Event m (EventCancellation m)
enqueueEventWithCancellation t e =
  Event $ \p ->
  do let s = runSession $ pointRun p
     cancelledRef <- newProtoRef s False
     cancellableRef <- newProtoRef s True
     finishedRef <- newProtoRef s False
     let cancel =
           Event $ \p ->
           do x <- readProtoRef cancellableRef
              when x $
                writeProtoRef cancelledRef True
         cancelled =
           Event $ \p -> readProtoRef cancelledRef
         finished =
           Event $ \p -> readProtoRef finishedRef
     invokeEvent p $
       enqueueEvent t $
       Event $ \p ->
       do writeProtoRef cancellableRef False
          x <- readProtoRef cancelledRef
          unless x $
            do invokeEvent p e
               writeProtoRef finishedRef True
     return EventCancellation { cancelEvent   = cancel,
                                eventCancelled = cancelled,
                                eventFinished = finished }

-- | Memoize the 'Event' computation, always returning the same value
-- within a simulation run.
memoEvent :: MonadComp m => Event m a -> Simulation m (Event m a)
memoEvent m =
  Simulation $ \r ->
  do let s = runSession r
     ref <- newProtoRef s Nothing
     return $ Event $ \p ->
       do x <- readProtoRef ref
          case x of
            Just v -> return v
            Nothing ->
              do v <- invokeEvent p m
                 writeProtoRef ref (Just v)
                 return v

-- | Memoize the 'Event' computation, always returning the same value
-- in the same modeling time. After the time changes, the value is
-- recalculated by demand.
--
-- It is possible to implement this function efficiently, for the 'Event'
-- computation is always synchronized with the event queue which time
-- flows in one direction only. This synchronization is a key difference
-- between the 'Event' and 'Dynamics' computations.
memoEventInTime :: MonadComp m => Event m a -> Simulation m (Event m a)
memoEventInTime m =
  Simulation $ \r ->
  do let s = runSession r
     ref <- newProtoRef s Nothing
     return $ Event $ \p ->
       do x <- readProtoRef ref
          case x of
            Just (t, v) | t == pointTime p ->
              return v
            _ ->
              do v <- invokeEvent p m
                 writeProtoRef ref (Just (pointTime p, v))
                 return v

-- | Enqueue the event which must be actuated with the current modeling time but later.
yieldEvent :: MonadComp m => Event m () -> Event m ()
yieldEvent m =
  Event $ \p ->
  invokeEvent p $
  enqueueEvent (pointTime p) m

-- | Defines a computation disposing some entity.
newtype DisposableEvent m =
  DisposableEvent { disposeEvent :: Event m ()
                    -- ^ Dispose something within the 'Event' computation.
                  }

instance Monad m => Monoid (DisposableEvent m) where

  {-# INLINE mempty #-}
  mempty = DisposableEvent $ return ()

  {-# INLINE mappend #-}
  mappend (DisposableEvent x) (DisposableEvent y) = DisposableEvent $ x >> y

-- | Show the debug message with the current simulation time.
traceEvent :: MonadComp m => String -> Event m a -> Event m a
traceEvent message m =
  Event $ \p ->
  trace ("t = " ++ show (pointTime p) ++ ": " ++ message) $
  invokeEvent p m
