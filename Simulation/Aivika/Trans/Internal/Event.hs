
{-# LANGUAGE RecursiveDo, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, MonoLocalBinds, RankNTypes #-}

-- |
-- Module     : Simulation.Aivika.Trans.Internal.Event
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines the 'Event' monad transformer which is very similar to the 'Dynamics'
-- monad transformer but only now the computation is strongly synchronized with the event queue.
--
module Simulation.Aivika.Trans.Internal.Event
       (-- * Event Monad
        Event(..),
        EventLift(..),
        EventProcessing(..),
        invokeEvent,
        runEventInStartTime,
        runEventInStopTime,
        -- * Event Queue
        EventQueueing(..),
        enqueueEventWithCancellation,
        enqueueEventWithStartTime,
        enqueueEventWithStopTime,
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
        -- * Retrying Computation
        retryEvent,
        -- * Synchronizing IO Actions
        EventIOQueueing(..),
        enqueueEventIOWithStartTime,
        enqueueEventIOWithStopTime,
        enqueueEventIOWithTimes,
        enqueueEventIOWithPoints,
        enqueueEventIOWithIntegTimes,
        -- * Debugging
        traceEvent) where

import Data.Monoid hiding ((<>))
import Data.Semigroup (Semigroup(..))

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import qualified Control.Monad.Catch as MC
import Control.Applicative

import Debug.Trace (trace)

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Internal.Types
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

instance Monad m => MonadCompTrans Event m where

  {-# INLINE liftComp #-}
  liftComp = Event . const

-- | A type class to lift the 'Event' computations into other computations.
class EventLift t m where
  
  -- | Lift the specified 'Event' computation into another computation.
  liftEvent :: Event m a -> t m a

instance Monad m => EventLift Event m where
  
  {-# INLINE liftEvent #-}
  liftEvent = id

instance Monad m => DynamicsLift Event m where
  
  {-# INLINE liftDynamics #-}
  liftDynamics (Dynamics x) = Event x

instance Monad m => SimulationLift Event m where

  {-# INLINE liftSimulation #-}
  liftSimulation (Simulation x) = Event $ x . pointRun 

instance Monad m => ParameterLift Event m where

  {-# INLINE liftParameter #-}
  liftParameter (Parameter x) = Event $ x . pointRun

-- | Exception handling within 'Event' computations.
catchEvent :: (MonadException m, Exception e) => Event m a -> (e -> Event m a) -> Event m a
{-# INLINABLE catchEvent #-}
catchEvent (Event m) h =
  Event $ \p -> 
  catchComp (m p) $ \e ->
  let Event m' = h e in m' p
                           
-- | A computation with finalization part like the 'finally' function.
finallyEvent :: MonadException m => Event m a -> Event m b -> Event m a
{-# INLINABLE finallyEvent #-}
finallyEvent (Event m) (Event m') =
  Event $ \p ->
  finallyComp (m p) (m' p)

-- | Like the standard 'throw' function.
throwEvent :: (MonadException m, Exception e) => e -> Event m a
{-# INLINABLE throwEvent #-}
throwEvent e =
  Event $ \p ->
  throwComp e

-- | Runs an action with asynchronous exceptions disabled.
maskEvent :: MC.MonadMask m => ((forall a. Event m a -> Event m a) -> Event m b) -> Event m b
{-# INLINABLE maskEvent #-}
maskEvent a =
  Event $ \p ->
  MC.mask $ \u ->
  invokeEvent p (a $ q u)
  where q u (Event b) = Event (u . b)

-- | Like 'maskEvent', but the masked computation is not interruptible.
uninterruptibleMaskEvent :: MC.MonadMask m => ((forall a. Event m a -> Event m a) -> Event m b) -> Event m b
{-# INLINABLE uninterruptibleMaskEvent #-}
uninterruptibleMaskEvent a =
  Event $ \p ->
  MC.uninterruptibleMask $ \u ->
  invokeEvent p (a $ q u)
  where q u (Event b) = Event (u . b)

instance MonadFix m => MonadFix (Event m) where

  {-# INLINE mfix #-}
  mfix f = 
    Event $ \p ->
    do { rec { a <- invokeEvent p (f a) }; return a }

instance MonadException m => MC.MonadThrow (Event m) where

  {-# INLINE throwM #-}
  throwM = throwEvent

instance MonadException m => MC.MonadCatch (Event m) where

  {-# INLINE catch #-}
  catch = catchEvent

instance (MonadException m, MC.MonadMask m) => MC.MonadMask (Event m) where

  {-# INLINE mask #-}
  mask = maskEvent
  
  {-# INLINE uninterruptibleMask #-}
  uninterruptibleMask = uninterruptibleMaskEvent

-- | Run the 'Event' computation in the start time involving all
-- pending 'CurrentEvents' in the processing too.
runEventInStartTime :: MonadDES m => Event m a -> Simulation m a
{-# INLINE runEventInStartTime #-}
runEventInStartTime = runDynamicsInStartTime . runEvent

-- | Run the 'Event' computation in the stop time involving all
-- pending 'CurrentEvents' in the processing too.
runEventInStopTime :: MonadDES m => Event m a -> Simulation m a
{-# INLINE runEventInStopTime #-}
runEventInStopTime = runDynamicsInStopTime . runEvent

-- | Actuate the event handler in the specified time points.
enqueueEventWithTimes :: MonadDES m => [Double] -> Event m () -> Event m ()
{-# INLINABLE enqueueEventWithTimes #-}
enqueueEventWithTimes ts e = loop ts
  where loop []       = return ()
        loop (t : ts) = enqueueEvent t $ e >> loop ts
       
-- | Actuate the event handler in the specified time points.
enqueueEventWithPoints :: MonadDES m => [Point m] -> Event m () -> Event m ()
{-# INLINABLE enqueueEventWithPoints #-}
enqueueEventWithPoints xs (Event e) = loop xs
  where loop []       = return ()
        loop (x : xs) = enqueueEvent (pointTime x) $ 
                        Event $ \p ->
                        do e x    -- N.B. we substitute the time point!
                           invokeEvent p $ loop xs
                           
-- | Actuate the event handler in the integration time points.
enqueueEventWithIntegTimes :: MonadDES m => Event m () -> Event m ()
{-# INLINABLE enqueueEventWithIntegTimes #-}
enqueueEventWithIntegTimes e =
  Event $ \p ->
  let points = integPointsStartingFrom p
  in invokeEvent p $ enqueueEventWithPoints points e

-- | Actuate the event handler in the start time point.
enqueueEventWithStartTime :: MonadDES m => Event m () -> Event m ()
{-# INLINABLE enqueueEventWithStartTime #-}
enqueueEventWithStartTime e =
  Event $ \p ->
  let p0 = integStartPoint $ pointRun p
  in invokeEvent p $ enqueueEventWithPoints [p0] e

-- | Actuate the event handler in the final time point.
enqueueEventWithStopTime :: MonadDES m => Event m () -> Event m ()
{-# INLINABLE enqueueEventWithStopTime #-}
enqueueEventWithStopTime e =
  Event $ \p ->
  let p0 = simulationStopPoint $ pointRun p
  in invokeEvent p $ enqueueEventWithPoints [p0] e

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
enqueueEventWithCancellation :: MonadDES m => Double -> Event m () -> Event m (EventCancellation m)
{-# INLINABLE enqueueEventWithCancellation #-}
enqueueEventWithCancellation t e =
  Event $ \p ->
  do let r = pointRun p
     cancelledRef <- invokeSimulation r $ newRef False
     cancellableRef <- invokeSimulation r $ newRef True
     finishedRef <- invokeSimulation r $ newRef False
     let cancel =
           Event $ \p ->
           do x <- invokeEvent p $ readRef cancellableRef
              when x $
                invokeEvent p $ writeRef cancelledRef True
         cancelled =
           readRef cancelledRef
         finished =
           readRef finishedRef
     invokeEvent p $
       enqueueEvent t $
       Event $ \p ->
       do invokeEvent p $ writeRef cancellableRef False
          x <- invokeEvent p $ readRef cancelledRef
          unless x $
            do invokeEvent p e
               invokeEvent p $ writeRef finishedRef True
     return EventCancellation { cancelEvent   = cancel,
                                eventCancelled = cancelled,
                                eventFinished = finished }

-- | Memoize the 'Event' computation, always returning the same value
-- within a simulation run.
memoEvent :: MonadDES m => Event m a -> Simulation m (Event m a)
{-# INLINABLE memoEvent #-}
memoEvent m =
  Simulation $ \r ->
  do ref <- invokeSimulation r $ newRef Nothing
     return $ Event $ \p ->
       do x <- invokeEvent p $ readRef ref
          case x of
            Just v -> return v
            Nothing ->
              do v <- invokeEvent p m
                 invokeEvent p $ writeRef ref (Just v)
                 return v

-- | Memoize the 'Event' computation, always returning the same value
-- in the same modeling time. After the time changes, the value is
-- recalculated by demand.
--
-- It is possible to implement this function efficiently, for the 'Event'
-- computation is always synchronized with the event queue which time
-- flows in one direction only. This synchronization is a key difference
-- between the 'Event' and 'Dynamics' computations.
memoEventInTime :: MonadDES m => Event m a -> Simulation m (Event m a)
{-# INLINABLE memoEventInTime #-}
memoEventInTime m =
  Simulation $ \r ->
  do ref <- invokeSimulation r $ newRef Nothing
     return $ Event $ \p ->
       do x <- invokeEvent p $ readRef ref
          case x of
            Just (t, v) | t == pointTime p ->
              return v
            _ ->
              do v <- invokeEvent p m
                 invokeEvent p $ writeRef ref (Just (pointTime p, v))
                 return v

-- | Enqueue the event which must be actuated with the current modeling time but later.
yieldEvent :: MonadDES m => Event m () -> Event m ()
{-# INLINABLE yieldEvent #-}
yieldEvent m =
  Event $ \p ->
  invokeEvent p $
  enqueueEvent (pointTime p) m

-- | Defines a computation disposing some entity.
newtype DisposableEvent m =
  DisposableEvent { disposeEvent :: Event m ()
                    -- ^ Dispose something within the 'Event' computation.
                  }

instance Monad m => Semigroup (DisposableEvent m) where
  {-# INLINE (<>) #-}
  DisposableEvent x <> DisposableEvent y = DisposableEvent $ x >> y

instance Monad m => Monoid (DisposableEvent m) where

  {-# INLINE mempty #-}
  mempty = DisposableEvent $ return ()

  {-# INLINE mappend #-}
  mappend = (<>)

-- | Retry the current computation as possible, using the specified argument
-- as a 'SimulationRetry' exception message in case of failure. It makes sense
-- for parallel distributed simulation, when we have to make a rollback,
-- awaiting for incoming messages.
retryEvent :: MonadException m => String -> Event m a
retryEvent message = throwEvent $ SimulationRetry message

-- | Show the debug message with the current simulation time.
traceEvent :: MonadDES m => String -> Event m a -> Event m a
{-# INLINABLE traceEvent #-}
traceEvent message m =
  Event $ \p ->
  trace ("t = " ++ show (pointTime p) ++ ": " ++ message) $
  invokeEvent p m

-- | A type class of monads that allows synchronizing the global modeling time
-- before calling the event handler so that it is rather safe to perform 'IO' actions
-- within such a handler. It is mainly destined for parallel distributed simulation,
-- but it should be supported in other cases too.
--
class (EventQueueing m, MonadIO (Event m)) => EventIOQueueing m where

  -- | Like 'enqueueEvent' but synchronizes the global modeling time before
  -- calling the specified event handler.
  enqueueEventIO :: Double -> Event m () -> Event m ()

-- | Like 'enqueueEventWithTimes' but synchronizes the global modeling time
-- before calling the specified event handler.
enqueueEventIOWithTimes :: (MonadDES m, EventIOQueueing m) => [Double] -> Event m () -> Event m ()
{-# INLINABLE enqueueEventIOWithTimes #-}
enqueueEventIOWithTimes ts e = loop ts
  where loop []       = return ()
        loop (t : ts) = enqueueEventIO t $ e >> loop ts
       
-- | Like 'enqueueEventWithPoints' but synchronizes the global modeling time
-- before calling the specified event handler.
enqueueEventIOWithPoints :: (MonadDES m, EventIOQueueing m) => [Point m] -> Event m () -> Event m ()
{-# INLINABLE enqueueEventIOWithPoints #-}
enqueueEventIOWithPoints xs (Event e) = loop xs
  where loop []       = return ()
        loop (x : xs) = enqueueEventIO (pointTime x) $ 
                        Event $ \p ->
                        do e x    -- N.B. we substitute the time point!
                           invokeEvent p $ loop xs
                           
-- | Like 'enqueueEventWithIntegTimes' but synchronizes the global modeling time
-- before calling the specified event handler.
enqueueEventIOWithIntegTimes :: (MonadDES m, EventIOQueueing m) => Event m () -> Event m ()
{-# INLINABLE enqueueEventIOWithIntegTimes #-}
enqueueEventIOWithIntegTimes e =
  Event $ \p ->
  let points = integPointsStartingFrom p
  in invokeEvent p $ enqueueEventIOWithPoints points e

-- | Like 'enqueueEventWithStartTime' but synchronizes the global modeling time
-- before calling the specified event handler.
enqueueEventIOWithStartTime :: (MonadDES m, EventIOQueueing m) => Event m () -> Event m ()
{-# INLINABLE enqueueEventIOWithStartTime #-}
enqueueEventIOWithStartTime e =
  Event $ \p ->
  let p0 = integStartPoint $ pointRun p
  in invokeEvent p $ enqueueEventIOWithPoints [p0] e

-- | Like 'enqueueEventWithStopTime' but synchronizes the global modeling time
-- before calling the specified event handler.
enqueueEventIOWithStopTime :: (MonadDES m, EventIOQueueing m) => Event m () -> Event m ()
{-# INLINABLE enqueueEventIOWithStopTime #-}
enqueueEventIOWithStopTime e =
  Event $ \p ->
  let p0 = simulationStopPoint $ pointRun p
  in invokeEvent p $ enqueueEventIOWithPoints [p0] e
