
-- |
-- Module     : Simulation.Aivika.Trans.Signal
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- This module defines the signal which we can subscribe handlers to. 
-- These handlers can be disposed. The signal is triggered in the 
-- current time point actuating the corresponded computations from 
-- the handlers. 
--

module Simulation.Aivika.Trans.Signal
       (-- * Handling and Triggering Signal
        Signal(..),
        handleSignal_,
        SignalSource,
        newSignalSource,
        newSignalSource0,
        publishSignal,
        triggerSignal,
        -- * Useful Combinators
        mapSignal,
        mapSignalM,
        apSignal,
        filterSignal,
        filterSignal_,
        filterSignalM,
        filterSignalM_,
        emptySignal,
        merge2Signals,
        merge3Signals,
        merge4Signals,
        merge5Signals,
        -- * Signal Arriving
        arrivalSignal,
        -- * Delaying Signal
        delaySignal,
        delaySignalM,
        -- * Creating Signal in Time Points
        newSignalInTimes,
        newSignalInIntegTimes,
        newSignalInStartTime,
        newSignalInStopTime,
        -- * Signalable Computations
        Signalable(..),
        signalableChanged,
        emptySignalable,
        appendSignalable,
        -- * Debugging
        traceSignal) where

import Data.Monoid
import Data.List
import Data.Array
import Data.Array.MArray.Safe

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Arrival (Arrival(..))

-- | The signal source that can publish its signal.
data SignalSource m a =
  SignalSource { publishSignal :: Signal m a,
                                  -- ^ Publish the signal.
                 triggerSignal :: a -> Event m ()
                                  -- ^ Trigger the signal actuating 
                                  -- all its handlers at the current 
                                  -- simulation time point.
               }
  
-- | The signal that can have disposable handlers.  
data Signal m a =
  Signal { handleSignal :: (a -> Event m ()) -> Event m (DisposableEvent m)
           -- ^ Subscribe the handler to the specified 
           -- signal and return a nested computation
           -- within a disposable object that, being applied,
           -- unsubscribes the handler from this signal.
         }

-- | The queue of signal handlers.
data SignalHandlerQueue m a =
  SignalHandlerQueue { queueList :: Ref m [SignalHandler m a] }
  
-- | It contains the information about the disposable queue handler.
data SignalHandler m a =
  SignalHandler { handlerComp :: a -> Event m (),
                  handlerRef  :: Ref m () }

instance MonadDES m => Eq (SignalHandler m a) where

  {-# INLINE (==) #-}
  x == y = (handlerRef x) == (handlerRef y)

-- | Subscribe the handler to the specified signal forever.
-- To subscribe the disposable handlers, use function 'handleSignal'.
handleSignal_ :: MonadDES m => Signal m a -> (a -> Event m ()) -> Event m ()
{-# INLINE handleSignal_ #-}
handleSignal_ signal h = 
  do x <- handleSignal signal h
     return ()
     
-- | Create a new signal source.
newSignalSource :: MonadDES m => Simulation m (SignalSource m a)
{-# INLINABLE newSignalSource #-}
newSignalSource =
  do list <- newRef []
     let queue  = SignalHandlerQueue { queueList = list }
         signal = Signal { handleSignal = handle }
         source = SignalSource { publishSignal = signal, 
                                 triggerSignal = trigger }
         handle h =
           Event $ \p ->
           do x <- invokeEvent p $ enqueueSignalHandler queue h
              return $
                DisposableEvent $
                dequeueSignalHandler queue x
         trigger a =
           triggerSignalHandlers queue a
     return source
     
-- | Create a new signal source within more low level computation than 'Simulation'.
newSignalSource0 :: (MonadDES m, MonadRef0 m) => m (SignalSource m a)
{-# INLINABLE newSignalSource0 #-}
newSignalSource0 =
  do list <- newRef0 []
     let queue  = SignalHandlerQueue { queueList = list }
         signal = Signal { handleSignal = handle }
         source = SignalSource { publishSignal = signal, 
                                 triggerSignal = trigger }
         handle h =
           Event $ \p ->
           do x <- invokeEvent p $ enqueueSignalHandler queue h
              return $
                DisposableEvent $
                dequeueSignalHandler queue x
         trigger a =
           triggerSignalHandlers queue a
     return source

-- | Trigger all next signal handlers.
triggerSignalHandlers :: MonadDES m => SignalHandlerQueue m a -> a -> Event m ()
{-# INLINABLE triggerSignalHandlers #-}
triggerSignalHandlers q a =
  Event $ \p ->
  do hs <- invokeEvent p $ readRef (queueList q)
     forM_ hs $ \h ->
       invokeEvent p $ handlerComp h a
            
-- | Enqueue the handler and return its representative in the queue.            
enqueueSignalHandler :: MonadDES m => SignalHandlerQueue m a -> (a -> Event m ()) -> Event m (SignalHandler m a)
{-# INLINABLE enqueueSignalHandler #-}
enqueueSignalHandler q h =
  Event $ \p ->
  do r <- invokeSimulation (pointRun p) $ newRef ()
     let handler = SignalHandler { handlerComp = h,
                                   handlerRef  = r }
     invokeEvent p $ modifyRef (queueList q) (handler :)
     return handler

-- | Dequeue the handler representative.
dequeueSignalHandler :: MonadDES m => SignalHandlerQueue m a -> SignalHandler m a -> Event m ()
{-# INLINABLE dequeueSignalHandler #-}
dequeueSignalHandler q h = 
  modifyRef (queueList q) (delete h)

instance MonadDES m => Functor (Signal m) where

  {-# INLINE fmap #-}
  fmap = mapSignal
  
instance MonadDES m => Monoid (Signal m a) where 

  {-# INLINE mempty #-}
  mempty = emptySignal

  {-# INLINE mappend #-}
  mappend = merge2Signals

  {-# INLINABLE mconcat #-}
  mconcat [] = emptySignal
  mconcat [x1] = x1
  mconcat [x1, x2] = merge2Signals x1 x2
  mconcat [x1, x2, x3] = merge3Signals x1 x2 x3
  mconcat [x1, x2, x3, x4] = merge4Signals x1 x2 x3 x4
  mconcat [x1, x2, x3, x4, x5] = merge5Signals x1 x2 x3 x4 x5
  mconcat (x1 : x2 : x3 : x4 : x5 : xs) = 
    mconcat $ merge5Signals x1 x2 x3 x4 x5 : xs
  
-- | Map the signal according the specified function.
mapSignal :: MonadDES m => (a -> b) -> Signal m a -> Signal m b
{-# INLINABLE mapSignal #-}
mapSignal f m =
  Signal { handleSignal = \h -> 
            handleSignal m $ h . f }

-- | Filter only those signal values that satisfy 
-- the specified predicate.
filterSignal :: MonadDES m => (a -> Bool) -> Signal m a -> Signal m a
{-# INLINABLE filterSignal #-}
filterSignal p m =
  Signal { handleSignal = \h ->
            handleSignal m $ \a ->
            when (p a) $ h a }

-- | Filter only those signal values that satisfy 
-- the specified predicate, but then ignoring the values.
filterSignal_ :: MonadDES m => (a -> Bool) -> Signal m a -> Signal m ()
{-# INLINABLE filterSignal_ #-}
filterSignal_ p m =
  Signal { handleSignal = \h ->
            handleSignal m $ \a ->
            when (p a) $ h () }
  
-- | Filter only those signal values that satisfy 
-- the specified predicate.
filterSignalM :: MonadDES m => (a -> Event m Bool) -> Signal m a -> Signal m a
{-# INLINABLE filterSignalM #-}
filterSignalM p m =
  Signal { handleSignal = \h ->
            handleSignal m $ \a ->
            do x <- p a
               when x $ h a }
  
-- | Filter only those signal values that satisfy 
-- the specified predicate, but then ignoring the values.
filterSignalM_ :: MonadDES m => (a -> Event m Bool) -> Signal m a -> Signal m ()
{-# INLINABLE filterSignalM_ #-}
filterSignalM_ p m =
  Signal { handleSignal = \h ->
            handleSignal m $ \a ->
            do x <- p a
               when x $ h () }
  
-- | Merge two signals.
merge2Signals :: MonadDES m => Signal m a -> Signal m a -> Signal m a
{-# INLINABLE merge2Signals #-}
merge2Signals m1 m2 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               return $ x1 <> x2 }

-- | Merge three signals.
merge3Signals :: MonadDES m => Signal m a -> Signal m a -> Signal m a -> Signal m a
{-# INLINABLE merge3Signals #-}
merge3Signals m1 m2 m3 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               x3 <- handleSignal m3 h
               return $ x1 <> x2 <> x3 }

-- | Merge four signals.
merge4Signals :: MonadDES m
                 => Signal m a -> Signal m a -> Signal m a
                 -> Signal m a -> Signal m a
{-# INLINABLE merge4Signals #-}
merge4Signals m1 m2 m3 m4 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               x3 <- handleSignal m3 h
               x4 <- handleSignal m4 h
               return $ x1 <> x2 <> x3 <> x4 }
           
-- | Merge five signals.
merge5Signals :: MonadDES m
                 => Signal m a -> Signal m a -> Signal m a
                 -> Signal m a -> Signal m a -> Signal m a
{-# INLINABLE merge5Signals #-}
merge5Signals m1 m2 m3 m4 m5 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               x3 <- handleSignal m3 h
               x4 <- handleSignal m4 h
               x5 <- handleSignal m5 h
               return $ x1 <> x2 <> x3 <> x4 <> x5 }

-- | Compose the signal.
mapSignalM :: MonadDES m => (a -> Event m b) -> Signal m a -> Signal m b
{-# INLINABLE mapSignalM #-}
mapSignalM f m =
  Signal { handleSignal = \h ->
            handleSignal m (f >=> h) }
  
-- | Transform the signal.
apSignal :: MonadDES m => Event m (a -> b) -> Signal m a -> Signal m b
{-# INLINABLE apSignal #-}
apSignal f m =
  Signal { handleSignal = \h ->
            handleSignal m $ \a -> do { x <- f; h (x a) } }

-- | An empty signal which is never triggered.
emptySignal :: MonadDES m => Signal m a
{-# INLINABLE emptySignal #-}
emptySignal =
  Signal { handleSignal = \h -> return mempty }
     
-- | Trigger the signal with the current time.
triggerSignalWithCurrentTime :: MonadDES m => SignalSource m Double -> Event m ()
{-# INLINABLE triggerSignalWithCurrentTime #-}
triggerSignalWithCurrentTime s =
  Event $ \p -> invokeEvent p $ triggerSignal s (pointTime p)

-- | Return a signal that is triggered in the specified time points.
newSignalInTimes :: MonadDES m => [Double] -> Event m (Signal m Double)
{-# INLINABLE newSignalInTimes #-}
newSignalInTimes xs =
  do s <- liftSimulation newSignalSource
     enqueueEventWithTimes xs $ triggerSignalWithCurrentTime s
     return $ publishSignal s
       
-- | Return a signal that is triggered in the integration time points.
-- It should be called with help of 'runEventInStartTime'.
newSignalInIntegTimes :: MonadDES m => Event m (Signal m Double)
{-# INLINABLE newSignalInIntegTimes #-}
newSignalInIntegTimes =
  do s <- liftSimulation newSignalSource
     enqueueEventWithIntegTimes $ triggerSignalWithCurrentTime s
     return $ publishSignal s
     
-- | Return a signal that is triggered in the start time.
-- It should be called with help of 'runEventInStartTime'.
newSignalInStartTime :: MonadDES m => Event m (Signal m Double)
{-# INLINABLE newSignalInStartTime #-}
newSignalInStartTime =
  do s <- liftSimulation newSignalSource
     t <- liftParameter starttime
     enqueueEvent t $ triggerSignalWithCurrentTime s
     return $ publishSignal s

-- | Return a signal that is triggered in the final time.
newSignalInStopTime :: MonadDES m => Event m (Signal m Double)
{-# INLINABLE newSignalInStopTime #-}
newSignalInStopTime =
  do s <- liftSimulation newSignalSource
     t <- liftParameter stoptime
     enqueueEvent t $ triggerSignalWithCurrentTime s
     return $ publishSignal s

-- | Describes a computation that also signals when changing its value.
data Signalable m a =
  Signalable { readSignalable :: Event m a,
               -- ^ Return a computation of the value.
               signalableChanged_ :: Signal m ()
               -- ^ Return a signal notifying that the value has changed
               -- but without providing the information about the changed value.
             }

-- | Return a signal notifying that the value has changed.
signalableChanged :: MonadDES m => Signalable m a -> Signal m a
{-# INLINABLE signalableChanged #-}
signalableChanged x = mapSignalM (const $ readSignalable x) $ signalableChanged_ x

instance Functor m => Functor (Signalable m) where

  {-# INLINE fmap #-}
  fmap f x = x { readSignalable = fmap f (readSignalable x) }

instance (MonadDES m, Monoid a) => Monoid (Signalable m a) where

  {-# INLINE mempty #-}
  mempty = emptySignalable

  {-# INLINE mappend #-}
  mappend = appendSignalable

-- | Return an identity.
emptySignalable :: (MonadDES m, Monoid a) => Signalable m a
{-# INLINABLE emptySignalable #-}
emptySignalable =
  Signalable { readSignalable = return mempty,
               signalableChanged_ = mempty }

-- | An associative operation.
appendSignalable :: (MonadDES m, Monoid a) => Signalable m a -> Signalable m a -> Signalable m a
{-# INLINABLE appendSignalable #-}
appendSignalable m1 m2 =
  Signalable { readSignalable = liftM2 (<>) (readSignalable m1) (readSignalable m2),
               signalableChanged_ = (signalableChanged_ m1) <> (signalableChanged_ m2) }

-- | Transform a signal so that the resulting signal returns a sequence of arrivals
-- saving the information about the time points at which the original signal was received.
arrivalSignal :: MonadDES m => Signal m a -> Signal m (Arrival a)
{-# INLINABLE arrivalSignal #-}
arrivalSignal m = 
  Signal { handleSignal = \h ->
             do r <- liftSimulation $ newRef Nothing
                handleSignal m $ \a ->
                  Event $ \p ->
                  do t0 <- invokeEvent p $ readRef r
                     let t = pointTime p
                     invokeEvent p $ writeRef r (Just t)
                     invokeEvent p $
                       h Arrival { arrivalValue = a,
                                   arrivalTime  = t,
                                   arrivalDelay =
                                     case t0 of
                                       Nothing -> Nothing
                                       Just t0 -> Just (t - t0) } }

-- | Delay the signal values for the specified time interval.
delaySignal :: MonadDES m => Double -> Signal m a -> Signal m a
{-# INLINABLE delaySignal #-}
delaySignal delta m =
  Signal { handleSignal = \h ->
            do r <- liftSimulation $ newRef False
               h <- handleSignal m $ \a ->
                 Event $ \p ->
                 invokeEvent p $
                 enqueueEvent (pointTime p + delta) $ 
                 do x <- readRef r
                    unless x $ h a
               return $ DisposableEvent $
                 disposeEvent h >>
                 writeRef r True
         }

-- | Delay the signal values for time intervals recalculated for each value.
delaySignalM :: MonadDES m => Event m Double -> Signal m a -> Signal m a
{-# INLINABLE delaySignalM #-}
delaySignalM delta m =
  Signal { handleSignal = \h ->
            do r <- liftSimulation $ newRef False
               h <- handleSignal m $ \a ->
                 Event $ \p ->
                 do delta' <- invokeEvent p delta
                    invokeEvent p $
                      enqueueEvent (pointTime p + delta') $ 
                      do x <- readRef r
                         unless x $ h a
               return $ DisposableEvent $
                 disposeEvent h >>
                 writeRef r True
         }

-- | Show the debug message with the current simulation time.
traceSignal :: MonadDES m => String -> Signal m a -> Signal m a 
{-# INLINABLE traceSignal #-}
traceSignal message m =
  Signal { handleSignal = \h ->
            handleSignal m $ traceEvent message . h }
