
-- |
-- Module     : Simulation.Aivika.Trans.Signal
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
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
        publishSignal,
        triggerSignal,
        -- * Useful Combinators
        mapSignal,
        mapSignalM,
        apSignal,
        filterSignal,
        filterSignalM,
        emptySignal,
        merge2Signals,
        merge3Signals,
        merge4Signals,
        merge5Signals,
        -- * Signal Arriving
        arrivalSignal,
        -- * Creating Signal in Time Points
        newSignalInTimes,
        newSignalInIntegTimes,
        newSignalInStartTime,
        newSignalInStopTime,
        -- * Signal History
        SignalHistory,
        signalHistorySignal,
        newSignalHistory,
        newSignalHistoryStartingWith,
        readSignalHistory,
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

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import qualified Simulation.Aivika.Trans.Vector as V
import qualified Simulation.Aivika.Trans.Vector.Unboxed as UV
import Simulation.Aivika.Trans.Comp
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
  SignalHandlerQueue { queueList :: ProtoRef m [SignalHandler m a] }
  
-- | It contains the information about the disposable queue handler.
data SignalHandler m a =
  SignalHandler { handlerComp   :: a -> Event m (),
                  handlerMarker :: SessionMarker m }

instance SessionMonad m => Eq (SignalHandler m a) where

  {-# INLINE (==) #-}
  x == y = (handlerMarker x) == (handlerMarker y)

-- | Subscribe the handler to the specified signal forever.
-- To subscribe the disposable handlers, use function 'handleSignal'.
handleSignal_ :: MonadComp m => Signal m a -> (a -> Event m ()) -> Event m ()
{-# INLINE handleSignal_ #-}
handleSignal_ signal h = 
  do x <- handleSignal signal h
     return ()
     
-- | Create a new signal source.
newSignalSource :: MonadComp m => Simulation m (SignalSource m a)
newSignalSource =
  Simulation $ \r ->
  do let s = runSession r
     list <- newProtoRef s []
     let queue  = SignalHandlerQueue { queueList = list }
         signal = Signal { handleSignal = handle }
         source = SignalSource { publishSignal = signal, 
                                 triggerSignal = trigger }
         handle h =
           Event $ \p ->
           do m <- newSessionMarker s
              x <- enqueueSignalHandler queue h m
              return $
                DisposableEvent $
                Event $ \p -> dequeueSignalHandler queue x
         trigger a =
           Event $ \p -> triggerSignalHandlers queue a p
     return source

-- | Trigger all next signal handlers.
triggerSignalHandlers :: MonadComp m => SignalHandlerQueue m a -> a -> Point m -> m ()
triggerSignalHandlers q a p =
  do hs <- readProtoRef (queueList q)
     forM_ hs $ \h ->
       invokeEvent p $ handlerComp h a
            
-- | Enqueue the handler and return its representative in the queue.            
enqueueSignalHandler :: MonadComp m => SignalHandlerQueue m a -> (a -> Event m ()) -> SessionMarker m -> m (SignalHandler m a)
enqueueSignalHandler q h m = 
  do let handler = SignalHandler { handlerComp   = h,
                                   handlerMarker = m }
     modifyProtoRef (queueList q) (handler :)
     return handler

-- | Dequeue the handler representative.
dequeueSignalHandler :: MonadComp m => SignalHandlerQueue m a -> SignalHandler m a -> m ()
dequeueSignalHandler q h = 
  modifyProtoRef (queueList q) (delete h)

instance MonadComp m => Functor (Signal m) where

  {-# INLINE fmap #-}
  fmap = mapSignal
  
instance MonadComp m => Monoid (Signal m a) where 

  {-# INLINE mempty #-}
  mempty = emptySignal

  {-# INLINE mappend #-}
  mappend = merge2Signals

  {-# INLINE mconcat #-}
  mconcat [] = emptySignal
  mconcat [x1] = x1
  mconcat [x1, x2] = merge2Signals x1 x2
  mconcat [x1, x2, x3] = merge3Signals x1 x2 x3
  mconcat [x1, x2, x3, x4] = merge4Signals x1 x2 x3 x4
  mconcat [x1, x2, x3, x4, x5] = merge5Signals x1 x2 x3 x4 x5
  mconcat (x1 : x2 : x3 : x4 : x5 : xs) = 
    mconcat $ merge5Signals x1 x2 x3 x4 x5 : xs
  
-- | Map the signal according the specified function.
mapSignal :: MonadComp m => (a -> b) -> Signal m a -> Signal m b
mapSignal f m =
  Signal { handleSignal = \h -> 
            handleSignal m $ h . f }

-- | Filter only those signal values that satisfy to 
-- the specified predicate.
filterSignal :: MonadComp m => (a -> Bool) -> Signal m a -> Signal m a
filterSignal p m =
  Signal { handleSignal = \h ->
            handleSignal m $ \a ->
            when (p a) $ h a }
  
-- | Filter only those signal values that satisfy to 
-- the specified predicate.
filterSignalM :: MonadComp m => (a -> Event m Bool) -> Signal m a -> Signal m a
filterSignalM p m =
  Signal { handleSignal = \h ->
            handleSignal m $ \a ->
            do x <- p a
               when x $ h a }
  
-- | Merge two signals.
merge2Signals :: MonadComp m => Signal m a -> Signal m a -> Signal m a
merge2Signals m1 m2 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               return $ x1 <> x2 }

-- | Merge three signals.
merge3Signals :: MonadComp m => Signal m a -> Signal m a -> Signal m a -> Signal m a
merge3Signals m1 m2 m3 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               x3 <- handleSignal m3 h
               return $ x1 <> x2 <> x3 }

-- | Merge four signals.
merge4Signals :: MonadComp m
                 => Signal m a -> Signal m a -> Signal m a
                 -> Signal m a -> Signal m a
merge4Signals m1 m2 m3 m4 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               x3 <- handleSignal m3 h
               x4 <- handleSignal m4 h
               return $ x1 <> x2 <> x3 <> x4 }
           
-- | Merge five signals.
merge5Signals :: MonadComp m
                 => Signal m a -> Signal m a -> Signal m a
                 -> Signal m a -> Signal m a -> Signal m a
merge5Signals m1 m2 m3 m4 m5 =
  Signal { handleSignal = \h ->
            do x1 <- handleSignal m1 h
               x2 <- handleSignal m2 h
               x3 <- handleSignal m3 h
               x4 <- handleSignal m4 h
               x5 <- handleSignal m5 h
               return $ x1 <> x2 <> x3 <> x4 <> x5 }

-- | Compose the signal.
mapSignalM :: MonadComp m => (a -> Event m b) -> Signal m a -> Signal m b
mapSignalM f m =
  Signal { handleSignal = \h ->
            handleSignal m (f >=> h) }
  
-- | Transform the signal.
apSignal :: MonadComp m => Event m (a -> b) -> Signal m a -> Signal m b
apSignal f m =
  Signal { handleSignal = \h ->
            handleSignal m $ \a -> do { x <- f; h (x a) } }

-- | An empty signal which is never triggered.
emptySignal :: MonadComp m => Signal m a
emptySignal =
  Signal { handleSignal = \h -> return mempty }
                                    
-- | Represents the history of the signal values.
data SignalHistory m a =
  SignalHistory { signalHistorySignal :: Signal m a,  
                  -- ^ The signal for which the history is created.
                  signalHistoryTimes  :: UV.Vector m Double,
                  signalHistoryValues :: V.Vector m a }

-- | Create a history of the signal values.
newSignalHistory :: MonadComp m => Signal m a -> Event m (SignalHistory m a)
newSignalHistory =
  newSignalHistoryStartingWith Nothing

-- | Create a history of the signal values starting with
-- the optional initial value.
newSignalHistoryStartingWith :: MonadComp m => Maybe a -> Signal m a -> Event m (SignalHistory m a)
newSignalHistoryStartingWith init signal =
  Event $ \p ->
  do let s = runSession $ pointRun p
     ts <- UV.newVector s
     xs <- V.newVector s
     case init of
       Nothing -> return ()
       Just a ->
         do UV.appendVector ts (pointTime p)
            V.appendVector xs a
     invokeEvent p $
       handleSignal_ signal $ \a ->
       Event $ \p ->
       do UV.appendVector ts (pointTime p)
          V.appendVector xs a
     return SignalHistory { signalHistorySignal = signal,
                            signalHistoryTimes  = ts,
                            signalHistoryValues = xs }
       
-- | Read the history of signal values.
readSignalHistory :: MonadComp m => SignalHistory m a -> Event m (Array Int Double, Array Int a)
readSignalHistory history =
  Event $ \p ->
  do xs <- UV.freezeVector (signalHistoryTimes history)
     ys <- V.freezeVector (signalHistoryValues history)
     return (xs, ys)     
     
-- | Trigger the signal with the current time.
triggerSignalWithCurrentTime :: MonadComp m => SignalSource m Double -> Event m ()
triggerSignalWithCurrentTime s =
  Event $ \p -> invokeEvent p $ triggerSignal s (pointTime p)

-- | Return a signal that is triggered in the specified time points.
newSignalInTimes :: MonadComp m => [Double] -> Event m (Signal m Double)
newSignalInTimes xs =
  do s <- liftSimulation newSignalSource
     enqueueEventWithTimes xs $ triggerSignalWithCurrentTime s
     return $ publishSignal s
       
-- | Return a signal that is triggered in the integration time points.
-- It should be called with help of 'runEventInStartTime'.
newSignalInIntegTimes :: MonadComp m => Event m (Signal m Double)
newSignalInIntegTimes =
  do s <- liftSimulation newSignalSource
     enqueueEventWithIntegTimes $ triggerSignalWithCurrentTime s
     return $ publishSignal s
     
-- | Return a signal that is triggered in the start time.
-- It should be called with help of 'runEventInStartTime'.
newSignalInStartTime :: MonadComp m => Event m (Signal m Double)
newSignalInStartTime =
  do s <- liftSimulation newSignalSource
     t <- liftParameter starttime
     enqueueEvent t $ triggerSignalWithCurrentTime s
     return $ publishSignal s

-- | Return a signal that is triggered in the final time.
newSignalInStopTime :: MonadComp m => Event m (Signal m Double)
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
signalableChanged :: MonadComp m => Signalable m a -> Signal m a
signalableChanged x = mapSignalM (const $ readSignalable x) $ signalableChanged_ x

instance Functor m => Functor (Signalable m) where

  {-# INLINE fmap #-}
  fmap f x = x { readSignalable = fmap f (readSignalable x) }

instance (MonadComp m, Monoid a) => Monoid (Signalable m a) where

  {-# INLINE mempty #-}
  mempty = emptySignalable

  {-# INLINE mappend #-}
  mappend = appendSignalable

-- | Return an identity.
emptySignalable :: (MonadComp m, Monoid a) => Signalable m a
emptySignalable =
  Signalable { readSignalable = return mempty,
               signalableChanged_ = mempty }

-- | An associative operation.
appendSignalable :: (MonadComp m, Monoid a) => Signalable m a -> Signalable m a -> Signalable m a
appendSignalable m1 m2 =
  Signalable { readSignalable = liftM2 (<>) (readSignalable m1) (readSignalable m2),
               signalableChanged_ = (signalableChanged_ m1) <> (signalableChanged_ m2) }

-- | Transform a signal so that the resulting signal returns a sequence of arrivals
-- saving the information about the time points at which the original signal was received.
arrivalSignal :: MonadComp m => Signal m a -> Signal m (Arrival a)
arrivalSignal m = 
  Signal { handleSignal = \h ->
             Event $ \p ->
             do let s = runSession $ pointRun p
                r <- newProtoRef s Nothing
                invokeEvent p $
                  handleSignal m $ \a ->
                  Event $ \p ->
                  do t0 <- readProtoRef r
                     let t = pointTime p
                     writeProtoRef r (Just t)
                     invokeEvent p $
                       h Arrival { arrivalValue = a,
                                   arrivalTime  = t,
                                   arrivalDelay =
                                     case t0 of
                                       Nothing -> Nothing
                                       Just t0 -> Just (t - t0) } }

-- | Show the debug message with the current simulation time.
traceSignal :: MonadComp m => String -> Signal m a -> Signal m a 
traceSignal message m =
  Signal { handleSignal = \h ->
            handleSignal m $ traceEvent message . h }
