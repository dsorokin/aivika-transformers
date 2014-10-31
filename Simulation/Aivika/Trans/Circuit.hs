
{-# LANGUAGE RecursiveDo, Arrows #-}

-- |
-- Module     : Simulation.Aivika.Trans.Circuit
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It represents a circuit synchronized with the event queue.
-- Also it allows creating the recursive links with help of
-- the proc-notation.
--
-- The implementation is based on the <http://en.wikibooks.org/wiki/Haskell/Arrow_tutorial Arrow Tutorial>.
--
module Simulation.Aivika.Trans.Circuit
       (-- * The Circuit Arrow
        Circuit(..),
        iterateCircuitInIntegTimes,
        iterateCircuitInIntegTimes_,
        iterateCircuitInIntegTimesMaybe,
        iterateCircuitInIntegTimesEither,
        iterateCircuitInTimes,
        iterateCircuitInTimes_,
        iterateCircuitInTimesMaybe,
        iterateCircuitInTimesEither,
        -- * Circuit Primitives
        arrCircuit,
        accumCircuit,
        -- * The Arrival Circuit
        arrivalCircuit,
        -- * Delaying the Circuit
        delayCircuit,
        -- * The Time Circuit
        timeCircuit,
        -- * Conditional Computation
        (<?<),
        (>?>),
        filterCircuit,
        filterCircuitM,
        neverCircuit,
        -- * Converting to Signals and Processors
        circuitSignaling,
        circuitProcessor,
        -- * Integrals and Difference Equations
        integCircuit,
        integCircuitEither,
        sumCircuit,
        sumCircuitEither,
        -- * The Circuit Transform
        circuitTransform) where

import qualified Control.Category as C
import Control.Arrow
import Control.Monad.Fix

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Dynamics.Memo
import Simulation.Aivika.Trans.Transform
import Simulation.Aivika.Trans.SystemDynamics
import Simulation.Aivika.Trans.Signal
import Simulation.Aivika.Trans.Stream
import Simulation.Aivika.Trans.Process
import Simulation.Aivika.Trans.Processor
import Simulation.Aivika.Trans.Task
import Simulation.Aivika.Arrival (Arrival(..))

-- | Represents a circuit synchronized with the event queue.
-- Besides, it allows creating the recursive links with help of
-- the proc-notation.
--
newtype Circuit m a b =
  Circuit { runCircuit :: a -> Event m (b, Circuit m a b)
            -- ^ Run the circuit.
          }

instance MonadComp m => C.Category (Circuit m) where

  id = Circuit $ \a -> return (a, C.id)

  (.) = dot
    where 
      (Circuit g) `dot` (Circuit f) =
        Circuit $ \a ->
        Event $ \p ->
        do (b, cir1) <- invokeEvent p (f a)
           (c, cir2) <- invokeEvent p (g b)
           return (c, cir2 `dot` cir1)

instance MonadComp m => Arrow (Circuit m) where

  arr f = Circuit $ \a -> return (f a, arr f)

  first (Circuit f) =
    Circuit $ \(b, d) ->
    Event $ \p ->
    do (c, cir) <- invokeEvent p (f b)
       return ((c, d), first cir)

  second (Circuit f) =
    Circuit $ \(d, b) ->
    Event $ \p ->
    do (c, cir) <- invokeEvent p (f b)
       return ((d, c), second cir)

  (Circuit f) *** (Circuit g) =
    Circuit $ \(b, b') ->
    Event $ \p ->
    do (c, cir1) <- invokeEvent p (f b)
       (c', cir2) <- invokeEvent p (g b')
       return ((c, c'), cir1 *** cir2)
       
  (Circuit f) &&& (Circuit g) =
    Circuit $ \b ->
    Event $ \p ->
    do (c, cir1) <- invokeEvent p (f b)
       (c', cir2) <- invokeEvent p (g b)
       return ((c, c'), cir1 &&& cir2)

instance (MonadComp m, MonadFix m) => ArrowLoop (Circuit m) where

  loop (Circuit f) =
    Circuit $ \b ->
    Event $ \p ->
    do rec ((c, d), cir) <- invokeEvent p (f (b, d))
       return (c, loop cir)

instance MonadComp m => ArrowChoice (Circuit m) where

  left x@(Circuit f) =
    Circuit $ \ebd ->
    Event $ \p ->
    case ebd of
      Left b ->
        do (c, cir) <- invokeEvent p (f b)
           return (Left c, left cir)
      Right d ->
        return (Right d, left x)

  right x@(Circuit f) =
    Circuit $ \edb ->
    Event $ \p ->
    case edb of
      Right b ->
        do (c, cir) <- invokeEvent p (f b)
           return (Right c, right cir)
      Left d ->
        return (Left d, right x)

  x@(Circuit f) +++ y@(Circuit g) =
    Circuit $ \ebb' ->
    Event $ \p ->
    case ebb' of
      Left b ->
        do (c, cir1) <- invokeEvent p (f b)
           return (Left c, cir1 +++ y)
      Right b' ->
        do (c', cir2) <- invokeEvent p (g b')
           return (Right c', x +++ cir2)

  x@(Circuit f) ||| y@(Circuit g) =
    Circuit $ \ebc ->
    Event $ \p ->
    case ebc of
      Left b ->
        do (d, cir1) <- invokeEvent p (f b)
           return (d, cir1 ||| y)
      Right b' ->
        do (d, cir2) <- invokeEvent p (g b')
           return (d, x ||| cir2)

-- | Get a signal transform by the specified circuit.
circuitSignaling :: MonadComp m => Circuit m a b -> Signal m a -> Signal m b
circuitSignaling (Circuit cir) sa =
  Signal { handleSignal = \f ->
            Event $ \p ->
            do let s = runSession (pointRun p)
               r <- newProtoRef s cir
               invokeEvent p $
                 handleSignal sa $ \a ->
                 Event $ \p ->
                 do cir <- readProtoRef r
                    (b, Circuit cir') <- invokeEvent p (cir a)
                    writeProtoRef r cir'
                    invokeEvent p (f b) }

-- | Transform the circuit to a processor.
circuitProcessor :: MonadComp m => Circuit m a b -> Processor m a b
circuitProcessor (Circuit cir) = Processor $ \sa ->
  Cons $
  do (a, xs) <- runStream sa
     (b, cir') <- liftEvent (cir a)
     let f = runProcessor (circuitProcessor cir')
     return (b, f xs)

-- | Create a simple circuit by the specified handling function
-- that runs the computation for each input value to get an output.
arrCircuit :: MonadComp m => (a -> Event m b) -> Circuit m a b
arrCircuit f =
  let x =
        Circuit $ \a ->
        Event $ \p ->
        do b <- invokeEvent p (f a)
           return (b, x)
  in x

-- | Accumulator that outputs a value determined by the supplied function.
accumCircuit :: MonadComp m => (acc -> a -> Event m (acc, b)) -> acc -> Circuit m a b
accumCircuit f acc =
  Circuit $ \a ->
  Event $ \p ->
  do (acc', b) <- invokeEvent p (f acc a)
     return (b, accumCircuit f acc') 

-- | A circuit that adds the information about the time points at which 
-- the values were received.
arrivalCircuit :: MonadComp m => Circuit m a (Arrival a)
arrivalCircuit =
  let loop t0 =
        Circuit $ \a ->
        Event $ \p ->
        let t = pointTime p
            b = Arrival { arrivalValue = a,
                          arrivalTime  = t,
                          arrivalDelay = 
                            case t0 of
                              Nothing -> Nothing
                              Just t0 -> Just (t - t0) }
        in return (b, loop $ Just t)
  in loop Nothing

-- | Delay the input by one step using the specified initial value.
delayCircuit :: MonadComp m => a -> Circuit m a a
delayCircuit a0 =
  Circuit $ \a ->
  return (a0, delayCircuit a)

-- | A circuit that returns the current modeling time.
timeCircuit :: MonadComp m => Circuit m a Double
timeCircuit =
  Circuit $ \a ->
  Event $ \p ->
  return (pointTime p, timeCircuit)

-- | Like '>>>' but processes only the represented events.
(>?>) :: MonadComp m
         => Circuit m a (Maybe b)
         -- ^ whether there is an event
         -> Circuit m b c
         -- ^ process the event if it presents
         -> Circuit m a (Maybe c)
         -- ^ the resulting circuit that processes only the represented events
whether >?> process =
  Circuit $ \a ->
  Event $ \p ->
  do (b, whether') <- invokeEvent p (runCircuit whether a)
     case b of
       Nothing ->
         return (Nothing, whether' >?> process)
       Just b  ->
         do (c, process') <- invokeEvent p (runCircuit process b)
            return (Just c, whether' >?> process')

-- | Like '<<<' but processes only the represented events.
(<?<) :: MonadComp m
         => Circuit m b c
         -- ^ process the event if it presents
         -> Circuit m a (Maybe b)
         -- ^ whether there is an event
         -> Circuit m a (Maybe c)
         -- ^ the resulting circuit that processes only the represented events
(<?<) = flip (>?>)

-- | Filter the circuit, calculating only those parts of the circuit that satisfy
-- the specified predicate.
filterCircuit :: MonadComp m => (a -> Bool) -> Circuit m a b -> Circuit m a (Maybe b)
filterCircuit pred = filterCircuitM (return . pred)

-- | Filter the circuit within the 'Event' computation, calculating only those parts
-- of the circuit that satisfy the specified predicate.
filterCircuitM :: MonadComp m => (a -> Event m Bool) -> Circuit m a b -> Circuit m a (Maybe b)
filterCircuitM pred cir =
  Circuit $ \a ->
  Event $ \p ->
  do x <- invokeEvent p (pred a)
     if x
       then do (b, cir') <- invokeEvent p (runCircuit cir a)
               return (Just b, filterCircuitM pred cir')
       else return (Nothing, filterCircuitM pred cir)

-- | The source of events that never occur.
neverCircuit :: MonadComp m => Circuit m a (Maybe b)
neverCircuit =
  Circuit $ \a -> return (Nothing, neverCircuit)

-- | An approximation of the integral using Euler's method.
--
-- This function can be rather inaccurate as it depends on
-- the time points at wich the 'Circuit' computation is actuated.
-- Also Euler's method per se is not most accurate, although simple
-- enough for implementation.
--
-- Consider using the 'integ' function whenever possible.
-- That function can integrate with help of the Runge-Kutta method by
-- the specified integration time points that are passed in the simulation
-- specs to every 'Simulation', when running the model.
--
-- At the same time, the 'integCircuit' function has no mutable state
-- unlike the former. The latter consumes less memory but at the cost
-- of inaccuracy and relatively more slow simulation, had we requested
-- the integral in the same time points.
--
-- Regarding the recursive equations, the both functions allow defining them
-- but whithin different computations (either with help of the recursive
-- do-notation or the proc-notation).
integCircuit :: MonadComp m
                => Double
                -- ^ the initial value
                -> Circuit m Double Double
                -- ^ map the derivative to an integral
integCircuit init = start
  where
    start = 
      Circuit $ \a ->
      Event $ \p ->
      do let t = pointTime p
         return (init, next t init a)
    next t0 v0 a0 =
      Circuit $ \a ->
      Event $ \p ->
      do let t  = pointTime p
             dt = t - t0
             v  = v0 + a0 * dt
         v `seq` return (v, next t v a)

-- | Like 'integCircuit' but allows either setting a new 'Left' integral value,
-- or using the 'Right' derivative when integrating by Euler's method.
integCircuitEither :: MonadComp m
                      => Double
                      -- ^ the initial value
                      -> Circuit m (Either Double Double) Double
                      -- ^ map either a new 'Left' value or
                      -- the 'Right' derivative to an integral
integCircuitEither init = start
  where
    start = 
      Circuit $ \a ->
      Event $ \p ->
      do let t = pointTime p
         return (init, next t init a)
    next t0 v0 a0 =
      Circuit $ \a ->
      Event $ \p ->
      do let t = pointTime p
         case a0 of
           Left v ->
             v `seq` return (v, next t v a)
           Right a0 -> do
             let dt = t - t0
                 v  = v0 + a0 * dt
             v `seq` return (v, next t v a)

-- | A sum of differences starting from the specified initial value.
--
-- Consider using the more accurate 'diffsum' function whener possible as
-- it is calculated in every integration time point specified by specs
-- passed in to every 'Simulation', when running the model.
--
-- At the same time, the 'sumCircuit' function has no mutable state and
-- it consumes less memory than the former.
--
-- Regarding the recursive equations, the both functions allow defining them
-- but whithin different computations (either with help of the recursive
-- do-notation or the proc-notation).
sumCircuit :: (MonadComp m, Num a)
              => a
              -- ^ the initial value
              -> Circuit m a a
              -- ^ map the difference to a sum
sumCircuit init = start
  where
    start = 
      Circuit $ \a ->
      Event $ \p ->
      return (init, next init a)
    next v0 a0 =
      Circuit $ \a ->
      Event $ \p ->
      do let v = v0 + a0
         v `seq` return (v, next v a)

-- | Like 'sumCircuit' but allows either setting a new 'Left' value for the sum, or updating it
-- by specifying the 'Right' difference.
sumCircuitEither :: (MonadComp m, Num a)
                    => a
                    -- ^ the initial value
                    -> Circuit m (Either a a) a
                    -- ^ map either a new 'Left' value or
                    -- the 'Right' difference to a sum
sumCircuitEither init = start
  where
    start = 
      Circuit $ \a ->
      Event $ \p ->
      return (init, next init a)
    next v0 a0 =
      Circuit $ \a ->
      Event $ \p ->
      case a0 of
        Left v ->
          v `seq` return (v, next v a)
        Right a0 -> do
          let v = v0 + a0
          v `seq` return (v, next v a)

-- | Approximate the circuit as a transform of time varying function,
-- calculating the values in the integration time points and then
-- interpolating in all other time points. The resulting transform
-- computation is synchronized with the event queue.         
--
-- This procedure consumes memory as the underlying memoization allocates
-- an array to store the calculated values.
circuitTransform :: MonadComp m => Circuit m a b -> Transform m a b
circuitTransform cir = Transform start
  where
    start m =
      Simulation $ \r ->
      do let s = runSession r
         ref <- newProtoRef s cir
         invokeSimulation r $
           memo0Dynamics (next ref m)
    next ref m =
      Dynamics $ \p ->
      do a <- invokeDynamics p m
         cir <- readProtoRef ref
         (b, cir') <-
           invokeDynamics p $
           runEvent (runCircuit cir a)
         writeProtoRef ref cir'
         return b

-- | Iterate the circuit in the specified time points.
iterateCircuitInPoints_ :: MonadComp m => [Point m] -> Circuit m a a -> a -> Event m ()
iterateCircuitInPoints_ [] cir a = return ()
iterateCircuitInPoints_ (p : ps) cir a =
  enqueueEvent (pointTime p) $
  Event $ \p' ->
  do (a', cir') <- invokeEvent p $ runCircuit cir a
     invokeEvent p $ iterateCircuitInPoints_ ps cir' a'

-- | Iterate the circuit in the specified time points returning a task
-- which completes after the final output of the circuit is received.
iterateCircuitInPoints :: MonadComp m => [Point m] -> Circuit m a a -> a -> Event m (Task m a)
iterateCircuitInPoints ps cir a =
  do let loop [] cir a source = triggerSignal source a
         loop (p : ps) cir a source =
           enqueueEvent (pointTime p) $
           Event $ \p' ->
           do (a', cir') <- invokeEvent p $ runCircuit cir a
              invokeEvent p $ loop ps cir' a' source
     source <- liftSimulation newSignalSource
     task <- runTask $ processAwait $ publishSignal source
     loop ps cir a source
     return task

-- | Iterate the circuit in the integration time points.
iterateCircuitInIntegTimes_ :: MonadComp m => Circuit m a a -> a -> Event m ()
iterateCircuitInIntegTimes_ cir a =
  Event $ \p ->
  do let ps = nextIntegPoints (pointRun p) (pointIteration p + 1)
     invokeEvent p $ 
       iterateCircuitInPoints_ ps cir a

-- | Iterate the circuit in the specified time points.
iterateCircuitInTimes_ :: MonadComp m => [Double] -> Circuit m a a -> a -> Event m ()
iterateCircuitInTimes_ ts cir a =
  Event $ \p ->
  do let ps = map (pointAt $ pointRun p) ts
     invokeEvent p $ 
       iterateCircuitInPoints_ ps cir a 

-- | Iterate the circuit in the integration time points returning a task
-- which completes after the final output of the circuit is received.
iterateCircuitInIntegTimes :: MonadComp m => Circuit m a a -> a -> Event m (Task m a)
iterateCircuitInIntegTimes cir a =
  Event $ \p ->
  do let ps = nextIntegPoints (pointRun p) (pointIteration p + 1)
     invokeEvent p $ 
       iterateCircuitInPoints ps cir a

-- | Iterate the circuit in the specified time points returning a task
-- which completes after the final output of the circuit is received.
iterateCircuitInTimes :: MonadComp m => [Double] -> Circuit m a a -> a -> Event m (Task m a)
iterateCircuitInTimes ts cir a =
  Event $ \p ->
  do let ps = map (pointAt $ pointRun p) ts
     invokeEvent p $ 
       iterateCircuitInPoints ps cir a 

-- | Iterate the circuit in the specified time points, interrupting the iteration
-- immediately if 'Nothing' is returned within the 'Circuit' computation.
iterateCircuitInPointsMaybe :: MonadComp m => [Point m] -> Circuit m a (Maybe a) -> a -> Event m ()
iterateCircuitInPointsMaybe [] cir a = return ()
iterateCircuitInPointsMaybe (p : ps) cir a =
  enqueueEvent (pointTime p) $
  Event $ \p' ->
  do (a', cir') <- invokeEvent p $ runCircuit cir a
     case a' of
       Nothing -> return ()
       Just a' ->
         invokeEvent p $ iterateCircuitInPointsMaybe ps cir' a'

-- | Iterate the circuit in the integration time points, interrupting the iteration
-- immediately if 'Nothing' is returned within the 'Circuit' computation.
iterateCircuitInIntegTimesMaybe :: MonadComp m => Circuit m a (Maybe a) -> a -> Event m ()
iterateCircuitInIntegTimesMaybe cir a =
  Event $ \p ->
  do let ps = nextIntegPoints (pointRun p) (pointIteration p + 1)
     invokeEvent p $ 
       iterateCircuitInPointsMaybe ps cir a

-- | Iterate the circuit in the specified time points, interrupting the iteration
-- immediately if 'Nothing' is returned within the 'Circuit' computation.
iterateCircuitInTimesMaybe :: MonadComp m => [Double] -> Circuit m a (Maybe a) -> a -> Event m ()
iterateCircuitInTimesMaybe ts cir a =
  Event $ \p ->
  do let ps = map (pointAt $ pointRun p) ts
     invokeEvent p $ 
       iterateCircuitInPointsMaybe ps cir a

-- | Iterate the circuit in the specified time points returning a task
-- that computes the final output of the circuit either after all points
-- are exhausted, or after the 'Left' result of type @b@ is received,
-- which interrupts the computation immediately.
iterateCircuitInPointsEither :: MonadComp m => [Point m] -> Circuit m a (Either b a) -> a -> Event m (Task m (Either b a))
iterateCircuitInPointsEither ps cir a =
  do let loop [] cir ba source = triggerSignal source ba
         loop ps cir ba@(Left b) source = triggerSignal source ba 
         loop (p : ps) cir (Right a) source =
           enqueueEvent (pointTime p) $
           Event $ \p' ->
           do (ba', cir') <- invokeEvent p $ runCircuit cir a
              invokeEvent p $ loop ps cir' ba' source
     source <- liftSimulation newSignalSource
     task <- runTask $ processAwait $ publishSignal source
     loop ps cir (Right a) source
     return task

-- | Iterate the circuit in the integration time points returning a task
-- that computes the final output of the circuit either after all points
-- are exhausted, or after the 'Left' result of type @b@ is received,
-- which interrupts the computation immediately.
iterateCircuitInIntegTimesEither :: MonadComp m => Circuit m a (Either b a) -> a -> Event m (Task m (Either b a))
iterateCircuitInIntegTimesEither cir a =
  Event $ \p ->
  do let ps = nextIntegPoints (pointRun p) (pointIteration p + 1)
     invokeEvent p $ 
       iterateCircuitInPointsEither ps cir a

-- | Iterate the circuit in the specified time points returning a task
-- that computes the final output of the circuit either after all points
-- are exhausted, or after the 'Left' result of type @b@ is received,
-- which interrupts the computation immediately.
iterateCircuitInTimesEither :: MonadComp m => [Double] -> Circuit m a (Either b a) -> a -> Event m (Task m (Either b a))
iterateCircuitInTimesEither ts cir a =
  Event $ \p ->
  do let ps = map (pointAt $ pointRun p) ts
     invokeEvent p $ 
       iterateCircuitInPointsEither ps cir a
