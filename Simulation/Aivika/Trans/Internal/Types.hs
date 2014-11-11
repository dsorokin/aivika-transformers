
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Trans.Internal.Types
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It defines the implementation details of some types.
--
module Simulation.Aivika.Trans.Internal.Types
       (Specs(..),
        Method(..),
        Run(..),
        Point(..),
        Parameter(..),
        Simulation(..),
        Dynamics(..),
        Event(..),
        EventProcessing(..),
        EventQueueing(..),
        invokeParameter,
        invokeSimulation,
        invokeDynamics,
        invokeEvent) where

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.Generator

-- | It defines the simulation specs.
data Specs m = Specs { spcStartTime :: Double,    -- ^ the start time
                       spcStopTime :: Double,     -- ^ the stop time
                       spcDT :: Double,           -- ^ the integration time step
                       spcMethod :: Method,       -- ^ the integration method
                       spcGeneratorType :: GeneratorType m
                       -- ^ the type of the random number generator
                     }

-- | It defines the integration method.
data Method = Euler          -- ^ Euler's method
            | RungeKutta2    -- ^ the 2nd order Runge-Kutta method
            | RungeKutta4    -- ^ the 4th order Runge-Kutta method
            deriving (Eq, Ord, Show)

-- | It indentifies the simulation run.
data Run m = Run { runSpecs :: Specs m,            -- ^ the simulation specs
                   runSession :: Session m,        -- ^ the simulation session
                   runIndex :: Int,       -- ^ the current simulation run index
                   runCount :: Int,       -- ^ the total number of runs in this experiment
                   runEventQueue :: EventQueue m,  -- ^ the event queue
                   runGenerator :: Generator m     -- ^ the random number generator
                 }

-- | It defines the simulation point appended with the additional information.
data Point m = Point { pointSpecs :: Specs m,      -- ^ the simulation specs
                       pointRun :: Run m,          -- ^ the simulation run
                       pointTime :: Double,        -- ^ the current time
                       pointIteration :: Int,      -- ^ the current iteration
                       pointPhase :: Int           -- ^ the current phase
                     }

-- | The 'Parameter' monad that allows specifying the model parameters.
-- For example, they can be used when running the Monte-Carlo simulation.
-- 
-- In general, this monad is very useful for representing a computation which is external
-- relative to the model itself.
newtype Parameter m a = Parameter (Run m -> m a)

-- | A value in the 'Simulation' monad represents a computation
-- within the simulation run.
newtype Simulation m a = Simulation (Run m -> m a)

-- | A value in the 'Dynamics' monad represents a polymorphic time varying function
-- defined in the whole spectrum of time values as a single entity. It is ideal for
-- numerical approximating integrals.
newtype Dynamics m a = Dynamics (Point m -> m a)

-- | A value in the 'Event' monad transformer represents a polymorphic time varying
-- function which is strongly synchronized with the event queue.
newtype Event m a = Event (Point m -> m a)

-- | Invoke the 'Parameter' computation.
invokeParameter :: Run m -> Parameter m a -> m a
{-# INLINE invokeParameter #-}
invokeParameter r (Parameter m) = m r

-- | Invoke the 'Simulation' computation.
invokeSimulation :: Run m -> Simulation m a -> m a
{-# INLINE invokeSimulation #-}
invokeSimulation r (Simulation m) = m r

-- | Invoke the 'Dynamics' computation.
invokeDynamics :: Point m -> Dynamics m a -> m a
{-# INLINE invokeDynamics #-}
invokeDynamics p (Dynamics m) = m p

-- | Invoke the 'Event' computation.
invokeEvent :: Point m -> Event m a -> m a
{-# INLINE invokeEvent #-}
invokeEvent p (Event m) = m p

-- | Defines how the events are processed.
data EventProcessing = CurrentEvents
                       -- ^ either process all earlier and then current events,
                       -- or raise an error if the current simulation time is less
                       -- than the actual time of the event queue (safe within
                       -- the 'Event' computation as this is protected by the type system)
                     | EarlierEvents
                       -- ^ either process all earlier events not affecting
                       -- the events at the current simulation time,
                       -- or raise an error if the current simulation time is less
                       -- than the actual time of the event queue (safe within
                       -- the 'Event' computation as this is protected by the type system)
                     | CurrentEventsOrFromPast
                       -- ^ either process all earlier and then current events,
                       -- or do nothing if the current simulation time is less
                       -- than the actual time of the event queue
                       -- (do not use unless the documentation states the opposite)
                     | EarlierEventsOrFromPast
                       -- ^ either process all earlier events,
                       -- or do nothing if the current simulation time is less
                       -- than the actual time of the event queue
                       -- (do not use unless the documentation states the opposite)
                     deriving (Eq, Ord, Show)

-- | A type class of monads that allow enqueueing the events.
class EventQueueing m where

  -- | It represents the event queue.
  data EventQueue m :: *

  -- | Create a new event queue by the specified specs with simulation session.
  newEventQueue :: Session m -> Specs m -> m (EventQueue m)

  -- | Enqueue the event which must be actuated at the specified time.
  enqueueEvent :: Double -> Event m () -> Event m ()

  -- | Run the 'EventT' computation in the current simulation time
  -- within the 'DynamicsT' computation involving all pending
  -- 'CurrentEvents' in the processing too.
  runEvent :: Event m a -> Dynamics m a
  {-# INLINE runEvent #-}
  runEvent = runEventWith CurrentEvents

  -- | Run the 'EventT' computation in the current simulation time
  -- within the 'DynamicsT' computation specifying what pending events 
  -- should be involved in the processing.
  runEventWith :: EventProcessing -> Event m a -> Dynamics m a

  -- | Return the number of pending events that should
  -- be yet actuated.
  eventQueueCount :: Event m Int
