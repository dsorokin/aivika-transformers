
-- |
-- Module     : Simulation.Aivika.Trans.Simulation
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines the 'Simulation' monad transformer that represents a simulation run.
-- 
module Simulation.Aivika.Trans.Simulation
       (-- * Simulation
        Simulation,
        SimulationLift(..),
        runSimulation,
        runSimulations,
        runSimulationByIndex,
        -- * Error Handling
        catchSimulation,
        finallySimulation,
        throwSimulation,
        -- * Exceptions
        SimulationException(..),
        SimulationAbort(..),
        SimulationRetry(..)) where

import Simulation.Aivika.Trans.Internal.Simulation
