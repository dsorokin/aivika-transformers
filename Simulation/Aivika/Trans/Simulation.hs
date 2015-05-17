
-- |
-- Module     : Simulation.Aivika.Trans.Simulation
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- The module defines the 'SimulationT' monad transformer that represents a simulation run.
-- 
module Simulation.Aivika.Trans.Simulation
       (-- * Simulation
        Simulation,
        SimulationLift(..),
        runSimulation,
        runSimulations,
        -- * Error Handling
        catchSimulation,
        finallySimulation,
        throwSimulation,
        -- * Exceptions
        SimulationException(..),
        SimulationAbort(..)) where

import Simulation.Aivika.Trans.Internal.Simulation
