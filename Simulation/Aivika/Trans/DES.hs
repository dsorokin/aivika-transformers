
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.DES
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- It defines a type class of monads for Discrete Event Simulation (DES).
--
module Simulation.Aivika.Trans.DES (MonadDES) where

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.Internal.Types
import Simulation.Aivika.Trans.QueueStrategy

-- | It defines a type class of monads for Discrete Event Simulation (DES).
class (MonadComp m,
       MonadRef m,
       EventQueueing m,
       EnqueueStrategy m FCFS,
       EnqueueStrategy m LCFS) => MonadDES m
