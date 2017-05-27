
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.DES
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- It defines a type class of monads for Discrete Event Simulation (DES).
--
module Simulation.Aivika.Trans.DES (MonadDES) where

import Simulation.Aivika.Trans.Comp
import qualified Simulation.Aivika.Trans.Ref.Base.Strict as StrictRef
import qualified Simulation.Aivika.Trans.Ref.Base.Lazy as LazyRef
import Simulation.Aivika.Trans.Internal.Types
import Simulation.Aivika.Trans.QueueStrategy

-- | It defines a type class of monads for DES.
class (MonadComp m,
       StrictRef.MonadRef m,
       LazyRef.MonadRef m,
       EventQueueing m,
       EnqueueStrategy m FCFS,
       EnqueueStrategy m LCFS) => MonadDES m
