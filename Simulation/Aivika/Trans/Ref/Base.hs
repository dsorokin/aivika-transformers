
-- |
-- Module     : Simulation.Aivika.Trans.Ref.Base
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines a plain and more fast version of an updatable reference
-- that depends on the event queue but that doesn't supply with the signal notification.
--
-- By default, the module uses a strict version defined in "Simulation.Aivika.Trans.Ref.Base.Strict".
-- There is also another lazy version defined in "Simulation.Aivika.Trans.Ref.Base.Lazy".
--
module Simulation.Aivika.Trans.Ref.Base
       (module Simulation.Aivika.Trans.Ref.Base.Strict) where

import Simulation.Aivika.Trans.Ref.Base.Strict
import Simulation.Aivika.Trans.Ref.Base.Lazy
