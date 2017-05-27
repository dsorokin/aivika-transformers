
-- |
-- Module     : Simulation.Aivika.IO.Ref.Base
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The 'IO' monad in an instance of strict 'MonadRef', but it can also be 
-- an instance of similar lazy reference.
--
module Simulation.Aivika.IO.Ref.Base 
       (module Simulation.Aivika.IO.Ref.Base.Strict) where

import Simulation.Aivika.Trans.Ref.Base.Strict
import Simulation.Aivika.IO.Ref.Base.Strict
