
-- |
-- Module     : Simulation.Aivika.Trans.Template
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- It defines an explicit type sub-class of 'IO'-based monads on top of which
-- the simulation monads can be automatically generated.
--
module Simulation.Aivika.Trans.Template (MonadTemplate) where

import Control.Monad.Trans

-- | It defines a type class based on which the simulation computations can be automatically generated.
class Monad m => MonadTemplate m

-- | An instance of the type class.
instance MonadTemplate IO
