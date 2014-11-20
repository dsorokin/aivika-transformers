
-- |
-- Module     : Simulation.Aivika.Trans.Template
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It defines an explicit type sub-class of 'IO'-based monads on top of which
-- the simulation monads can be automatically generated.
--
module Simulation.Aivika.Trans.Template (MonadTemplate) where

import Control.Monad.Trans

-- It defines a type class based on which the simulation monads can be automatically generated.
class Monad m => MonadTemplate m

instance MonadTemplate IO
