
-- |
-- Module     : Simulation.Aivika.Trans.Template
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- It defines explicit type sub-classes of 'IO'-based monads on top of which
-- the simulation monads can be automatically generated.
--
module Simulation.Aivika.Trans.Template
       (MonadTemplate,
        MonadEventQueueTemplate) where

import Control.Monad.Trans

-- | It defines a type class based on which the simulation computations can be automatically generated.
class Monad m => MonadTemplate m

-- | It defines a type class based on which the event queue can be automatically generated.
class Monad m => MonadEventQueueTemplate m

-- | An instance of the type class.
instance MonadTemplate IO

-- | An instance of the type class.
instance MonadEventQueueTemplate IO
