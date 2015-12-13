
-- |
-- Module     : Simulation.Aivika.Trans.Exception
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- It defines a type class of monads with exception handling capabilities.
--
module Simulation.Aivika.Trans.Exception
       (MonadException(..)) where

import Control.Exception

-- | A computation within which we can throw an exception.
class Monad m => MonadException m where

  -- | Catch an exception within the computation.
  catchComp :: Exception e => m a -> (e -> m a) -> m a

  -- | Introduce a finalisation block.
  finallyComp :: m a -> m b -> m a

  -- | Throw an exception.
  throwComp :: Exception e => e -> m a
