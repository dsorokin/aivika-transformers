
-- |
-- Module     : Simulation.Aivika.Trans.Exception
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It defines a type class of monads with exception handling capabilities.
--
module Simulation.Aivika.Trans.Exception
       (MonadException(..)) where

import Control.Monad.Trans
import Control.Exception

-- | A computation within which we can throw an exception.
class MonadException m where

  -- | Catch an exception within the computation.
  catchComp :: Exception e => m a -> (e -> m a) -> m a

  -- | Introduce a finalisation block.
  finallyComp :: m a -> m b -> m a

  -- | Throw an exception.
  throwComp :: Exception e => e -> m a

instance MonadException IO where

  {-# INLINE catchComp #-}
  catchComp = catch

  {-# INLINE finallyComp #-}
  finallyComp = finally

  {-# INLINE throwComp #-}
  throwComp = throw
