
-- |
-- Module     : Simulation.Aivika.Trans.Exception
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It defines a type class of monads with 'IO' exception handling capabilities.
--
module Simulation.Aivika.Trans.Exception
       (ExceptionHandling(..)) where

import Control.Monad.Trans
import Control.Exception

-- | A computation within which we can throw an exception.
class ExceptionHandling m where

  -- | Catch an 'IO' exception within the computation.
  catchComp :: Exception e => m a -> (e -> m a) -> m a

  -- | Introduce a finalisation block.
  finallyComp :: m a -> m b -> m a

  -- | Throw an exception.
  throwComp :: Exception e => e -> m a

instance ExceptionHandling IO where

  {-# INLINE catchComp #-}
  catchComp = catch

  {-# INLINE finallyComp #-}
  finallyComp = finally

  {-# INLINE throwComp #-}
  throwComp = throw
