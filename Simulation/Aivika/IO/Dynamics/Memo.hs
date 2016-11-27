
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.IO.Dynamics.Memo
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The 'IO' monad is an instance of the 'MonadMemo' type class.
--

module Simulation.Aivika.IO.Dynamics.Memo () where

import Control.Monad
import Control.Monad.Trans

import Data.Array.IO.Safe
import Data.Array.MArray.Safe
import Data.IORef

import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Dynamics.Memo
import Simulation.Aivika.Trans.Dynamics.Extra
import Simulation.Aivika.Trans.Array

-- | The 'IO' monad is an instance of the 'MonadMemo' type class.
instance MonadMemo IO where
-- instance (Monad m, MonadIO m, MonadTemplate m) => MonadMemo m where

  {-# SPECIALISE instance MonadMemo IO #-}

  {-# INLINE memoDynamics #-}
  memoDynamics (Dynamics m) = 
    Simulation $ \r ->
    do let sc  = runSpecs r
           (phl, phu) = integPhaseBnds sc
           (nl, nu)   = integIterationBnds sc
       arr   <- liftIO $ newIOArray_ ((phl, nl), (phu, nu))
       nref  <- liftIO $ newIORef 0
       phref <- liftIO $ newIORef 0
       let r p = 
             do let n  = pointIteration p
                    ph = pointPhase p
                    loop n' ph' = 
                      if (n' > n) || ((n' == n) && (ph' > ph)) 
                      then 
                        liftIO $ readArray arr (ph, n)
                      else 
                        let p' = p { pointIteration = n', pointPhase = ph',
                                     pointTime = basicTime sc n' ph' }
                        in do a <- m p'
                              a `seq` liftIO $ writeArray arr (ph', n') a
                              if ph' >= phu 
                                then do liftIO $ writeIORef phref 0
                                        liftIO $ writeIORef nref (n' + 1)
                                        loop (n' + 1) 0
                                else do liftIO $ writeIORef phref (ph' + 1)
                                        loop n' (ph' + 1)
                n'  <- liftIO $ readIORef nref
                ph' <- liftIO $ readIORef phref
                loop n' ph'
       return $ interpolateDynamics $ Dynamics r

  {-# INLINE memo0Dynamics #-}
  memo0Dynamics (Dynamics m) = 
    Simulation $ \r ->
    do let sc = runSpecs r
           bnds = integIterationBnds sc
       arr  <- liftIO $ newIOArray_ bnds
       nref <- liftIO $ newIORef 0
       let r p =
             do let sc = pointSpecs p
                    n  = pointIteration p
                    loop n' = 
                      if n' > n
                      then 
                        liftIO $ readArray arr n
                      else 
                        let p' = p { pointIteration = n', pointPhase = 0,
                                     pointTime = basicTime sc n' 0 }
                        in do a <- m p'
                              a `seq` liftIO $ writeArray arr n' a
                              liftIO $ writeIORef nref (n' + 1)
                              loop (n' + 1)
                n' <- liftIO $ readIORef nref
                loop n'
       return $ discreteDynamics $ Dynamics r

  {-# INLINE iterateDynamics #-}
  iterateDynamics (Dynamics m) = 
    Simulation $ \r ->
    do let sc = runSpecs r
       nref <- liftIO $ newIORef 0
       let r p =
             do let sc = pointSpecs p
                    n  = pointIteration p
                    loop n' = 
                      unless (n' > n) $
                      let p' = p { pointIteration = n', pointPhase = 0,
                                   pointTime = basicTime sc n' 0 }
                      in do a <- m p'
                            a `seq` liftIO $ writeIORef nref (n' + 1)
                            loop (n' + 1)
                n' <- liftIO $ readIORef nref
                loop n'
       return $ discreteDynamics $ Dynamics r
