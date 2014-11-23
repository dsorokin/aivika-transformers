
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.IO.Dynamics.Memo.Unboxed
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The 'MonadIO' based monad is an instance of the 'MonadMemo' type class.
--

module Simulation.Aivika.IO.Dynamics.Memo.Unboxed where

import Control.Monad
import Control.Monad.Trans

import Data.Array.IO.Safe
import Data.Array.MArray.Safe
import Data.IORef

import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Dynamics.Memo.Unboxed
import Simulation.Aivika.Trans.Dynamics.Extra
import Simulation.Aivika.Trans.Template
import Simulation.Aivika.Trans.Array

-- | The 'MonadIO' based monad is an instance of the 'MonadMemo' type class.
instance (MonadIO m, MonadTemplate m, MArray IOUArray e IO) => MonadMemo m e where

  {-# INLINABLE memoDynamics #-}
  {-# SPECIALISE memoDynamics :: Dynamics IO Double -> Simulation IO (Dynamics IO Double) #-}
  {-# SPECIALISE memoDynamics :: Dynamics IO Float -> Simulation IO (Dynamics IO Float) #-}
  {-# SPECIALISE memoDynamics :: Dynamics IO Int -> Simulation IO (Dynamics IO Int) #-}
  memoDynamics (Dynamics m) = 
    Simulation $ \r ->
    do let sc  = runSpecs r
           (phl, phu) = integPhaseBnds sc
           (nl, nu)   = integIterationBnds sc
       arr   <- liftIO $ newIOUArray_ ((phl, nl), (phu, nu))
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

  {-# INLINABLE memo0Dynamics #-}
  {-# SPECIALISE memo0Dynamics :: Dynamics IO Double -> Simulation IO (Dynamics IO Double) #-}
  {-# SPECIALISE memo0Dynamics :: Dynamics IO Float -> Simulation IO (Dynamics IO Float) #-}
  {-# SPECIALISE memo0Dynamics :: Dynamics IO Int -> Simulation IO (Dynamics IO Int) #-}
  memo0Dynamics (Dynamics m) = 
    Simulation $ \r ->
    do let sc = runSpecs r
           bnds = integIterationBnds sc
       arr  <- liftIO $ newIOUArray_ bnds
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