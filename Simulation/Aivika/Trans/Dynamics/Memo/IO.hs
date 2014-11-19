
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.Trans.Dynamics.Memo.IO
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The 'MonadIO' based monad is an instance of the 'MonadMemo' type class.
--

module Simulation.Aivika.Trans.Dynamics.Memo.IO where

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
import Simulation.Aivika.Trans.Template
import Simulation.Aivika.Trans.Array

-- | The 'MonadIO' based monad is an instance of the 'MonadMemo' type class.
instance TemplateIO m => MonadMemo m where

  {-# INLINABLE memoDynamics #-}
  memoDynamics (Dynamics m) = 
    Simulation $ \r ->
    do let sc  = runSpecs r
           phs = integPhaseBnds sc
           ns  = integIterationBnds sc
           (ph1, ph2) = phs 
       arr   <- liftIO $ newIOArray_ (ns, phs)
       nref  <- liftIO $ newIORef 0
       phref <- liftIO $ newIORef 0
       let r p = 
             do let n  = pointIteration p
                    ph = pointPhase p
                    i  = (n, ph)
                    loop n' ph' = 
                      if (n' > n) || ((n' == n) && (ph' > ph)) 
                      then 
                        liftIO $ readArray arr i
                      else 
                        let p' = p { pointIteration = n', pointPhase = ph',
                                     pointTime = basicTime sc n' ph' }
                            i' = (n', ph')
                        in do a <- m p'
                              a `seq` liftIO $ writeArray arr i' a
                              if ph' >= ph2 
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
  memo0Dynamics (Dynamics m) = 
    Simulation $ \r ->
    do let sc = runSpecs r
           ns = integIterationBnds sc
       arr  <- liftIO $ newIOArray_ ns
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

  {-# INLINABLE iterateDynamics #-}
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
