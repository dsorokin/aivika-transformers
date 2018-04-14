
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- |
-- Module     : Simulation.Aivika.Trans.Internal.Cont
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The 'Cont' monad is a variation of the standard Cont monad 
-- and F# async workflow, where the result of applying 
-- the continuations is the 'Event' computation.
--
module Simulation.Aivika.Trans.Internal.Cont
       (ContParams,
        ContCancellation(..),
        Cont(..),
        ContId,
        ContEvent(..),
        FrozenCont,
        newContId,
        contSignal,
        contCancellationInitiated,
        contCancellationInitiate,
        contCancellationInitiating,
        contCancellationActivated,
        contCancellationBind,
        contCancellationConnect,
        contPreemptionBegun,
        contPreemptionBegin,
        contPreemptionBeginning,
        contPreemptionEnd,
        contPreemptionEnding,
        invokeCont,
        runCont,
        rerunCont,
        spawnCont,
        contParallel,
        contParallel_,
        catchCont,
        finallyCont,
        throwCont,
        resumeCont,
        resumeECont,
        reenterCont,
        freezeCont,
        freezeContReentering,
        unfreezeCont,
        substituteCont,
        contCanceled,
        contAwait,
        transferCont,
        traceCont) where

import Data.Array
import Data.Monoid

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Catch as MC
import Control.Applicative

import Debug.Trace (trace)

import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Signal

-- | It defines how the parent and child computations should be cancelled.
data ContCancellation = CancelTogether
                        -- ^ Cancel the both computations together.
                      | CancelChildAfterParent
                        -- ^ Cancel the child if its parent is cancelled.
                      | CancelParentAfterChild
                        -- ^ Cancel the parent if its child is cancelled.
                      | CancelInIsolation
                        -- ^ Cancel the computations in isolation.

-- | It identifies the 'Cont' computation.
data ContId m =
  ContId { contCancellationInitiatedRef :: Ref m Bool,
           contCancellationActivatedRef :: Ref m Bool,
           contPreemptionCountRef :: Ref m Int,
           contSignalSource :: SignalSource m ContEvent
         }

instance MonadDES m => Eq (ContId m) where
  x == y = contCancellationInitiatedRef x == contCancellationInitiatedRef y

-- | The event that occurs within the 'Cont' computation.
data ContEvent = ContCancellationInitiating
                 -- ^ Cancel the computation.
               | ContPreemptionBeginning
                 -- ^ Preempt the computation.
               | ContPreemptionEnding
                 -- ^ Proceed with the computation after if was preempted.
               deriving (Eq, Ord, Show)

-- | Create a computation identifier.
newContId :: MonadDES m => Simulation m (ContId m)
{-# INLINABLE newContId #-}
newContId =
  Simulation $ \r ->
  do r1 <- invokeSimulation r $ newRef False
     r2 <- invokeSimulation r $ newRef False
     r3 <- invokeSimulation r $ newRef 0
     s  <- invokeSimulation r newSignalSource
     return ContId { contCancellationInitiatedRef = r1,
                     contCancellationActivatedRef = r2,
                     contPreemptionCountRef = r3,
                     contSignalSource = s
                   }

-- | Signal when the computation state changes.
contSignal :: ContId m -> Signal m ContEvent
{-# INLINABLE contSignal #-}
contSignal = publishSignal . contSignalSource

-- | Signal when the cancellation is intiating.
contCancellationInitiating :: MonadDES m => ContId m -> Signal m ()
{-# INLINABLE contCancellationInitiating #-}
contCancellationInitiating =
  filterSignal_ (ContCancellationInitiating ==) . contSignal

-- | Whether the cancellation was initiated.
contCancellationInitiated :: MonadDES m => ContId m -> Event m Bool
{-# INLINABLE contCancellationInitiated #-}
contCancellationInitiated =
  readRef . contCancellationInitiatedRef

-- | Whether the cancellation was activated.
contCancellationActivated :: MonadDES m => ContId m -> Event m Bool
{-# INLINABLE contCancellationActivated #-}
contCancellationActivated =
  readRef . contCancellationActivatedRef

-- | Deactivate the cancellation.
contCancellationDeactivate :: MonadDES m => ContId m -> Event m ()
{-# INLINABLE contCancellationDeactivate #-}
contCancellationDeactivate x =
  writeRef (contCancellationActivatedRef x) False

-- | If the main computation is cancelled then all the nested ones will be cancelled too.
contCancellationBind :: MonadDES m => ContId m -> [ContId m] -> Event m (DisposableEvent m)
{-# INLINABLE contCancellationBind #-}
contCancellationBind x ys =
  Event $ \p ->
  do hs1 <- forM ys $ \y ->
       invokeEvent p $
       handleSignal (contCancellationInitiating x) $ \_ ->
       contCancellationInitiate y
     hs2 <- forM ys $ \y ->
       invokeEvent p $
       handleSignal (contCancellationInitiating y) $ \_ ->
       contCancellationInitiate x
     return $ mconcat hs1 <> mconcat hs2

-- | Connect the parent computation to the child one.
contCancellationConnect :: MonadDES m
                           => ContId m
                           -- ^ the parent
                           -> ContCancellation
                           -- ^ how to connect
                           -> ContId m
                           -- ^ the child
                           -> Event m (DisposableEvent m)
                           -- ^ computation of the disposable handler
{-# INLINABLE contCancellationConnect #-}
contCancellationConnect parent cancellation child =
  Event $ \p ->
  do let m1 =
           handleSignal (contCancellationInitiating parent) $ \_ ->
           contCancellationInitiate child
         m2 =
           handleSignal (contCancellationInitiating child) $ \_ ->
           contCancellationInitiate parent
     h1 <- 
       case cancellation of
         CancelTogether -> invokeEvent p m1
         CancelChildAfterParent -> invokeEvent p m1
         CancelParentAfterChild -> return mempty
         CancelInIsolation -> return mempty
     h2 <-
       case cancellation of
         CancelTogether -> invokeEvent p m2
         CancelChildAfterParent -> return mempty
         CancelParentAfterChild -> invokeEvent p m2
         CancelInIsolation -> return mempty
     return $ h1 <> h2

-- | Initiate the cancellation.
contCancellationInitiate :: MonadDES m => ContId m -> Event m ()
{-# INLINABLE contCancellationInitiate #-}
contCancellationInitiate x =
  Event $ \p ->
  do f <- invokeEvent p $ readRef (contCancellationInitiatedRef x)
     unless f $
       do invokeEvent p $ writeRef (contCancellationInitiatedRef x) True
          invokeEvent p $ writeRef (contCancellationActivatedRef x) True
          invokeEvent p $ triggerSignal (contSignalSource x) ContCancellationInitiating

-- | Preempt the computation.
contPreemptionBegin :: MonadDES m => ContId m -> Event m ()
{-# INLINABLE contPreemptionBegin #-}
contPreemptionBegin x =
  Event $ \p ->
  do f <- invokeEvent p $ readRef (contCancellationInitiatedRef x)
     unless f $
       do n <- invokeEvent p $ readRef (contPreemptionCountRef x)
          let n' = n + 1
          n' `seq` invokeEvent p $ writeRef (contPreemptionCountRef x) n'
          when (n == 0) $
            invokeEvent p $
            triggerSignal (contSignalSource x) ContPreemptionBeginning

-- | Proceed with the computation after it was preempted earlier.
contPreemptionEnd :: MonadDES m => ContId m -> Event m ()
{-# INLINABLE contPreemptionEnd #-}
contPreemptionEnd x =
  Event $ \p ->
  do f <- invokeEvent p $ readRef (contCancellationInitiatedRef x)
     unless f $
       do n <- invokeEvent p $ readRef (contPreemptionCountRef x)
          let n' = n - 1
          n' `seq` invokeEvent p $ writeRef (contPreemptionCountRef x) n'
          when (n' == 0) $
            invokeEvent p $
            triggerSignal (contSignalSource x) ContPreemptionEnding

-- | Signal when the computation is preempted.
contPreemptionBeginning :: MonadDES m => ContId m -> Signal m ()
{-# INLINABLE contPreemptionBeginning #-}
contPreemptionBeginning =
  filterSignal_ (ContPreemptionBeginning ==) . contSignal

-- | Signal when the computation is proceeded after it was preempted before.
contPreemptionEnding :: MonadDES m => ContId m -> Signal m ()
{-# INLINABLE contPreemptionEnding #-}
contPreemptionEnding =
  filterSignal_ (ContPreemptionEnding ==) . contSignal

-- | Whether the computation was preemtped.
contPreemptionBegun :: MonadDES m => ContId m -> Event m Bool
{-# INLINABLE contPreemptionBegun #-}
contPreemptionBegun x =
  Event $ \p ->
  do n <- invokeEvent p $ readRef (contPreemptionCountRef x)
     return (n > 0)

-- | The 'Cont' type is similar to the standard Cont monad 
-- and F# async workflow but only the result of applying
-- the continuations return the 'Event' computation.
newtype Cont m a = Cont (ContParams m a -> Event m ())

-- | The continuation parameters.
data ContParams m a = 
  ContParams { contCont :: a -> Event m (), 
               contAux  :: ContParamsAux m }

-- | The auxiliary continuation parameters.
data ContParamsAux m =
  ContParamsAux { contECont :: SomeException -> Event m (),
                  contCCont :: () -> Event m (),
                  contId :: ContId m,
                  contCancelRef :: Ref m Bool,
                  contCatchFlag  :: Bool }

instance MonadDES m => Monad (Cont m) where

  {-# INLINE return #-}
  return a = 
    Cont $ \c ->
    Event $ \p ->
    do z <- invokeEvent p $ contCanceled c
       if z 
         then invokeEvent p $ cancelCont c
         else invokeEvent p $ contCont c a

  {-# INLINE (>>=) #-}
  (Cont m) >>= k =
    Cont $ \c ->
    Event $ \p ->
    do z <- invokeEvent p $ contCanceled c
       if z 
         then invokeEvent p $ cancelCont c
         else invokeEvent p $ m $ 
              let cont a = invokeCont c (k a)
              in c { contCont = cont }

instance MonadDES m => MonadCompTrans Cont m where

  {-# INLINE liftComp #-}
  liftComp m =
    Cont $ \c ->
    Event $ \p ->
    if contCatchFlag . contAux $ c
    then liftWithCatching m p c
    else liftWithoutCatching m p c

instance MonadDES m => ParameterLift Cont m where

  {-# INLINE liftParameter #-}
  liftParameter (Parameter m) = 
    Cont $ \c ->
    Event $ \p ->
    if contCatchFlag . contAux $ c
    then liftWithCatching (m $ pointRun p) p c
    else liftWithoutCatching (m $ pointRun p) p c

instance MonadDES m => SimulationLift Cont m where

  {-# INLINE liftSimulation #-}
  liftSimulation (Simulation m) = 
    Cont $ \c ->
    Event $ \p ->
    if contCatchFlag . contAux $ c
    then liftWithCatching (m $ pointRun p) p c
    else liftWithoutCatching (m $ pointRun p) p c

instance MonadDES m => DynamicsLift Cont m where

  {-# INLINE liftDynamics #-}
  liftDynamics (Dynamics m) = 
    Cont $ \c ->
    Event $ \p ->
    if contCatchFlag . contAux $ c
    then liftWithCatching (m p) p c
    else liftWithoutCatching (m p) p c

instance MonadDES m => EventLift Cont m where

  {-# INLINE liftEvent #-}
  liftEvent (Event m) = 
    Cont $ \c ->
    Event $ \p ->
    if contCatchFlag . contAux $ c
    then liftWithCatching (m p) p c
    else liftWithoutCatching (m p) p c

instance (MonadDES m, MonadIO m) => MonadIO (Cont m) where

  {-# INLINE liftIO #-}
  liftIO m =
    Cont $ \c ->
    Event $ \p ->
    if contCatchFlag . contAux $ c
    then liftWithCatching (liftIO m) p c
    else liftWithoutCatching (liftIO m) p c

instance MonadDES m => Functor (Cont m) where

  {-# INLINE fmap #-}
  fmap = liftM

instance MonadDES m => Applicative (Cont m) where

  {-# INLINE pure #-}
  pure = return

  {-# INLINE (<*>) #-}
  (<*>) = ap

instance MonadDES m => MC.MonadThrow (Cont m) where

  {-# INLINE throwM #-}
  throwM = throwCont

instance MonadDES m => MC.MonadCatch (Cont m) where

  {-# INLINE catch #-}
  catch = catchCont

-- | Invoke the computation.
invokeCont :: ContParams m a -> Cont m a -> Event m ()
{-# INLINE invokeCont #-}
invokeCont p (Cont m) = m p

-- | Cancel the computation.
cancelCont :: MonadDES m => ContParams m a -> Event m ()
{-# NOINLINE cancelCont #-}
cancelCont c =
  Event $ \p ->
  do invokeEvent p $ contCancellationDeactivate (contId $ contAux c)
     invokeEvent p $ (contCCont $ contAux c) ()

-- | Like @return a >>= k@.
callCont :: MonadDES m => (a -> Cont m b) -> a -> ContParams m b -> Event m ()
{-# INLINABLE callCont #-}
callCont k a c =
  Event $ \p ->
  do z <- invokeEvent p $ contCanceled c
     if z 
       then invokeEvent p $ cancelCont c
       else invokeEvent p $ invokeCont c (k a)

-- | Exception handling within 'Cont' computations.
catchCont :: (MonadDES m, Exception e) => Cont m a -> (e -> Cont m a) -> Cont m a
{-# INLINABLE catchCont #-}
catchCont (Cont m) h = 
  Cont $ \c0 ->
  Event $ \p -> 
  do let c = c0 { contAux = (contAux c0) { contCatchFlag = True } }
     z <- invokeEvent p $ contCanceled c
     if z 
       then invokeEvent p $ cancelCont c
       else invokeEvent p $ m $
            let econt e0 =
                  case fromException e0 of
                    Just e  -> callCont h e c
                    Nothing -> (contECont . contAux $ c) e0
            in c { contAux = (contAux c) { contECont = econt } }
               
-- | A computation with finalization part.
finallyCont :: MonadDES m => Cont m a -> Cont m b -> Cont m a
{-# INLINABLE finallyCont #-}
finallyCont (Cont m) (Cont m') = 
  Cont $ \c0 -> 
  Event $ \p ->
  do let c = c0 { contAux = (contAux c0) { contCatchFlag = True } }
     z <- invokeEvent p $ contCanceled c
     if z 
       then invokeEvent p $ cancelCont c
       else invokeEvent p $ m $
            let cont a   = 
                  Event $ \p ->
                  invokeEvent p $ m' $
                  let cont b = contCont c a
                  in c { contCont = cont }
                econt e  =
                  Event $ \p ->
                  invokeEvent p $ m' $
                  let cont b = (contECont . contAux $ c) e
                  in c { contCont = cont }
                ccont () = 
                  Event $ \p ->
                  invokeEvent p $ m' $
                  let cont b  = (contCCont . contAux $ c) ()
                      econt e = (contCCont . contAux $ c) ()
                  in c { contCont = cont,
                         contAux  = (contAux c) { contECont = econt } }
            in c { contCont = cont,
                   contAux  = (contAux c) { contECont = econt,
                                            contCCont = ccont } }

-- | Throw the exception with the further exception handling.
--
-- By some reason, an exception raised with help of the standard 'throw' function
-- is not handled properly within 'Cont' computation, altough it will be still handled 
-- if it will be wrapped in the 'IO' monad. Therefore, you should use specialised
-- functions like the stated one that use the 'throw' function but within the 'IO' computation,
-- which allows already handling the exception.
throwCont :: (MonadDES m, Exception e) => e -> Cont m a
{-# INLINABLE throwCont #-}
throwCont = liftEvent . throwEvent

-- | Run the 'Cont' computation with the specified cancelation source 
-- and flag indicating whether to catch exceptions from the beginning.
runCont :: MonadDES m
           => Cont m a
           -- ^ the computation to run
           -> (a -> Event m ())
           -- ^ the main branch 
           -> (SomeException -> Event m ())
           -- ^ the branch for handing exceptions
           -> (() -> Event m ())
           -- ^ the branch for cancellation
           -> ContId m
           -- ^ the computation identifier
           -> Bool
           -- ^ whether to support the exception handling from the beginning
           -> Event m ()
{-# INLINABLE runCont #-}
runCont (Cont m) cont econt ccont cid catchFlag = 
  m ContParams { contCont = cont,
                 contAux  = 
                   ContParamsAux { contECont = econt,
                                   contCCont = ccont,
                                   contId    = cid,
                                   contCancelRef = contCancellationActivatedRef cid, 
                                   contCatchFlag  = catchFlag } }
  
liftWithoutCatching :: MonadDES m => m a -> Point m -> ContParams m a -> m ()
{-# INLINE liftWithoutCatching #-}
liftWithoutCatching m p c =
  do z <- invokeEvent p $ contCanceled c
     if z
       then invokeEvent p $ cancelCont c
       else do a <- m
               invokeEvent p $ contCont c a

liftWithCatching :: MonadDES m => m a -> Point m -> ContParams m a -> m ()
{-# NOINLINE liftWithCatching #-}
liftWithCatching m p c =
  do z <- invokeEvent p $ contCanceled c
     if z
       then invokeEvent p $ cancelCont c
       else do let r = pointRun p
               aref <- invokeSimulation r $ newRef undefined
               eref <- invokeSimulation r $ newRef Nothing
               catchComp
                 (m >>= invokeEvent p . writeRef aref) 
                 (invokeEvent p . writeRef eref . Just)
               e <- invokeEvent p $ readRef eref
               case e of
                 Nothing -> 
                   do a <- invokeEvent p $ readRef aref
                      -- tail recursive
                      invokeEvent p $ contCont c a
                 Just e ->
                   -- tail recursive
                   invokeEvent p $ (contECont . contAux) c e

-- | Resume the computation by the specified parameters.
resumeCont :: MonadDES m => ContParams m a -> a -> Event m ()
{-# INLINE resumeCont #-}
resumeCont c a = 
  Event $ \p ->
  do z <- invokeEvent p $ contCanceled c
     if z
       then invokeEvent p $ cancelCont c
       else invokeEvent p $ contCont c a

-- | Resume the exception handling by the specified parameters.
resumeECont :: MonadDES m => ContParams m a -> SomeException -> Event m ()
{-# INLINE resumeECont #-}
resumeECont c e = 
  Event $ \p ->
  do z <- invokeEvent p $ contCanceled c
     if z
       then invokeEvent p $ cancelCont c
       else invokeEvent p $ (contECont $ contAux c) e

-- | Test whether the computation is canceled.
contCanceled :: MonadDES m => ContParams m a -> Event m Bool
{-# INLINE contCanceled #-}
contCanceled c = readRef $ contCancelRef $ contAux c

-- | Execute the specified computations in parallel within
-- the current computation and return their results. The cancellation
-- of any of the nested computations affects the current computation.
-- The exception raised in any of the nested computations is propogated
-- to the current computation as well (if the exception handling is
-- supported).
--
-- Here word @parallel@ literally means that the computations are
-- actually executed on a single operating system thread but
-- they are processed simultaneously by the event queue.
contParallel :: MonadDES m
                => [(Cont m a, ContId m)]
                -- ^ the list of pairs:
                -- the nested computation,
                -- the computation identifier
                -> Cont m [a]
{-# INLINABLE contParallel #-}
contParallel xs =
  Cont $ \c ->
  Event $ \p ->
  do let n = length xs
         r = pointRun p
         worker =
           do results   <- forM [1..n] $ \i -> invokeSimulation r $ newRef undefined
              counter   <- invokeSimulation r $ newRef 0
              catchRef  <- invokeSimulation r $ newRef Nothing
              hs <- invokeEvent p $
                    contCancellationBind (contId $ contAux c) $
                    map snd xs
              let propagate =
                    Event $ \p ->
                    do n' <- invokeEvent p $ readRef counter
                       when (n' == n) $
                         do invokeEvent p $ disposeEvent hs  -- unbind the cancellation sources
                            f1 <- invokeEvent p $ contCanceled c
                            f2 <- invokeEvent p $ readRef catchRef
                            case (f1, f2) of
                              (False, Nothing) ->
                                do rs <- forM results $ invokeEvent p . readRef
                                   invokeEvent p $ resumeCont c rs
                              (False, Just e) ->
                                invokeEvent p $ resumeECont c e
                              (True, _) ->
                                invokeEvent p $ cancelCont c
                  cont result a =
                    Event $ \p ->
                    do invokeEvent p $ modifyRef counter (+ 1)
                       invokeEvent p $ writeRef result a
                       invokeEvent p propagate
                  econt e =
                    Event $ \p ->
                    do invokeEvent p $ modifyRef counter (+ 1)
                       r <- invokeEvent p $ readRef catchRef
                       case r of
                         Nothing -> invokeEvent p $ writeRef catchRef $ Just e
                         Just e' -> return ()  -- ignore the next error
                       invokeEvent p propagate
                  ccont e =
                    Event $ \p ->
                    do invokeEvent p $ modifyRef counter (+ 1)
                       -- the main computation was automatically canceled
                       invokeEvent p propagate
              forM_ (zip results xs) $ \(result, (x, cid)) ->
                invokeEvent p $
                runCont x (cont result) econt ccont cid (contCatchFlag $ contAux c)
     z <- invokeEvent p $ contCanceled c
     if z
       then invokeEvent p $ cancelCont c
       else if n == 0
            then invokeEvent p $ contCont c []
            else worker

-- | A partial case of 'contParallel' when we are not interested in
-- the results but we are interested in the actions to be peformed by
-- the nested computations.
contParallel_ :: MonadDES m
                 => [(Cont m a, ContId m)]
                 -- ^ the list of pairs:
                 -- the nested computation,
                 -- the computation identifier
                 -> Cont m ()
{-# INLINABLE contParallel_ #-}
contParallel_ xs =
  Cont $ \c ->
  Event $ \p ->
  do let n = length xs
         r = pointRun p
         worker =
           do counter  <- invokeSimulation r $ newRef 0
              catchRef <- invokeSimulation r $ newRef Nothing
              hs <- invokeEvent p $
                    contCancellationBind (contId $ contAux c) $
                    map snd xs
              let propagate =
                    Event $ \p ->
                    do n' <- invokeEvent p $ readRef counter
                       when (n' == n) $
                         do invokeEvent p $ disposeEvent hs  -- unbind the cancellation sources
                            f1 <- invokeEvent p $ contCanceled c
                            f2 <- invokeEvent p $ readRef catchRef
                            case (f1, f2) of
                              (False, Nothing) ->
                                invokeEvent p $ resumeCont c ()
                              (False, Just e) ->
                                invokeEvent p $ resumeECont c e
                              (True, _) ->
                                invokeEvent p $ cancelCont c
                  cont a =
                    Event $ \p ->
                    do invokeEvent p $ modifyRef counter (+ 1)
                       -- ignore the result
                       invokeEvent p propagate
                  econt e =
                    Event $ \p ->
                    do invokeEvent p $ modifyRef counter (+ 1)
                       r <- invokeEvent p $ readRef catchRef
                       case r of
                         Nothing -> invokeEvent p $ writeRef catchRef $ Just e
                         Just e' -> return ()  -- ignore the next error
                       invokeEvent p propagate
                  ccont e =
                    Event $ \p ->
                    do invokeEvent p $ modifyRef counter (+ 1)
                       -- the main computation was automatically canceled
                       invokeEvent p propagate
              forM_ (zip [0..n-1] xs) $ \(i, (x, cid)) ->
                invokeEvent p $
                runCont x cont econt ccont cid (contCatchFlag $ contAux c)
     z <- invokeEvent p $ contCanceled c
     if z
       then invokeEvent p $ cancelCont c
       else if n == 0
            then invokeEvent p $ contCont c ()
            else worker

-- | Rerun the 'Cont' computation with the specified identifier.
rerunCont :: MonadDES m => Cont m a -> ContId m -> Cont m a
{-# INLINABLE rerunCont #-}
rerunCont x cid =
  Cont $ \c ->
  Event $ \p ->
  do let worker =
           do hs <- invokeEvent p $
                    contCancellationBind (contId $ contAux c) [cid]
              let cont a  =
                    Event $ \p ->
                    do invokeEvent p $ disposeEvent hs  -- unbind the cancellation source
                       invokeEvent p $ resumeCont c a
                  econt e =
                    Event $ \p ->
                    do invokeEvent p $ disposeEvent hs  -- unbind the cancellation source
                       invokeEvent p $ resumeECont c e
                  ccont e =
                    Event $ \p ->
                    do invokeEvent p $ disposeEvent hs  -- unbind the cancellation source
                       invokeEvent p $ cancelCont c
              invokeEvent p $
                runCont x cont econt ccont cid (contCatchFlag $ contAux c)
     z <- invokeEvent p $ contCanceled c
     if z
       then invokeEvent p $ cancelCont c
       else worker

-- | Run the 'Cont' computation in parallel but connect the computations.
spawnCont :: MonadDES m => ContCancellation -> Cont m () -> ContId m -> Cont m ()
{-# INLINABLE spawnCont #-}
spawnCont cancellation x cid =
  Cont $ \c ->
  Event $ \p ->
  do let worker =
           do hs <- invokeEvent p $
                    contCancellationConnect
                    (contId $ contAux c) cancellation cid
              let cont a  =
                    Event $ \p ->
                    do invokeEvent p $ disposeEvent hs  -- unbind the cancellation source
                       -- do nothing and it will finish the computation
                  econt e =
                    Event $ \p ->
                    do invokeEvent p $ disposeEvent hs  -- unbind the cancellation source
                       invokeEvent p $ throwEvent e  -- this is all we can do
                  ccont e =
                    Event $ \p ->
                    do invokeEvent p $ disposeEvent hs  -- unbind the cancellation source
                       -- do nothing and it will finish the computation
              invokeEvent p $
                enqueueEvent (pointTime p) $
                runCont x cont econt ccont cid False
              invokeEvent p $
                resumeCont c ()
     z <- invokeEvent p $ contCanceled c
     if z
       then invokeEvent p $ cancelCont c
       else worker

-- | Represents a temporarily frozen computation.
newtype FrozenCont m a =
  FrozenCont { unfreezeCont :: Event m (Maybe (ContParams m a))
               -- ^ Unfreeze the computation.
             }

-- | Freeze the computation parameters temporarily.
freezeCont :: MonadDES m => ContParams m a -> Event m (FrozenCont m a)
{-# INLINABLE freezeCont #-}
freezeCont c =
  Event $ \p ->
  do let r = pointRun p
     rh <- invokeSimulation r $ newRef Nothing
     rc <- invokeSimulation r $ newRef $ Just c
     h <- invokeEvent p $
          handleSignal (contCancellationInitiating $
                        contId $
                        contAux c) $ \e ->
          Event $ \p ->
          do h <- invokeEvent p $ readRef rh
             case h of
               Nothing ->
                 error "The handler was lost: freezeCont."
               Just h ->
                 do invokeEvent p $ disposeEvent h
                    c <- invokeEvent p $ readRef rc
                    case c of
                      Nothing -> return ()
                      Just c  ->
                        do invokeEvent p $ writeRef rc Nothing
                           invokeEvent p $
                             enqueueEvent (pointTime p) $
                             Event $ \p ->
                             do z <- invokeEvent p $ contCanceled c
                                when z $ invokeEvent p $ cancelCont c
     invokeEvent p $ writeRef rh (Just h)
     return $
       FrozenCont $
       Event $ \p ->
       do invokeEvent p $ disposeEvent h
          c <- invokeEvent p $ readRef rc
          invokeEvent p $ writeRef rc Nothing
          return c

-- | Freeze the computation parameters specifying what should be done when reentering the computation.
freezeContReentering :: MonadDES m => ContParams m a -> a -> Event m () -> Event m (FrozenCont m a)
{-# INLINABLE freezeContReentering #-}
freezeContReentering c a m =
  Event $ \p ->
  do let r = pointRun p
     rh <- invokeSimulation r $ newRef Nothing
     rc <- invokeSimulation r $ newRef $ Just c
     h <- invokeEvent p $
          handleSignal (contCancellationInitiating $
                        contId $ contAux c) $ \e ->
          Event $ \p ->
          do h <- invokeEvent p $ readRef rh
             case h of
               Nothing ->
                 error "The handler was lost: freezeContReentering."
               Just h ->
                 do invokeEvent p $ disposeEvent h
                    c <- invokeEvent p $ readRef rc
                    case c of
                      Nothing -> return ()
                      Just c  ->
                        do invokeEvent p $ writeRef rc Nothing
                           invokeEvent p $
                             enqueueEvent (pointTime p) $
                             Event $ \p ->
                             do z <- invokeEvent p $ contCanceled c
                                when z $ invokeEvent p $ cancelCont c
     invokeEvent p $ writeRef rh (Just h)
     return $
       FrozenCont $
       Event $ \p ->
       do invokeEvent p $ disposeEvent h
          c <- invokeEvent p $ readRef rc
          invokeEvent p $ writeRef rc Nothing
          case c of
            Nothing -> return Nothing
            z @ (Just c) ->
              do f <- invokeEvent p $
                      contPreemptionBegun $
                      contId $ contAux c
                 if not f
                   then return z
                   else do let c = c { contCont = \a -> m }
                           invokeEvent p $ sleepCont c a
                           return Nothing
     
-- | Reenter the computation parameters when needed.
reenterCont :: MonadDES m => ContParams m a -> a -> Event m ()
{-# INLINE reenterCont #-}
reenterCont c a =
  Event $ \p ->
  do f <- invokeEvent p $
          contPreemptionBegun $
          contId $ contAux c
     if not f
       then invokeEvent p $
            enqueueEvent (pointTime p) $
            Event $ \p ->
            do f <- invokeEvent p $
                    contPreemptionBegun $
                    contId $ contAux c
               if not f
                 then invokeEvent p $
                      resumeCont c a
                 else invokeEvent p $
                      sleepCont c a
       else invokeEvent p $
            sleepCont c a

-- | Sleep until the preempted computation will be reentered.
sleepCont :: MonadDES m => ContParams m a -> a -> Event m ()
{-# INLINABLE sleepCont #-}
sleepCont c a =
  Event $ \p ->
  do let r = pointRun p
     rh <- invokeSimulation r $ newRef Nothing
     h  <- invokeEvent p $
           handleSignal (contSignal $
                         contId $ contAux c) $ \e ->
           Event $ \p ->
           do h <- invokeEvent p $ readRef rh
              case h of
                Nothing ->
                  error "The handler was lost: sleepCont."
                Just h ->
                  do invokeEvent p $ disposeEvent h
                     case e of
                       ContCancellationInitiating ->
                         invokeEvent p $
                         enqueueEvent (pointTime p) $
                         Event $ \p ->
                         do z <- invokeEvent p $ contCanceled c
                            when z $ invokeEvent p $ cancelCont c
                       ContPreemptionEnding ->
                         invokeEvent p $
                         enqueueEvent (pointTime p) $
                         reenterCont c a
                       ContPreemptionBeginning ->
                         error "The computation was already preempted: sleepCont."
     invokeEvent p $ writeRef rh (Just h)

-- | Substitute the continuation.
substituteCont :: MonadDES m => ContParams m a -> (a -> Event m ()) -> ContParams m a
{-# INLINE substituteCont #-}
substituteCont c m = c { contCont = m }

-- | Await the signal.
contAwait :: MonadDES m => Signal m a -> Cont m a
{-# INLINABLE contAwait #-}
contAwait signal =
  Cont $ \c ->
  Event $ \p ->
  do let r = pointRun p
     c <- invokeEvent p $ freezeCont c
     rh <- invokeSimulation r $ newRef Nothing
     h <- invokeEvent p $
          handleSignal signal $ 
          \a -> Event $ 
                \p -> do x <- invokeEvent p $ readRef rh
                         case x of
                           Nothing ->
                             error "The signal was lost: contAwait."
                           Just x ->
                             do invokeEvent p $ disposeEvent x
                                c <- invokeEvent p $ unfreezeCont c
                                case c of
                                  Nothing -> return ()
                                  Just c  ->
                                    invokeEvent p $ reenterCont c a
     invokeEvent p $ writeRef rh $ Just h          

-- | Like the GoTo statement it transfers the direction of computation,
-- but raises an exception when used within 'catchCont' or 'finallyCont'.
transferCont :: MonadDES m => Cont m () -> Cont m a
{-# INLINABLE transferCont #-}
transferCont x =
  Cont $ \c ->
  Event $ \p ->
  do let worker =
           do let cid   = contId $ contAux c
                  cont  = return
                  econt = throwEvent
                  ccont = return
              when (contCatchFlag $ contAux c) $
                error "Cannot be combined with the exception handling: unsafeTransferCont"
              invokeEvent p $
                runCont x cont econt ccont cid False
     z <- invokeEvent p $ contCanceled c
     if z
       then invokeEvent p $ cancelCont c
       else worker

-- | Show the debug message with the current simulation time.
traceCont :: MonadDES m => String -> Cont m a -> Cont m a
{-# INLINABLE traceCont #-}
traceCont message (Cont m) =
  Cont $ \c ->
  Event $ \p ->
  do z <- invokeEvent p $ contCanceled c
     if z
       then invokeEvent p $ cancelCont c
       else trace ("t = " ++ show (pointTime p) ++ ": " ++ message) $
            invokeEvent p $ m c
