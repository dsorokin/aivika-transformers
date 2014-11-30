
-- |
-- Module     : Simulation.Aivika.Trans.Agent
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module introduces basic entities for the agent-based modeling.
--
module Simulation.Aivika.Trans.Agent
       (Agent,
        AgentState,
        newAgent,
        newState,
        newSubstate,
        selectedState,
        selectedStateChanged,
        selectedStateChanged_,
        selectState,
        stateAgent,
        stateParent,
        addTimeout,
        addTimer,
        setStateActivation,
        setStateDeactivation,
        setStateTransition) where

import Control.Monad

import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Signal

--
-- Agent-based Modeling
--

-- | Represents an agent.
data Agent m = Agent { agentModeRef            :: Ref m AgentMode,
                       agentStateRef           :: Ref m (Maybe (AgentState m)), 
                       agentStateChangedSource :: SignalSource m (Maybe (AgentState m)) }

-- | Represents the agent state.
data AgentState m = AgentState { stateAgent         :: Agent m,
                                 -- ^ Return the corresponded agent.
                                 stateParent        :: Maybe (AgentState m),
                                 -- ^ Return the parent state or 'Nothing'.
                                 stateActivateRef   :: Ref m (Event m ()),
                                 stateDeactivateRef :: Ref m (Event m ()),
                                 stateTransitRef    :: Ref m (Event m (Maybe (AgentState m))),
                                 stateVersionRef    :: Ref m Int }
                  
data AgentMode = CreationMode
               | TransientMode
               | ProcessingMode
                      
instance MonadDES m => Eq (Agent m) where

  {-# INLINE (==) #-}
  x == y = agentStateRef x == agentStateRef y
  
instance MonadDES m => Eq (AgentState m) where

  {-# INLINE (==) #-}
  x == y = stateVersionRef x == stateVersionRef y

fullPath :: AgentState m -> [AgentState m] -> [AgentState m]
fullPath st acc =
  case stateParent st of
    Nothing  -> st : acc
    Just st' -> fullPath st' (st : acc)

partitionPath :: MonadDES m => [AgentState m] -> [AgentState m] -> ([AgentState m], [AgentState m])
{-# INLINABLE partitionPath #-}
partitionPath path1 path2 =
  case (path1, path2) of
    (h1 : t1, [h2]) | h1 == h2 -> 
      (reverse path1, path2)
    (h1 : t1, h2 : t2) | h1 == h2 -> 
      partitionPath t1 t2
    _ ->
      (reverse path1, path2)

findPath :: MonadDES m => Maybe (AgentState m) -> AgentState m -> ([AgentState m], [AgentState m])
{-# INLINABLE findPath #-}
findPath Nothing target = ([], fullPath target [])
findPath (Just source) target
  | stateAgent source /= stateAgent target =
    error "Different agents: findPath."
  | otherwise =
    partitionPath path1 path2
  where
    path1 = fullPath source []
    path2 = fullPath target []

traversePath :: MonadDES m => Maybe (AgentState m) -> AgentState m -> Event m ()
{-# INLINABLE traversePath #-}
traversePath source target =
  let (path1, path2) = findPath source target
      agent = stateAgent target
      activate st p   = invokeEvent p =<< (invokeEvent p $ readRef (stateActivateRef st))
      deactivate st p = invokeEvent p =<< (invokeEvent p $ readRef (stateDeactivateRef st))
      transit st p    = invokeEvent p =<< (invokeEvent p $ readRef (stateTransitRef st))
      continue st p   = invokeEvent p $ traversePath (Just target) st
  in Event $ \p ->
       unless (null path1 && null path2) $
       do invokeEvent p $ writeRef (agentModeRef agent) TransientMode
          forM_ path1 $ \st ->
            do invokeEvent p $ writeRef (agentStateRef agent) (Just st)
               deactivate st p
               -- it makes all timeout and timer handlers outdated
               invokeEvent p $ modifyRef (stateVersionRef st) (1 +)
          forM_ path2 $ \st ->
            do invokeEvent p $ writeRef (agentStateRef agent) (Just st)
               activate st p
          st' <- transit target p
          case st' of
            Nothing ->
              do invokeEvent p $ writeRef (agentModeRef agent) ProcessingMode
                 triggerAgentStateChanged p agent
            Just st' ->
              continue st' p

-- | Add to the state a timeout handler that will be actuated 
-- in the specified time period if the state will remain active.
addTimeout :: MonadDES m => AgentState m -> Double -> Event m () -> Event m ()
{-# INLINABLE addTimeout #-}
addTimeout st dt action =
  Event $ \p ->
  do v <- invokeEvent p $ readRef (stateVersionRef st)
     let m1 = Event $ \p ->
           do v' <- invokeEvent p $ readRef (stateVersionRef st)
              when (v == v') $
                invokeEvent p action
         m2 = enqueueEvent (pointTime p + dt) m1
     invokeEvent p m2

-- | Add to the state a timer handler that will be actuated
-- in the specified time period and then repeated again many times,
-- while the state remains active.
addTimer :: MonadDES m => AgentState m -> Event m Double -> Event m () -> Event m ()
{-# INLINABLE addTimer #-}
addTimer st dt action =
  Event $ \p ->
  do v <- invokeEvent p $ readRef (stateVersionRef st)
     let m1 = Event $ \p ->
           do v' <- invokeEvent p $ readRef (stateVersionRef st)
              when (v == v') $
                do invokeEvent p m2
                   invokeEvent p action
         m2 = Event $ \p ->
           do dt' <- invokeEvent p dt
              invokeEvent p $ enqueueEvent (pointTime p + dt') m1
     invokeEvent p m2

-- | Create a new state.
newState :: MonadDES m => Agent m -> Simulation m (AgentState m)
{-# INLINABLE newState #-}
newState agent =
  do aref <- newRef $ return ()
     dref <- newRef $ return ()
     tref <- newRef $ return Nothing
     vref <- newRef 0
     return AgentState { stateAgent = agent,
                         stateParent = Nothing,
                         stateActivateRef = aref,
                         stateDeactivateRef = dref,
                         stateTransitRef = tref,
                         stateVersionRef = vref }

-- | Create a child state.
newSubstate :: MonadDES m => AgentState m -> Simulation m (AgentState m)
{-# INLINABLE newSubstate #-}
newSubstate parent =
  do let agent = stateAgent parent
     aref <- newRef $ return ()
     dref <- newRef $ return ()
     tref <- newRef $ return Nothing
     vref <- newRef 0
     return AgentState { stateAgent = agent,
                         stateParent = Just parent,
                         stateActivateRef= aref,
                         stateDeactivateRef = dref,
                         stateTransitRef = tref,
                         stateVersionRef = vref }

-- | Create an agent.
newAgent :: MonadDES m => Simulation m (Agent m)
{-# INLINABLE newAgent #-}
newAgent =
  do modeRef  <- newRef CreationMode
     stateRef <- newRef Nothing
     stateChangedSource <- newSignalSource
     return Agent { agentModeRef = modeRef,
                    agentStateRef = stateRef, 
                    agentStateChangedSource = stateChangedSource }

-- | Return the selected active state.
selectedState :: MonadDES m => Agent m -> Event m (Maybe (AgentState m))
{-# INLINABLE selectedState #-}
selectedState agent = readRef (agentStateRef agent)
                   
-- | Select the state. The activation and selection are repeated while
-- there is the transition state defined by 'setStateTransition'.
selectState :: MonadDES m => AgentState m -> Event m ()
{-# INLINABLE selectState #-}
selectState st =
  Event $ \p ->
  do let agent = stateAgent st
     mode <- invokeEvent p $ readRef (agentModeRef agent)
     case mode of
       CreationMode ->
         do x0 <- invokeEvent p $ readRef (agentStateRef agent)
            invokeEvent p $ traversePath x0 st
       TransientMode ->
         error $
         "Use the setStateTransition function to define " ++
         "the transition state: activateState."
       ProcessingMode ->
         do x0 @ (Just st0) <- invokeEvent p $ readRef (agentStateRef agent)
            invokeEvent p $ traversePath x0 st

-- | Set the activation computation for the specified state.
setStateActivation :: MonadDES m => AgentState m -> Event m () -> Event m ()
{-# INLINABLE setStateActivation #-}
setStateActivation st action =
  writeRef (stateActivateRef st) action
  
-- | Set the deactivation computation for the specified state.
setStateDeactivation :: MonadDES m => AgentState m -> Event m () -> Event m ()
{-# INLINABLE setStateDeactivation #-}
setStateDeactivation st action =
  writeRef (stateDeactivateRef st) action
  
-- | Set the transition state which will be next and which is used only
-- when selecting the state directly with help of 'selectState'.
-- If the state was activated intermediately, when selecting
-- another state, then this computation is not used.
setStateTransition :: MonadDES m => AgentState m -> Event m (Maybe (AgentState m)) -> Event m ()
{-# INLINABLE setStateTransition #-}
setStateTransition st action =
  writeRef (stateTransitRef st) action
  
-- | Trigger the signal when the agent state changes.
triggerAgentStateChanged :: MonadDES m => Point m -> Agent m -> m ()
{-# INLINABLE triggerAgentStateChanged #-}
triggerAgentStateChanged p agent =
  do st <- invokeEvent p $ readRef (agentStateRef agent)
     invokeEvent p $ triggerSignal (agentStateChangedSource agent) st

-- | Return a signal that notifies about every change of the selected state.
selectedStateChanged :: Agent m -> Signal m (Maybe (AgentState m))
{-# INLINABLE selectedStateChanged #-}
selectedStateChanged agent =
  publishSignal (agentStateChangedSource agent)

-- | Return a signal that notifies about every change of the selected state.
selectedStateChanged_ :: MonadDES m => Agent m -> Signal m ()
{-# INLINABLE selectedStateChanged_ #-}
selectedStateChanged_ agent =
  mapSignal (const ()) $ selectedStateChanged agent
