
-- |
-- Module     : Simulation.Aivika.Trans.Agent
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL
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

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Signal

--
-- Agent-based Modeling
--

-- | Represents an agent.
data Agent m = Agent { agentMarker             :: SessionMarker m,
                       agentModeRef            :: ProtoRef m AgentMode,
                       agentStateRef           :: ProtoRef m (Maybe (AgentState m)), 
                       agentStateChangedSource :: SignalSource m (Maybe (AgentState m)) }

-- | Represents the agent state.
data AgentState m = AgentState { stateAgent         :: Agent m,
                                 -- ^ Return the corresponded agent.
                                 stateParent        :: Maybe (AgentState m),
                                 -- ^ Return the parent state or 'Nothing'.
                                 stateMarker        :: SessionMarker m,
                                 stateActivateRef   :: ProtoRef m (Event m ()),
                                 stateDeactivateRef :: ProtoRef m (Event m ()),
                                 stateTransitRef    :: ProtoRef m (Event m (Maybe (AgentState m))),
                                 stateVersionRef    :: ProtoRef m Int }
                  
data AgentMode = CreationMode
               | TransientMode
               | ProcessingMode
                      
instance MonadComp m => Eq (Agent m) where
  x == y = agentMarker x == agentMarker y
  
instance MonadComp m => Eq (AgentState m) where
  x == y = stateMarker x == stateMarker y

fullPath :: AgentState m -> [AgentState m] -> [AgentState m]
fullPath st acc =
  case stateParent st of
    Nothing  -> st : acc
    Just st' -> fullPath st' (st : acc)

partitionPath :: MonadComp m => [AgentState m] -> [AgentState m] -> ([AgentState m], [AgentState m])
partitionPath path1 path2 =
  case (path1, path2) of
    (h1 : t1, [h2]) | h1 == h2 -> 
      (reverse path1, path2)
    (h1 : t1, h2 : t2) | h1 == h2 -> 
      partitionPath t1 t2
    _ ->
      (reverse path1, path2)

findPath :: MonadComp m => Maybe (AgentState m) -> AgentState m -> ([AgentState m], [AgentState m])
findPath Nothing target = ([], fullPath target [])
findPath (Just source) target
  | stateAgent source /= stateAgent target =
    error "Different agents: findPath."
  | otherwise =
    partitionPath path1 path2
  where
    path1 = fullPath source []
    path2 = fullPath target []

traversePath :: MonadComp m => Maybe (AgentState m) -> AgentState m -> Event m ()
traversePath source target =
  let (path1, path2) = findPath source target
      agent = stateAgent target
      activate st p   = invokeEvent p =<< readProtoRef (stateActivateRef st)
      deactivate st p = invokeEvent p =<< readProtoRef (stateDeactivateRef st)
      transit st p    = invokeEvent p =<< readProtoRef (stateTransitRef st)
      continue st p   = invokeEvent p $ traversePath (Just target) st
  in Event $ \p ->
       unless (null path1 && null path2) $
       do writeProtoRef (agentModeRef agent) TransientMode
          forM_ path1 $ \st ->
            do writeProtoRef (agentStateRef agent) (Just st)
               deactivate st p
               -- it makes all timeout and timer handlers outdated
               modifyProtoRef (stateVersionRef st) (1 +)
          forM_ path2 $ \st ->
            do writeProtoRef (agentStateRef agent) (Just st)
               activate st p
          st' <- transit target p
          case st' of
            Nothing ->
              do writeProtoRef (agentModeRef agent) ProcessingMode
                 triggerAgentStateChanged p agent
            Just st' ->
              continue st' p

-- | Add to the state a timeout handler that will be actuated 
-- in the specified time period if the state will remain active.
addTimeout :: MonadComp m => AgentState m -> Double -> Event m () -> Event m ()
addTimeout st dt action =
  Event $ \p ->
  do v <- readProtoRef (stateVersionRef st)
     let m1 = Event $ \p ->
           do v' <- readProtoRef (stateVersionRef st)
              when (v == v') $
                invokeEvent p action
         m2 = enqueueEvent (pointTime p + dt) m1
     invokeEvent p m2

-- | Add to the state a timer handler that will be actuated
-- in the specified time period and then repeated again many times,
-- while the state remains active.
addTimer :: MonadComp m => AgentState m -> Event m Double -> Event m () -> Event m ()
addTimer st dt action =
  Event $ \p ->
  do v <- readProtoRef (stateVersionRef st)
     let m1 = Event $ \p ->
           do v' <- readProtoRef (stateVersionRef st)
              when (v == v') $
                do invokeEvent p m2
                   invokeEvent p action
         m2 = Event $ \p ->
           do dt' <- invokeEvent p dt
              invokeEvent p $ enqueueEvent (pointTime p + dt') m1
     invokeEvent p m2

-- | Create a new state.
newState :: MonadComp m => Agent m -> Simulation m (AgentState m)
newState agent =
  Simulation $ \r ->
  do let s = runSession r
     aref <- newProtoRef s $ return ()
     dref <- newProtoRef s $ return ()
     tref <- newProtoRef s $ return Nothing
     vref <- newProtoRef s 0
     mrkr <- newSessionMarker s
     return AgentState { stateAgent = agent,
                         stateParent = Nothing,
                         stateMarker = mrkr,
                         stateActivateRef = aref,
                         stateDeactivateRef = dref,
                         stateTransitRef = tref,
                         stateVersionRef = vref }

-- | Create a child state.
newSubstate :: MonadComp m => AgentState m -> Simulation m (AgentState m)
newSubstate parent =
  Simulation $ \r ->
  do let agent = stateAgent parent
         s = runSession r
     aref <- newProtoRef s $ return ()
     dref <- newProtoRef s $ return ()
     tref <- newProtoRef s $ return Nothing
     vref <- newProtoRef s 0
     mrkr <- newSessionMarker s
     return AgentState { stateAgent = agent,
                         stateParent = Just parent,
                         stateMarker = mrkr,
                         stateActivateRef= aref,
                         stateDeactivateRef = dref,
                         stateTransitRef = tref,
                         stateVersionRef = vref }

-- | Create an agent.
newAgent :: MonadComp m => Simulation m (Agent m)
newAgent =
  Simulation $ \r ->
  do let s = runSession r
     modeRef  <- newProtoRef s CreationMode
     stateRef <- newProtoRef s Nothing
     stateChangedSource <- invokeSimulation r newSignalSource
     mrkr <- newSessionMarker s
     return Agent { agentMarker = mrkr,
                    agentModeRef = modeRef,
                    agentStateRef = stateRef, 
                    agentStateChangedSource = stateChangedSource }

-- | Return the selected active state.
selectedState :: MonadComp m => Agent m -> Event m (Maybe (AgentState m))
selectedState agent =
  Event $ \p -> readProtoRef (agentStateRef agent)
                   
-- | Select the state. The activation and selection are repeated while
-- there is the transition state defined by 'setStateTransition'.
selectState :: MonadComp m => AgentState m -> Event m ()
selectState st =
  Event $ \p ->
  do let agent = stateAgent st
     mode <- readProtoRef (agentModeRef agent)
     case mode of
       CreationMode ->
         do x0 <- readProtoRef (agentStateRef agent)
            invokeEvent p $ traversePath x0 st
       TransientMode ->
         error $
         "Use the setStateTransition function to define " ++
         "the transition state: activateState."
       ProcessingMode ->
         do x0 @ (Just st0) <- readProtoRef (agentStateRef agent)
            invokeEvent p $ traversePath x0 st

-- | Set the activation computation for the specified state.
setStateActivation :: MonadComp m => AgentState m -> Event m () -> Simulation m ()
setStateActivation st action =
  Simulation $ \r ->
  writeProtoRef (stateActivateRef st) action
  
-- | Set the deactivation computation for the specified state.
setStateDeactivation :: MonadComp m => AgentState m -> Event m () -> Simulation m ()
setStateDeactivation st action =
  Simulation $ \r ->
  writeProtoRef (stateDeactivateRef st) action
  
-- | Set the transition state which will be next and which is used only
-- when selecting the state directly with help of 'selectState'.
-- If the state was activated intermediately, when selecting
-- another state, then this computation is not used.
setStateTransition :: MonadComp m => AgentState m -> Event m (Maybe (AgentState m)) -> Simulation m ()
setStateTransition st action =
  Simulation $ \r ->
  writeProtoRef (stateTransitRef st) action
  
-- | Trigger the signal when the agent state changes.
triggerAgentStateChanged :: MonadComp m => Point m -> Agent m -> m ()
triggerAgentStateChanged p agent =
  do st <- readProtoRef (agentStateRef agent)
     invokeEvent p $ triggerSignal (agentStateChangedSource agent) st

-- | Return a signal that notifies about every change of the selected state.
selectedStateChanged :: Agent m -> Signal m (Maybe (AgentState m))
selectedStateChanged agent =
  publishSignal (agentStateChangedSource agent)

-- | Return a signal that notifies about every change of the selected state.
selectedStateChanged_ :: MonadComp m => Agent m -> Signal m ()
selectedStateChanged_ agent =
  mapSignal (const ()) $ selectedStateChanged agent
