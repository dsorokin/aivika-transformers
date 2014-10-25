
-- This is the Bass Diffusion model solved with help of 
-- the Agent-based Modeling as described in the AnyLogic 
-- documentation.

import Data.Array

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans

n = 500    -- the number of agents

advertisingEffectiveness = 0.011
contactRate = 100.0
adoptionFraction = 0.015

specs = Specs { spcStartTime = 0.0, 
                spcStopTime = 8.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

data Person m = Person { personAgent :: Agent m,
                         personPotentialAdopter :: AgentState m,
                         personAdopter :: AgentState m }
              
createPerson :: MonadComp m => Simulation m (Person m) 
createPerson =    
  do agent <- newAgent
     potentialAdopter <- newState agent
     adopter <- newState agent
     return Person { personAgent = agent,
                     personPotentialAdopter = potentialAdopter,
                     personAdopter = adopter }
       
createPersons :: MonadComp m => Simulation m (Array Int (Person m))
createPersons =
  do list <- forM [1 .. n] $ \i ->
       do p <- createPerson
          return (i, p)
     return $ array (1, n) list
     
definePerson :: MonadComp m => Person m -> Array Int (Person m) -> Ref m Int -> Ref m Int -> Simulation m ()
definePerson p ps potentialAdopters adopters =
  do setStateActivation (personPotentialAdopter p) $
       do modifyRef potentialAdopters $ \a -> a + 1
          -- add a timeout
          t <- liftParameter $
               randomExponential (1 / advertisingEffectiveness) 
          let st  = personPotentialAdopter p
              st' = personAdopter p
          addTimeout st t $ selectState st'
     setStateActivation (personAdopter p) $ 
       do modifyRef adopters  $ \a -> a + 1
          -- add a timer that works while the state is active
          let t = liftParameter $
                  randomExponential (1 / contactRate)    -- many times!
          addTimer (personAdopter p) t $
            do i <- liftParameter $
                    randomUniformInt 1 n
               let p' = ps ! i
               st <- selectedState (personAgent p')
               when (st == Just (personPotentialAdopter p')) $
                 do b <- liftParameter $
                         randomTrue adoptionFraction
                    when b $ selectState (personAdopter p')
     setStateDeactivation (personPotentialAdopter p) $
       modifyRef potentialAdopters $ \a -> a - 1
     setStateDeactivation (personAdopter p) $
       modifyRef adopters $ \a -> a - 1
        
definePersons :: MonadComp m => Array Int (Person m) -> Ref m Int -> Ref m Int -> Simulation m ()
definePersons ps potentialAdopters adopters =
  forM_ (elems ps) $ \p -> 
  definePerson p ps potentialAdopters adopters
                               
activatePerson :: MonadComp m => Person m -> Event m ()
activatePerson p = selectState (personPotentialAdopter p)

activatePersons :: MonadComp m => Array Int (Person m) -> Event m ()
activatePersons ps =
  forM_ (elems ps) $ \p -> activatePerson p

model :: MonadComp m => Simulation m (Results m)
model =
  do potentialAdopters <- newRef 0
     adopters <- newRef 0
     ps <- createPersons
     definePersons ps potentialAdopters adopters
     runEventInStartTime $
       activatePersons ps
     return $ 
       results
       [resultSource 
        "potentialAdopter" "potential adopters" potentialAdopters,
        resultSource 
        "adopters" "adopters" adopters]

main =
  printSimulationResultsInIntegTimes
  printResultSourceInEnglish
  model specs
