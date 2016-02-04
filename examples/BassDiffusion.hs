
-- This is the Bass Diffusion model solved with help of 
-- the Agent-based Modeling as described in the AnyLogic 
-- documentation.
--
-- The model describes a product diffusion process. Potential 
-- adopters of a product are influenced into buying the product 
-- by advertising and by word of mouth from adopters, those 
-- who have already purchased the new product. Adoption of 
-- a new product driven by word of mouth is likewise an epidemic. 
-- Potential adopters come into contact with adopters through 
-- social interactions. A fraction of these contacts results 
-- in the purchase of the new product. The advertising causes 
-- a constant fraction of the potential adopter population 
-- to adopt each time period.

import Data.Array

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans
import Simulation.Aivika.IO

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
              
createPerson :: Simulation IO (Person IO) 
createPerson =    
  do agent <- newAgent
     potentialAdopter <- newState agent
     adopter <- newState agent
     return Person { personAgent = agent,
                     personPotentialAdopter = potentialAdopter,
                     personAdopter = adopter }
       
createPersons :: Simulation IO (Array Int (Person IO))
createPersons =
  do list <- forM [1 .. n] $ \i ->
       do p <- createPerson
          return (i, p)
     return $ array (1, n) list
     
definePerson :: Person IO -> Array Int (Person IO) -> Ref IO Int -> Ref IO Int -> Event IO ()
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
        
definePersons :: Array Int (Person IO) -> Ref IO Int -> Ref IO Int -> Event IO ()
definePersons ps potentialAdopters adopters =
  forM_ (elems ps) $ \p -> 
  definePerson p ps potentialAdopters adopters
                               
activatePerson :: Person IO -> Event IO ()
activatePerson p = selectState (personPotentialAdopter p)

activatePersons :: Array Int (Person IO) -> Event IO ()
activatePersons ps =
  forM_ (elems ps) $ \p -> activatePerson p

model :: Simulation IO (Results IO)
model =
  do potentialAdopters <- newRef 0
     adopters <- newRef 0
     ps <- createPersons
     runEventInStartTime $
       do definePersons ps potentialAdopters adopters
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
