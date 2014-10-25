
{-# LANGUAGE RecursiveDo #-}

import Control.Monad.Fix

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.SystemDynamics

specs = Specs { spcStartTime = 0, 
                spcStopTime = 13, 
                spcDT = 0.01,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

model :: (MonadComp m, MonadFix m) => Simulation m (Results m)
model = 
  mdo a <- integ (- ka * a) 100
      b <- integ (ka * a - kb * b) 0
      c <- integ (kb * b) 0
      let ka = 1
          kb = 1
      return $ results
        [resultSource "a" "variable A" a,
         resultSource "b" "variable B" b,
         resultSource "c" "variable C" c]

main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  model specs
