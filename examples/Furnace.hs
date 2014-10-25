
-- This is a model of the Furnace. It is described in different sources [1, 2].
--
-- [1] A. Alan B. Pritsker, Simulation with Visual SLAM and AweSim, 2nd ed.
--
-- [2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006

import Data.Maybe
import System.Random
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Queue.Infinite

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                -- spcStopTime = 1000.0,
                spcStopTime = 300.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }
        
-- | Return a random initial temperature of the item.     
randomTemp :: MonadComp m => Parameter m Double
randomTemp = randomUniform 400 600

-- | Represents the furnace.
data Furnace m = 
  Furnace { furnacePits :: [Pit m],
            -- ^ The pits for ingots.
            furnacePitCount :: Ref m Int,
            -- ^ The count of active pits with ingots.
            furnaceQueue :: FCFSQueue m (Ingot m),
            -- ^ The furnace queue.
            furnaceUnloadedSource :: SignalSource m (),
            -- ^ Notifies when the ingots have been
            -- unloaded from the furnace.
            furnaceHeatingTime :: Ref m (SamplingStats Double),
            -- ^ The heating time for the ready ingots.
            furnaceTemp :: Ref m Double,
            -- ^ The furnace temperature.
            furnaceReadyCount :: Ref m Int,
            -- ^ The count of ready ingots.
            furnaceReadyTemps :: Ref m [Double]
            -- ^ The temperatures of all ready ingots.
            }

-- | Notifies when the ingots have been unloaded from the furnace.
furnaceUnloaded :: Furnace m -> Signal m ()
furnaceUnloaded = publishSignal . furnaceUnloadedSource

-- | A pit in the furnace to place the ingots.
data Pit m = 
  Pit { pitIngot :: Ref m (Maybe (Ingot m)),
        -- ^ The ingot in the pit.
        pitTemp :: Ref m Double
        -- ^ The ingot temperature in the pit.
        }

data Ingot m = 
  Ingot { ingotFurnace :: Furnace m,
          -- ^ The furnace.
          ingotReceiveTime :: Double,
          -- ^ The time at which the ingot was received.
          ingotReceiveTemp :: Double,
          -- ^ The temperature with which the ingot was received.
          ingotLoadTime :: Double,
          -- ^ The time of loading in the furnace.
          ingotLoadTemp :: Double,
          -- ^ The temperature when the ingot was loaded in the furnace.
          ingotCoeff :: Double
          -- ^ The heating coefficient.
          }

-- | Create a furnace.
newFurnace :: MonadComp m => Simulation m (Furnace m)
newFurnace =
  do pits <- sequence [newPit | i <- [1..10]]
     pitCount <- newRef 0
     queue <- runEventInStartTime newFCFSQueue
     heatingTime <- newRef emptySamplingStats
     h <- newRef 1650.0
     readyCount <- newRef 0
     readyTemps <- newRef []
     s <- newSignalSource
     return Furnace { furnacePits = pits,
                      furnacePitCount = pitCount,
                      furnaceQueue = queue,
                      furnaceUnloadedSource = s,
                      furnaceHeatingTime = heatingTime,
                      furnaceTemp = h,
                      furnaceReadyCount = readyCount, 
                      furnaceReadyTemps = readyTemps }

-- | Create a new pit.
newPit :: MonadComp m => Simulation m (Pit m)
newPit =
  do ingot <- newRef Nothing
     h' <- newRef 0.0
     return Pit { pitIngot = ingot,
                  pitTemp  = h' }

-- | Create a new ingot.
newIngot :: MonadComp m => Furnace m -> Event m (Ingot m)
newIngot furnace =
  do t  <- liftDynamics time
     xi <- liftParameter $ randomNormal 0.05 0.01
     h' <- liftParameter randomTemp
     let c = 0.1 + xi
     return Ingot { ingotFurnace = furnace,
                    ingotReceiveTime = t,
                    ingotReceiveTemp = h',
                    ingotLoadTime = t,
                    ingotLoadTemp = h',
                    ingotCoeff = c }

-- | Heat the ingot up in the pit if there is such an ingot.
heatPitUp :: MonadComp m => Pit m -> Event m ()
heatPitUp pit =
  do ingot <- readRef (pitIngot pit)
     case ingot of
       Nothing -> 
         return ()
       Just ingot -> do
         
         -- update the temperature of the ingot.
         let furnace = ingotFurnace ingot
         dt' <- liftParameter dt
         h'  <- readRef (pitTemp pit)
         h   <- readRef (furnaceTemp furnace)
         writeRef (pitTemp pit) $ 
           h' + dt' * (h - h') * ingotCoeff ingot

-- | Check whether there are ready ingots in the pits.
ingotsReady :: MonadComp m => Furnace m -> Event m Bool
ingotsReady furnace =
  fmap (not . null) $ 
  filterM (fmap (>= 2200.0) . readRef . pitTemp) $ 
  furnacePits furnace

-- | Try to unload the ready ingot from the specified pit.
tryUnloadPit :: MonadComp m => Furnace m -> Pit m -> Event m ()
tryUnloadPit furnace pit =
  do h' <- readRef (pitTemp pit)
     when (h' >= 2000.0) $
       do Just ingot <- readRef (pitIngot pit)  
          unloadIngot furnace ingot pit

-- | Try to load an awaiting ingot in the specified empty pit.
tryLoadPit :: MonadComp m => Furnace m -> Pit m -> Event m ()       
tryLoadPit furnace pit =
  do ingot <- tryDequeue (furnaceQueue furnace)
     case ingot of
       Nothing ->
         return ()
       Just ingot ->
         do t' <- liftDynamics time
            loadIngot furnace (ingot { ingotLoadTime = t',
                                       ingotLoadTemp = 400.0 }) pit
              
-- | Unload the ingot from the specified pit.       
unloadIngot :: MonadComp m => Furnace m -> Ingot m -> Pit m -> Event m ()
unloadIngot furnace ingot pit = 
  do h' <- readRef (pitTemp pit)
     writeRef (pitIngot pit) Nothing
     writeRef (pitTemp pit) 0.0

     -- count the active pits
     modifyRef (furnacePitCount furnace) (+ (- 1))
     
     -- how long did we heat the ingot up?
     t' <- liftDynamics time
     modifyRef (furnaceHeatingTime furnace) $
       addSamplingStats (t' - ingotLoadTime ingot)
     
     -- what is the temperature of the unloaded ingot?
     modifyRef (furnaceReadyTemps furnace) (h' :)
     
     -- count the ready ingots
     modifyRef (furnaceReadyCount furnace) (+ 1)
     
-- | Load the ingot in the specified pit
loadIngot :: MonadComp m => Furnace m -> Ingot m -> Pit m -> Event m ()
loadIngot furnace ingot pit =
  do writeRef (pitIngot pit) $ Just ingot
     writeRef (pitTemp pit) $ ingotLoadTemp ingot

     -- count the active pits
     modifyRef (furnacePitCount furnace) (+ 1)
     count <- readRef (furnacePitCount furnace)
     
     -- decrease the furnace temperature
     h <- readRef (furnaceTemp furnace)
     let h' = ingotLoadTemp ingot
         dh = - (h - h') / fromIntegral count
     writeRef (furnaceTemp furnace) $ h + dh
 
-- | Start iterating the furnace processing through the event queue.
startIteratingFurnace :: MonadComp m => Furnace m -> Event m ()
startIteratingFurnace furnace = 
  let pits = furnacePits furnace
  in enqueueEventWithIntegTimes $
     do -- try to unload ready ingots
        ready <- ingotsReady furnace
        when ready $ 
          do mapM_ (tryUnloadPit furnace) pits
             triggerSignal (furnaceUnloadedSource furnace) ()

        -- heat up
        mapM_ heatPitUp pits
        
        -- update the temperature of the furnace
        dt' <- liftParameter dt
        h   <- readRef (furnaceTemp furnace)
        writeRef (furnaceTemp furnace) $
          h + dt' * (2600.0 - h) * 0.2

-- | Return all empty pits.
emptyPits :: MonadComp m => Furnace m -> Event m [Pit m]
emptyPits furnace =
  filterM (fmap isNothing . readRef . pitIngot) $
  furnacePits furnace

-- | This process takes ingots from the queue and then
-- loads them in the furnace.
loadingProcess :: MonadComp m => Furnace m -> Process m ()
loadingProcess furnace =
  do ingot <- dequeue (furnaceQueue furnace)
     let wait =
           do count <- liftEvent $ readRef (furnacePitCount furnace)
              when (count >= 10) $
                do processAwait (furnaceUnloaded furnace)
                   wait
     wait
     --  take any empty pit and load it
     liftEvent $
       do pit: _ <- emptyPits furnace
          loadIngot furnace ingot pit
     -- repeat it again
     loadingProcess furnace
                  
-- | The input process that adds new ingots to the queue.
inputProcess :: MonadComp m => Furnace m -> Process m ()
inputProcess furnace =
  do delay <- liftParameter $
              randomExponential 2.5
     holdProcess delay
     -- we have got a new ingot
     liftEvent $
       do ingot <- newIngot furnace
          enqueue (furnaceQueue furnace) ingot
     -- repeat it again
     inputProcess furnace

-- | Initialize the furnace.
initializeFurnace :: MonadComp m => Furnace m -> Event m ()
initializeFurnace furnace =
  do x1 <- newIngot furnace
     x2 <- newIngot furnace
     x3 <- newIngot furnace
     x4 <- newIngot furnace
     x5 <- newIngot furnace
     x6 <- newIngot furnace
     let p1 : p2 : p3 : p4 : p5 : p6 : ps = 
           furnacePits furnace
     loadIngot furnace (x1 { ingotLoadTemp = 550.0 }) p1
     loadIngot furnace (x2 { ingotLoadTemp = 600.0 }) p2
     loadIngot furnace (x3 { ingotLoadTemp = 650.0 }) p3
     loadIngot furnace (x4 { ingotLoadTemp = 700.0 }) p4
     loadIngot furnace (x5 { ingotLoadTemp = 750.0 }) p5
     loadIngot furnace (x6 { ingotLoadTemp = 800.0 }) p6
     writeRef (furnaceTemp furnace) 1650.0
     
-- | The simulation model.
model :: MonadComp m => Simulation m (Results m)
model =
  do furnace <- newFurnace
  
     -- initialize the furnace and start its iterating in start time
     runEventInStartTime $
       do initializeFurnace furnace
          startIteratingFurnace furnace
     
     -- generate randomly new input ingots
     runProcessInStartTime $
       inputProcess furnace

     -- load permanently the input ingots in the furnace
     runProcessInStartTime $
       loadingProcess furnace

     -- return the simulation results
     return $
       resultSummary $
       results
       [resultSource "inputIngotCount" "the input ingot count" $
        enqueueStoreCount (furnaceQueue furnace),
        --
        resultSource "loadedIngotCount" "the loaded ingot count" $
        dequeueCount (furnaceQueue furnace),
        --
        resultSource "outputIngotCount" "the output ingot count" $
        furnaceReadyCount furnace,
        --
        resultSource "outputIngotTemp" "the output ingot temperature" $
        fmap listSamplingStats $ readRef $ furnaceReadyTemps furnace,
        --
        resultSource "heatingTime" "the heating time" $
        furnaceHeatingTime furnace,
        --
        resultSource "pitCount" "the number of ingots in pits" $
        furnacePitCount furnace,
        --
        resultSource "furnaceQueue" "the furnace queue" $
        furnaceQueue furnace]

-- | The main program.
main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  model specs
