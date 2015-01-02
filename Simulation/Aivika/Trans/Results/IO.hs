
-- |
-- Module     : Simulation.Aivika.Trans.Results.IO
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module allows printing and converting the 'Simulation' 'Results' to a 'String'.
--
module Simulation.Aivika.Trans.Results.IO
       (-- * Basic Types
        ResultSourcePrint,
        ResultSourceShowS,
        -- * Printing the Results
        printResultsWithTime,
        printResultsInStartTime,
        printResultsInStopTime,
        printResultsInIntegTimes,
        printResultsInTime,
        printResultsInTimes,
        -- * Simulating and Printing the Results
        printSimulationResultsInStartTime,
        printSimulationResultsInStopTime,
        printSimulationResultsInIntegTimes,
        printSimulationResultsInTime,
        printSimulationResultsInTimes,
        -- * Showing the Results
        showResultsWithTime,
        showResultsInStartTime,
        showResultsInStopTime,
        showResultsInIntegTimes,
        showResultsInTime,
        showResultsInTimes,
        -- * Simulating and Showing the Results
        showSimulationResultsInStartTime,
        showSimulationResultsInStopTime,
        showSimulationResultsInIntegTimes,
        showSimulationResultsInTime,
        showSimulationResultsInTimes,
        -- * Printing the Result Source
        hPrintResultSourceIndented,
        hPrintResultSource,
        hPrintResultSourceInRussian,
        hPrintResultSourceInEnglish,
        printResultSourceIndented,
        printResultSource,
        printResultSourceInRussian,
        printResultSourceInEnglish,
        -- * Showing the Result Source
        showResultSourceIndented,
        showResultSource,
        showResultSourceInRussian,
        showResultSourceInEnglish) where

import Control.Monad
import Control.Monad.Trans

import qualified Data.Map as M
import qualified Data.Array as A

import System.IO

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Specs
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Dynamics
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.Results
import Simulation.Aivika.Trans.Results.Locale

-- | This is a function that shows the simulation results within
-- the 'Event' computation synchronized with the event queue.
type ResultSourceShowS m = ResultSource m -> Event m ShowS

-- | This is a function that prints the simulation results within
-- the 'Event' computation synchronized with the event queue.
type ResultSourcePrint m = ResultSource m -> Event m ()

-- | Print a localised text representation of the results by the specified source
-- and with the given indent.
hPrintResultSourceIndented :: (MonadDES m, MonadIO m)
                              => Handle
                              -- ^ a handle
                              -> Int
                              -- ^ an indent
                              -> ResultLocalisation
                              -- ^ a localisation
                              -> ResultSourcePrint m
{-# INLINABLE hPrintResultSourceIndented #-}
hPrintResultSourceIndented h indent loc source@(ResultItemSource (ResultItem x)) =
  hPrintResultSourceIndentedLabelled h indent (resultItemName x) loc source
hPrintResultSourceIndented h indent loc source@(ResultVectorSource x) =
  hPrintResultSourceIndentedLabelled h indent (resultVectorName x) loc source
hPrintResultSourceIndented h indent loc source@(ResultObjectSource x) =
  hPrintResultSourceIndentedLabelled h indent (resultObjectName x) loc source
hPrintResultSourceIndented h indent loc source@(ResultSeparatorSource x) =
  hPrintResultSourceIndentedLabelled h indent (resultSeparatorText x) loc source

-- | Print an indented and labelled text representation of the results by
-- the specified source.
hPrintResultSourceIndentedLabelled :: (MonadDES m, MonadIO m)
                                      => Handle
                                      -- ^ a handle
                                      -> Int
                                      -- ^ an indent
                                      -> ResultName
                                      -- ^ a label
                                      -> ResultLocalisation
                                      -- ^ a localisation
                                      -> ResultSourcePrint m
{-# INLINABLE hPrintResultSourceIndentedLabelled #-}
hPrintResultSourceIndentedLabelled h indent label loc (ResultItemSource (ResultItem x)) =
  do a <- resultValueData $ resultItemToStringValue x
     let tab = replicate indent ' '
     liftIO $
       do hPutStr h tab
          hPutStr h "-- "
          hPutStr h (loc $ resultItemId x)
          hPutStrLn h ""
          hPutStr h tab
          hPutStr h label
          hPutStr h " = "
          hPutStrLn h a
          hPutStrLn h ""
hPrintResultSourceIndentedLabelled h indent label loc (ResultVectorSource x) =
  do let tab = replicate indent ' '
     liftIO $
       do hPutStr h tab
          hPutStr h "-- "
          hPutStr h (loc $ resultVectorId x)
          hPutStrLn h ""
          hPutStr h tab
          hPutStr h label
          hPutStrLn h ":"
          hPutStrLn h ""
     let items = A.elems (resultVectorItems x)
         subscript = A.elems (resultVectorSubscript x)
     forM_ (zip items subscript) $ \(i, s) ->
       hPrintResultSourceIndentedLabelled h (indent + 2) (label ++ s) loc i
hPrintResultSourceIndentedLabelled h indent label loc (ResultObjectSource x) =
  do let tab = replicate indent ' '
     liftIO $
       do hPutStr h tab
          hPutStr h "-- "
          hPutStr h (loc $ resultObjectId x)
          hPutStrLn h ""
          hPutStr h tab
          hPutStr h label
          hPutStrLn h ":"
          hPutStrLn h ""
     forM_ (resultObjectProperties x) $ \p ->
       do let indent' = 2 + indent
              tab'    = "  " ++ tab
              label'  = resultPropertyLabel p
              source' = resultPropertySource p
          hPrintResultSourceIndentedLabelled h indent' label' loc source'
hPrintResultSourceIndentedLabelled h indent label loc (ResultSeparatorSource x) =
  do let tab = replicate indent ' '
     liftIO $
       do hPutStr h tab
          hPutStr h label
          hPutStrLn h ""
          hPutStrLn h ""

-- | Print a localised text representation of the results by the specified source
-- and with the given indent.
printResultSourceIndented :: (MonadDES m, MonadIO m)
                             => Int
                             -- ^ an indent
                             -> ResultLocalisation
                             -- ^ a localisation
                             -> ResultSourcePrint m
{-# INLINABLE printResultSourceIndented #-}
printResultSourceIndented = hPrintResultSourceIndented stdout

-- | Print a localised text representation of the results by the specified source.
hPrintResultSource :: (MonadDES m, MonadIO m)
                      => Handle
                      -- ^ a handle
                      -> ResultLocalisation
                      -- ^ a localisation
                      -> ResultSourcePrint m
{-# INLINABLE hPrintResultSource #-}
hPrintResultSource h = hPrintResultSourceIndented h 0

-- | Print a localised text representation of the results by the specified source.
printResultSource :: (MonadDES m, MonadIO m)
                     => ResultLocalisation
                     -- ^ a localisation
                     -> ResultSourcePrint m
{-# INLINABLE printResultSource #-}
printResultSource = hPrintResultSource stdout

-- | Print in Russian a text representation of the results by the specified source.
hPrintResultSourceInRussian :: (MonadDES m, MonadIO m) => Handle -> ResultSourcePrint m
{-# INLINABLE hPrintResultSourceInRussian #-}
hPrintResultSourceInRussian h = hPrintResultSource h russianResultLocalisation

-- | Print in English a text representation of the results by the specified source.
hPrintResultSourceInEnglish :: (MonadDES m, MonadIO m) => Handle -> ResultSourcePrint m
{-# INLINABLE hPrintResultSourceInEnglish #-}
hPrintResultSourceInEnglish h = hPrintResultSource h englishResultLocalisation

-- | Print in Russian a text representation of the results by the specified source.
printResultSourceInRussian :: (MonadDES m, MonadIO m) => ResultSourcePrint m
{-# INLINABLE printResultSourceInRussian #-}
printResultSourceInRussian = hPrintResultSourceInRussian stdout

-- | Print in English a text representation of the results by the specified source.
printResultSourceInEnglish :: (MonadDES m, MonadIO m) => ResultSourcePrint m
{-# INLINABLE printResultSourceInEnglish #-}
printResultSourceInEnglish = hPrintResultSourceInEnglish stdout

-- | Show a localised text representation of the results by the specified source
-- and with the given indent.
showResultSourceIndented :: MonadDES m
                            => Int
                            -- ^ an indent
                            -> ResultLocalisation
                            -- ^ a localisation
                            -> ResultSourceShowS m
{-# INLINABLE showResultSourceIndented #-}
showResultSourceIndented indent loc source@(ResultItemSource (ResultItem x)) =
  showResultSourceIndentedLabelled indent (resultItemName x) loc source
showResultSourceIndented indent loc source@(ResultVectorSource x) =
  showResultSourceIndentedLabelled indent (resultVectorName x) loc source
showResultSourceIndented indent loc source@(ResultObjectSource x) =
  showResultSourceIndentedLabelled indent (resultObjectName x) loc source
showResultSourceIndented indent loc source@(ResultSeparatorSource x) =
  showResultSourceIndentedLabelled indent (resultSeparatorText x) loc source

-- | Show an indented and labelled text representation of the results by the specified source.
showResultSourceIndentedLabelled :: MonadDES m
                                    => Int
                                    -- ^ an indent
                                    -> String
                                    -- ^ a label
                                    -> ResultLocalisation
                                    -- ^ a localisation
                                    -> ResultSourceShowS m
{-# INLINABLE showResultSourceIndentedLabelled #-}
showResultSourceIndentedLabelled indent label loc (ResultItemSource (ResultItem x)) =
  do a <- resultValueData $ resultItemToStringValue x
     let tab = replicate indent ' '
     return $
       showString tab .
       showString "-- " .
       showString (loc $ resultItemId x) .
       showString "\n" .
       showString tab .
       showString label .
       showString " = " .
       showString a .
       showString "\n\n"
showResultSourceIndentedLabelled indent label loc (ResultVectorSource x) =
  do let tab = replicate indent ' '
         items = A.elems (resultVectorItems x)
         subscript = A.elems (resultVectorSubscript x)
     contents <-
       forM (zip items subscript) $ \(i, s) ->
       showResultSourceIndentedLabelled (indent + 2) (label ++ s) loc i
     let showContents = foldr (.) id contents
     return $
       showString tab .
       showString "-- " .
       showString (loc $ resultVectorId x) .
       showString "\n" .
       showString tab .
       showString label .
       showString ":\n\n" .
       showContents
showResultSourceIndentedLabelled indent label loc (ResultObjectSource x) =
  do let tab = replicate indent ' '
     contents <-
       forM (resultObjectProperties x) $ \p ->
       do let indent' = 2 + indent
              tab'    = "  " ++ tab
              label'  = resultPropertyLabel p
              output' = resultPropertySource p
          showResultSourceIndentedLabelled indent' label' loc output'
     let showContents = foldr (.) id contents
     return $
       showString tab .
       showString "-- " .
       showString (loc $ resultObjectId x) .
       showString "\n" .
       showString tab .
       showString label .
       showString ":\n\n" .
       showContents
showResultSourceIndentedLabelled indent label loc (ResultSeparatorSource x) =
  do let tab = replicate indent ' '
     return $
       showString tab .
       showString label .
       showString "\n\n"

-- | Show a localised text representation of the results by the specified source.
showResultSource :: MonadDES m
                    => ResultLocalisation
                    -- ^ a localisation
                    -> ResultSourceShowS m
{-# INLINABLE showResultSource #-}
showResultSource = showResultSourceIndented 0

-- | Show in Russian a text representation of the results by the specified source.
showResultSourceInRussian :: MonadDES m => ResultSourceShowS m
{-# INLINABLE showResultSourceInRussian #-}
showResultSourceInRussian = showResultSource russianResultLocalisation

-- | Show in English a text representation of the results by the specified source.
showResultSourceInEnglish :: MonadDES m => ResultSourceShowS m
{-# INLINABLE showResultSourceInEnglish #-}
showResultSourceInEnglish = showResultSource englishResultLocalisation

-- | Print the results with the information about the modeling time.
printResultsWithTime :: (MonadDES m, MonadIO m) => ResultSourcePrint m -> Results m -> Event m ()
{-# INLINABLE printResultsWithTime #-}
printResultsWithTime print results =
  do let x1 = textResultSource "----------"
         x2 = timeResultSource
         x3 = textResultSource ""
         xs = resultSourceList results
     print x1
     print x2
     -- print x3
     mapM_ print xs
     -- print x3

-- | Print the simulation results in start time.
printResultsInStartTime :: (MonadDES m, MonadIO m) => ResultSourcePrint m -> Results m -> Simulation m ()
{-# INLINABLE printResultsInStartTime #-}
printResultsInStartTime print results =
  runEventInStartTime $ printResultsWithTime print results

-- | Print the simulation results in stop time.
printResultsInStopTime :: (MonadDES m, MonadIO m) => ResultSourcePrint m -> Results m -> Simulation m ()
{-# INLINABLE printResultsInStopTime #-}
printResultsInStopTime print results =
  runEventInStopTime $ printResultsWithTime print results

-- | Print the simulation results in the integration time points.
printResultsInIntegTimes :: (MonadDES m, MonadIO m) => ResultSourcePrint m -> Results m -> Simulation m ()
{-# INLINABLE printResultsInIntegTimes #-}
printResultsInIntegTimes print results =
  do let loop (m : ms) = m >> loop ms
         loop [] = return ()
     ms <- runDynamicsInIntegTimes $ runEvent $
           printResultsWithTime print results
     liftComp $ loop ms

-- | Print the simulation results in the specified time.
printResultsInTime :: (MonadDES m, MonadIO m) => Double -> ResultSourcePrint m -> Results m -> Simulation m ()
{-# INLINABLE printResultsInTime #-}
printResultsInTime t print results =
  runDynamicsInTime t $ runEvent $
  printResultsWithTime print results

-- | Print the simulation results in the specified time points.
printResultsInTimes :: (MonadDES m, MonadIO m) => [Double] -> ResultSourcePrint m -> Results m -> Simulation m ()
{-# INLINABLE printResultsInTimes #-}
printResultsInTimes ts print results =
  do let loop (m : ms) = m >> loop ms
         loop [] = return ()
     ms <- runDynamicsInTimes ts $ runEvent $
           printResultsWithTime print results
     liftComp $ loop ms

-- | Show the results with the information about the modeling time.
showResultsWithTime :: MonadDES m => ResultSourceShowS m -> Results m -> Event m ShowS
{-# INLINABLE showResultsWithTime #-}
showResultsWithTime f results =
  do let x1 = textResultSource "----------"
         x2 = timeResultSource
         x3 = textResultSource ""
         xs = resultSourceList results
     y1 <- f x1
     y2 <- f x2
     y3 <- f x3
     ys <- forM xs f
     return $
       y1 .
       y2 .
       -- y3 .
       foldr (.) id ys
       -- y3

-- | Show the simulation results in start time.
showResultsInStartTime :: MonadDES m => ResultSourceShowS m -> Results m -> Simulation m ShowS
{-# INLINABLE showResultsInStartTime #-}
showResultsInStartTime f results =
  runEventInStartTime $ showResultsWithTime f results

-- | Show the simulation results in stop time.
showResultsInStopTime :: MonadDES m => ResultSourceShowS m -> Results m -> Simulation m ShowS
{-# INLINABLE showResultsInStopTime #-}
showResultsInStopTime f results =
  runEventInStopTime $ showResultsWithTime f results

-- | Show the simulation results in the integration time points.
--
-- It may consume much memory, for we have to traverse all the integration
-- points to create the resulting function within the 'Simulation' computation.
showResultsInIntegTimes :: MonadDES m => ResultSourceShowS m -> Results m -> Simulation m ShowS
{-# INLINABLE showResultsInIntegTimes #-}
showResultsInIntegTimes f results =
  do let loop (m : ms) = return (.) `ap` m `ap` loop ms
         loop [] = return id
     ms <- runDynamicsInIntegTimes $ runEvent $
           showResultsWithTime f results
     liftComp $ loop ms

-- | Show the simulation results in the specified time point.
showResultsInTime :: MonadDES m => Double -> ResultSourceShowS m -> Results m -> Simulation m ShowS
{-# INLINABLE showResultsInTime #-}
showResultsInTime t f results =
  runDynamicsInTime t $ runEvent $
  showResultsWithTime f results

-- | Show the simulation results in the specified time points.
--
-- It may consume much memory, for we have to traverse all the specified
-- points to create the resulting function within the 'Simulation' computation.
showResultsInTimes :: MonadDES m => [Double] -> ResultSourceShowS m -> Results m -> Simulation m ShowS
{-# INLINABLE showResultsInTimes #-}
showResultsInTimes ts f results =
  do let loop (m : ms) = return (.) `ap` m `ap` loop ms
         loop [] = return id
     ms <- runDynamicsInTimes ts $ runEvent $
           showResultsWithTime f results
     liftComp $ loop ms

-- | Run the simulation and then print the results in the start time.
printSimulationResultsInStartTime :: (MonadDES m, MonadIO m) => ResultSourcePrint m -> Simulation m (Results m) -> Specs m -> m ()
{-# INLINABLE printSimulationResultsInStartTime #-}
printSimulationResultsInStartTime print model specs =
  flip runSimulation specs $
  model >>= printResultsInStartTime print

-- | Run the simulation and then print the results in the final time.
printSimulationResultsInStopTime :: (MonadDES m, MonadIO m) => ResultSourcePrint m -> Simulation m (Results m) -> Specs m -> m ()
{-# INLINABLE printSimulationResultsInStopTime #-}
printSimulationResultsInStopTime print model specs =
  flip runSimulation specs $
  model >>= printResultsInStopTime print

-- | Run the simulation and then print the results in the integration time points.
printSimulationResultsInIntegTimes :: (MonadDES m, MonadIO m) => ResultSourcePrint m -> Simulation m (Results m) -> Specs m -> m ()
{-# INLINABLE printSimulationResultsInIntegTimes #-}
printSimulationResultsInIntegTimes print model specs =
  flip runSimulation specs $
  model >>= printResultsInIntegTimes print

-- | Run the simulation and then print the results in the specified time point.
printSimulationResultsInTime :: (MonadDES m, MonadIO m) => Double -> ResultSourcePrint m -> Simulation m (Results m) -> Specs m -> m ()
{-# INLINABLE printSimulationResultsInTime #-}
printSimulationResultsInTime t print model specs =
  flip runSimulation specs $
  model >>= printResultsInTime t print

-- | Run the simulation and then print the results in the specified time points.
printSimulationResultsInTimes :: (MonadDES m, MonadIO m) => [Double] -> ResultSourcePrint m -> Simulation m (Results m) -> Specs m -> m ()
{-# INLINABLE printSimulationResultsInTimes #-}
printSimulationResultsInTimes ts print model specs =
  flip runSimulation specs $
  model >>= printResultsInTimes ts print

-- | Run the simulation and then show the results in the start time.
showSimulationResultsInStartTime :: MonadDES m => ResultSourceShowS m -> Simulation m (Results m) -> Specs m -> m ShowS
{-# INLINABLE showSimulationResultsInStartTime #-}
showSimulationResultsInStartTime f model specs =
  flip runSimulation specs $
  model >>= showResultsInStartTime f

-- | Run the simulation and then show the results in the final time.
showSimulationResultsInStopTime :: MonadDES m => ResultSourceShowS m -> Simulation m (Results m) -> Specs m -> m ShowS
{-# INLINABLE showSimulationResultsInStopTime #-}
showSimulationResultsInStopTime f model specs =
  flip runSimulation specs $
  model >>= showResultsInStopTime f

-- | Run the simulation and then show the results in the integration time points.
--
-- It may consume much memory, for we have to traverse all the integration
-- points to create the resulting function within the 'IO' computation.
showSimulationResultsInIntegTimes :: MonadDES m => ResultSourceShowS m -> Simulation m (Results m) -> Specs m -> m ShowS
{-# INLINABLE showSimulationResultsInIntegTimes #-}
showSimulationResultsInIntegTimes f model specs =
  flip runSimulation specs $
  model >>= showResultsInIntegTimes f

-- | Run the simulation and then show the results in the integration time point.
showSimulationResultsInTime :: MonadDES m => Double -> ResultSourceShowS m -> Simulation m (Results m) -> Specs m -> m ShowS
{-# INLINABLE showSimulationResultsInTime #-}
showSimulationResultsInTime t f model specs =
  flip runSimulation specs $
  model >>= showResultsInTime t f

-- | Run the simulation and then show the results in the specified time points.
--
-- It may consume much memory, for we have to traverse all the specified
-- points to create the resulting function within the 'IO' computation.
showSimulationResultsInTimes :: MonadDES m => [Double] -> ResultSourceShowS m -> Simulation m (Results m) -> Specs m -> m ShowS
{-# INLINABLE showSimulationResultsInTimes #-}
showSimulationResultsInTimes ts f model specs =
  flip runSimulation specs $
  model >>= showResultsInTimes ts f
