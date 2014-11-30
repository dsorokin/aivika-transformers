
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.IO
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module re-exports the most part of the library functionality related
-- to 'IO'-based computation.
--
module Simulation.Aivika.IO
       (-- * Modules
        module Simulation.Aivika.IO.Comp,
        module Simulation.Aivika.IO.DES,
        module Simulation.Aivika.IO.Dynamics.Memo.Unboxed,
        module Simulation.Aivika.IO.Event,
        module Simulation.Aivika.IO.Exception,
        module Simulation.Aivika.IO.Generator,
        module Simulation.Aivika.IO.QueueStrategy,
        module Simulation.Aivika.IO.SD,
        module Simulation.Aivika.IO.Signal,
        module Simulation.Aivika.IO.Ref.Base,
        module Simulation.Aivika.IO.Var.Unboxed) where

import Simulation.Aivika.IO.Comp
import Simulation.Aivika.IO.DES
import Simulation.Aivika.IO.Dynamics.Memo.Unboxed
import Simulation.Aivika.IO.Event
import Simulation.Aivika.IO.Exception
import Simulation.Aivika.IO.Generator
import Simulation.Aivika.IO.QueueStrategy
import Simulation.Aivika.IO.SD
import Simulation.Aivika.IO.Signal
import Simulation.Aivika.IO.Ref.Base
import Simulation.Aivika.IO.Var.Unboxed

import Control.Exception

import Simulation.Aivika.Statistics

import Simulation.Aivika.Trans.Arrival
import Simulation.Aivika.Trans.Cont
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Process
import Simulation.Aivika.Trans.Processor
import Simulation.Aivika.Trans.Processor.RoundRobbin
import Simulation.Aivika.Trans.QueueStrategy
import Simulation.Aivika.Trans.Server
import Simulation.Aivika.Trans.Signal
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Stream
import Simulation.Aivika.Trans.Stream.Random

import qualified Simulation.Aivika.Trans.Queue as Q

{-# SPECIALISE holdProcess :: Double -> Process IO () #-}
{-# SPECIALISE interruptProcess :: ProcessId IO -> Event IO () #-}
{-# SPECIALISE processInterrupted :: ProcessId IO -> Event IO Bool #-}
{-# SPECIALISE passivateProcess :: Process IO () #-}
{-# SPECIALISE processPassive :: ProcessId IO -> Event IO Bool #-}
{-# SPECIALISE reactivateProcess :: ProcessId IO -> Event IO () #-}
{-# SPECIALISE runProcess :: Process IO () -> Event IO () #-}
{-# SPECIALISE runProcessUsingId :: ProcessId IO -> Process IO () -> Event IO () #-}
{-# SPECIALISE runProcessInStartTime :: Process IO () -> Simulation IO () #-}
{-# SPECIALISE runProcessInStartTimeUsingId :: ProcessId IO -> Process IO () -> Simulation IO () #-}
{-# SPECIALISE runProcessInStopTime :: Process IO () -> Simulation IO () #-}
{-# SPECIALISE runProcessInStopTimeUsingId :: ProcessId IO -> Process IO () -> Simulation IO () #-}
{-# SPECIALISE enqueueProcess :: Double -> Process IO () -> Event IO () #-}
{-# SPECIALISE enqueueProcessUsingId :: Double -> ProcessId IO -> Process IO () -> Event IO () #-}
{-# SPECIALISE processId :: Process IO (ProcessId IO) #-}
{-# SPECIALISE newProcessId :: Simulation IO (ProcessId IO) #-}
{-# SPECIALISE cancelProcessWithId :: ProcessId IO -> Event IO () #-}
{-# SPECIALISE cancelProcess :: Process IO a #-}
{-# SPECIALISE processCancelled :: ProcessId IO -> Event IO Bool #-}
{-# SPECIALISE processCancelling :: ProcessId IO -> Signal IO () #-}
{-# SPECIALISE whenCancellingProcess :: Event IO () -> Process IO () #-}
{-# SPECIALISE catchProcess :: Exception e => Process IO a -> (e -> Process IO a) -> Process IO a #-}
{-# SPECIALISE finallyProcess :: Process IO a -> Process IO b -> Process IO a #-}
{-# SPECIALISE throwProcess :: Exception e => e -> Process IO a #-}
{-# SPECIALISE processParallel :: [Process IO a] -> Process IO [a] #-}
{-# SPECIALISE processParallelUsingIds :: [(ProcessId IO, Process IO a)] -> Process IO [a] #-}
{-# SPECIALISE processParallel_ :: [Process IO a] -> Process IO () #-}
{-# SPECIALISE processParallelUsingIds_ :: [(ProcessId IO, Process IO a)] -> Process IO () #-}
{-# SPECIALISE processUsingId :: ProcessId IO -> Process IO a -> Process IO a #-}
{-# SPECIALISE spawnProcess :: Process IO () -> Process IO () #-}
{-# SPECIALISE spawnProcessUsingId :: ProcessId IO -> Process IO () -> Process IO () #-}
{-# SPECIALISE spawnProcessWith :: ContCancellation -> Process IO () -> Process IO () #-}
{-# SPECIALISE spawnProcessUsingIdWith :: ContCancellation -> ProcessId IO -> Process IO () -> Process IO () #-}
{-# SPECIALISE processAwait :: Signal IO a -> Process IO a #-}
{-# SPECIALISE memoProcess :: Process IO a -> Simulation IO (Process IO a) #-}
{-# SPECIALISE zipProcessParallel :: Process IO a -> Process IO b -> Process IO (a, b) #-}
{-# SPECIALISE zip3ProcessParallel :: Process IO a -> Process IO b -> Process IO c -> Process IO (a, b, c) #-}
{-# SPECIALISE unzipProcess :: Process IO (a, b) -> Simulation IO (Process IO a, Process IO b) #-}
{-# SPECIALISE timeoutProcess :: Double -> Process IO a -> Process IO (Maybe a) #-}
{-# SPECIALISE timeoutProcessUsingId :: Double -> ProcessId IO -> Process IO a -> Process IO (Maybe a) #-}
{-# SPECIALISE processYield :: Process IO () #-}
{-# SPECIALISE neverProcess :: Process IO a #-}
{-# SPECIALISE traceProcess :: String -> Process IO a -> Process IO a #-}

{-# SPECIALISE streamUsingId :: ProcessId IO -> Stream IO a -> Stream IO a #-}
{-# SPECIALISE memoStream :: Stream IO a -> Simulation IO (Stream IO a) #-}
{-# SPECIALISE zipStreamSeq :: Stream IO a -> Stream IO b -> Stream IO (a, b) #-}
{-# SPECIALISE zipStreamParallel :: Stream IO a -> Stream IO b -> Stream IO (a, b) #-}
{-# SPECIALISE zip3StreamSeq :: Stream IO a -> Stream IO b -> Stream IO c -> Stream IO (a, b, c) #-}
{-# SPECIALISE zip3StreamParallel :: Stream IO a -> Stream IO b -> Stream IO c -> Stream IO (a, b, c) #-}
{-# SPECIALISE unzipStream :: Stream IO (a, b) -> Simulation IO (Stream IO a, Stream IO b) #-}
{-# SPECIALISE streamSeq :: [Stream IO a] -> Stream IO [a] #-}
{-# SPECIALISE streamParallel :: [Stream IO a] -> Stream IO [a] #-}
{-# SPECIALISE repeatProcess :: Process IO a -> Stream IO a #-}
{-# SPECIALISE mapStream :: (a -> b) -> Stream IO a -> Stream IO b #-}
{-# SPECIALISE mapStreamM :: (a -> Process IO b) -> Stream IO a -> Stream IO b #-}
{-# SPECIALISE apStream :: Stream IO (a -> b) -> Stream IO a -> Stream IO b #-}
{-# SPECIALISE apStreamM :: Stream IO (a -> Process IO b) -> Stream IO a -> Stream IO b #-}
{-# SPECIALISE filterStream :: (a -> Bool) -> Stream IO a -> Stream IO a #-}
{-# SPECIALISE filterStreamM :: (a -> Process IO Bool) -> Stream IO a -> Stream IO a #-}
{-# SPECIALISE leftStream :: Stream IO (Either a b) -> Stream IO a #-}
{-# SPECIALISE rightStream :: Stream IO (Either a b) -> Stream IO b #-}
{-# SPECIALISE replaceLeftStream :: Stream IO (Either a b) -> Stream IO c -> Stream IO (Either c b) #-}
{-# SPECIALISE replaceRightStream :: Stream IO (Either a b) -> Stream IO c -> Stream IO (Either a c) #-}
{-# SPECIALISE partitionEitherStream :: Stream IO (Either a b) -> Simulation IO (Stream IO a, Stream IO b) #-}
{-# SPECIALISE splitStream :: Int -> Stream IO a -> Simulation IO [Stream IO a] #-}
{-# SPECIALISE splitStreamQueueing :: EnqueueStrategy IO s => s -> Int -> Stream IO a -> Simulation IO [Stream IO a] #-}
{-# SPECIALISE splitStreamPrioritising :: PriorityQueueStrategy IO s p => s -> [Stream IO p] -> Stream IO a -> Simulation IO [Stream IO a] #-}
{-# SPECIALISE concatStreams :: [Stream IO a] -> Stream IO a #-}
{-# SPECIALISE concatQueuedStreams :: EnqueueStrategy IO s => s -> [Stream IO a] -> Stream IO a #-}
{-# SPECIALISE concatPriorityStreams :: PriorityQueueStrategy IO s p => s -> [Stream IO (p, a)] -> Stream IO a #-}
{-# SPECIALISE mergeStreams :: Stream IO a -> Stream IO a -> Stream IO a #-}
{-# SPECIALISE mergeQueuedStreams :: EnqueueStrategy IO s => s -> Stream IO a -> Stream IO a -> Stream IO a #-}
{-# SPECIALISE mergePriorityStreams :: PriorityQueueStrategy IO s p => s -> Stream IO (p, a) -> Stream IO (p, a) -> Stream IO a #-}
{-# SPECIALISE emptyStream :: Stream IO a #-}
{-# SPECIALISE consumeStream :: (a -> Process IO ()) -> Stream IO a -> Process IO () #-}
{-# SPECIALISE sinkStream :: Stream IO a -> Process IO () #-}
{-# SPECIALISE prefetchStream :: Stream IO a -> Stream IO a #-}
{-# SPECIALISE signalStream :: Signal IO a -> Process IO (Stream IO a) #-}
{-# SPECIALISE streamSignal :: Stream IO a -> Process IO (Signal IO a) #-}
{-# SPECIALISE arrivalStream :: Stream IO a -> Stream IO (Arrival a) #-}
{-# SPECIALISE delayStream :: a -> Stream IO a -> Stream IO a #-}
{-# SPECIALISE singletonStream :: a -> Stream IO a #-}
{-# SPECIALISE traceStream :: Maybe String -> Maybe String -> Stream IO a -> Stream IO a #-}

{-# SPECIALISE randomStream :: Parameter IO (Double, a) -> Stream IO (Arrival a) #-}
{-# SPECIALISE randomUniformStream :: Double -> Double -> Stream IO (Arrival Double) #-}
{-# SPECIALISE randomUniformIntStream :: Int -> Int -> Stream IO (Arrival Int) #-}
{-# SPECIALISE randomNormalStream :: Double -> Double -> Stream IO (Arrival Double) #-}
{-# SPECIALISE randomExponentialStream :: Double -> Stream IO (Arrival Double) #-}
{-# SPECIALISE randomErlangStream :: Double -> Int -> Stream IO (Arrival Double) #-}
{-# SPECIALISE randomPoissonStream :: Double -> Stream IO (Arrival Int) #-}
{-# SPECIALISE randomBinomialStream :: Double -> Int -> Stream IO (Arrival Int) #-}

{-# SPECIALISE emptyProcessor :: Processor IO a b #-}
{-# SPECIALISE arrProcessor :: (a -> Process IO b) -> Processor IO a b #-}
{-# SPECIALISE accumProcessor :: (acc -> a -> Process IO (acc, b)) -> acc -> Processor IO a b #-}
{-# SPECIALISE processorUsingId :: ProcessId IO -> Processor IO a b -> Processor IO a b #-}
{-# SPECIALISE processorQueuedParallel :: (EnqueueStrategy IO si, EnqueueStrategy IO so) => si -> so -> [Processor IO a b] -> Processor IO a b #-}
{-# SPECIALISE processorPrioritisingOutputParallel :: (EnqueueStrategy IO si, PriorityQueueStrategy IO so po) => si -> so -> [Processor IO a (po, b)] -> Processor IO a b #-}
{-# SPECIALISE processorPrioritisingInputParallel :: (PriorityQueueStrategy IO si pi, EnqueueStrategy IO so) => si -> so -> [(Stream IO pi, Processor IO a b)] -> Processor IO a b #-}
{-# SPECIALISE processorPrioritisingInputOutputParallel :: (PriorityQueueStrategy IO si pi, PriorityQueueStrategy IO so po) => si -> so -> [(Stream IO pi, Processor IO a (po, b))] -> Processor IO a b #-}
{-# SPECIALISE processorParallel :: [Processor IO a b] -> Processor IO a b #-}
{-# SPECIALISE processorSeq :: [Processor IO a a] -> Processor IO a a #-}
{-# SPECIALISE bufferProcessor :: (Stream IO a -> Process IO ()) -> Stream IO b -> Processor IO a b #-}
{-# SPECIALISE bufferProcessorLoop :: (Stream IO a -> Stream IO c -> Process IO ()) -> Stream IO d -> Processor IO d (Either e b) -> Processor IO e c -> Processor IO a b #-}
{-# SPECIALISE queueProcessor :: (a -> Process IO ()) -> Process IO b -> Processor IO a b #-}
{-# SPECIALISE queueProcessorLoopMerging :: (Stream IO a -> Stream IO d -> Stream IO e) -> (e -> Process IO ()) -> Process IO c -> Processor IO c (Either f b) -> Processor IO f d -> Processor IO a b #-}
{-# SPECIALISE queueProcessorLoopSeq :: (a -> Process IO ()) -> Process IO c -> Processor IO c (Either e b) -> Processor IO e a -> Processor IO a b #-}
{-# SPECIALISE queueProcessorLoopParallel :: (a -> Process IO ()) -> Process IO c -> Processor IO c (Either e b) -> Processor IO e a -> Processor IO a b #-}
{-# SPECIALISE prefetchProcessor :: Processor IO a a #-}
{-# SPECIALISE signalProcessor :: (Signal IO a -> Signal IO b) -> Processor IO a b #-}
{-# SPECIALISE processorSignaling :: Processor IO a b -> Signal IO a -> Process IO (Signal IO b) #-}
{-# SPECIALISE arrivalProcessor :: Processor IO a (Arrival a) #-}
{-# SPECIALISE delayProcessor :: a -> Processor IO a a #-}
{-# SPECIALISE traceProcessor :: Maybe String -> Maybe String -> Processor IO a b -> Processor IO a b #-}

{-# SPECIALISE roundRobbinProcessor :: Processor IO (Process IO Double, Process IO a) a #-}
{-# SPECIALISE roundRobbinProcessorUsingIds :: Processor IO (Process IO (Double, ProcessId IO), Process IO a) a #-}

{-# SPECIALISE newServer :: (a -> Process IO b) -> Simulation IO (Server IO () a b) #-}
{-# SPECIALISE newStateServer :: (s -> a -> Process IO (s, b)) -> s -> Simulation IO (Server IO s a b) #-}
{-# SPECIALISE newInterruptibleServer :: Bool -> (a -> Process IO b) -> Simulation IO (Server IO () a b) #-}
{-# SPECIALISE newInterruptibleStateServer :: Bool -> (s -> a -> Process IO (s, b)) -> s -> Simulation IO (Server IO s a b) #-}
{-# SPECIALISE serverProcessor :: Server IO s a b -> Processor IO a b #-}
{-# SPECIALISE serverState :: Server IO s a b -> Event IO s #-}
{-# SPECIALISE serverStateChanged :: Server IO s a b -> Signal IO s #-}
{-# SPECIALISE serverStateChanged_ :: Server IO s a b -> Signal IO () #-}
{-# SPECIALISE serverTotalInputWaitTime :: Server IO s a b -> Event IO Double #-}
{-# SPECIALISE serverTotalInputWaitTimeChanged :: Server IO s a b -> Signal IO Double #-}
{-# SPECIALISE serverTotalInputWaitTimeChanged_ :: Server IO s a b -> Signal IO () #-}
{-# SPECIALISE serverTotalProcessingTime :: Server IO s a b -> Event IO Double #-}
{-# SPECIALISE serverTotalProcessingTimeChanged :: Server IO s a b -> Signal IO Double #-}
{-# SPECIALISE serverTotalProcessingTimeChanged_ :: Server IO s a b -> Signal IO () #-}
{-# SPECIALISE serverTotalOutputWaitTime :: Server IO s a b -> Event IO Double #-}
{-# SPECIALISE serverTotalOutputWaitTimeChanged :: Server IO s a b -> Signal IO Double #-}
{-# SPECIALISE serverTotalOutputWaitTimeChanged_ :: Server IO s a b -> Signal IO () #-}
{-# SPECIALISE serverInputWaitTime :: Server IO s a b -> Event IO (SamplingStats Double) #-}
{-# SPECIALISE serverInputWaitTimeChanged :: Server IO s a b -> Signal IO (SamplingStats Double) #-}
{-# SPECIALISE serverInputWaitTimeChanged_ :: Server IO s a b -> Signal IO () #-}
{-# SPECIALISE serverProcessingTime :: Server IO s a b -> Event IO (SamplingStats Double) #-}
{-# SPECIALISE serverProcessingTimeChanged :: Server IO s a b -> Signal IO (SamplingStats Double) #-}
{-# SPECIALISE serverProcessingTimeChanged_ :: Server IO s a b -> Signal IO () #-}
{-# SPECIALISE serverOutputWaitTime :: Server IO s a b -> Event IO (SamplingStats Double) #-}
{-# SPECIALISE serverOutputWaitTimeChanged :: Server IO s a b -> Signal IO (SamplingStats Double) #-}
{-# SPECIALISE serverOutputWaitTimeChanged_ :: Server IO s a b -> Signal IO () #-}
{-# SPECIALISE serverInputWaitFactor :: Server IO s a b -> Event IO Double #-}
{-# SPECIALISE serverInputWaitFactorChanged :: Server IO s a b -> Signal IO Double #-}
{-# SPECIALISE serverInputWaitFactorChanged_ :: Server IO s a b -> Signal IO () #-}
{-# SPECIALISE serverProcessingFactor :: Server IO s a b -> Event IO Double #-}
{-# SPECIALISE serverProcessingFactorChanged :: Server IO s a b -> Signal IO Double #-}
{-# SPECIALISE serverProcessingFactorChanged_ :: Server IO s a b -> Signal IO () #-}
{-# SPECIALISE serverOutputWaitFactor :: Server IO s a b -> Event IO Double #-}
{-# SPECIALISE serverOutputWaitFactorChanged :: Server IO s a b -> Signal IO Double #-}
{-# SPECIALISE serverOutputWaitFactorChanged_ :: Server IO s a b -> Signal IO () #-}
{-# SPECIALISE serverInputReceived :: Server IO s a b -> Signal IO a #-}
{-# SPECIALISE serverTaskInterrupted :: Server IO s a b -> Signal IO (ServerInterruption a) #-}
{-# SPECIALISE serverTaskProcessed :: Server IO s a b -> Signal IO (a, b) #-}
{-# SPECIALISE serverOutputProvided :: Server IO s a b -> Signal IO (a, b) #-}
{-# SPECIALISE serverChanged_ :: Server IO s a b -> Signal IO () #-}
{-# SPECIALISE serverSummary :: Server IO s a b -> Int -> Event IO ShowS #-}

{-# SPECIALISE newArrivalTimer :: Simulation IO (ArrivalTimer IO) #-}
{-# SPECIALISE arrivalProcessingTime :: ArrivalTimer IO -> Event IO (SamplingStats Double) #-}
{-# SPECIALISE arrivalProcessingTimeChanged :: ArrivalTimer IO -> Signal IO (SamplingStats Double) #-}
{-# SPECIALISE arrivalProcessingTimeChanged_ :: ArrivalTimer IO -> Signal IO () #-}
{-# SPECIALISE arrivalTimerProcessor :: ArrivalTimer IO -> Processor IO (Arrival a) (Arrival a) #-}

{-# SPECIALISE Q.newFCFSQueue :: Int -> Event IO (Q.FCFSQueue IO a) #-}
{-# SPECIALISE Q.newLCFSQueue :: Int -> Event IO (Q.LCFSQueue IO a) #-}
{-# SPECIALISE Q.newSIROQueue :: QueueStrategy IO SIRO => Int -> Event IO (Q.SIROQueue IO a) #-}
{-# SPECIALISE Q.newPriorityQueue :: QueueStrategy IO StaticPriorities => Int -> Event IO (Q.PriorityQueue IO a)  #-}
{-# SPECIALISE Q.newQueue :: (QueueStrategy IO si, QueueStrategy IO sm, QueueStrategy IO so) => si -> sm -> so -> Int -> Event IO (Q.Queue IO si sm so a) #-}
{-# SPECIALISE Q.queueNull :: Q.Queue IO si sm so a -> Event IO Bool #-}
{-# SPECIALISE Q.queueNullChanged :: Q.Queue IO si sm so a -> Signal IO Bool #-}
{-# SPECIALISE Q.queueNullChanged_ :: Q.Queue IO si sm so a -> Signal IO () #-}
{-# SPECIALISE Q.queueFull :: Q.Queue IO si sm so a -> Event IO Bool #-}
{-# SPECIALISE Q.queueFullChanged :: Q.Queue IO si sm so a -> Signal IO Bool #-}
{-# SPECIALISE Q.queueFullChanged_ :: Q.Queue IO si sm so a -> Signal IO () #-}
{-# SPECIALISE Q.queueCount :: Q.Queue IO si sm so a -> Event IO Int #-}
{-# SPECIALISE Q.queueCountStats :: Q.Queue IO si sm so a -> Event IO (TimingStats Int) #-}
{-# SPECIALISE Q.queueCountChanged :: Q.Queue IO si sm so a -> Signal IO Int #-}
{-# SPECIALISE Q.queueCountChanged_ :: Q.Queue IO si sm so a -> Signal IO () #-}
{-# SPECIALISE Q.enqueueCount :: Q.Queue IO si sm so a -> Event IO Int #-}
{-# SPECIALISE Q.enqueueCountChanged :: Q.Queue IO si sm so a -> Signal IO Int #-}
{-# SPECIALISE Q.enqueueCountChanged_ :: Q.Queue IO si sm so a -> Signal IO () #-}
{-# SPECIALISE Q.enqueueLostCount :: Q.Queue IO si sm so a -> Event IO Int #-}
{-# SPECIALISE Q.enqueueLostCountChanged :: Q.Queue IO si sm so a -> Signal IO Int #-}
{-# SPECIALISE Q.enqueueLostCountChanged_ :: Q.Queue IO si sm so a -> Signal IO () #-}
{-# SPECIALISE Q.enqueueStoreCount :: Q.Queue IO si sm so a -> Event IO Int #-}
{-# SPECIALISE Q.enqueueStoreCountChanged :: Q.Queue IO si sm so a -> Signal IO Int #-}
{-# SPECIALISE Q.enqueueStoreCountChanged_ :: Q.Queue IO si sm so a -> Signal IO () #-}
{-# SPECIALISE Q.dequeueCount :: Q.Queue IO si sm so a -> Event IO Int #-}
{-# SPECIALISE Q.dequeueCountChanged :: Q.Queue IO si sm so a -> Signal IO Int #-}
{-# SPECIALISE Q.dequeueCountChanged_ :: Q.Queue IO si sm so a -> Signal IO () #-}
{-# SPECIALISE Q.dequeueExtractCount :: Q.Queue IO si sm so a -> Event IO Int #-}
{-# SPECIALISE Q.dequeueExtractCountChanged :: Q.Queue IO si sm so a -> Signal IO Int #-}
{-# SPECIALISE Q.dequeueExtractCountChanged_ :: Q.Queue IO si sm so a -> Signal IO () #-}
{-# SPECIALISE Q.queueLoadFactor :: Q.Queue IO si sm so a -> Event IO Double #-}
{-# SPECIALISE Q.queueLoadFactorChanged :: Q.Queue IO si sm so a -> Signal IO Double #-}
{-# SPECIALISE Q.queueLoadFactorChanged_ :: Q.Queue IO si sm so a -> Signal IO () #-}
{-# SPECIALISE Q.enqueueRate :: Q.Queue IO si sm so a -> Event IO Double #-}
{-# SPECIALISE Q.enqueueStoreRate :: Q.Queue IO si sm so a -> Event IO Double #-}
{-# SPECIALISE Q.dequeueRate :: Q.Queue IO si sm so a -> Event IO Double #-}
{-# SPECIALISE Q.dequeueExtractRate :: Q.Queue IO si sm so a -> Event IO Double #-}
{-# SPECIALISE Q.queueWaitTime :: Q.Queue IO si sm so a -> Event IO (SamplingStats Double) #-}
{-# SPECIALISE Q.queueWaitTimeChanged :: Q.Queue IO si sm so a -> Signal IO (SamplingStats Double) #-}
{-# SPECIALISE Q.queueWaitTimeChanged_ :: Q.Queue IO si sm so a -> Signal IO () #-}
{-# SPECIALISE Q.queueTotalWaitTime :: Q.Queue IO si sm so a -> Event IO (SamplingStats Double) #-} 
{-# SPECIALISE Q.queueTotalWaitTimeChanged :: Q.Queue IO si sm so a -> Signal IO (SamplingStats Double) #-}
{-# SPECIALISE Q.queueTotalWaitTimeChanged_ :: Q.Queue IO si sm so a -> Signal IO () #-}      
{-# SPECIALISE Q.enqueueWaitTime :: Q.Queue IO si sm so a -> Event IO (SamplingStats Double) #-}
{-# SPECIALISE Q.enqueueWaitTimeChanged :: Q.Queue IO si sm so a -> Signal IO (SamplingStats Double) #-}
{-# SPECIALISE Q.enqueueWaitTimeChanged_ :: Q.Queue IO si sm so a -> Signal IO () #-}
{-# SPECIALISE Q.dequeueWaitTime :: Q.Queue IO si sm so a -> Event IO (SamplingStats Double) #-}
{-# SPECIALISE Q.dequeueWaitTimeChanged :: Q.Queue IO si sm so a -> Signal IO (SamplingStats Double) #-}
{-# SPECIALISE Q.dequeueWaitTimeChanged_ :: Q.Queue IO si sm so a -> Signal IO () #-}
{-# SPECIALISE Q.queueRate :: Q.Queue IO si sm so a -> Event IO Double #-}
{-# SPECIALISE Q.queueRateChanged :: Q.Queue IO si sm so a -> Signal IO Double #-}
{-# SPECIALISE Q.queueRateChanged_ :: Q.Queue IO si sm so a -> Signal IO () #-}
{-# SPECIALISE Q.dequeue :: (DequeueStrategy IO si, DequeueStrategy IO sm, EnqueueStrategy IO so) => Q.Queue IO si sm so a -> Process IO a #-}
{-# SPECIALISE Q.dequeueWithOutputPriority :: (DequeueStrategy IO si, DequeueStrategy IO sm, PriorityQueueStrategy IO so po) => Q.Queue IO si sm so a -> po -> Process IO a #-}
{-# SPECIALISE Q.tryDequeue :: (DequeueStrategy IO si, DequeueStrategy IO sm) => Q.Queue IO si sm so a -> Event IO (Maybe a) #-}
{-# SPECIALISE Q.enqueue :: (EnqueueStrategy IO si, EnqueueStrategy IO sm, DequeueStrategy IO so) => Q.Queue IO si sm so a -> a -> Process IO () #-}
{-# SPECIALISE Q.enqueueWithInputPriority :: (PriorityQueueStrategy IO si pi, EnqueueStrategy IO sm, DequeueStrategy IO so) => Q.Queue IO si sm so a -> pi -> a -> Process IO () #-}
{-# SPECIALISE Q.enqueueWithStoringPriority :: (EnqueueStrategy IO si, PriorityQueueStrategy IO sm pm, DequeueStrategy IO so) => Q.Queue IO si sm so a -> pm -> a -> Process IO () #-}
{-# SPECIALISE Q.enqueueWithInputStoringPriorities :: (PriorityQueueStrategy IO si pi, PriorityQueueStrategy IO sm pm, DequeueStrategy IO so) => Q.Queue IO si sm so a -> pi -> pm -> a -> Process IO () #-}
{-# SPECIALISE Q.tryEnqueue :: (EnqueueStrategy IO sm, DequeueStrategy IO so) => Q.Queue IO si sm so a -> a -> Event IO Bool #-}
{-# SPECIALISE Q.tryEnqueueWithStoringPriority :: (PriorityQueueStrategy IO sm pm, DequeueStrategy IO so) => Q.Queue IO si sm so a -> pm -> a -> Event IO Bool #-}
{-# SPECIALISE Q.enqueueOrLost :: (EnqueueStrategy IO sm, DequeueStrategy IO so) => Q.Queue IO si sm so a -> a -> Event IO Bool #-}
{-# SPECIALISE Q.enqueueWithStoringPriorityOrLost :: (PriorityQueueStrategy IO sm pm, DequeueStrategy IO so) => Q.Queue IO si sm so a -> pm -> a -> Event IO Bool #-}
{-# SPECIALISE Q.enqueueOrLost_ :: (EnqueueStrategy IO sm, DequeueStrategy IO so) => Q.Queue IO si sm so a -> a -> Event IO () #-}
{-# SPECIALISE Q.enqueueWithStoringPriorityOrLost_ :: (PriorityQueueStrategy IO sm pm, DequeueStrategy IO so) => Q.Queue IO si sm so a -> pm -> a -> Event IO () #-}
{-# SPECIALISE Q.enqueueInitiated :: Q.Queue IO si sm so a -> Signal IO a #-}
{-# SPECIALISE Q.enqueueStored :: Q.Queue IO si sm so a -> Signal IO a #-}
{-# SPECIALISE Q.enqueueLost :: Q.Queue IO si sm so a -> Signal IO a #-}
{-# SPECIALISE Q.dequeueRequested :: Q.Queue IO si sm so a -> Signal IO () #-}
{-# SPECIALISE Q.dequeueExtracted :: Q.Queue IO si sm so a -> Signal IO a #-}
{-# SPECIALISE Q.waitWhileFullQueue :: Q.Queue IO si sm so a -> Process IO () #-}
{-# SPECIALISE Q.queueChanged_ :: Q.Queue IO si sm so a -> Signal IO () #-}
{-# SPECIALISE Q.queueSummary :: (Show si, Show sm, Show so) => Q.Queue IO si sm so a -> Int -> Event IO ShowS #-}
