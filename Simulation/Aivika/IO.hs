
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

import Simulation.Aivika.Trans.Arrival
import Simulation.Aivika.Trans.Cont
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Process
import Simulation.Aivika.Trans.Processor
import Simulation.Aivika.Trans.Processor.RoundRobbin
import Simulation.Aivika.Trans.QueueStrategy
import Simulation.Aivika.Trans.Signal
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Stream
import Simulation.Aivika.Trans.Stream.Random

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
