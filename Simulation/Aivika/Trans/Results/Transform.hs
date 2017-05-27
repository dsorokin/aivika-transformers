
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Trans.Results.Transform
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines useful result transformations that can
-- be used in simulation experiments.
--
module Simulation.Aivika.Trans.Results.Transform
       (-- * Basic Class Type
        ResultTransformer(..),
        -- * Sampling Statistics
        SamplingStats(..),
        samplingStatsCount,
        samplingStatsMin,
        samplingStatsMax,
        samplingStatsMean,
        samplingStatsMean2,
        samplingStatsVariance,
        samplingStatsDeviation,
        -- * Time-dependent Statistics
        TimingStats(..),
        timingStatsCount,
        timingStatsMin,
        timingStatsMax,
        timingStatsMean,
        timingStatsVariance,
        timingStatsDeviation,
        timingStatsMinTime,
        timingStatsMaxTime,
        timingStatsStartTime,
        timingStatsLastTime,
        timingStatsSum,
        timingStatsSum2,
        -- * Sampling-based Counter
        SamplingCounter(..),
        samplingCounterValue,
        samplingCounterStats,
        -- * Time-dependent Counter
        TimingCounter(..),
        timingCounterValue,
        timingCounterStats,
        -- * Queue
        Queue(..),
        enqueueStrategy,
        enqueueStoringStrategy,
        dequeueStrategy,
        queueNull,
        queueFull,
        queueMaxCount,
        queueCount,
        queueCountStats,
        enqueueCount,
        enqueueLostCount,
        enqueueStoreCount,
        dequeueCount,
        dequeueExtractCount,
        queueLoadFactor,
        enqueueRate,
        enqueueStoreRate,
        dequeueRate,
        dequeueExtractRate,
        queueWaitTime,
        queueTotalWaitTime,
        enqueueWaitTime,
        dequeueWaitTime,
        queueRate,
        -- * Arrival Timer
        ArrivalTimer(..),
        arrivalProcessingTime,
        -- * Server
        Server(..),
        serverInitState,
        serverState,
        serverTotalInputWaitTime,
        serverTotalProcessingTime,
        serverTotalOutputWaitTime,
        serverTotalPreemptionTime,
        serverInputWaitTime,
        serverProcessingTime,
        serverOutputWaitTime,
        serverPreemptionTime,
        serverInputWaitFactor,
        serverProcessingFactor,
        serverOutputWaitFactor,
        serverPreemptionFactor,
        -- * Activity
        Activity(..),
        activityInitState,
        activityState,
        activityTotalUtilisationTime,
        activityTotalIdleTime,
        activityTotalPreemptionTime,
        activityUtilisationTime,
        activityIdleTime,
        activityPreemptionTime,
        activityUtilisationFactor,
        activityIdleFactor,
        activityPreemptionFactor,
        -- * Resource
        Resource(..),
        resourceCount,
        resourceCountStats,
        resourceUtilisationCount,
        resourceUtilisationCountStats,
        resourceQueueCount,
        resourceQueueCountStats,
        resourceTotalWaitTime,
        resourceWaitTime,
        -- * Operation
        Operation(..),
        operationTotalUtilisationTime,
        operationTotalPreemptionTime,
        operationUtilisationTime,
        operationPreemptionTime,
        operationUtilisationFactor,
        operationPreemptionFactor) where

import Control.Arrow

import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Results
import Simulation.Aivika.Trans.Results.Locale

-- | Something that can transform the results.
class ResultTransformer t m where

  -- | Return the result transform.
  tr :: t m -> ResultTransform m

-- | Represents a statistics based upon observations.
newtype SamplingStats m = SamplingStats (ResultTransform m)

instance ResultTransformer SamplingStats m where
  tr (SamplingStats a) = a

-- | The total number of samples.
samplingStatsCount :: MonadDES m => SamplingStats m -> ResultTransform m
samplingStatsCount (SamplingStats a) =
  a >>> expandResults >>> resultById SamplingStatsCountId

-- | The minimum value among the samples.
samplingStatsMin :: MonadDES m => SamplingStats m -> ResultTransform m
samplingStatsMin (SamplingStats a) =
  a >>> expandResults >>> resultById SamplingStatsMinId

-- | The maximum value among the samples.
samplingStatsMax :: MonadDES m => SamplingStats m -> ResultTransform m
samplingStatsMax (SamplingStats a) =
  a >>> expandResults >>> resultById SamplingStatsMaxId
  
-- | The average value.
samplingStatsMean :: MonadDES m => SamplingStats m -> ResultTransform m
samplingStatsMean (SamplingStats a) =
  a >>> expandResults >>> resultById SamplingStatsMeanId

-- | The average square value.
samplingStatsMean2 :: MonadDES m => SamplingStats m -> ResultTransform m
samplingStatsMean2 (SamplingStats a) =
  a >>> expandResults >>> resultById SamplingStatsMean2Id

-- | Return tha variance.
samplingStatsVariance :: MonadDES m => SamplingStats m -> ResultTransform m
samplingStatsVariance (SamplingStats a) =
  a >>> expandResults >>> resultById SamplingStatsVarianceId

-- | Return the deviation.
samplingStatsDeviation :: MonadDES m => SamplingStats m -> ResultTransform m
samplingStatsDeviation (SamplingStats a) =
  a >>> expandResults >>> resultById SamplingStatsDeviationId

-- | A counter for which the statistics is collected too.
newtype SamplingCounter m = SamplingCounter (ResultTransform m)

instance ResultTransformer SamplingCounter m where
  tr (SamplingCounter a) = a

-- | The counter value.
samplingCounterValue :: SamplingCounter m -> ResultTransform m
samplingCounterValue (SamplingCounter a) =
  a >>> resultById SamplingCounterValueId

-- | The counter statistics.
samplingCounterStats :: SamplingCounter m -> SamplingStats m
samplingCounterStats (SamplingCounter a) =
  SamplingStats (a >>> resultById SamplingCounterStatsId)

-- | The time-dependent statistics.
newtype TimingStats m = TimingStats (ResultTransform m)

instance ResultTransformer TimingStats m where
  tr (TimingStats a) = a

-- | Return the number of samples.
timingStatsCount :: MonadDES m => TimingStats m -> ResultTransform m
timingStatsCount (TimingStats a) =
  a >>> expandResults >>> resultById TimingStatsCountId

-- | Return the minimum value.
timingStatsMin :: MonadDES m => TimingStats m -> ResultTransform m
timingStatsMin (TimingStats a) =
  a >>> expandResults >>> resultById TimingStatsMinId

-- | Return the maximum value.
timingStatsMax :: MonadDES m => TimingStats m -> ResultTransform m
timingStatsMax (TimingStats a) =
  a >>> expandResults >>> resultById TimingStatsMaxId

-- | Return the average value.
timingStatsMean :: MonadDES m => TimingStats m -> ResultTransform m
timingStatsMean (TimingStats a) =
  a >>> expandResults >>> resultById TimingStatsMeanId

-- | Return the variance.
timingStatsVariance :: MonadDES m => TimingStats m -> ResultTransform m
timingStatsVariance (TimingStats a) =
  a >>> expandResults >>> resultById TimingStatsVarianceId

-- | Return the deviation.
timingStatsDeviation :: MonadDES m => TimingStats m -> ResultTransform m
timingStatsDeviation (TimingStats a) =
  a >>> expandResults >>> resultById TimingStatsDeviationId

-- | Return the time at which the minimum is attained.
timingStatsMinTime :: MonadDES m => TimingStats m -> ResultTransform m
timingStatsMinTime (TimingStats a) =
  a >>> expandResults >>> resultById TimingStatsMinTimeId

-- | Return the time at which the maximum is attained.
timingStatsMaxTime :: MonadDES m => TimingStats m -> ResultTransform m
timingStatsMaxTime (TimingStats a) =
  a >>> expandResults >>> resultById TimingStatsMaxTimeId

-- | Return the start time of sampling.
timingStatsStartTime :: MonadDES m => TimingStats m -> ResultTransform m
timingStatsStartTime (TimingStats a) =
  a >>> expandResults >>> resultById TimingStatsStartTimeId

-- | Return the last time of sampling.
timingStatsLastTime :: MonadDES m => TimingStats m -> ResultTransform m
timingStatsLastTime (TimingStats a) =
  a >>> expandResults >>> resultById TimingStatsLastTimeId

-- | Return the sum of values.
timingStatsSum :: MonadDES m => TimingStats m -> ResultTransform m
timingStatsSum (TimingStats a) =
  a >>> expandResults >>> resultById TimingStatsSumId

-- | Return the sum of square values.
timingStatsSum2 :: MonadDES m => TimingStats m -> ResultTransform m
timingStatsSum2 (TimingStats a) =
  a >>> expandResults >>> resultById TimingStatsSum2Id

-- | A time-dependent counter that collects the statistics too.
newtype TimingCounter m = TimingCounter (ResultTransform m)

instance ResultTransformer TimingCounter m where
  tr (TimingCounter a) = a

-- | The counter value.
timingCounterValue :: TimingCounter m -> ResultTransform m
timingCounterValue (TimingCounter a) =
  a >>> resultById TimingCounterValueId

-- | The counter statistics.
timingCounterStats :: TimingCounter m -> TimingStats m
timingCounterStats (TimingCounter a) =
  TimingStats (a >>> resultById TimingCounterStatsId)

-- | Represents either finite or infinite queue.
newtype Queue m = Queue (ResultTransform m)

instance ResultTransformer Queue m where
  tr (Queue a) = a

-- | The strategy applied to the enqueueing (input) processes when the finite queue is full.
enqueueStrategy :: Queue m -> ResultTransform m
enqueueStrategy (Queue a) =
  a >>> resultById EnqueueStrategyId

-- | The strategy applied when storing (in memory) items in the queue.
enqueueStoringStrategy :: Queue m -> ResultTransform m
enqueueStoringStrategy (Queue a) =
  a >>> resultById EnqueueStoringStrategyId

-- | The strategy applied to the dequeueing (output) processes when the queue is empty.
dequeueStrategy :: Queue m -> ResultTransform m
dequeueStrategy (Queue a) =
  a >>> resultById DequeueStrategyId

-- | Test whether the queue is empty.
queueNull :: Queue m -> ResultTransform m
queueNull (Queue a) =
  a >>> resultById QueueNullId

-- | Test whether the finite queue is full.
queueFull :: Queue m -> ResultTransform m
queueFull (Queue a) =
  a >>> resultById QueueFullId

-- | The finite queue capacity.
queueMaxCount :: Queue m -> ResultTransform m
queueMaxCount (Queue a) =
  a >>> resultById QueueMaxCountId

-- | Return the current queue size.
queueCount :: Queue m -> ResultTransform m
queueCount (Queue a) =
  a >>> resultById QueueCountId

-- | Return the queue size statistics.
queueCountStats :: Queue m -> TimingStats m
queueCountStats (Queue a) =
  TimingStats (a >>> resultById QueueCountStatsId)

-- | Return the total number of input items that were enqueued in the finite queue.
enqueueCount :: Queue m -> ResultTransform m
enqueueCount (Queue a) =
  a >>> resultById EnqueueCountId

-- | Return the number of lost items for the finite queue.
enqueueLostCount :: Queue m -> ResultTransform m
enqueueLostCount (Queue a) =
  a >>> resultById EnqueueLostCountId

-- | Return the total number of input items that were stored.
enqueueStoreCount :: Queue m -> ResultTransform m
enqueueStoreCount (Queue a) =
  a >>> resultById EnqueueStoreCountId

-- | Return the total number of requests for dequeueing the items, not taking
-- into account the failed attempts to dequeue immediately without suspension.
dequeueCount :: Queue m -> ResultTransform m
dequeueCount (Queue a) =
  a >>> resultById DequeueCountId

-- | Return the total number of output items that were actually dequeued.
dequeueExtractCount :: Queue m -> ResultTransform m
dequeueExtractCount (Queue a) =
  a >>> resultById DequeueExtractCountId

-- | Return the load factor: the finite queue size divided by its capacity.
queueLoadFactor :: Queue m -> ResultTransform m
queueLoadFactor (Queue a) =
  a >>> resultById QueueLoadFactorId

-- | Return the rate of the input items that were enqueued in the finite queue:
-- how many items per time.
enqueueRate :: Queue m -> ResultTransform m
enqueueRate (Queue a) =
  a >>> resultById EnqueueRateId

-- | Return the rate of the items that were stored: how many items per time.
enqueueStoreRate :: Queue m -> ResultTransform m
enqueueStoreRate (Queue a) =
  a >>> resultById EnqueueStoreRateId

-- | Return the rate of the requests for dequeueing the items: how many
-- requests per time. It does not include the failed attempts to dequeue
-- immediately without suspension.
dequeueRate :: Queue m -> ResultTransform m
dequeueRate (Queue a) =
  a >>> resultById DequeueRateId

-- | Return the rate of the output items that were dequeued: how many items per time.
dequeueExtractRate :: Queue m -> ResultTransform m
dequeueExtractRate (Queue a) =
  a >>> resultById DequeueExtractRateId

-- | Return the wait time from the time at which the item was stored in
-- the queue to the time at which it was dequeued.
queueWaitTime :: Queue m -> SamplingStats m
queueWaitTime (Queue a) =
  SamplingStats (a >>> resultById QueueWaitTimeId)

-- | Return the total wait time for the finite queue from the time at which
-- the enqueueing operation was initiated to the time at which the item was dequeued.
queueTotalWaitTime :: Queue m -> SamplingStats m
queueTotalWaitTime (Queue a) =
  SamplingStats (a >>> resultById QueueTotalWaitTimeId)

-- | Return the wait time from the time at which the item was stored in
-- the queue to the time at which it was dequeued.
enqueueWaitTime :: Queue m -> SamplingStats m
enqueueWaitTime (Queue a) =
  SamplingStats (a >>> resultById EnqueueWaitTimeId)

-- | Return the dequeue wait time from the time at which the item was requested
-- for dequeueing to the time at which it was actually dequeued.
dequeueWaitTime :: Queue m -> SamplingStats m
dequeueWaitTime (Queue a) =
  SamplingStats (a >>> resultById DequeueWaitTimeId)

-- | Return a long-term average queue rate calculated as the average queue size
-- divided by the average wait time.
queueRate :: Queue m -> ResultTransform m
queueRate (Queue a) =
  a >>> resultById QueueRateId

-- | Accumulates the statistics about that how long the arrived events are processed.
newtype ArrivalTimer m = ArrivalTimer (ResultTransform m)

instance ResultTransformer ArrivalTimer m where
  tr (ArrivalTimer a) = a

-- | Return the statistics about that how long the arrived events were processed.
arrivalProcessingTime :: ArrivalTimer m -> SamplingStats m
arrivalProcessingTime (ArrivalTimer a) =
  SamplingStats (a >>> resultById ArrivalProcessingTimeId)

-- | It models the server that prodives a service.
newtype Server m = Server (ResultTransform m)

instance ResultTransformer Server m where
  tr (Server a) = a

-- | The initial state of the server.
serverInitState :: Server m -> ResultTransform m
serverInitState (Server a) =
  a >>> resultById ServerInitStateId

-- | Return the current state of the server.
serverState :: Server m -> ResultTransform m
serverState (Server a) =
  a >>> resultById ServerStateId

-- | Return the counted total time when the server was locked while
-- awaiting the input.
serverTotalInputWaitTime :: Server m -> ResultTransform m
serverTotalInputWaitTime (Server a) =
  a >>> resultById ServerTotalInputWaitTimeId

-- | Return the counted total time spent by the server while
-- processing the tasks.
serverTotalProcessingTime :: Server m -> ResultTransform m
serverTotalProcessingTime (Server a) =
  a >>> resultById ServerTotalProcessingTimeId

-- | Return the counted total time when the server was locked while
-- trying to deliver the output.
serverTotalOutputWaitTime :: Server m -> ResultTransform m
serverTotalOutputWaitTime (Server a) =
  a >>> resultById ServerTotalOutputWaitTimeId

-- | Return the counted total time spent by the server while it was
-- preempted waiting for the further proceeding.
serverTotalPreemptionTime :: Server m -> ResultTransform m
serverTotalPreemptionTime (Server a) =
  a >>> resultById ServerTotalPreemptionTimeId

-- | Return the statistics of the time when the server was locked
-- while awaiting the input.
serverInputWaitTime :: Server m -> SamplingStats m
serverInputWaitTime (Server a) =
  SamplingStats (a >>> resultById ServerInputWaitTimeId)

-- | Return the statistics of the time spent by the server while
-- processing the tasks.
serverProcessingTime :: Server m -> SamplingStats m
serverProcessingTime (Server a) =
  SamplingStats (a >>> resultById ServerProcessingTimeId)

-- | Return the statistics of the time when the server was locked
-- while trying to deliver the output.
serverOutputWaitTime :: Server m -> SamplingStats m
serverOutputWaitTime (Server a) =
  SamplingStats (a >>> resultById ServerOutputWaitTimeId)

-- | Return the statistics of the time spent by the server while
-- it was preempted waiting for the further proceeding.
serverPreemptionTime :: Server m -> SamplingStats m
serverPreemptionTime (Server a) =
  SamplingStats (a >>> resultById ServerPreemptionTimeId)

-- | It returns the factor changing from 0 to 1, which estimates
-- how often the server was awaiting for the next input task.
serverInputWaitFactor :: Server m -> ResultTransform m
serverInputWaitFactor (Server a) =
  a >>> resultById ServerInputWaitFactorId

-- | It returns the factor changing from 0 to 1, which estimates
-- how often the server was busy with direct processing its tasks.
serverProcessingFactor :: Server m -> ResultTransform m
serverProcessingFactor (Server a) =
  a >>> resultById ServerProcessingFactorId

-- | It returns the factor changing from 0 to 1, which estimates
-- how often the server was locked trying to deliver the output
-- after the task is finished.
serverOutputWaitFactor :: Server m -> ResultTransform m
serverOutputWaitFactor (Server a) =
  a >>> resultById ServerOutputWaitFactorId

-- | It returns the factor changing from 0 to 1, which estimates
-- how often the server was preempted waiting for the further proceeding.
serverPreemptionFactor :: Server m -> ResultTransform m
serverPreemptionFactor (Server a) =
  a >>> resultById ServerPreemptionFactorId

-- | It models an activity that can be utilised.
newtype Activity m = Activity (ResultTransform m)

instance ResultTransformer Activity m where
  tr (Activity a) = a

-- | The initial state of the activity.
activityInitState :: Activity m -> ResultTransform m
activityInitState (Activity a) =
  a >>> resultById ActivityInitStateId

-- | Return the current state of the activity.
activityState :: Activity m -> ResultTransform m
activityState (Activity a) =
  a >>> resultById ActivityStateId

-- | Return the counted total time when the activity was utilised.
activityTotalUtilisationTime :: Activity m -> ResultTransform m
activityTotalUtilisationTime (Activity a) =
  a >>> resultById ActivityTotalUtilisationTimeId

-- | Return the counted total time when the activity was idle.
activityTotalIdleTime :: Activity m -> ResultTransform m
activityTotalIdleTime (Activity a) =
  a >>> resultById ActivityTotalIdleTimeId

-- | Return the counted total time when the activity was preemted
-- waiting for the further proceeding.
activityTotalPreemptionTime :: Activity m -> ResultTransform m
activityTotalPreemptionTime (Activity a) =
  a >>> resultById ActivityTotalPreemptionTimeId

-- | Return the statistics for the time when the activity was utilised.
activityUtilisationTime :: Activity m -> SamplingStats m
activityUtilisationTime (Activity a) =
  SamplingStats (a >>> resultById ActivityUtilisationTimeId)

-- | Return the statistics for the time when the activity was idle.
activityIdleTime :: Activity m -> SamplingStats m
activityIdleTime (Activity a) =
  SamplingStats (a >>> resultById ActivityIdleTimeId)

-- | Return the statistics for the time when the activity was preempted
-- waiting for the further proceeding.
activityPreemptionTime :: Activity m -> SamplingStats m
activityPreemptionTime (Activity a) =
  SamplingStats (a >>> resultById ActivityPreemptionTimeId)

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the activity was utilised.
activityUtilisationFactor :: Activity m -> ResultTransform m
activityUtilisationFactor (Activity a) =
  a >>> resultById ActivityUtilisationFactorId

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the activity was idle.
activityIdleFactor :: Activity m -> ResultTransform m
activityIdleFactor (Activity a) =
  a >>> resultById ActivityIdleFactorId

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the activity was preempted waiting for the further proceeding.
activityPreemptionFactor :: Activity m -> ResultTransform m
activityPreemptionFactor (Activity a) =
  a >>> resultById ActivityPreemptionFactorId

-- | The resource which can be acquired and then released.
newtype Resource m = Resource (ResultTransform m)

instance ResultTransformer Resource m where
  tr (Resource a) = a

-- | Return the current available count of the resource.
resourceCount :: Resource m -> ResultTransform m
resourceCount (Resource a) =
  a >>> resultById ResourceCountId

-- | Return the statistics for the available count of the resource.
resourceCountStats :: Resource m -> TimingStats m
resourceCountStats (Resource a) =
  TimingStats (a >>> resultById ResourceCountStatsId)

-- | Return the current utilisation count of the resource.
resourceUtilisationCount :: Resource m -> ResultTransform m
resourceUtilisationCount (Resource a) =
  a >>> resultById ResourceUtilisationCountId

-- | Return the statistics for the utilisation count of the resource.
resourceUtilisationCountStats :: Resource m -> TimingStats m
resourceUtilisationCountStats (Resource a) =
  TimingStats (a >>> resultById ResourceUtilisationCountStatsId)

-- | Return the current queue length of the resource.
resourceQueueCount :: Resource m -> ResultTransform m
resourceQueueCount (Resource a) =
  a >>> resultById ResourceQueueCountId

-- | Return the statistics for the queue length of the resource.
resourceQueueCountStats :: Resource m -> TimingStats m
resourceQueueCountStats (Resource a) =
  TimingStats (a >>> resultById ResourceQueueCountStatsId)

-- | Return the total wait time of the resource.
resourceTotalWaitTime :: Resource m -> ResultTransform m
resourceTotalWaitTime (Resource a) =
  a >>> resultById ResourceTotalWaitTimeId

-- | Return the statistics for the wait time of the resource.
resourceWaitTime :: Resource m -> SamplingStats m
resourceWaitTime (Resource a) =
  SamplingStats (a >>> resultById ResourceWaitTimeId)

-- | It models an opreation which actvity can be utilised.
newtype Operation m = Operation (ResultTransform m)

instance ResultTransformer Operation m where
  tr (Operation a) = a

-- | Return the counted total time when the operation activity was utilised.
operationTotalUtilisationTime :: Operation m -> ResultTransform m
operationTotalUtilisationTime (Operation a) =
  a >>> resultById OperationTotalUtilisationTimeId

-- | Return the counted total time when the operation activity was preemted
-- waiting for the further proceeding.
operationTotalPreemptionTime :: Operation m -> ResultTransform m
operationTotalPreemptionTime (Operation a) =
  a >>> resultById OperationTotalPreemptionTimeId

-- | Return the statistics for the time when the operation activity was utilised.
operationUtilisationTime :: Operation m -> SamplingStats m
operationUtilisationTime (Operation a) =
  SamplingStats (a >>> resultById OperationUtilisationTimeId)

-- | Return the statistics for the time when the operation activity was preempted
-- waiting for the further proceeding.
operationPreemptionTime :: Operation m -> SamplingStats m
operationPreemptionTime (Operation a) =
  SamplingStats (a >>> resultById OperationPreemptionTimeId)

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the operation activity was utilised.
operationUtilisationFactor :: Operation m -> ResultTransform m
operationUtilisationFactor (Operation a) =
  a >>> resultById OperationUtilisationFactorId

-- | It returns the factor changing from 0 to 1, which estimates how often
-- the operation activity was preempted waiting for the further proceeding.
operationPreemptionFactor :: Operation m -> ResultTransform m
operationPreemptionFactor (Operation a) =
  a >>> resultById OperationPreemptionFactorId
