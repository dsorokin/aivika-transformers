
-- |
-- Module     : Simulation.Aivika.Trans.Statistics
-- Copyright  : Copyright (c) 2009-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL-3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- Represents statistics.
--

module Simulation.Aivika.Trans.Statistics
       (-- * Simple Statistics
        SamplingStats(..),
        SamplingData(..),
        combineSamplingStatsEither,
        samplingStatsVariance,
        samplingStatsDeviation,
        samplingStatsSummary,
        returnSamplingStats,
        listSamplingStats,
        fromIntSamplingStats,
        -- * Timing Statistics
        TimingStats(..),
        TimingData(..),
        timingStatsDeviation,
        timingStatsSummary,
        returnTimingStats,
        fromIntTimingStats,
        normTimingStats,
        -- * Simple Counter
        SamplingCounter(..),
        emptySamplingCounter,
        incSamplingCounter,
        decSamplingCounter,
        setSamplingCounter,
        returnSamplingCounter,
        -- * Timing Counter
        TimingCounter(..),
        emptyTimingCounter,
        incTimingCounter,
        decTimingCounter,
        setTimingCounter,
        returnTimingCounter) where

import Simulation.Aivika.Statistics
