
-- |
-- Module     : Simulation.Aivika.Trans.Statistics
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
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
        fromIntTimingStats) where

import Simulation.Aivika.Statistics
