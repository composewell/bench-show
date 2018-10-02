{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- |
-- Module      : BenchGraph.Tutorial
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
--
-- BenchGraph generates text reports and graphs from benchmarking results. It
-- is very flexible and allows you to manipulate the benchmarking data to
-- present it in many useful ways.

module BenchGraph.Tutorial
    (
    -- * Generating benchmark results
    -- $generating

    -- * Reports and Charts
    -- $plotting

    -- * Grouping
    -- $grouping

    -- * Difference
    -- $difference

    -- * Percentage Difference
    -- $percent

    -- * Sorting
    -- $sorting

    -- * Regression
    -- $regression
    )
where

import BenchGraph

-- $generating
--
-- To generate benchmark results use @gauge@ or @criterion@ benchmarking
-- libraries, define some benchmarks and run it with @--csv=results.csv@.
--
-- The resulting @results.csv@ may look like the following, for simplicity we
-- have removed some of the fields::
--
-- @
-- Name,time,maxrss
-- vector/fold,6.41620933137583e-4,2879488
-- streamly/fold,6.399582632376517e-4,2879488
-- vector/map,6.388913781259641e-4,2854912
-- streamly/map,6.533649051066093e-4,2793472
-- vector/zip,6.514202653014291e-4,2707456
-- streamly/zip,6.443344209329669e-4,2711552
-- @
--
-- If you run the benchmarks again (maybe after a change) the new results are
-- appended to the file. BenchGraph can compare the two or more result sets and
-- compare the results in different ways. We will use the above data for the
-- examples below, you can copy it and paste it in a file to play with it.
--
-- @gauge@ supports generating a raw csv file using @--csvraw@ option. The raw
-- csv file has results for all the benchmarking fields e.g. @maxrss@ or
-- @allocated@ and many more.

-- $plotting
--
-- 'report' with 'Fields' presentation style generates a multi-column report.
-- Units of the fields are automatically determined based on the range of
-- values:
--
-- @
-- 'report' "results.csv" Nothing 'defaultConfig' { 'presentation' = 'Fields' }
-- @
--
-- @
-- Benchmark     time(μs) maxrss(MiB)
-- ------------- -------- -----------
-- vector/fold     641.62        2.75
-- streamly/fold   639.96        2.75
-- vector/map      638.89        2.72
-- streamly/map    653.36        2.66
-- vector/zip      651.42        2.58
-- streamly/zip    644.33        2.59
-- @
--
-- 'graph' generates one bar chart per field:
--
-- @
-- 'graph' "results.csv" "output" 'defaultConfig'
-- @
--
-- When the input file contains results from a single benchmark run, by default
-- all the benchmarks are placed in a single benchmark group named "default".
--
-- <<full-time.svg Mean Time Full>>
--

-- $grouping
--
-- Let's write a benchmark classifier to put the @streamly@ and @vector@
-- benchmarks in their own groups:
--
-- @
--    classifier name =
--        case splitOn "/" name of
--            grp : bench -> Just (grp, concat bench)
--            _          -> Nothing
-- @
--
-- Now we can show the two benchmark groups as separate columns. We can
-- generate reports comparing different benchmark fields (e.g. @time@ and
-- @maxrss@) for all the groups::
--
-- @
--    'report' "results.csv" Nothing
--      'defaultConfig' { 'classifyBenchmark' = classifier }
-- @
--
-- @
-- (time)
-- Benchmark streamly(μs) vector(μs)
-- --------- ------------ ----------
-- fold            639.96     641.62
-- map             653.36     638.89
-- zip             644.33     651.42
-- @
--
-- We can do the same graphically as well, just replace 'report' with 'graph'
-- in the code above.  Each group is placed as a cluster on the graph. Multiple
-- clusters are placed side by side (i.e. on the same scale) for easy
-- comparison. For example:
--
-- <<grouped-time.svg Mean Time Grouped>>

-- $difference
--
-- We can make the first group as baseline and report the subsequent groups as
-- a difference from the baseline:
--
-- @
--    'report' "results.csv" Nothing
--      'defaultConfig'
--          { 'classifyBenchmark' = classifier
--          , 'presentation' = 'Groups' 'Diff'
--          }
-- @
--
-- @
-- (time)(Diff from baseline)
-- Benchmark streamly(μs)(base) vector(μs)(-base)
-- --------- ------------------ -----------------
-- fold                  639.96             +1.66
-- map                   653.36            -14.47
-- zip                   644.33             +7.09
-- @
--
-- In a chart, the second cluster plots the difference @streamly - vector@.
--
-- <<grouped-delta-time.svg Mean Time Grouped Delta>>

-- $percent
--
-- Absolute difference does not give us a good idea about how good or bad
-- the comparison is. We can report precentage difference instead:
--
-- @
--    'report' "results.csv" Nothing
--      'defaultConfig'
--          { 'classifyBenchmark' = classifier
--          , 'presentation' = 'Groups' 'PercentDiff'
--          }
-- @
--
-- @
-- (time)(Diff from baseline)
-- Benchmark streamly(μs)(base) vector(%)(-base)
-- --------- ------------------ ----------------
-- fold                  639.96            +0.26
-- map                   653.36            -2.22
-- zip                   644.33            +1.10
-- @
--
-- Graphically:
--
-- <<grouped-percent-delta-time.svg Mean Time Percent Delta>>

-- $sorting
--
-- Percentage difference does not immediately tell us the worst affected
-- benchmarks. We can sort the results by the difference:
--
-- @
--    'report' "results.csv" Nothing
--      'defaultConfig'
--          { 'classifyBenchmark' = classifier
--          , 'presentation' = 'Groups' 'PercentDiff'
--          , 'selectBenchmarks' = \f ->
--               reverse $ map fst $
--               sortBy (comparing snd) $ f $ 'ColumnIndex' 1
--          }
-- @
--
-- @
-- (time)(Diff from baseline)
-- Benchmark streamly(μs)(base) vector(%)(-base)
-- --------- ------------------ ----------------
-- zip                   644.33            +1.10
-- fold                  639.96            +0.26
-- map                   653.36            -2.22
-- @
--
-- This tells us that zip is the relatively worst benchmark for vector compared
-- to streamly, as it takes 1.10% more time, whereas map is the best taking
-- 2.22% less time..
--
-- Graphically:
--
-- <<grouped-percent-delta-sorted-time.svg Mean Time Percent Delta>>

-- $regression
--
-- We can append benchmarks results from multiple runs to the same file. These
-- runs can then be compared. We can run benchmarks before and after a change
-- and then report the regressions by percentage change in a sorted order:
--
-- Given the following results file with two runs appended:
--
-- @
-- Name,time
-- streamly/fold,1.755309435106302e-2
-- streamly/zip,2.960114434592148e-2
-- streamly/map,2.4673020708256527e-2
-- Name,time
-- streamly/fold,8.970816964261911e-3
-- streamly/zip,8.439519884529081e-3
-- streamly/map,6.972814233286865e-3
-- @
--
-- This code generates the report that follows:
--
-- @
--    'report' "results.csv" Nothing
--      'defaultConfig'
--          { 'classifyBenchmark' = classifier
--          , 'presentation' = 'Groups' 'PercentDiff'
--          , 'selectBenchmarks' = \f ->
--               reverse $ map fst $
--               sortBy (comparing snd) $ f $ 'ColumnIndex' 1
--          }
-- @
--
-- @
-- (time)(Diff from baseline)
-- Benchmark streamly(0)(μs)(base) streamly(1)(%)(-base)
-- --------- --------------------- ---------------------
-- zip                      644.33                +23.28
-- map                      653.36                 +7.65
-- fold                     639.96                -15.63
-- @
--
-- It tells us that in the second run the worst affected benchmark is zip
-- taking 23.28 percent more time comapred to the baseline.
--
-- Graphically:
--
-- <<regression-percent-descending-time.svg Mean Time Regression>>
--
