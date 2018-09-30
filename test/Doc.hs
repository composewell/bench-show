{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Ord (comparing)
import Data.List (sortBy)
import Data.List.Split (splitOn)
import BenchGraph
       (defaultConfig, Config(..), Presentation(..), GroupStyle(..),
        SortColumn(..), graph, report)

main :: IO ()
main = do
    let classifier name =
            case splitOn "/" name of
                grp : rest -> Just (grp, concat rest)
                _      -> Nothing

    graph "test/results-doc.csv" "docs/full" defaultConfig
    report "test/results-doc.csv" Nothing
        defaultConfig { presentation = Fields }

    graph "test/results-doc.csv" "docs/grouped"
        defaultConfig
        { classifyBenchmark = classifier }

    report "test/results-doc.csv" Nothing
        defaultConfig
        { classifyBenchmark = classifier }

    graph "test/results-doc.csv" "docs/grouped-percent"
        defaultConfig
        { classifyBenchmark = classifier
        , presentation = Groups Percent
        }

    report "test/results-doc.csv" Nothing
        defaultConfig
        { classifyBenchmark = classifier
        , presentation = Groups Percent
        }

    graph "test/results-doc.csv" "docs/grouped-delta"
        defaultConfig
        { classifyBenchmark = classifier
        , presentation = Groups Diff
        }

    report "test/results-doc.csv" Nothing
        defaultConfig
        { classifyBenchmark = classifier
        , presentation = Groups Diff
        }

    graph "test/results-doc.csv" "docs/grouped-percent-delta"
        defaultConfig
        { classifyBenchmark = classifier
        , presentation = Groups PercentDiff
        }

    report "test/results-doc.csv" Nothing
        defaultConfig
        { classifyBenchmark = classifier
        , presentation = Groups PercentDiff
        }

    graph "test/results-doc.csv" "docs/grouped-percent-delta-sorted"
        defaultConfig
        { classifyBenchmark = classifier
        , presentation = Groups PercentDiff
        , selectBenchmarks =
              \f ->
                  reverse $ map fst $
                  sortBy (comparing snd) $ f $ ColumnIndex 1
        }

    report "test/results-doc.csv" Nothing
        defaultConfig
        { classifyBenchmark = classifier
        , presentation = Groups PercentDiff
        , selectBenchmarks =
              \f ->
                  reverse $ map fst $
                  sortBy (comparing snd) $ f $ ColumnIndex 1
        }

    graph
        "test/results-doc-multi.csv"
        "docs/regression-percent-descending"
        defaultConfig
        { classifyBenchmark = classifier
        , presentation = Groups PercentDiff
        , selectBenchmarks =
              \f ->
                  reverse $ map fst $
                  sortBy (comparing snd) $ f $ ColumnIndex 1
        }

    report
        "test/results-doc-multi.csv"
        Nothing
        defaultConfig
        { classifyBenchmark = classifier
        , presentation = Groups PercentDiff
        , selectBenchmarks =
              \f ->
                  reverse $ map fst $
                  sortBy (comparing snd) $ f $ ColumnIndex 1
        }
