{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Functor.Identity (runIdentity)

import qualified Streamly.Prelude as Stream
import qualified Streamly.Data.Fold as Fold

import BenchShow

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn d =
    runIdentity
        . Stream.toList . Stream.splitOn (== d) Fold.toList . Stream.fromList

main :: IO ()
main = do
    let classifier name =
            case splitOn '/' name of
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

    graph "test/results-doc.csv" "docs/grouped-percent-delta"
        defaultConfig
        { classifyBenchmark = classifier
        , presentation = Groups PercentDiff
        , estimator = Regression
        }
    report "test/results-doc.csv" Nothing
        defaultConfig
        { classifyBenchmark = classifier
        , presentation = Groups PercentDiff
        , estimator = Regression
        }

    graph "test/results-doc.csv" "docs/grouped-single-estimator"
        defaultConfig
        { classifyBenchmark = classifier
        , presentation = Groups PercentDiff
        , estimator = Regression
        , diffStrategy = SingleEstimator
        }
    report "test/results-doc.csv" Nothing
        defaultConfig
        { classifyBenchmark = classifier
        , presentation = Groups PercentDiff
        , estimator = Regression
        , diffStrategy = SingleEstimator
        , verbose = True
        }

    graph "test/results-doc.csv" "docs/grouped-percent-delta-sorted"
        defaultConfig
        { classifyBenchmark = classifier
        , presentation = Groups PercentDiff
        , selectBenchmarks =
              \f ->
                    reverse
                  $ map fst
                  $ sortBy (comparing snd)
                  $ either error id $ f (ColumnIndex 1) Nothing
        }
    report "test/results-doc.csv" Nothing
        defaultConfig
        { classifyBenchmark = classifier
        , presentation = Groups PercentDiff
        , selectBenchmarks =
              \f ->
                    reverse
                  $ map fst
                  $ sortBy (comparing snd)
                  $ either error id $ f (ColumnIndex 1) Nothing
        }

    graph
        "test/results-doc-multi.csv"
        "docs/regression-percent-descending"
        defaultConfig
        { classifyBenchmark = classifier
        , presentation = Groups PercentDiff
        , selectBenchmarks =
              \f ->
                    reverse
                  $ map fst
                  $ sortBy (comparing snd)
                  $ either error id $ f (ColumnIndex 1) Nothing
        }
    report
        "test/results-doc-multi.csv"
        Nothing
        defaultConfig
        { classifyBenchmark = classifier
        , presentation = Groups PercentDiff
        , selectBenchmarks =
              \f ->
                    reverse
                  $ map fst
                  $ sortBy (comparing snd)
                  $ either error id $ f (ColumnIndex 1) Nothing
        }
