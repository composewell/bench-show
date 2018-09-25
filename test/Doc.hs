{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Ord (comparing)
import Data.List (intersect, sortBy)
import Data.List.Split (splitOn)
import BenchGraph
       (defaultConfig, Config(..), ComparisonStyle(..),
        SortField(..), graphCmp)

main :: IO ()
main = do
    let classifier name =
            case splitOn "/" name of
                grp : rest -> Just (grp, concat rest)
                _      -> Nothing

    graphCmp "test/results-doc.csv" "docs/full" defaultConfig
    graphCmp "test/results-doc.csv" "docs/grouped"
        defaultConfig
        { classifyBenchmark = classifier
        , sortBenchFields = (`intersect` ["Mean"])
        }
    graphCmp "test/results-doc.csv" "docs/grouped-percent"
        defaultConfig
        { classifyBenchmark = classifier
        , comparisonStyle = ComparePercent
        , sortBenchFields = (`intersect` ["Mean"])
        }
    graphCmp "test/results-doc.csv" "docs/grouped-delta"
        defaultConfig
        { classifyBenchmark = classifier
        , comparisonStyle = CompareAbsoluteDiff
        , sortBenchFields = (`intersect` ["Mean"])
        }
    graphCmp "test/results-doc.csv" "docs/grouped-percent-delta"
        defaultConfig
        { classifyBenchmark = classifier
        , comparisonStyle = ComparePercentDiff
        , sortBenchFields = (`intersect` ["Mean"])
        }
    graphCmp "test/results-doc-multi.csv" "docs/regression"
        defaultConfig
        { classifyBenchmark = classifier
        , comparisonStyle = CompareAbsoluteDiff
        , sortBenchFields = (`intersect` ["Mean"])
        }
    graphCmp "test/results-doc-multi.csv" "docs/regression-percent"
        defaultConfig
        { classifyBenchmark = classifier
        , comparisonStyle = ComparePercentDiff
        , sortBenchFields = (`intersect` ["Mean"])
        }
    graphCmp
        "test/results-doc-multi.csv"
        "docs/regression-percent-descending"
        defaultConfig
        { classifyBenchmark = classifier
        , comparisonStyle = ComparePercentDiff
        , sortBenchFields = (`intersect` ["Mean"])
        , sortBenchmarks =
              \f ->
                  map fst $
                  sortBy (comparing snd) $ f $ Name "streamly(2)(-base %)"
        }
