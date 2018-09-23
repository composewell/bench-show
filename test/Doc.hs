{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.List.Split (splitOn)
import BenchGraph (bgraph, defaultConfig, Config(..), ComparisonStyle(..))

main :: IO ()
main = do
    bgraph "test/results-doc.csv" "docs/mean-full" "mean" defaultConfig
    let classifier name =
            case splitOn "/" name of
                grp : rest -> Just (grp, concat rest)
                _      -> Nothing
    bgraph "test/results-doc.csv" "docs/mean-grouped" "mean" defaultConfig
        { classifyBenchmark = classifier }
    bgraph "test/results-doc.csv" "docs/mean-grouped-delta" "mean" defaultConfig
        { classifyBenchmark = classifier
        , comparisonStyle = CompareDelta
        }
    bgraph "test/results-doc-multi.csv" "docs/mean-regression" "mean" defaultConfig
        { classifyBenchmark = classifier
        , comparisonStyle = CompareDelta
        }
