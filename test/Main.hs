{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.List
import Data.List.Split (splitOn)
-- import Data.Maybe (catMaybes)
-- import System.Process.Typed (readProcess_)
import BenchGraph
       (defaultConfig, Config(..), ComparisonStyle(..), Granularity(..),
        SortField(..), graphCmp)

-- import qualified Data.Text.Lazy as T
-- import qualified Data.Text.Lazy.Encoding as T

-- XXX use package name and a tag
packages :: [String]
packages =
    [ "list"
    , "pure-vector"
    , "vector"
    , "streamly"
    , "streaming"
    , "conduit"
    , "pipes"
    , "machines"
    , "drinkery"
    ]

-------------------------------------------------------------------------------
main :: IO ()
main = do
    {-
    (out, _) <- readProcess_ "stack --system-ghc list-dependencies --bench"

    -- Get our streaming packages and their versions
    let match [] = Nothing
        match (_ : []) = Nothing
        match (x : y : _) =
            case elem x packages of
                False -> Nothing
                True -> Just (x, y)

        -- pkginfo is [(packagename, version)]
        pkginfo =
              catMaybes
            $ map match
            $ map words (lines (T.unpack $ T.decodeUtf8 out))
    -}
    let pkginfo = []

    -- suffix versions to packages
    let suffixVersion p =
            case lookup p pkginfo of
                Nothing -> p
                Just v -> p ++ "-" ++ v

    let chartTitle = "Cheaper Operations (Lower is Better)"
        prefixes =
            [ "elimination/toNull"
            , "filtering/drop-all"
            , "filtering/dropWhile-true"
            , "filtering/filter-all-out"
            , "elimination/last"
            , "elimination/fold"
            , "transformation/map"
            , "filtering/take-all"
            , "filtering/takeWhile-true"
            , "filtering/filter-all-in"
            , "filtering/filter-even"
            , "transformation/scan"
            ]
        bsort bs =
                let i = intersect (map (last . splitOn "/") prefixes) bs
                in i ++ (bs \\ i)
        cfg = defaultConfig
            { title = Just chartTitle
            , outputDir = Just "charts"
            , classifyBenchmark = \bm ->
                case any (`isPrefixOf` bm) prefixes of
                    True ->
                        let xs = reverse (splitOn "/" bm)
                        in Just (suffixVersion (xs !! 0), xs !! 1)
                    False -> Nothing
            , sortBenchmarks = \g -> bsort $ map fst (g (Index 0))
            , sortBenchGroups = \gs ->
                let i = intersect (map suffixVersion packages) gs
                in i ++ (gs \\ i)
            }

    -- csv format
    graphCmp "test/results.csv" "csv-full" cfg

    -- raw csv format
    graphCmp "test/results.csvraw" "csvraw-full"
            cfg { sortBenchFields = (\\ ["name", "iters"]) }

    -- Other types of comparisons
    graphCmp "test/results.csvraw" "csvraw-delta"
            cfg { fieldRanges = [("mean", -20000, 50000)]
                , fieldGranularities = [("mean", GrainCount 7)]
                , comparisonStyle = CompareAbsoluteDiff
                , sortBenchFields = (`intersect` ["time"])
                }
    graphCmp "test/results.csvraw" "csvraw-percent"
            cfg { comparisonStyle = ComparePercent
                , sortBenchFields = (`intersect` ["time"])
                }
    graphCmp "test/results.csvraw" "csvraw-percent-delta"
            cfg { comparisonStyle = ComparePercentDiff
                , sortBenchFields = (`intersect` ["time"])
                }
