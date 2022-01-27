{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Function (on)
import Data.List
import Data.Functor.Identity (runIdentity)
-- import Data.Maybe (catMaybes)
-- import System.Process.Typed (readProcess_)

import qualified Streamly.Prelude as Stream
import qualified Streamly.Data.Fold as Fold

import BenchShow

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

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn d =
    runIdentity
        . Stream.toList . Stream.splitOn (== d) Fold.toList . Stream.fromList

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
                let i = intersect (map (last . splitOn '/') prefixes) bs
                in i ++ (bs \\ i)
        cfg = defaultConfig
            { mkTitle = Just (\_ -> chartTitle)
            , outputDir = Just "charts"
            , classifyBenchmark = \bm ->
                case any (`isPrefixOf` bm) prefixes of
                    True ->
                        let xs = reverse (splitOn '/' bm)
                        in Just (suffixVersion (xs !! 0), xs !! 1)
                    False -> Nothing
            , selectBenchmarks = \g -> bsort $
                either error (map fst) $ g (ColumnIndex 0) Nothing
            , selectGroups = \gs ->
                let gs' = map fst gs
                    i = intersect (map suffixVersion packages) gs'
                    new = i ++ (gs' \\ i)
                in concat $ map (\x -> filter (\(y,_) -> y == x) gs) new
            }

    -- csv format
    graph "test/results.csv" "csv-full" cfg

    -- raw csv format

    graph "test/results.csvraw" "csvraw-solo"
            cfg { presentation = Solo }

    -- multi-field graph
    graph "test/results.csvraw" "csvraw-full-fields"
            cfg { presentation = Fields }

    -- multi-groupd graphs
    graph "test/results.csvraw" "csvraw-full"
            cfg { presentation = Groups Absolute }

    graph "test/results.csvraw" "csvraw-delta"
            cfg { fieldRanges = [("mean", -20000, 50000)]
                , fieldTicks = [("mean", TickCount 7)]
                , presentation = Groups Diff
                , selectFields = (`intersect` ["time"])
                }
    graph "test/results.csvraw" "csvraw-percent-delta"
            cfg { presentation = Groups PercentDiff
                , selectFields = (`intersect` ["time"])
                }

    -- Multi-field text reports
    report "test/results.csv" Nothing
            cfg { presentation = Fields }

    report "test/results.csvraw" Nothing
            cfg { presentation = Solo }

    report "test/results.csvraw" Nothing
            cfg { presentation = Fields }

    -- XXX disabled until a fixed version of statistics is released
            {-
    report "test/results.csvraw" Nothing
            cfg { presentation = Fields
                , selectFields = drop 7
                }
                -}

    -- Multi-group text reports
    report "test/results.csv" Nothing
            cfg { presentation = Groups Absolute
                , selectBenchmarks = \g ->
                    either error (map fst . sortBy (compare `on` snd))
                           (g (ColumnIndex 1) Nothing)
                }

    report "test/results.csv" Nothing
            cfg { presentation = Groups PercentDiff
                , selectBenchmarks = \g ->
                    either error (map fst . sortBy (compare `on` snd))
                           (g (ColumnIndex 1) Nothing)
                }
    report "test/results.csv" Nothing
            cfg { presentation = Groups Diff
                }
