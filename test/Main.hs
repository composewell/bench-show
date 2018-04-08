module Main where

import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import System.Process.Typed (readProcess_)
import BenchGraph (bgraph, Config(..), ComparisonStyle (..))

import Data.List

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

-- XXX use package name and a tag
packages :: [String]
packages = ["list", "pure-vector", "vector", "streamly", "streaming", "conduit", "pipes", "machines", "drinkery"]

{-
-- pairs of benchmark group titles and corresponding list of benchmark
-- prefixes i.e. without the package name at the end.
bmGroups :: [(String, [String])]
bmGroups =
    [
      -- Operations are listed in increasing cost order
      ( "Cheaper Operations (Shorter is Faster)"
      , [
          "elimination/toNull"
        , "filtering/drop-all"
        , "filtering/dropWhile-true"
        , "filtering/filter-all-out"
        , "elimination/last"
        , "elimination/fold"
        -- "filtering/take-one"
        , "transformation/map"
        , "filtering/take-all"
        , "filtering/takeWhile-true"
        , "filtering/filter-all-in"
        , "filtering/filter-even"
        , "transformation/scan"
        ]
      )

    , ( "Expensive operations (Shorter is Faster)"
      , [ "transformation/mapM"
        , "zip"
        , "transformation/concat"
        , "elimination/toList"
        ]
      )
    , ( "Composed (4x) operations (Shorter is Faster)"
      , [ "compose/all-out-filters"
        , "compose/all-in-filters"
        , "compose/mapM"
        , "compose/map-with-all-in-filter"
        ]
      )
    ]
    -}

-------------------------------------------------------------------------------
main :: IO ()
main = do
    let input = "test/results.csv"

    (out, _) <- readProcess_ "stack --system-ghc list-dependencies --bench"

    -- Get our streaming packages and their versions
    let match [] = Nothing
        match (_ : []) = Nothing
        match (x : y : _) =
            case elem x packages of
                False -> Nothing
                True -> Just (x, y)
        -- [(packagename, version)]
        pkginfo =
              catMaybes
            $ map match
            $ map words (lines (T.unpack $ T.decodeUtf8 out))

    -- suffix versions to packages
    let suffixVersion p =
            case lookup p pkginfo of
                Nothing -> p
                Just v -> p ++ "-" ++ v

    let titleToFileName t = filter (not . isSpace) (takeWhile (/= '(') t)
        title = "Cheaper Operations (Shorter is Faster)"
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
        cfg = Config
            { inputFile = input
            , chartTitle = title
            , outputDir = "charts"
            , outputFile = Just $ titleToFileName title
            , classifyBenchmark = \bm ->
                case any (`isPrefixOf` bm) prefixes of
                    True ->
                        let xs = reverse (splitOn "/" bm)
                        in Just (suffixVersion (xs !! 0), xs !! 1)
                    False -> Nothing
            , sortBenchmarks = \bs ->
                let i = intersect (map (last . splitOn "/") prefixes) bs
                in i ++ (bs \\ i)
            , sortBenchGroups = \gs ->
                let i = intersectBy (\x y -> head (splitOn "-" x) == y)
                                    gs packages
                in i ++ (gs \\ i)
            , selectBenchGroups = Nothing
            , setYScale = Nothing
            , comparisonStyle = CompareFull
            }
     in bgraph cfg
