{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module BenchGraph (bgraph, Config(..), ComparisonStyle (..)) where

import Control.Arrow (second)
import Control.Monad.Trans.State.Lazy (get, put)
import Data.List (nub, transpose)
import Data.Maybe (catMaybes, fromMaybe)
import System.Directory (createDirectoryIfMissing)
import Text.CSV (CSV, parseCSVFromFile)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

data ComparisonStyle = CompareDelta | CompareFull

data Config = Config
    {
      inputFile  :: FilePath
    , chartTitle  :: String
    , outputDir   :: FilePath
    -- | Default is the same as the title
    , outputFile  :: Maybe FilePath
    -- | Given a list of translated benchmark names - reorder them for
    -- presentation in the chart.
    , sortBenchmarks :: [String] -> [String]
    , sortBenchGroups :: [String] -> [String]
    -- | Given a benchmark name return a tuple (groupname, benchname), where
    -- groupname is the name of the group the benchmark belongs to and the
    -- benchname is the translated benhcmark name to be displayed on the graph.
    -- if it returns Nothing the benchmark will be ignored.
    , classifyBenchmark :: String -> Maybe (String, String)
    -- | Given the total number of bgroups found in the benchmarked data,
    -- select the indexes of the ones you want to plot, indexes start from 0.
    -- default is to plot last 3.
    , selectBenchGroups :: Maybe (Int -> [Int])
    -- | (RangeMax, NumMajorTicks) of the plot on the y (time) axis in
    -- microseconds
    , setYScale :: Maybe (Double, Int)
    -- | How to show the comparisons
    , comparisonStyle :: ComparisonStyle
    }

-- "values" is [(benchGroupName, [benchResult])]
-- benchResult contains results for each benchmark in "benchNames" in exactly
-- the same order.
genGroupGraph :: Config -> [String] -> [(String, [Maybe Double])] -> IO ()
genGroupGraph Config{..} benchNames values =
    -- XXX use filepath/path concatenation
    toFile def (outputDir ++ "/" ++ fromMaybe chartTitle outputFile ++ ".svg") $ do
        layout_title .= chartTitle
        layout_title_style . font_size .= 25
        layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
        layout_x_axis . laxis_style . axis_label_style . font_size .= 16
        layout_y_axis . laxis_style . axis_label_style . font_size .= 14

        layout <- get
        case _layout_legend layout of
            Nothing -> return ()
            Just style@LegendStyle{..} -> do
                let s = style { _legend_plot_size = 22
                              -- , _legend_margin = 40
                              , _legend_position = LegendBelow
                              , _legend_label_style = _legend_label_style
                                    { _font_size = 14 }
                              }
                put $ layout { _layout_legend = Just s }

        -- layout_y_axis . laxis_override .= axisGridAtTicks
        let modifyLabels ad = ad {
                _axis_labels = map (map (second (++ " ms"))) (_axis_labels ad)
            }
        layout_y_axis . laxis_override .= modifyLabels

        case setYScale of
            Nothing -> return ()
            Just (rangeMax, nticks) ->
                layout_y_axis . laxis_override .= \_ ->
                    let indexes = take nticks [0,rangeMax/(fromIntegral nticks)..]
                    in makeAxis (map ((++ " ms") . show . floor)) (indexes, [], [])

        -- XXX We are mapping a missing value to 0, can we label it missing
        -- instead?
        let modifyVal x = map ((*1000) . fromMaybe 0) (snd x)
        plot $ fmap plotBars $ bars benchNames (addIndexes (map modifyVal values))

-- [[Double]] each list is multiple results for each benchmark
transposeLists :: [[a]] -> Maybe [[Maybe a]]
transposeLists xs =
    -- If each benchmark does not have the same number of results then reject
    -- all because the results may not correspond with each other when zipped.
    case nub $ map length xs of
        [0] -> Nothing
        [n] ->
            let ys = map (convertToMaybe n) xs
            in Just $ transpose ys
        [0,n] ->
            -- some packages may have missing benchmarks
            -- fill the empty results with Nothing
            let ys = map (convertToMaybe n) xs
            in Just $ transpose ys
        _ -> Nothing
    where
        convertToMaybe n zs = case zs of
            [] -> replicate n Nothing
            x  -> map Just x

-- We return a list of lists as the same benchmark may appear more than once if
-- we ran benchmarks for the same package multiple times. This is helpful in
-- comparing the benchmarks for the same package after fixing something.
getResultsForBenchGroup
    :: CSV
    -> (String -> Maybe (String, String))
    -> String
    -> [String]
    -> Maybe [[Maybe Double]]
getResultsForBenchGroup csvData classify groupName bmnames  =
    -- XXX this can be quite inefficient, need to do it better
    transposeLists $ map getBenchmarkMeans bmnames

    where

    match name origName =
        case classify origName of
            Nothing -> False
            Just (g, n) -> g == groupName && n == name

    getBenchmarkMeans :: String -> [Double]
    getBenchmarkMeans bmname =
        -- field at index 1 is the mean
        map read $ map (!! 1) $ filter (match bmname .  head) csvData

genGraph :: Config -> CSV -> IO ()
genGraph cfg@Config{..} csvData =
    -- bmResults contains benchmark results for bmnames for each group
    genGroupGraph cfg bmnames bmResults

    where

    getBenchNames = nub $ map head csvData
    bmTuples = catMaybes $ map classifyBenchmark getBenchNames
    -- XXX assert that for each group we get the same bmnames
    bmnames = sortBenchmarks $ nub $ map snd bmTuples
    bmgroups = sortBenchGroups $ nub $ map fst bmTuples

    grpGetResults groupName =
        let res = getResultsForBenchGroup csvData classifyBenchmark
                                          groupName bmnames
        in case res of
            Nothing -> Nothing
            Just xs ->
                case length xs of
                    0 -> Nothing
                    1 -> Just $ map (groupName,) xs
                    _ -> Just $ zipWith (withIndexes groupName) [(1::Int)..] xs
    withIndexes groupName indx y = (groupName ++ "(" ++ show indx ++ ")", y)

    -- this produces results for all groups
    -- [(groupName, [Maybe Double])]
    bmResults = concat $ catMaybes $ map grpGetResults bmgroups

-- XXX display GHC version as well
-- XXX display the OS/arch
-- This data should be in the measurement data

bgraph :: Config -> IO ()
bgraph cfg@Config{..} = do
    createDirectoryIfMissing True outputDir

    csvData <- parseCSVFromFile inputFile
    case csvData of
        Left e -> error $ show e
        Right dat -> genGraph cfg dat
