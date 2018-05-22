-- |
-- Module      : BenchGraph
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- BenchGraph generates a graph from benchmarking results (CSV) generated by
-- @guage@ or @criterion@, specifically, it generates comparative graphs for
-- several groups of benchmarks that can be compared.  The groups could
-- correspond to different packages providing similar and comparable operations
-- or even different versions of the same package. This is a convenient tool to
-- compare performance results of alternative libraries or versions of the same
-- package after you make a change that may impact the performance benchmarks
-- of the package.
--
-- The input is the CSV file generated by @gauge --csv=results.csv@ or a
-- similar output generated by @criterion@. You need to invoke the 'bgraph'
-- function with an appropriate 'Config' to control various parameters of graph
-- generation.
-- Benchmark results found in the CSV file can be classified into several
-- groups using a classifier function and each group is displayed side by side
-- in the graph on the same scale for comparison.  The constituent benchmarks
-- in each benchmark group are placed together as a group and a legend is
-- displayed to mark who is who.
--
-- See the test directory to for an example of how to use it.
-- A sample output can be found in the 'charts' directory.

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module BenchGraph
    ( ComparisonStyle(..)
    , Config(..)
    , defaultConfig
    , bgraph
    ) where

import Control.Arrow (second)
import Control.Monad (when)
import Control.Monad.Trans.State.Lazy (get, put)
import Data.Char (toUpper)
import Data.Function ((&))
import Data.List (nub, transpose, findIndex, groupBy, (\\))
import Data.Maybe (catMaybes, fromMaybe, maybe)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.CSV (CSV, parseCSVFromFile)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

import qualified Data.List.Unique as U

-- | How to show the comparisons among benchmark groups.
data ComparisonStyle =
      CompareFull  -- ^ Show full results for all groups
    | CompareDelta -- ^ Show the first group with full results, show delta
                   -- from the first group for the subsequent groups.
    deriving Eq

data Config = Config
    {
    -- | The directory where the output graph should be placed. 'defaultConfig'
    -- uses the current directory as the default.
      outputDir   :: FilePath
    -- | The title to be embedded in the generated graph.
    , chartTitle  :: Maybe String
    -- | Given a benchmark name return a tuple (groupname, benchname), where
    -- groupname is the name of the group the benchmark should be placed in and
    -- benchname is the translated benchmark name to be displayed on the graph.
    -- If it returns Nothing the benchmark is ignored from the results.
    -- 'defaultConfig' uses a default classify function to place all benchmarks
    -- under one group named "default".
    , classifyBenchmark :: String -> Maybe (String, String)
    -- XXX need ability to sort in ascending order of bar heights for a
    -- particular group. For that we will need access to the full data.
    -- | The input to this function is the benchmarks to be plotted for each
    -- benchmark group. You can reorder them or filter them for presentation in
    -- the chart. Note that this works on the names generated by
    -- 'classifyBenchmark' and not the original benchmark names in the csv
    -- file.  'defaultConfig' supplies 'id' as the sort function.
    , sortBenchmarks :: [String] -> [String]
    -- | The input to this function contains groups in the order in which they
    -- appear in the benchmark data input.  You can use this to reorder and
    -- filter them for presentation in the chart. Note that this works on the
    -- names generated by 'classifyBenchmark'.  'defaultConfig' supplies 'id'
    -- as the sort function.
    , sortBenchGroups :: [String] -> [String]
    -- XXX ability to sepcify different units e.g. Milliseconds etc.
    -- | (RangeMin, RangeMax, NumIntervals) of the plot on the y (time) axis in
    -- microseconds
    , setYScale :: Maybe (Double, Double, Int)
    -- | How to show the comparisons
    , comparisonStyle :: ComparisonStyle
    }

defaultConfig :: Config
defaultConfig = Config
    { outputDir         = "."
    , chartTitle        = Nothing
    , classifyBenchmark = \b -> Just ("default", b)
    , sortBenchmarks    = id
    , sortBenchGroups   = id
    , setYScale         = Nothing
    , comparisonStyle   = CompareFull
    }

-- "values" is [(benchGroupName, [benchResult])]
-- benchResult contains results for each benchmark in "benchNames" in exactly
-- the same order.
genGroupGraph
    :: FilePath
    -> String
    -> Maybe [Double]
    -> Config
    -> [String]
    -> [(String, [Maybe Double])]
    -> IO ()
genGroupGraph outputFile units yindexes Config{..} benchNames values = do
    toFile
        def ((outputDir </> outputFile) ++ ".svg") $ do
        case chartTitle of
            Just title -> do
                layout_title .= title
                layout_title_style . font_size .= 25
            Nothing -> return ()

        -- woraround for a bug that renders the plot badly when using a single
        -- cluster in the bar chart.
        let vals =
                if length values == 1
                then values ++ [([], [])]
                else if comparisonStyle == CompareDelta
                     then let (_, h) = head values
                              toDelta x1 x2 =
                                case x1 of
                                    Nothing -> Nothing
                                    Just v -> fmap (\v1 -> v1 - v) x2
                              convertTail (name, xs) = (name ++ "(-base)", zipWith toDelta h xs)
                              convertHead (name, xs) = (name ++ "(base)", xs)
                          in convertHead (head values) : map convertTail (tail values)
                     else values

        layout_x_axis . laxis_generate .= autoIndexAxis (map fst vals)
        layout_x_axis . laxis_style . axis_label_style . font_size .= 16
        layout_y_axis . laxis_style . axis_label_style . font_size .= 14

        layout <- get
        case _layout_legend layout of
            Nothing -> return ()
            Just style@LegendStyle{..} -> do
                let s = style { _legend_plot_size = 22
                              -- , _legend_margin = 40
                              -- This is not available in versions <= 1.8.2
                              -- , _legend_position = LegendBelow
                              , _legend_label_style = _legend_label_style
                                    { _font_size = 14 }
                              }
                put $ layout { _layout_legend = Just s }

        -- layout_y_axis . laxis_override .= axisGridAtTicks
        let modifyLabels ad = ad {
                _axis_labels = map (map (second (++ " " ++ units)))
                                   (_axis_labels ad)
            }
        layout_y_axis . laxis_override .= modifyLabels

        case yindexes of
            Nothing -> return ()
            Just indexes ->
                layout_y_axis . laxis_override .= \_ ->
                    makeAxis (let f = floor :: Double -> Int
                              in map ((++ " " ++ units) . show . f))
                             (indexes, [], [])

        -- XXX We are mapping a missing value to 0, can we label it missing
        -- instead?
        let modifyVal x = map (fromMaybe 0) (snd x)
        plot $ fmap plotBars $ bars benchNames (addIndexes (map modifyVal vals))

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

genGraph :: FilePath -> String -> Maybe [Double] -> Config -> CSV -> IO ()
genGraph outfile units yindexes cfg@Config{..} csvData = do
    let origNames = nub $ map head csvData
        bmTuples = catMaybes $ map classifyBenchmark origNames

    let origGroups = nub $ map fst bmTuples
        bmgroups = sortBenchGroups origGroups
    when (bmgroups == []) $ error
        "No benchmark groups to plot. Please check your benchmark \
        \classifier (classifyBenchmarks), group filter (sortBenchGroups) or \
        \the input data"

    let newGroups = bmgroups \\ origGroups
    when (newGroups /= []) $ error $
        "sortBenchGroups cannot add new groups to the original list. The\
        \following new groups were added: " ++ show newGroups

    let rep = U.repeated bmTuples
        z = zip origNames bmTuples
        zrep = filter (\(_, tup) -> tup `elem` rep) z
    when (zrep /= []) $ do
        error $
            "classifyBenchmark cannot map different benchmarks to the same \
            \name under the same group.\n"
            ++ unlines (map show zrep)

    let names = nub $ map snd bmTuples
        bmnames = sortBenchmarks names
    when (bmnames == []) $ error
        "No benchmark names to plot. Please check your benchmark \
        \classifier (classifyBenchmarks), filter (sortBenchmarks) or \
        \the input data"

    let newNames = bmnames \\ names
    when (newNames /= []) $ error $
        "sortBenchmarks cannot add new names to the original list. The\
        \following new names were added: " ++ show newNames

    mapM_ (checkBenchNameInGrps bmgroups bmTuples) bmnames

    -- bmResults contains benchmark results for bmnames for each group
    let res = bmResults bmgroups bmnames
    when (res == []) $ error
        "Each benchmark being plotted must have the same number of results \
        \in the CSV input"

    genGroupGraph outfile units yindexes cfg bmnames res

    where

    grpGetResults bmnames groupName =
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
    bmResults grps names = concat $ catMaybes $ map (grpGetResults names) grps

    checkBenchNameInGrps bmgroups bmTuples nm =
        let appearsIn = nub $ map fst $ filter (\(_, n) -> nm == n) bmTuples
            xs = bmgroups \\ appearsIn
        in if not (null xs)
           then error $
            "Each benchmark name must appear in all benchmark groups.\n\
            \Benchmark " ++ nm ++ " does not appear in " ++ show xs ++
            "\nPlease check your benchmark classifier (classifyBenchmarks).\n"
           else return ()

getFieldIndexInLine :: String -> [String] -> Maybe Int
getFieldIndexInLine fieldName fields =
    findIndex (\x -> map toUpper x == map toUpper fieldName) fields

-- can return multiple indexes or empty list
getFieldIndexUnchecked :: String -> [[String]] -> [Int]
getFieldIndexUnchecked fieldName csvlines =
    nub $ catMaybes $ map (getFieldIndexInLine fieldName) csvlines

getFieldIndexChecked :: String -> [[String]] -> Int
getFieldIndexChecked fieldName csvlines =
    let idxs = getFieldIndexUnchecked fieldName csvlines
    in case idxs of
        [x] -> x
        [] -> error $ "Field name [" ++ fieldName
                      ++ "] does not occur in any line of "
                      ++ "the CSV input. Is the header line missing?"
        _ -> error $ "Field [" ++ fieldName
                     ++ "] found at different indexes [" ++ show idxs
                     ++ "] in different lines of CSV input"

-- Find the indexes of benchmark name, iterations and the requested field to be
-- plotted, also remove the header lines from the csv input and return the
-- cleaned up lines.
findIndexes :: String
           -> String
           -> String
           -> [[String]]
           -> (Int, Maybe Int, Int, [[String]])
findIndexes benchmarkNameField iterationField fieldName csvlines =
    let fieldIdx = getFieldIndexChecked fieldName csvlines
        iterIdxs = getFieldIndexUnchecked iterationField csvlines
        nameIdxs = getFieldIndexUnchecked benchmarkNameField csvlines
        csvlines' = filter (\xs -> xs !! fieldIdx /= fieldName) csvlines
    in case nameIdxs of
        [] -> case iterIdxs of
            [] -> -- just to show an error
                case getFieldIndexChecked benchmarkNameField csvlines of
                    _ -> error "not reached"
            _ ->
                -- Avoid a gauge/csvraw bug where the name field is not
                -- present
                ( 0
                , Just $ getFieldIndexChecked iterationField csvlines + 1
                , fieldIdx + 1
                , csvlines'
                )
        _ ->
            ( getFieldIndexChecked benchmarkNameField csvlines
            , case iterIdxs of
                [] -> Nothing
                _ -> Just $ getFieldIndexChecked iterationField csvlines
            , fieldIdx
            , csvlines'
            )

-- XXX show a better error message when indexing fails
extractIndexes :: (Int, Maybe Int, Int, [[String]]) -> [[String]]
extractIndexes (nameIdx, iterIdx, fieldIdx, csvlines) =
    let zipList = zipWith (++)
    in map (\xs -> [xs !! nameIdx]) csvlines
        `zipList`
            case iterIdx of
                Nothing -> repeat ["1"]
                Just idx -> map (\xs -> [xs !! idx]) csvlines
        `zipList` map (\xs -> [xs !! fieldIdx]) csvlines

-- XXX display GHC version as well
-- XXX display the OS/arch
-- This data should be in the measurement data
--
-- TODO Specify the units of the field being plotted, this should be mentioned
-- in the CSV header.

-- | The first parameter is an input file containing CSV data as generated by
-- @gauge --csv=results.csv@ or a similar output generated by @criterion@.  The
-- second parameter is the name of the output file containing the graph SVG
-- image. The third parameter is the name of the field that should be plotted.
-- The field is matched with the fields in the header line of the CSV input
-- using a case insensitive match.  The last parameter is the configuration to
-- customize the graph, you can start with 'defaultConfig' as the base and set
-- any of the fields that you may want to change.
bgraph :: FilePath -> FilePath -> String -> Config -> IO ()
bgraph inputFile outputFile fieldName cfg@Config{..} = do
    createDirectoryIfMissing True outputDir
    putStrLn $ "Creating chart "
        ++ maybe "" (\x -> "[" ++ x ++ "]") chartTitle
        ++ " at "
        ++ show (outputDir </> outputFile)

    -- We assume the dataset is not big and therefore take liberties to process
    -- in a non-streaming fashion.
    csvData <- parseCSVFromFile inputFile
    case csvData of
        Left e -> error $ show e
        Right csvlines -> do
            let isTimeField =
                    let x = map toUpper fieldName
                    in x == "TIME" || x == "MEAN"

                (multiplier, units) =
                    case isTimeField of
                        -- XXX automatically use ns/us/ms/sec on the scale
                        -- get the max and convert it to appropriate unit
                        True -> (1000, "ms")
                        False -> (1, "")

                -- XXX need the ability to specify Units in the scale
                yindexes =
                    case setYScale of
                        Nothing -> Nothing
                        Just (rangeMin, rangeMax, nInterval) ->
                            let r = (rangeMax - rangeMin)/(fromIntegral nInterval)
                            in case isTimeField of
                                True ->
                                    let r'   = r/1000
                                        rmin = rangeMin/1000
                                    in Just $ take (nInterval + 1) [rmin, rmin + r'..]
                                False -> Just $ take (nInterval + 1) [rangeMin, rangeMin + r..]

                foldToMean xs =
                    let iters  = map (read . (!! 0)) xs :: [Double]
                        values = map (read . (!! 1)) xs :: [Double]
                        mean = sum values / sum iters
                    in show $ case isTimeField of
                        True -> mean * multiplier
                        False -> mean

             in    -- cleanup blank rows
                  filter (/= [""]) csvlines
                  -- An iteration field indicates that consecutive rows with
                  -- the same benchmark name have results from different
                  -- iterations of the same benchmark and the measurement
                  -- fields have to be scaled per iteration based on the number
                  -- of iterations in the iteration count field.
                & findIndexes "Name" "iters" fieldName
                & extractIndexes
                  -- from here on three elements are guaranteed in each row.
                  -- group successive iterations. If the iteration number does
                  -- not increase then it means it is another benchmark and not
                  -- a continuation. This can happen if there is only one
                  -- benchmark in two separate runs.
                & groupBy (\(x1:i1:_) (x2:i2:_) -> x1 == x2 && i2 > i1)
                  -- reduce grouped iterations to a single row with the mean
                & map (\xs -> [head $ map head xs, foldToMean $ map tail xs])
                 -- XXX send tuples [(String, Double)] instead of [[String]]
                 -- XXX determine the units based on the field name
                 -- We can pass here the units to be displayed by the chart
                & genGraph outputFile units yindexes cfg
