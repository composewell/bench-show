-- |
-- Module      : BenchGraph.Common
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module BenchGraph.Common
    ( ComparisonStyle(..)
    , Granularity (..)
    , SortField (..)
    , Config(..)
    , defaultConfig
    , isTimeField
    , getFieldRange
    , getFieldGranularity
    , rawReportComparing
    , prepareToReport
    , prepareOutputFile
    , comparisonStyleString
    , transformGroupValues
    , makeTitle
    ) where

import Control.Monad (when, unless)
import Data.Char (toLower)
import Data.Function ((&), on)
import Data.List
       (nub, nubBy, transpose, findIndex, groupBy, (\\), group, sort, find,
       intersect)
import Data.Maybe (catMaybes, fromMaybe)
import Debug.Trace (trace)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.CSV (CSV, parseCSVFromFile)
import Text.Read (readMaybe)

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- Find items that are repeated more than once
getRepeated :: Ord a => [a] -> [a]
getRepeated = map head . filter ((>1) . length) . group . sort

filterSanity :: (Eq a, Show a) => String -> [a] -> [a] -> IO ()
filterSanity label old new = do
    let added = new \\ old

    when (null new) $ error $
        label ++ " must select at least one item from the list: "
        ++ show old

    unless (null added) $ error $
        label ++
        " must not add any new items to the original list. The \
        \following items were added: " ++ show added

-------------------------------------------------------------------------------

-- | How to show the comparisons among benchmark groups. Different groups of
-- benchmarks are shown side by side on a graph or in a columnwise textual
-- report. The groups can be presented in many different ways for easy
-- comparison.
--
-- @since 0.2.0
data ComparisonStyle
    = CompareFull         -- ^ Show full results for all groups
    | CompareAbsoluteDiff -- ^ Show subsequent results as difference from the
                          -- baseline
    | ComparePercent      -- ^ Show subsequent results as percentage of baseline
    | ComparePercentDiff  -- ^ For subsequent results show the difference from
                          -- baseline as a percentage of baseline.
    deriving (Eq)

-- | Granularity is used in visual charts to generates the ticks. You can
-- specify either the size of a tick ('GrainSize') or the total number of ticks
-- ('GrainCount').
--
-- @since 0.2.0
data Granularity =
      GrainSize Int  -- ^ Size of a grain, the unit is microseconds for time
                     -- fields, and bytes for space fields.
    | GrainCount Int -- ^ Total number of grains in the range spread

-- | Specify the benchmark field (e.g. time) (in non-comparative presentation)
-- or benchmark group (in comparative presentations) used for deciding the
-- order of the presented benchmarks. 'sortBenchmarks' would be provided with
-- the data for the corresponding field or benchmark group. We can specify
-- either the name of the field or benchmark group or its index starting from
-- 0.
--
-- @since 0.2.0
data SortField = Name String | Index Int

-- | Configuration governing generation of chart. See 'defaultConfig' for the
-- default values of these fields.
--
-- @since 0.2.0
data Config = Config
    {
    -- | Report title, more information like the plotted field name or
    -- comparison type may be added to it.
      title  :: Maybe String
    -- | The directory where the output graph or report file should be placed.
    , outputDir   :: Maybe FilePath
    -- | How to show the comparisons among multiple sets of benchmarks..
    , comparisonStyle :: ComparisonStyle
    -- | Filter and reorder the benchmarking fields. It is invoked with a list
    -- of all available benchmarking fields. Only those fields present in the
    -- output of this function are plotted and in that order.
    , sortBenchFields :: [String] -> [String]
    -- | Filter, group and translate benchmark names. This function is invoked
    -- once for all benchmark names found in the results. It produces a tuple
    -- @(groupname, benchname)@, where @groupname@ is the name of the group the
    -- benchmark should be placed in and @benchname@ is the translated
    -- benchmark name to be used in the report.  If it returns 'Nothing' for a
    -- benchmark, that benchmark is omitted from the results.
    , classifyBenchmark :: String -> Maybe (String, String)
    -- | Filter and reorder the benchgroup names. A benchgroup may be assigned
    -- using 'classifyBenchmark'; when not assigned, all benchmarks are placed
    -- in the @default@ benchgroup. When there are multiple result sets in the
    -- input file, each group appears in each set and is assigned a suffix to
    -- identify the instance e.g.  @group(1)@ and @group(2)@. More suffixes are
    -- used depending on the comparison method as well. To know the actual
    -- names with the suffixes you can print them in this function.
    --
    -- The output produced by this function is a reordered subset of the input.
    -- Only those benchgroups present in the output are rendered and are
    -- presented in that order.
    , sortBenchGroups :: [String] -> [String]
    -- | Filter and reorder benchmarks. This is provided with a function which
    -- is invoked with a sorting column name and produces the benchmark names
    -- and corresponding value for that column. In comparative reports the
    -- column is a benchgroup name, and in non-comparative reports it is a
    -- benchmarking field name.
    , sortBenchmarks :: (SortField -> [(String, Maybe Double)]) -> [String]
    -- | Specify the min and max range of benchmarking fields. If the field
    -- value is outside the range it will be clipped to the range limit. This
    -- is useful to contain the size of the plot or the size of the text in a
    -- textual report.
    -- For time fields the range values are in microseconds and for space
    -- fields they are in bytes.
    -- The values in the tuple are @(fieldName, RangeMin, RangeMax)@.
    , fieldRanges :: [(String, Double, Double)]
    -- | Specify the granularity or tick size of fields.
    -- The values in the tuple are @(fieldName, granularity)@.
    , fieldGranularities :: [(String, Granularity)]
    }

-- | Default configuration. Use this as the base configuration and modify the
-- required fields. The defaults are:
--
-- @
--  title              = Nothing
--  outputDir          = Nothing
--  comparisonStyle    = CompareFull
--  sortBenchFields    = (`intersect` ["time", "Mean", "maxrss"])
--  classifyBenchmark  = \b -> Just ("default", b)
--  sortBenchGroups    = id
--  sortBenchmarks     = \f -> map fst (f (Index 0))
--  fieldRanges        = []
--  fieldGranularities = []
-- @
--
-- @since 0.2.0
defaultConfig :: Config
defaultConfig = Config
    { title              = Nothing
    , outputDir          = Nothing
    , comparisonStyle    = CompareFull
    , sortBenchFields    = (`intersect` ["time", "Mean", "maxrss"])
    , classifyBenchmark  = \b -> Just ("default", b)
    , sortBenchGroups    = id
    , sortBenchmarks     = \f -> map fst (f (Index 0))
    , fieldRanges        = []
    , fieldGranularities = []
    }

-------------------------------------------------------------------------------
-- Benchmarking field specific handling
-------------------------------------------------------------------------------

isTimeField :: String -> Bool
isTimeField fieldName =
    let x = map toLower fieldName
    in x == "time" || x == "mean"

isAllocationField :: String -> Bool
isAllocationField fieldName =
    let x = map toLower fieldName
    in x == "allocated" || x == "maxrss"

-- By default the fields are considered "scaled" fields that is
-- they scale by iterations. However in case of maxrss field it is
-- a max value across the experiment and does not scale by
-- iterations, in this case we just need to take a mean or max
-- without scaling.
isMaxField :: String -> Bool
isMaxField fieldName =
    let x = map toLower fieldName
    in x == "maxrss"

-- XXX make the multiplier and units configurable
-- XXX Use a separate table for the defaults
-- returns (multiplier, units)
fieldUnits :: String -> ComparisonStyle -> (Double, String)
fieldUnits fieldName style =
    case style of
        ComparePercent      -> (1,"%")
        ComparePercentDiff  -> (1,"%")
        _ ->
            case isTimeField fieldName of
                -- XXX automatically use ns/us/ms/sec on the scale
                -- get the max and convert it to appropriate unit
                True -> (1000, "ms")
                False -> case isAllocationField fieldName of
                    True -> (1/2^(20 :: Int), "MiB")
                    False -> (1, "")

-------------------------------------------------------------------------------
-- Comparison
-------------------------------------------------------------------------------

comparisonStyleString :: ComparisonStyle -> Maybe String
comparisonStyleString style =
    case style of
        CompareFull         -> Nothing
        CompareAbsoluteDiff -> Just $ "Diff from baseline"
        ComparePercent      -> Just $ "% of baseline"
        ComparePercentDiff  -> Just $ "% diff from baseline"

toDiff :: Num a => (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
toDiff diff x1 x2 =
    case x1 of
        Nothing -> Nothing
        Just v1 -> fmap (diff v1) x2

absoluteDiff :: Num a => a -> a -> a
absoluteDiff v1 v2 = v2 - v1

percentDiff :: (Fractional a, Num a) => a -> a -> a
percentDiff v1 v2 = ((v2 - v1) * 100) / v1

percent :: (Fractional a, Num a) => a -> a -> a
percent v1 v2 = (v2 * 100) / v1

cmpTransformGroupValues :: ComparisonStyle
                         -> [(String, [Maybe Double])]
                         -> [(String, [Maybe Double])]
cmpTransformGroupValues style values =
    let (grpName, grpVals) = head values
        cmp diff =
            (grpName, zipWith diff grpVals grpVals)
          : map (compTransform diff grpVals) (tail values)
    in case style of
            CompareAbsoluteDiff -> cmp $ toDiff absoluteDiff
            ComparePercent      -> cmp $ toDiff percent
            ComparePercentDiff  -> cmp $ toDiff percentDiff
            CompareFull         -> values

    where

    compTransform diff base (name, vals) = (name, zipWith diff base vals)

transformGroupValues :: ComparisonStyle
                     -> [(String, [Maybe Double])]
                     -> [(String, [Maybe Double])]
transformGroupValues style values =
    if length values == 1
    -- workaround for a bug that renders the plot badly when using
    -- a single cluster in the bar chart.
    then values ++ [([], [])]
    else cmpTransformGroupValues style values

cmpTransformGroupNames :: ComparisonStyle
                         -> [(String, [Maybe Double])]
                         -> [(String, [Maybe Double])]
cmpTransformGroupNames style values =
    let (grpName, grpVals) = head values
        cmp modName =
            (baseName grpName, grpVals)
          : map (\(name, vals) -> (modName name, vals)) (tail values)
    in case style of
            CompareAbsoluteDiff -> cmp diffName
            ComparePercent      -> cmp percentName
            ComparePercentDiff  -> cmp percentDiffName
            CompareFull         -> values

    where

    baseName        = (++ "(base)")
    diffName        = (++ "(-base)")
    percentName     = (++ "(% of base)")
    percentDiffName = (++ "(-base %)")

-------------------------------------------------------------------------------
-- Split benchmarks names in group name and benchmark names
-------------------------------------------------------------------------------

-- If the user has specified a classifyBenchmark function use it to split the
-- benchmarks in groups. Each group must have the same benchmarks. Returns the
-- groups and benchmarks in each group.
classifyBench :: Config -> CSV -> IO ([String], [String])
classifyBench Config{..} csvData = do
    let origNames = nub $ map head csvData
        bmTuples = catMaybes $ map classifyBenchmark origNames

    let rep = getRepeated bmTuples
        -- Add mapping to original name for error reporting
        z = zip origNames bmTuples
        zrep = filter (\(_, tup) -> tup `elem` rep) z
    when (zrep /= []) $ do
        error $
            "classifyBenchmark must not map different benchmarks to the same \
            \name under the same group.\n"
            ++ unlines (map show zrep)

    let bmGroups = nub $ map fst bmTuples
        bmNames  = nub $ map snd bmTuples
    mapM_ (checkBenchNameInGrps bmGroups bmTuples) bmNames

    return (bmGroups, bmNames)

    where

    checkBenchNameInGrps bmgroups bmTuples nm =
        let appearsIn = nub $ map fst $ filter (\(_, n) -> nm == n) bmTuples
            xs = bmgroups \\ appearsIn
        in if not (null xs)
           then error $
            "Each benchmark name must appear in all benchmark groups.\n\
            \Benchmark " ++ nm ++ " does not appear in " ++ show xs ++
            "\nPlease check your benchmark classifier (classifyBenchmarks).\n"
           else return ()

-------------------------------------------------------------------------------
-- Get the results for each benchmark group
-------------------------------------------------------------------------------

-- [[Double]] each list is multiple results for each benchmark
transposeLists :: Show a => String -> [[a]] -> Maybe [[Maybe a]]
transposeLists gname xs =
    -- If each benchmark does not have the same number of results then reject
    -- all because the results may not correspond with each other when zipped.
    case nub $ map length xs of
        [0] -> trace ("Warning! " ++ gname ++ ": No results") Nothing
        [n] -> -- all lists are of the same length 'n'
            let ys = map (convertToMaybe n) xs
            in Just $ transpose ys
        [0,n] -> -- some lists are empty others are of the same length 'n'
            -- some packages may have missing benchmarks
            -- fill the empty results with Nothing
            let ys = map (convertToMaybe n) xs
            in Just $ transpose ys
        _ -> trace ("Warning! " ++ gname
                ++ ": has multiple runs with different number of benchmarks\n"
                ++ show xs) Nothing
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
    -- the map results in a list of lists. Where each inner list consists of
    -- multiple values belonging to the same benchmark, each value is from a
    -- different benchmark run. We transpose the values to get all benchmark
    -- values from a single run together.
    transposeLists groupName $ map getBenchmarkValues bmnames

    where

    match name origName =
        case classify origName of
            Nothing -> False
            Just (g, n) -> g == groupName && n == name

    getBenchmarkValues :: String -> [Double]
    getBenchmarkValues bmname =
        -- Filter all the lines from the CSV data that belong to the given
        -- benchmark group and bmname and read the field value at index 1.
        -- Usually the resulting list will have a single element. However, when
        -- we have multiple runs with the same benchgroup name then we may have
        -- multiple elements in the resulting list.
        map (\(_, y) -> read y)
            $ map (\xs -> (xs !! 0, xs !! 1))
            $ filter (match bmname .  head) csvData

-- Returns:
-- 1) a list of benchmark names
-- 2) a list of benchmark groups and for each group a list of values
-- corresponding to the benchmark names.
makeGroups
    :: Config
    -> CSV
    -> ([String], [String])
    -> IO ([String], [(String, [Maybe Double])])
makeGroups Config{..} csvData (bmgroups, bmnames) = do
    -- this produces results for all groups
    -- Each group may have multiple results, in that case the group name is
    -- indexed e.g. streamly(1), streamly(2) etc.
    -- returns [(groupName, Maybe [Maybe Double])]
    let grpResults = concat $ map (grpGetResults bmnames) bmgroups
        filterJust (gName, res) =
            case res of
                Nothing -> do
                    putStrLn $ "Warning! no results found for benchmark group "
                               ++ gName
                    return Nothing
                Just x -> return $ Just (gName, x)
    res0 <- mapM filterJust grpResults
    let res = catMaybes res0
    when (res == []) $ error
        "Each benchmark being plotted must have the same number of results \
        \in the CSV input"

    return (bmnames, res)

    where

    grpGetResults names groupName =
        let res = getResultsForBenchGroup csvData classifyBenchmark
                                          groupName names
        in case res of
            Nothing -> [(groupName, Nothing)]
            Just xs ->
                case length xs of
                    0 -> [(groupName, Nothing)]
                    1 -> map (groupName,) (map Just xs)
                    _ -> zipWith (withIndexes groupName) [(1::Int)..]
                                 (map Just xs)
    withIndexes groupName indx y = (groupName ++ "(" ++ show indx ++ ")", y)

-------------------------------------------------------------------------------
-- sort the benchmark groups
-------------------------------------------------------------------------------

sortGroups :: Config
           -> [(String, [Maybe Double])]
           -> IO [(String, [Maybe Double])]
sortGroups Config{..} groupResults = do
    let origGroups = map fst groupResults
        bmgroups = sortBenchGroups origGroups
        getGroupResult grp =
            case lookup grp groupResults of
                Nothing -> error "Not reached"
                Just res -> (grp, res)

    filterSanity "sortBenchGroups" origGroups bmgroups
    return $ cmpTransformGroupNames comparisonStyle
           $ map getGroupResult bmgroups

-------------------------------------------------------------------------------
-- sort the benchmarks
-------------------------------------------------------------------------------

sortBench
    :: Config
    -> ([String], [(String, [Maybe Double])])
    -> IO ([String], [(String, [Maybe Double])])
sortBench Config{..} (names, groupResults) = do
    -- sort benchmarks
    let extractGroup (Name name) =
            case lookup name groupResults of
                Nothing -> error $ "Benchmark group [" ++ name
                            ++ "] not found. Available groups are: "
                            ++ show (map fst groupResults)

                Just res -> zip names res
        extractGroup (Index n) = zip names $ snd (groupResults !! n)

        bmnames = sortBenchmarks extractGroup
        sortResults results =
            let zresults = zip names results
            in map (getBenchResults zresults) bmnames
        getBenchResults res bench =
            case lookup bench res of
                Nothing -> error "not possible"
                Just r -> r

    filterSanity "sortBenchmarks" names bmnames
    return (bmnames, map (\(g, rs) -> (g, sortResults rs)) groupResults)

-------------------------------------------------------------------------------
-- Parse the CSV file generated by gauge or criterion
-------------------------------------------------------------------------------

eqCaseInsensitive :: String -> String -> Bool
eqCaseInsensitive a b = map toLower a == map toLower b

getFieldIndexInLine :: String -> (Int, [String]) -> Maybe (Int, Int)
getFieldIndexInLine fieldName (lineno, fields) =
    fmap (lineno,) $
        findIndex (\x -> eqCaseInsensitive x fieldName) fields

-- can return multiple indexes or empty list
getFieldIndexUnchecked :: String -> [(Int, [String])] -> [(Int, Int)]
getFieldIndexUnchecked fieldName csvlines =
    nubBy ((==) `on` snd) $
        catMaybes $ map (getFieldIndexInLine fieldName) csvlines

getFieldIndexChecked :: String -> [(Int, [String])] -> Int
getFieldIndexChecked fieldName csvlines =
    let idxs = getFieldIndexUnchecked fieldName csvlines
    in case idxs of
        [(_, x)] -> x
        [] -> error $ "Field name [" ++ fieldName
                      ++ "] does not occur in any line of "
                      ++ "the CSV input. Is the header line missing?"
        _ -> error $ "Field [" ++ fieldName
                     ++ "] found at different indexes [(line, column): " ++ show idxs
                     ++ "] in different lines of CSV input"

-- Find the indexes of benchmark name, iterations and the requested field to be
-- plotted, also remove the header lines from the csv input and return the
-- cleaned up lines.
-- Returns
-- ( index of the name field
-- , index of the iter field
-- , index of the field being plotted
-- , CSV lines without the header lines
-- )
--
findIndexes :: String
           -> String
           -> String
           -> [(Int, [String])]
           -> (Int, Maybe Int, Int, [(Int, [String])])
findIndexes benchmarkNameField iterationField fieldName csvlines =
    let fieldIdx = getFieldIndexChecked fieldName csvlines
        iterIdxs = getFieldIndexUnchecked iterationField csvlines
        nameIdxs = getFieldIndexUnchecked benchmarkNameField csvlines
        -- Remove the header lines
        csvlines' = filter (\(_, xs) ->
            (not $ (xs !! fieldIdx) `eqCaseInsensitive` fieldName)) csvlines
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

--  Keep the CSV columns corresponding to the provided indexes and remove the
--  unwanted columns.
extractIndexes :: (Int, Maybe Int, Int, [(Int, [String])]) -> [(Int, [String])]
extractIndexes (nameIdx, iterIdx, fieldIdx, csvlines) =
    let zipList = zipWith (\(l, xs) (_, ys) -> (l, xs ++ ys))
    in map (indexWithError nameIdx "name") csvlines
        `zipList`
            case iterIdx of
                Nothing  -> repeat (1, ["1"])
                Just idx -> map (indexWithError idx "iter") csvlines
        `zipList` map (indexWithError fieldIdx "requested") csvlines
    where

    indexWithError :: Int -> String -> (Int, [String]) -> (Int, [String])
    indexWithError idx colName (l, xs) =
        if (length xs < idx)
        then error $ "Line " ++ show l ++ " " ++ show colName
                     ++ " column at index " ++ show idx ++ " not present"
        else (l, [xs !! idx])

parseCSVLines :: String -> Double -> CSV -> CSV
parseCSVLines fieldName multiplier csvlines =
    let readWithError :: Read a => String -> String -> Int -> (Int, [String]) -> a
        readWithError fname typ idx (lno, xs) =
            case readMaybe (xs !! idx) of
                Nothing -> error $ "Cannot read " ++ show fname
                    ++ " field as a " ++ typ ++ " type at line number "
                    ++ show lno
                Just n -> n

        -- xs is [(lineno, [iter, field])]
        foldToMean xs =
            let iters  = map (readWithError "iter" "Double" 0) xs :: [Double]
                values = map (readWithError "requested" "Double" 1) xs :: [Double]
                mean = sum values / sum iters
            in show $ mean * multiplier

        foldToMax xs =
            let values = map (readWithError "requested" "Double" 1) xs :: [Double]
            in show $ (maximum values) * multiplier

     in   -- Add line numbers for error reporting
          zip [1..] csvlines
          -- cleanup blank rows
        & filter (\(_,xs) -> xs /= [""])
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
          -- the next iteration of the previous benchmark. This can
          -- happen if there is only one benchmark in two separate
          -- runs.
        & groupBy (\l1@(_, x:_) l2@(_, y:_) -> x == y
            && ((readWithError "iter" "Int" 1 l2 :: Int) >
                (readWithError "iter" "Int" 1 l1 :: Int)))
          -- reduce grouped iterations to a single row with the mean.
          -- xs below is a list of tuples [(lineno, [name, iter, field]]
          -- we send [(lineno, [iter, field])] to foldToMean.
        & map (\xs -> [ head $ map (head . snd) xs
                      , if isMaxField fieldName
                        then foldToMax  $ map (\(l, ys) -> (l, tail ys)) xs
                        else foldToMean $ map (\(l, ys) -> (l, tail ys)) xs
                      ]
              )

getFieldRange :: String -> Config -> Maybe (Double, Double)
getFieldRange fieldName Config{..} =
    let res = find (\(x, _, _) -> x == fieldName) fieldRanges
    in case res of
        -- XXX determine the range from the values
        Nothing -> Nothing
        Just (_, x, y) -> Just (x, y)

getFieldGranularity :: String -> Config -> Maybe Granularity
getFieldGranularity fieldName Config{..} =
    fmap snd $ find (\x -> fst x == fieldName) fieldGranularities

prepareOutputFile :: FilePath -> FilePath -> String -> String -> IO FilePath
prepareOutputFile dir file atitle field = do
    let path = dir </> (file ++ "-" ++ field ++ ".svg")
    putStrLn $ "Creating chart "
        ++ "[" ++ atitle ++ "]"
        ++ " at "
        ++ show path
    return path

prepareToReport :: FilePath -> Config -> IO (CSV, [String])
prepareToReport inputFile Config{..} = do
    case outputDir of
        Nothing -> return ()
        Just dir -> createDirectoryIfMissing True dir
    -- We assume the dataset is not big and therefore take liberties to process
    -- in a non-streaming fashion.
    csvData <- parseCSVFromFile inputFile
    case csvData of
        Left e -> error $ show e
        Right csvlines -> do
            when (null csvlines) $ error $ "The input file ["
                ++ show inputFile ++ "] is empty"
            let allFields = head csvlines
                fields = sortBenchFields allFields
            filterSanity "sortBenchFields" allFields fields
            return (csvlines, fields)

-- XXX display GHC version as well
-- XXX display the OS/arch
-- XXX display compiler/RTS options as well e.g. -threaded and -N
-- This data should be in the measurement data
-- TODO Specify the units of the field being plotted

-- returns:
--  outputFile
--  Units
--  benchmark names
--  benchmark groups with benchmark values
rawReportComparing
    :: CSV
    -> String
    -> Config
    -> IO (String, Double, [String], [(String, [Maybe Double])])
rawReportComparing csvlines fieldName cfg@Config{..} = do
    let _fieldRange = getFieldRange fieldName cfg
        -- XXX determine it based on the field range
        (multiplier, units) = fieldUnits fieldName comparisonStyle

    -- XXX clip the values based on the range
    let csv = parseCSVLines fieldName multiplier csvlines
    r <- classifyBench cfg csv
    (bmnames, grpres) <- makeGroups cfg csv r
    grpres' <- sortGroups cfg grpres
    (names, groups) <- sortBench cfg (bmnames, grpres')
    return (units, multiplier, names, groups)

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

inParens :: String -> String
inParens str = "(" ++ str ++ ")"

makeTitle :: String -> Config -> String
makeTitle field Config{..} =
       fromMaybe "" title
    ++ inParens field
    ++ case comparisonStyleString comparisonStyle of
            Nothing -> ""
            Just str -> inParens str
