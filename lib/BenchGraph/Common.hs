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
    ( Presentation(..)
    , GroupStyle(..)
    , FieldTick (..)
    , SortColumn (..)
    , RelativeUnit (..)
    , Config(..)
    , defaultConfig

    , getFieldRange
    , getFieldTick

    , GroupMatrix(..)
    , prepareGroupMatrices

    , ReportColumn(..)
    , RawReport(..)
    , getCmpReportUnit
    , makeTitle
    , prepareToReport
    , reportComparingGroups
    , reportPerGroup
    ) where

import Control.Applicative (ZipList(..))
import Control.Arrow (second)
import Control.Exception (assert)
import Control.Monad (when, unless)
import Data.Char (toLower)
import Data.Function ((&), on)
import Data.List (transpose, groupBy, (\\), find, foldl1', sortBy, elemIndex)
import Data.List.Split (linesBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import Debug.Trace (trace)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.CSV (CSV, parseCSVFromFile)
import Text.Read (readMaybe)

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

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

-- | How to show the results for multiple benchmark groups presented in columns
-- or bar chart clusters.
--
-- @since 0.2.0
data GroupStyle =
      Absolute       -- ^ Show absolute field values for all groups
    | Diff        -- ^ Show baseline group values as usual and values for
                     -- the subsequent groups as differences from the baseline
    | Percent     -- ^ Show baseline group values as 100% and values for
                     -- subsequent groups as a percentage of the baseline
    | PercentDiff -- ^ Show baseline group values as usual and values for
                     -- subsequent groups as precentage difference from the
                     -- baseline
    deriving (Eq, Show)

-- | How to present the reports or graphs. Each report presents a number of
-- benchmarks as rows, it may have, (1) a single column presenting the values
-- for a single field, (2) multiple columns presenting values for different
-- fields, or (3) multiple columns presenting values of the same field for
-- different groups.
--
-- @since 0.2.0
data Presentation =
      Solo              -- ^ Reports are generated for each group and for
                        -- each field selected by the configuration. Each
                        -- report presents benchmarks in a single group with a
                        -- single column presenting a single field.  If there
                        -- are @m@ fields and @n@ groups selected by the
                        -- configuration then a total of @m x n@ reports are
                        -- generated.  Output files are named using
                        -- @-groupname-fieldname@ as suffix.
    | Groups GroupStyle -- ^ One report is generated for each field selected by
                        -- the configuration. Each report presents a field
                        -- with all the groups selected by the configuration as
                        -- columns or clusters. Output files are named using
                        -- @-fieldname@ as suffix.
    | Fields            -- ^ One report is generated for each group selected by
                        -- the configuration. Each report presents a group
                        -- with all the fields selected by the configuration as
                        -- columns or clusters. Output files are named using
                        -- @-groupname@ as suffix.
    deriving (Eq, Show)

-- | FieldTick is used only in visual charts to generate the major ticks on
-- the y-axis. You can specify either the size of a tick ('TickSize') or the
-- total number of ticks ('TickCount').
--
-- @since 0.2.0
data FieldTick =
      TickSize Int  -- ^ Size of a tick, the unit is microseconds for time
                     -- fields, and bytes for space fields.
    | TickCount Int -- ^ Total number of ticks in the range spread

-- | When sorting and filtering the benchmarks using 'selectBenchmarks' we can
-- choose a column as a sort criterion.  'selectBenchmarks' is provided with
-- the data for the corresponding column which can be used for sorting the
-- benchmarks. The column could be a group or a field depending on the
-- 'Presentation'.
--
-- @since 0.2.0
data SortColumn =
      ColumnIndex Int -- ^ Specify the index of the sort column. Note that only
        -- fields' or groups' columns are considered. In a textual report
        -- presentation the first column shown is benchmark names and the rest
        -- of the columns are fields or group columns. Therefore, an index 0
        -- actually addresses the second column shown by the textual report.
    | ColumnName (Either String (String, Int)) -- ^ Specify the column using
        -- the name of the group or the field it represents. A field name can
        -- always be specified using just the name with 'Left' constructor. A
        -- group can be specified with just the name when there is a single
        -- benchmark run in the file. When there are multiple runs, a group
        -- needs to specify a @runId@ as well using the 'Right' constructor.

-- | Configuration governing generation of chart. See 'defaultConfig' for the
-- default values of these fields.
--
-- @since 0.2.0
data Config = Config
    {
    -- | The directory where the output graph or report file should be placed.
      outputDir   :: Maybe FilePath

    -- | Report title, more information like the plotted field name or
    -- the presentation style may be added to it.
    , title  :: Maybe String

    -- | How to determine the layout of the report or the chart.
    , presentation :: Presentation

    ---------------------------------------------------------------------------
    -- Fields (Columns)
    ---------------------------------------------------------------------------

    -- | Filter and reorder the benchmarking fields. It is invoked with a list
    -- of all available benchmarking fields. Only those fields present in the
    -- output of this function are plotted and in that order.
    , selectFields :: [String] -> [String]

    -- | The values in the tuple are @(fieldName, RangeMin, RangeMax)@.
    -- Specify the min and max range of benchmarking fields. If the field
    -- value is outside the range it is clipped to the range limit.
    -- For time fields, the range values are in microseconds, and for space
    -- fields they are in bytes. The minimum of the range is used to determine
    -- the unit for the field.
    , fieldRanges :: [(String, Double, Double)]

    -- | The values in the tuple are @(fieldName, tick)@.  Specify the
    -- tick size of the fields to be used for the graphical reports.
    , fieldTicks :: [(String, FieldTick)]

    ---------------------------------------------------------------------------
    -- Groups (Row Grouping)
    ---------------------------------------------------------------------------

    -- | Filter, group and translate benchmark names. This function is invoked
    -- once for all benchmark names found in the results. It produces a tuple
    -- @(groupname, benchname)@, where @groupname@ is the name of the group the
    -- benchmark should be placed in, and @benchname@ is the translated
    -- benchmark name to be used in the report.  If it returns 'Nothing' for a
    -- benchmark, that benchmark is omitted from the results.
    , classifyBenchmark :: String -> Maybe (String, String)

    -- | Filter and reorder the benchmark group names. A benchmark group may be
    -- assigned using 'classifyBenchmark'; when not assigned, all benchmarks
    -- are placed in the @default@ group. The input to this function is alist
    -- of tuples with benchmark group names and the @runId@s.  The output
    -- produced by this function is a filtered and reordered subset of the
    -- input.  Only those benchmark groups present in the output are rendered
    -- and are presented in that order.
    , selectGroups :: [(String, Int)] -> [(String, Int)]

    ---------------------------------------------------------------------------
    -- Benchmarks (Rows)
    ---------------------------------------------------------------------------

    -- | Filter and reorder benchmarks. 'selectBenchmarks' is provided with a
    -- function which is invoked with a sorting column name or index, the
    -- function produces the benchmark names and corresponding values for that
    -- column which can be used as a sorting criterion. The output of
    -- 'selectBenchmarks' is a list of benchmarks in the order in which they
    -- are to be rendered.
    , selectBenchmarks :: (SortColumn -> [(String, Double)]) -> [String]
    }

-- | Default configuration. Use this as the base configuration and modify the
-- required fields. The defaults are:
--
-- @
--  outputDir         = Nothing
--  title             = Nothing
--  presentation      = Groups Absolute
--  selectFields      = filter (flip elem ["time", "mean", "maxrss"] . map toLower)
--  fieldRanges       = []
--  fieldTicks        = []
--  classifyBenchmark = Just . ("default",)
--  selectGroups      = id
--  selectBenchmarks  = \f -> map fst (f (ColumnIndex 0))
-- @
--
-- @since 0.2.0
defaultConfig :: Config
defaultConfig = Config
    { title             = Nothing
    , outputDir         = Nothing
    , presentation      = Groups Absolute
    , selectFields      = filter (flip elem ["time", "mean", "maxrss"] . map toLower)
    , fieldRanges       = []
    , fieldTicks        = []
    , classifyBenchmark = Just . ("default",)
    , selectGroups      = id
    , selectBenchmarks  = \f -> map fst (f (ColumnIndex 0))
    }

-------------------------------------------------------------------------------
-- Benchmarking field types
-------------------------------------------------------------------------------

timeFields :: [String]
timeFields = map (map toLower)
    [ "time"
    , "mean"
    , "cpuTime"
    , "utime"
    , "stime"
    , "mutatorWallSeconds"
    , "mutatorCpuSeconds"
    , "gcWallSeconds"
    , "gcCpuSeconds"
    ]

allocFields :: [String]
allocFields = map (map toLower) ["allocated", "bytesCopied", "maxrss"]

isTimeField :: String -> Bool
isTimeField fieldName = map toLower fieldName `elem` timeFields

isAllocationField :: String -> Bool
isAllocationField fieldName = map toLower fieldName `elem` allocFields

-- By default the fields are considered "scaled" fields that is
-- they scale by iterations. However in case of maxrss field it is
-- a max value across the experiment and does not scale by
-- iterations, in this case we just need to take a mean or max
-- without scaling.
isMaxField :: String -> Bool
isMaxField fieldName = map toLower fieldName == "maxrss"

-------------------------------------------------------------------------------
-- Units
-------------------------------------------------------------------------------

-- | Describe a relative unit i.e. a unit in terms of another unit. A relative
-- unit has a label and a ratio which when multiplied with the unit gives us
-- the other unit. For example, if the known time unit is seconds, we can
-- describe a millisecond as @Unit "ms" (1/1000)@.
data RelativeUnit = RelativeUnit String Double deriving Show

getTimeUnit :: Double -> RelativeUnit
getTimeUnit k
    | k < 0      = getTimeUnit (-k)
    | k >= 1     = RelativeUnit "s" 1
    | k >= 1e-3  = RelativeUnit "ms" 1e-3
    | k >= 1e-6  = RelativeUnit "us" 1e-6
    | otherwise  = RelativeUnit "ns" 1e-9

getSpaceUnit :: Double -> RelativeUnit
getSpaceUnit k
    | k < 0             = getTimeUnit (-k)
    | k >= 2^(30 ::Int) = RelativeUnit "GiB" (2^(30 :: Int))
    | k >= 2^(20 ::Int) = RelativeUnit "MiB" (2^(20 :: Int))
    | k >= 2^(10 ::Int) = RelativeUnit "KiB" (2^(10 :: Int))
    | otherwise         = RelativeUnit "Bytes" 1

getUnitByFieldName :: String -> Double -> RelativeUnit
getUnitByFieldName fieldName fieldMin =
    case isTimeField fieldName of
        True -> getTimeUnit fieldMin
        False -> case isAllocationField fieldName of
            True -> getSpaceUnit fieldMin
            False -> RelativeUnit "" 1

-- returns (multiplier, units)
fieldUnits :: String -> Double -> GroupStyle -> RelativeUnit
fieldUnits fieldName fieldMin style =
    case style of
        Percent      -> RelativeUnit "%" 1
        PercentDiff  -> RelativeUnit "%" 1
        _ -> getUnitByFieldName fieldName fieldMin

-------------------------------------------------------------------------------
-- Comparison
-------------------------------------------------------------------------------

comparisonStyleString :: GroupStyle -> Maybe String
comparisonStyleString style =
    case style of
        Absolute       -> Nothing
        Percent     -> Nothing
        Diff        -> Just $ "Diff from baseline"
        PercentDiff -> Just $ "Diff from baseline"

absoluteDiff :: Num a => a -> a -> a
absoluteDiff v1 v2 = v2 - v1

percentDiff :: (Fractional a, Num a) => a -> a -> a
percentDiff v1 v2 = ((v2 - v1) * 100) / v1

percent :: (Fractional a, Num a) => a -> a -> a
percent v1 v2 = (v2 * 100) / v1

cmpTransformColumns :: GroupStyle
                    -> [[(String, Double)]]
                    -> [[(String, Double)]]
cmpTransformColumns style columns =
    let cmpWith diff =
            let firstCol = head columns
                colTransform col =
                    let mkDiff (n1, v1) (n2,v2) =
                            assert (n1 == n2) (n2, diff v1 v2)
                    in zipWith mkDiff firstCol col
            in colTransform firstCol : map colTransform (tail columns)
    in case style of
            Diff        -> head columns : drop 1 (cmpWith absoluteDiff)
            Percent     -> cmpWith percent
            PercentDiff -> map (\(n,_) -> (n,100)) (head columns)
                            : drop 1 (cmpWith percentDiff)
            Absolute       -> columns

transformColumnNames :: GroupStyle -> [ReportColumn] -> [ReportColumn]
transformColumnNames style columns =
    let cmpWith bXlate xlate =
            let firstCol = head columns
                colTransform xl col = col { colName = xl (colName col) }
            in    colTransform bXlate firstCol
                : map (colTransform xlate) (tail columns)
    in case style of
            Diff        -> cmpWith baseName diffName
            PercentDiff -> cmpWith baseName diffName
            Percent     -> cmpWith id id
            Absolute       -> columns

    where

    baseName        = (++ "(base)")
    diffName        = (++ "(-base)")

-- Stored by rows
-- XXX store as a rowMap, each row having a colMap?
data BenchmarkMatrix = BenchmarkMatrix
    { colNames :: [String]
    , rowValues :: [(String, [Double])]
    } deriving Show

-- Represents the data for a single benchmark run
data GroupMatrix = GroupMatrix
    { groupIndex :: Int
    , groupName   :: String
    , groupBenches :: [(String, String)] -- (newname, origname)
    , groupMatrix :: BenchmarkMatrix
    } deriving Show

-- Each run may be split into multiple groups of benchmarks.  Benchmarks can be
-- renamed by the classifier. Sanity checks:
-- Two original benchmarks cannot map to the same target benchmark
--
-- When using a comparative style report, after filtering and sorting:
-- Same original benchmark cannot belong to multiple groups
-- All groups must have exactly the same benchmark names
splitGroup :: (String -> Maybe (String, String))
           -> (Int, BenchmarkMatrix)
           -> [GroupMatrix]
splitGroup classify (serial, matrix@BenchmarkMatrix{..}) =
      mapMaybe (\x -> fmap (,x) $ classify x) (map fst rowValues)
    & sortBy (comparing (fst . fst))
    & groupBy ((==) `on` (fst . fst))
    & map (foldr foldGroup ("",[]))
    & map sanityCheckGroup
    & map (\(name, benches) ->
        GroupMatrix
        { groupIndex  = serial
        , groupName    = name
        , groupBenches = benches
        , groupMatrix  = matrix
        })

    where

    foldGroup ((grp, bench), srcBench) (_, tuples) =
        (grp, (bench, srcBench) : tuples)

    sanityCheckGroup orig@(grp, tuples) =
        let duplicated =
                  sortBy (comparing fst) tuples
                & groupBy ((==) `on` fst)
                & filter ((>1) . length)
        in if not $ null duplicated
           then
            let msg = unlines (map show duplicated)
            in error $ "Two benchmarks must not map to the same target \
               \benchmark. Please check your 'classifyBenchmark' operation. \
               \In group " ++ show grp ++ ", the following target benchmarks \
               \are mapped to more than one source benchmarks:\n" ++ msg
            else orig

-------------------------------------------------------------------------------
-- sort the benchmark groups
-------------------------------------------------------------------------------

findGroup :: [GroupMatrix] -> (String, Int) -> Maybe GroupMatrix
findGroup matrices (name, i) =
    find (\x -> groupName x == name && groupIndex x == i) matrices

sortGroups :: Config -> [GroupMatrix] -> IO [GroupMatrix]
sortGroups Config{..} matrices = do
    let origGroups = map (\x -> (groupName x, groupIndex x)) matrices
        newGroups = selectGroups origGroups

    filterSanity "selectGroups" origGroups newGroups
    return $ mapMaybe (findGroup matrices) newGroups

-------------------------------------------------------------------------------
-- sort the benchmarks
-------------------------------------------------------------------------------

extractColumn :: String -> GroupMatrix -> [(String, Double)]
extractColumn field GroupMatrix{..} =
    let idx = elemIndex field (colNames groupMatrix)
        vals = case idx of
            Just i -> map (!! i) (map snd (rowValues groupMatrix))
            Nothing -> error $ "Field [" ++ field
                ++ "] does not exist in group ["
                ++ groupName ++ "] and run id [" ++ show groupIndex ++ "]"
    in zip (map fst groupBenches) vals

benchmarkCompareSanity :: [String] -> GroupMatrix -> [String]
benchmarkCompareSanity benchmarks GroupMatrix{..} = do
    let benches = map fst groupBenches
    let absent = benchmarks \\ benches
    let msg =
            "selectBenchmarks: Group [" ++ groupName ++ "] run id ["
            ++ show groupIndex
            ++ "] does not contain the following selected benchmarks; \
            \ignoring them: " ++ show absent
            ++ "\nAvailable benchmarks in this group are: "
            ++ show benches

    if (null absent)
    then benchmarks
    else trace msg (benchmarks \\ absent)

selectBenchmarksByField :: Config
                        -> [GroupMatrix]
                        -> [[(String, Double)]]
                        -> [String]
selectBenchmarksByField Config{..} matrices columns =
    let bmnames = selectBenchmarks extractGroup
    in if (null bmnames)
       then error $ "selectBenchmarks must select at least one benchmark"
       else
           -- XXX instead of matrices we can just use columns here
           let xs = foldl benchmarkCompareSanity bmnames matrices
           in if (null xs)
              then error $ "selectBenchmarks: none of the selected benchmarks "
                    ++ show bmnames
                    ++ " is common to all the benchmark groups "
                    ++ show grpNames
              else xs

    where

    grpNames =
        let getName x = (groupName x, groupIndex x)
        in map getName matrices

    -- columns are benchmark groups in this case
    extractGroup (ColumnName (Left name)) =
            let len = length columns
            in if len <= 1
               then extractGroup $ ColumnName (Right (name, 0))
               else error $ "selectBenchmarks: there are " ++ show len
                    ++ " runs in the input data, please specify the run \
                    \index [0-" ++ show (len - 1)
                    ++ "] along with the group name."
    extractGroup (ColumnName (Right (name, runId))) =
            extractColumnByGroupName name runId
    extractGroup (ColumnIndex n) = extractColumnByGroupIndex n

    -- The benchmark field is constant.  Extract all benchmark values for the
    -- given field and for the given group.
    findColumnIndex mxs (name, runId) =
        let foldFunc res@(idx, found) grp =
                case found of
                    False ->
                        if groupName grp == name && groupIndex grp == runId
                        then (idx, True)
                        else (idx + 1, found)
                    True -> res
        in foldl foldFunc (0, False) mxs

    extractColumnByGroupName name runId =
            case findColumnIndex matrices (name, runId) of
                (_, False) -> error $ "Benchmark group name [" ++ name
                            ++ "] and index [" ++ show runId
                            ++ "] not found. Available groups are: "
                            ++ show grpNames
                (i, True) -> extractGroup (ColumnIndex i)

    extractColumnByGroupIndex idx =
        let len = length columns
        in if idx >= len
           then error $ "Column index must be in the range [0-"
                ++ show (len - 1) ++ "]"
           else columns !! idx

selectBenchmarksByGroup :: Config -> GroupMatrix -> [String]
selectBenchmarksByGroup Config{..} grp@GroupMatrix{..} =
    -- XXX this is common to ByField and ByGroup
    let bmnames = selectBenchmarks extractField
    in if (null bmnames)
       then error $ "selectBenchmarks must select at least one benchmark"
       else bmnames

    where

    -- columns are benchmark fields in this case
    extractField (ColumnName (Left name)) = extractColumnByFieldName name
    extractField (ColumnName (Right (name, _))) =
        -- XXX runId does not make sense for fields
        extractColumnByFieldName name
    extractField (ColumnIndex n) = extractColumnByFieldIndex n

    -- The benchmark field is constant.  Extract all benchmark values for the
    -- given field and for the given group.
    extractColumnByFieldName name =
        let fields = colNames groupMatrix
        in case elem name fields of
            False -> error $ "Benchmark field name [" ++ name
                        ++ "] not found in group ["
                        ++ groupName ++ "]. Available fields are: "
                        ++ show fields
            True -> extractColumn name grp

    extractColumnByFieldIndex idx =
        let fields = colNames groupMatrix
            len = length fields
        in if idx >= len
           then error $ "Column index must be in the range [0-"
                ++ show (len - 1) ++ "]"
           else extractColumn (fields !! idx) grp

type NumberedLines = [(Int, [String])]

sanityCheckCSV :: CSV -> NumberedLines
sanityCheckCSV csvlines | null csvlines = error $ "The input file is empty"
sanityCheckCSV csvlines =
    let headRow = head csvlines
        rowLen = length headRow
    in  if not $ "name" `elem` map (map toLower) headRow
        then error "No 'Name' column found in the CSV header line"
        else
          -- Add line numbers for error reporting
          zip [1..] csvlines
          -- cleanup blank rows
        & filter (\(_,xs) -> xs /= [""])

          -- make sure all lines are of the same size, So that we can transpose
          -- back and forth without losing information.
        & map (\x@(i,xs) ->
               if length xs == rowLen
               then x
               else error $ "Line number " ++ show i
                        ++ " in the input file is not of the same length as\
                            \ the header line"
              )

-- Only keep those fields that are passed to this function
-- Make sure that "name" and "iters" are the first and second columns
filterFields :: [String] -> NumberedLines -> NumberedLines
filterFields fieldNames csvlines =
      unzip csvlines
    & second transpose
    & second (filter required)
    & second reorderNameIter
    & second transpose
    & uncurry zip

    where

    required [] = True
    required (x:_) =
           map toLower x == "name"
        || map toLower x == "iters"
        || x `elem` fieldNames

    notNameIters [] = True
    notNameIters (x:_) =
           map toLower x /= "name"
        && map toLower x /= "iters"

    reorderNameIter xs =
        let findField x = find (\(y:_) -> map toLower y == x)
        in    fromMaybe [] (findField "iters" xs)
           :  fromMaybe [] (findField "name" xs)
           : (filter notNameIters xs)

-- Split the file into different runs
-- return the header fields and list of runs without the header
splitRuns :: NumberedLines -> ([String], [NumberedLines])
splitRuns csvlines =
    let header = snd $ head csvlines
        ls = linesBy (\x -> snd x == header) (tail csvlines)
    in (header, ls)

readWithError :: Read a => Int -> String -> (String, String) -> a
readWithError lno typ (fname, fval) =
    case readMaybe fval of
        Nothing -> error $ "Cannot read " ++ show fname
            ++ " field as " ++ typ ++ " type at line number "
            ++ show lno
        Just n -> n

-- An iteration field indicates that consecutive rows with the same benchmark
-- name have results from different iterations of the same benchmark and the
-- measurement fields have to be scaled per iteration based on the number of
-- iterations in the iteration count field.
--
-- If the first column is iteration then fold all iterations and remove the
-- iteration column.
foldIterations :: [String] -> NumberedLines -> BenchmarkMatrix
foldIterations header csvlines =
    let (header', csvlines') = addIterField header csvlines
        tuples =
            map (parseNumericFields header') csvlines'
            -- we now have a list of triples [(iter, name, (fieldName, [Double])]
          & groupBy successiveIters
          & map (foldl1' addIters)
          & map getMeanOrMax
    in BenchmarkMatrix
        { colNames = drop 2 header'
        , rowValues = tuples
        }

    where

    addIterField hdr nlines =
        if map toLower (head hdr) /= "iters"
        then ("iters" : hdr, map (\(lno,vals) -> (lno, "1" : vals)) nlines)
        else (hdr, nlines)

    -- The first column is iters and the second is the name
    -- We zip the header for error reporting
    parseNumericFields hdr (lno, vals) = parseNumericTuples lno $ zip hdr vals

    parseNumericTuples lno (iter:(_,name):xs) =
          (readWithError lno "Int" iter :: Int
          , name
          , map (\x@(n,_) -> (n, readWithError lno "Double" x)) xs
                :: [(String, Double)]
          )
    parseNumericTuples _ _ = error "iters and name fields are needed"

    successiveIters (i1,name1,_) (i2,name2,_) = name2 == name1 && i2 > i1

    addField :: (Num a, Ord a) => (String, a) -> (String, a) -> (String, a)
    addField (name1, val1) (name2, val2) =
        assert (name1 == name2) $
            if isMaxField name1
            then (name1, max val1 val2)
            else (name1, val1 + val2)

    addIters (siter,sname,svals) (iter,name,vals) =
        assert (sname == name) $
            (siter + iter, name, zipWith addField svals vals)

    getMeanOrMax (iter,name,vals) =
        let meanOrMax (fname, val) =
                if isMaxField fname
                then val
                else val / fromIntegral iter
        in (name,map meanOrMax vals)

getFieldRange :: String -> Config -> Maybe (Double, Double)
getFieldRange fieldName Config{..} =
    let res = find (\(x, _, _) -> x == fieldName) fieldRanges
    in case res of
        Nothing -> Nothing
        Just (_, x, y) -> Just (x, y)

getFieldTick :: String -> Config -> Maybe FieldTick
getFieldTick fieldName Config{..} =
    fmap snd $ find (\x -> fst x == fieldName) fieldTicks

prepareOutputFile :: FilePath -> String -> FilePath -> String -> IO FilePath
prepareOutputFile dir ext file field = do
    let path = dir </> (file ++ "-" ++ field ++ ext)
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
                fields = selectFields allFields
            filterSanity "selectFields" allFields fields
            let filt x = notElem (map toLower x) ["name", "iters"]
            return (csvlines, filter filt fields)

-- Keep only those benchmarks that belong to the group.
filterGroupBenchmarks :: [GroupMatrix] -> IO [GroupMatrix]
filterGroupBenchmarks matrices = return $ map filterMatrix matrices
    where
    filterMatrix matrix =
        -- XXX make sure there are no duplicates
        let m = groupMatrix matrix
            -- vals :: [(String, Double)]
            vals = map (\(new,old) ->
                (new, fromMaybe (error "bug") $ lookup old (rowValues m)))
                (groupBenches matrix)
        in matrix {groupMatrix = m {rowValues = vals}}

prepareGroupMatrices :: Config -> CSV -> [String] -> IO (Int, [GroupMatrix])
prepareGroupMatrices cfg@Config{..} csvlines fields =
    let (hdr, runs) =
              sanityCheckCSV csvlines
            & filterFields fields
            & splitRuns
    in map (foldIterations hdr) runs
        & zip [0..]
        & map (splitGroup classifyBenchmark)
        & concat
        & sortGroups cfg
        >>= filterGroupBenchmarks
        >>= return . (length runs,)

-- XXX display GHC version as well
-- XXX display the OS/arch
-- XXX display compiler/RTS options as well e.g. -threaded and -N
-- This data should be in the measurement data

data ReportColumn = ReportColumn
    { colName   :: String
    , colUnit   :: RelativeUnit
    , colValues :: [Double]
    } deriving Show

data RawReport = RawReport
    { reportOutputFile :: Maybe FilePath
    , reportIdentifier :: String
    , reportRowIds     :: [String]
    , reportColumns    :: [ReportColumn]
    } deriving Show

getFieldMin :: Config -> Double -> String -> Double
getFieldMin cfg minval fieldName =
    case getFieldRange fieldName cfg of
        Nothing -> minval
        Just (minr, _) -> minr

prepareRawReportCmp :: Config
                    -> GroupStyle
                    -> Maybe FilePath
                    -> Int
                    -> String
                    -> [GroupMatrix]
                    -> RawReport
prepareRawReportCmp cfg style outfile runs field matrices =
    -- XXX Determine the unit based the whole range of values across all columns
    let sortValues :: [String] -> [(String, Double)] -> [Double]
        sortValues bmarks vals =
            map (\name -> fromMaybe (error "bug") (lookup name vals)) bmarks

        unsortedCols = map (extractColumn field) matrices
        transformedCols = cmpTransformColumns style unsortedCols
        benchmarks = selectBenchmarksByField cfg matrices transformedCols
        sortedCols = map (sortValues benchmarks) transformedCols

        fieldMinVal =
            if style /= Diff
            then minimum $ concat sortedCols
            -- if we use diff vlaues here then the units will change to
            -- potentially very small. If we use unsortedCols we may
            -- potentially take columns which are filtered out into account. We
            -- use the latter.
            else minimum $ concat $ (map (map snd)) unsortedCols

        unit@(RelativeUnit _ multiplier) =
            fieldUnits field (getFieldMin cfg fieldMinVal field) style

        mkColValues :: [[Double]]
        mkColValues = map (map (/multiplier)) sortedCols

        mkColNames :: [String]
        mkColNames =
                let withSuffix x =
                        groupName x ++
                            if runs > 1
                            then "(" ++ show (groupIndex x) ++ ")"
                            else ""
                in map withSuffix matrices

        mkColUnits :: [RelativeUnit]
        mkColUnits = replicate (length matrices) unit

        columns = getZipList $ ReportColumn
                    <$> ZipList mkColNames
                    <*> ZipList mkColUnits
                    <*> ZipList mkColValues

    in RawReport
            { reportOutputFile = outfile
            , reportIdentifier = field
            , reportRowIds     = benchmarks
            , reportColumns    = transformColumnNames style columns
            }

showStatusMessage
    :: Show a
    => Config -> String -> Maybe RelativeUnit -> Maybe a -> IO ()
showStatusMessage cfg field unit outfile =
    let atitle = makeTitle field unit cfg
    in case outfile of
        Just path ->
            putStrLn $ "Creating chart "
                ++ "[" ++ atitle ++ "]"
                ++ " at "
                ++ show path
        Nothing -> return ()

getCmpReportUnit :: RawReport -> RelativeUnit
getCmpReportUnit RawReport{..} =
    colUnit $ head reportColumns

reportComparingGroups
    :: GroupStyle
    -> FilePath
    -> Maybe FilePath
    -> String
    -> Int
    -> Config
    -> (RawReport -> Config -> IO ())
    -> [GroupMatrix]
    -> String
    -> IO ()
reportComparingGroups style dir outputFile ext runs cfg@Config{..} mkReport matrices field = do
    outfile <- case outputFile of
        Just file -> fmap Just $ prepareOutputFile dir ext file field
        Nothing -> return Nothing

    let rawReport = prepareRawReportCmp cfg style outfile runs field matrices
        unit = getCmpReportUnit rawReport
    showStatusMessage cfg field (Just unit) outfile
    mkReport rawReport cfg

prepareRawReport :: Config
                 -> Maybe FilePath
                 -> GroupMatrix
                 -> RawReport
prepareRawReport cfg outfile group =
    let mkColNames :: [String]
        mkColNames = colNames $ groupMatrix group

        benchmarks = selectBenchmarksByGroup cfg group

        getBenchValues name =
              fromMaybe (error "bug") $
                lookup name (rowValues $ groupMatrix group)

        sortedValues = map getBenchValues benchmarks
        minValues = map minimum (transpose sortedValues)

        mkColUnits :: [RelativeUnit]
        mkColUnits = map (\(x, v) -> getUnitByFieldName x (getFieldMin cfg v x))
                         (zip mkColNames minValues)

        mkColValues :: [[Double]]
        mkColValues =
                map (\ys -> zipWith (\(RelativeUnit _ multiplier) x -> x/multiplier)
                                mkColUnits ys)
                        sortedValues

        addUnitLabel name (RelativeUnit label _) =
            if label /= []
            then name ++ inParens label
            else name
        withUnits xs = zipWith addUnitLabel xs mkColUnits

        columns = getZipList $ ReportColumn
                <$> ZipList (withUnits mkColNames)
                <*> ZipList mkColUnits
                <*> ZipList (transpose mkColValues)

    in RawReport
            { reportOutputFile = outfile
            , reportIdentifier = groupName group
            , reportRowIds     = benchmarks
            , reportColumns    = columns
            }

reportPerGroup
    :: FilePath
    -> Maybe FilePath
    -> String
    -> Config
    -> (RawReport -> Config -> IO ())
    -> GroupMatrix
    -> IO ()
reportPerGroup dir outputFile ext cfg@Config{..} mkReport group = do
    outfile <- case outputFile of
        Just file -> fmap Just $ prepareOutputFile dir ext file (groupName group)
        Nothing -> return Nothing

    let rawReport = prepareRawReport cfg outfile group
    showStatusMessage cfg (groupName group) Nothing outfile
    mkReport rawReport cfg

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

inParens :: String -> String
inParens str = "(" ++ str ++ ")"

makeTitle :: String -> Maybe RelativeUnit -> Config -> String
makeTitle field unit Config{..} =
       fromMaybe "" title
    ++ inParens field
    ++ case unit of
            Nothing -> ""
            Just (RelativeUnit label _) ->
                if presentation /= Fields
                then inParens label
                else ""
    ++ case presentation of
            Groups style ->
                case comparisonStyleString style of
                    Nothing -> ""
                    Just str -> inParens str
            _ -> ""
