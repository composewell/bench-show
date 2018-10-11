-- |
-- Module      : BenchShow.Common
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

module BenchShow.Common
    ( Presentation(..)
    , GroupStyle(..)
    , FieldTick (..)
    , SortColumn (..)
    , RelativeUnit (..)
    , Estimator (..)
    , DiffStrategy (..)
    , Config(..)
    , defaultConfig

    , getFieldRange
    , getFieldTick

    , GroupMatrix(..)
    , prepareGroupMatrices

    , ReportColumn(..)
    , RawReport(..)
    , ReportType(..)
    , diffString
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
import Data.Foldable (foldl')
import Data.Function ((&), on)
import Data.List
       (transpose, groupBy, (\\), find, sortBy, elemIndex, intersect,
        intersectBy)
import Data.List.Split (linesBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import Debug.Trace (trace)
import Statistics.Types (Estimate(..), ConfInt(..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.CSV (CSV, parseCSVFromFile)
import Text.Read (readMaybe)

import BenchShow.Analysis

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

data ReportType = TextReport | GraphicalChart

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
                        -- @-estimator-groupname-fieldname@ as suffix.
    | Groups GroupStyle -- ^ One report is generated for each field selected by
                        -- the configuration. Each report presents a field
                        -- with all the groups selected by the configuration as
                        -- columns or clusters. Output files are named using
                        -- @-estimator-fieldname@ as suffix.
    | Fields            -- ^ One report is generated for each group selected by
                        -- the configuration. Each report presents a group
                        -- with all the fields selected by the configuration as
                        -- columns or clusters. Output files are named using
                        -- @-estimator-groupname@ as suffix.
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

-- | Strategy to compute the difference between two groups of benchmarks being
-- compared.
--
-- @since 0.2.0
data DiffStrategy =
      SingleEstimator -- ^ Use a single estimator to compute the difference
                      -- between the baseline and the candidate. The estimator
                      -- that is provided in the 'Config' is used.
    | MinEstimator    -- ^ Use 'Mean', 'Median' and 'Regression' estimators for
                      -- both baseline and candidate, and report the estimator
                      -- that shows the minimum difference. This is more robust
                      -- against random variations.
    {-
    | WorstBest
    | BestBest
    -}

-- | Configuration governing generation of chart. See 'defaultConfig' for the
-- default values of these fields.
--
-- @since 0.2.0
data Config = Config
    {
    -- | Provide more details in the report, especially the standard deviation,
    -- outlier variance, R-square estimate and an annotation to indicate the
    -- actual method used when using 'MinEstimator' are reported.
      verbose :: Bool

    -- | The directory where the output graph or report file should be placed.
    , outputDir   :: Maybe FilePath

    -- | Report title, more information like the plotted field name or
    -- the presentation style may be added to it.
    , title  :: Maybe String

    -- | How to determine the layout of the report or the chart.
    , presentation :: Presentation

    -- | The estimator used for the report.
    , estimator    :: Estimator

    -- | The minimum percentage difference between two runs of a benchmark
    -- beyond which the benchmark is flagged to have regressed or improved.
    , threshold :: Word

    -- | Strategy to compare two runs or groups of benchmarks.
    , diffStrategy  :: DiffStrategy

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
    , selectBenchmarks
        :: (SortColumn -> Either String [(String, Double)])
        -> [String]
    }

-- | Default configuration. Use this as the base configuration and modify the
-- required fields. The defaults are:
--
-- @
--  verbose           = False
--  title             = Nothing
--  outputDir         = Nothing
--  presentation      = Groups Absolute
--  estimator         = Median
--  threshold         = 3
--  diffStrategy      = MinEstimator
--  selectFields      = filter (flip elem ["time", "mean", "maxrss"] . map toLower)
--  fieldRanges       = []
--  fieldTicks        = []
--  classifyBenchmark = Just . ("default",)
--  selectGroups      = id
--  selectBenchmarks  = \f -> either error (map fst) $ f (ColumnIndex 0)
-- @
--
-- @since 0.2.0
defaultConfig :: Config
defaultConfig = Config
    { verbose           = False
    , title             = Nothing
    , outputDir         = Nothing
    , presentation      = Groups Absolute
    , estimator         = Median
    , threshold         = 3
    , diffStrategy      = MinEstimator
    , selectFields      = filter (flip elem ["time", "mean", "maxrss"] . map toLower)
    , fieldRanges       = []
    , fieldTicks        = []
    , classifyBenchmark = Just . ("default",)
    , selectGroups      = id
    , selectBenchmarks  = \f -> either error (map fst) $ f (ColumnIndex 0)
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

isTimeField :: String -> Bool
isTimeField fieldName = map toLower fieldName `elem` timeFields

allocFields :: [String]
allocFields = map (map toLower) ["allocated", "bytesCopied", "maxrss"]

isAllocationField :: String -> Bool
isAllocationField fieldName = map toLower fieldName `elem` allocFields

predictorFields :: [String]
predictorFields = map (map toLower)
    [ "iters"
    -- , "minflt"
    -- , "majflt"
    -- , "nvcsw"
    -- , "nivcsw"
    ]

isPredictorField :: String -> Bool
isPredictorField fieldName = map toLower fieldName `elem` predictorFields

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
    | k >= 1e-6  = RelativeUnit "μs" 1e-6
    | otherwise  = RelativeUnit "ns" 1e-9

getSpaceUnit :: Double -> RelativeUnit
getSpaceUnit k
    | k < 0             = getSpaceUnit (-k)
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

absoluteDiff :: Num a => a -> a -> a
absoluteDiff v1 v2 = v2 - v1

percentDiff :: (Fractional a, Num a) => a -> a -> a
percentDiff v1 v2 = ((v2 - v1) * 100) / v1

percent :: (Fractional a, Num a) => a -> a -> a
percent v1 v2 = (v2 * 100) / v1

cmpTransformColumns :: ReportType
                    -> GroupStyle
                    -> Estimator
                    -> DiffStrategy
                    -- XXX we do not really need the benchmark name here
                    -> [[(String, AnalyzedField)]]
                    -> (Maybe [[Estimator]], [[(String, Double)]])
cmpTransformColumns rtype style estimator diffStrategy cols =
    let cmpWith diff =
            let firstCol = head columns
                colTransform col =
                    let mkDiff (n1, v1) (n2,v2) =
                            verify (n1 == n2) (n2, diff v1 v2)
                    in zipWith mkDiff firstCol col
            in map colTransform (tail columns)

        cmpMinWith diff =
            let firstCol = head cols
                colTransform col = zipWith (mkMinDiff diff) firstCol col
            in map colTransform (tail cols)
    in case style of
            Absolute    -> (Nothing, columns)
            Percent     -> (Nothing, cmpWith percent)
            Diff        ->
                case diffStrategy of
                    MinEstimator ->
                        let (ests, vals) = unzip $ map unzip (cmpMinWith absoluteDiff)
                        in ( Just $ map (const estimator) (head cols) : ests
                           , head columns : vals
                           )
                    SingleEstimator ->
                        (Nothing, head columns : cmpWith absoluteDiff)
            PercentDiff ->
                -- In a comparative graphical chart we cannot show the absolute
                -- values in the baseline column as the units won't match for
                -- the baseline and the diff clusters.
                let baseCol =
                        case rtype of
                            TextReport -> head columns
                            GraphicalChart | length columns == 1 ->
                                head columns
                            GraphicalChart ->
                                map (\(n,_) -> (n,100)) (head columns)
                in case diffStrategy of
                    MinEstimator ->
                        let (ests, vals) = unzip $ map unzip (cmpMinWith percentDiff)
                        in ( Just $ map (const estimator) (head cols) : ests
                           , baseCol : vals
                           )
                    SingleEstimator ->
                       (Nothing, baseCol : cmpWith percentDiff)
    where
        verify a b = if a then b else error "bug: benchmark names mismatch"
        transformVals = map (map (second (getAnalyzedValue estimator)))
        columns = transformVals cols

        -- Find which estimator gives us the minimum diff
        mkMinDiff diff (n1, v1) (n2,v2) = verify (n1 == n2) $
            let meanDiff = diff (getAnalyzedValue Mean v1)
                                (getAnalyzedValue Mean v2)
                medDiff = diff (getAnalyzedValue Median v1)
                               (getAnalyzedValue Median v2)
                regDiff = diff (getAnalyzedValue Regression v1)
                               (getAnalyzedValue Regression v2)
            in if abs medDiff <= abs meanDiff
               then if abs medDiff <= abs regDiff
                    then (Median, (n2, medDiff))
                    else (Regression, (n2, regDiff))
                else if abs meanDiff <= abs regDiff
                     then (Mean, (n2, meanDiff))
                     else (Regression, (n2, regDiff))

transformColumnNames :: GroupStyle -> [ReportColumn] -> [ReportColumn]
transformColumnNames _ [] = []
transformColumnNames style columns@(h:t) =
    let withDiff = colSuffix baseName h : map (colSuffix diffName) t
    in case style of
            Diff        | length columns > 1 -> withDiff
            PercentDiff | length columns > 1 -> withDiff
            _           -> columns

    where
    colSuffix xl col = col { colName = xl (colName col) }
    baseName        = (++ "(base)")
    diffName        = (++ "(-base)")

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

extractColumn :: String -> GroupMatrix -> [(String, AnalyzedField)]
extractColumn field GroupMatrix{..} =
    let idx = elemIndex field (colNames groupMatrix)
        vals = case idx of
            Just i -> map (!! i) (map snd (rowValues groupMatrix))
            Nothing -> error $ "Field [" ++ field
                ++ "] does not exist in group ["
                ++ groupName ++ "] and run id [" ++ show groupIndex ++ "]"
    in zip (map fst groupBenches) vals

extractColumnValue :: String -> GroupMatrix -> Estimator -> [(String, Double)]
extractColumnValue field matrix estimator =
    map (second (getAnalyzedValue estimator)) $ extractColumn field matrix

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
               else Left $ "selectBenchmarks: there are " ++ show len
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
                (_, False) -> Left $ "Benchmark group name [" ++ name
                            ++ "] and index [" ++ show runId
                            ++ "] not found. Available groups are: "
                            ++ show grpNames
                (i, True) -> extractGroup (ColumnIndex i)

    extractColumnByGroupIndex idx =
        let len = length columns
        in if idx >= len
           then Left $ "Column index must be in the range [0-"
                ++ show (len - 1) ++ "]"
           else Right $ columns !! idx

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
            False -> Left $ "Benchmark field name [" ++ name
                        ++ "] not found in group ["
                        ++ groupName ++ "]. Available fields are: "
                        ++ show fields
            True -> Right $ extractColumnValue name grp estimator

    extractColumnByFieldIndex idx =
        let fields = colNames groupMatrix
            len = length fields
        in if idx >= len
           then Left $ "Column index must be in the range [0-"
                ++ show (len - 1) ++ "]"
           else Right $ extractColumnValue (fields !! idx) grp estimator

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

-- An iteration field indicates that consecutive rows with the same benchmark
-- name have results from different iterations of the same benchmark and the
-- measurement fields have to be scaled per iteration based on the number of
-- iterations in the iteration count field.
--
-- Make sure that "iters" and "name" are the first and second columns
ensureIterField :: ([String], [NumberedLines])  -> ([String], [NumberedLines])
ensureIterField (header, groups) =
    ( "iters" : "name" : filter isNotNameIter header
    , map reorderNameIter groups
    )

    where

    isNotNameIter x =
           map toLower x /= "name"
        && map toLower x /= "iters"

    notNameIters [] = True
    notNameIters (x:_) = isNotNameIter x

    nameNotFound = error "Name field is required in the csv file"

    reorderNameIter csvlines =
          unzip csvlines
        & second (header :)
        & second transpose
        & second reorder
        & second transpose
        & uncurry zip

        where

        reorder xs =
            let findField x = find (\(y:_) -> map toLower y == x)
                iterCol = replicate (length (head xs) - 1) "1"
            in   fromMaybe iterCol (fmap tail $ findField "iters" xs)
               : fromMaybe nameNotFound (fmap tail $ findField "name" xs)
               : map tail (filter notNameIters xs)

-- Only keep those fields that are passed to this function
-- Also, preserve any predictor fields for regression analysis
filterFields :: [String] -> BenchmarkIterMatrix -> BenchmarkIterMatrix
filterFields fieldNames BenchmarkIterMatrix{..} =
    BenchmarkIterMatrix
        { iterPredColNames = ["iters"] ++ filter isPredictorField iterRespColNames
        , iterRespColNames = filter isRequestedField iterRespColNames
        , iterRowValues = transform iterRowValues
        }

    where

    transform :: [(String, [([Double], [Double])])] -> [(String, [([Double], [Double])])]
    transform = map (\(name, tuples) ->
        let (ys, zs) = unzip tuples
            pcols = transpose (map Left iterPredColNames : map (map Right) ys)
            rcols = transpose (map Left iterRespColNames : map (map Right) zs)
            pcols' = pcols ++ filter isPredictor rcols
            rcols' = filter requested rcols
            pcols'' = map (map fromRt) $ tail $ transpose pcols'
            rcols'' = map (map fromRt) $ tail $ transpose rcols'
        in (name, zip pcols'' rcols''))

    fromRt (Right x) = x
    fromRt _ = error "bug"

    isRequestedField = (`elem` fieldNames)

    requested [] = True
    requested (Left x:_) = isRequestedField x
    requested _ = error "bug"

    isPredictor [] = True
    isPredictor (Left x:_) = isPredictorField x
    isPredictor _ = error "bug"

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
            ++ " field [" ++ show fval ++ "] as "
            ++ typ ++ " type at line number "
            ++ show lno
        Just n -> n

-- An iteration field indicates that consecutive rows with the same benchmark
-- name have results from different iterations of the same benchmark and the
-- measurement fields have to be scaled per iteration based on the number of
-- iterations in the iteration count field.
--
-- If the first column is iteration then fold all iterations and remove the
-- iteration column.
readIterations :: [String] -> NumberedLines -> BenchmarkIterMatrix
readIterations header csvlines =
    let tuples =
            map (parseNumericFields header) csvlines
            -- we now have a list of triples [(iter, name, (fieldName, [Double])]
          & groupBy successiveIters
          & map (foldl' addIters ("",[]))
    in BenchmarkIterMatrix
        { iterPredColNames = ["iters"]
        , iterRespColNames = drop 2 header
        , iterRowValues = tuples
        }

    where

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

    addIters (_,siters) (iter,name,vals) =
        (name, ([fromIntegral iter], map snd vals) : siters)

getFieldRange :: String -> Config -> Maybe (Double, Double)
getFieldRange fieldName Config{..} =
    let res = find (\(x, _, _) -> x == fieldName) fieldRanges
    in case res of
        Nothing -> Nothing
        Just (_, x, y) -> Just (x, y)

getFieldTick :: String -> Config -> Maybe FieldTick
getFieldTick fieldName Config{..} =
    fmap snd $ find (\x -> fst x == fieldName) fieldTicks

getReportExtension :: ReportType -> String
getReportExtension rtype =
    case rtype of
        TextReport -> ".txt"
        GraphicalChart -> ".svg"

prepareOutputFile :: FilePath -> ReportType -> FilePath -> Estimator -> String -> IO FilePath
prepareOutputFile dir rtype file est field = do
    let estStr = case est of
            Mean -> "mean"
            Median -> "median"
            Regression -> "coeff"
    let path = dir </> (file ++ "-" ++ estStr ++ "-" ++ field
                             ++ getReportExtension rtype)
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
            vals = map (\(new,old) ->
                (new, fromMaybe (error "bug") $ lookup old (rowValues m)))
                (groupBenches matrix)
        in matrix {groupMatrix = m {rowValues = vals}}

_filterCommonSubsets :: [BenchmarkIterMatrix] -> [BenchmarkIterMatrix]
_filterCommonSubsets matrices =
    let commonPreds =
            let initPreds = matrixPreds $ head matrices
            in foldl' intersectPreds initPreds (tail matrices)
    in map (isectCommonPreds commonPreds) matrices

    where

    pcols = iterPredColNames $ head matrices

    cmpPred name v1 v2 =
            case map toLower name of
                "iters" -> v1 == v2
                "nivcsw" -> v1 == v2
                _ -> v1 == v2

    isectBench (name1, preds1) (name2, preds2) =
        let isect row1 row2 = all id $ zipWith3 cmpPred pcols row1 row2
        in assert (name1 == name2) $ (name1, intersectBy isect preds1 preds2)

    matrixPreds = map (second (map fst)) . iterRowValues

    intersectPreds preds matrix = zipWith isectBench preds (matrixPreds matrix)

    isectRows (name1, preds1) (name2, xs) =
        let isect row1 = find (\(x,_) -> all id
                                $ zipWith3 cmpPred pcols row1 x) xs
        in assert (name1 == name2) $ (name1, mapMaybe isect preds1)

    isectCommonPreds preds matrix@BenchmarkIterMatrix{..} =
        matrix
            { iterRowValues = zipWith isectRows preds iterRowValues
            }

-- when comparing make sure all groups have same benchmarks and sort the other
-- ones based on the first column so that they are all in the same order.
selectCommon :: [GroupMatrix] -> IO [GroupMatrix]
selectCommon matrices =
    let commonBenches =
            let initBenches = map fst $ groupBenches $ head matrices
            in foldl' intersectBenches initBenches (tail matrices)
    in mapM (isectCommonBenches commonBenches) matrices

    where

    intersectBenches benches matrix =
        intersect benches (map fst $ groupBenches matrix)

    isectCommonBenches benches matrix@GroupMatrix{..} = do
        let absent = map fst groupBenches \\ benches
            msg =
                "Removing exclusive benchmarks " ++ show absent
                ++ " from column [" ++ groupName
                ++ "] run id [" ++ show groupIndex
            lookupBench x = lookup x groupBenches
            findBench x = (x, fromMaybe undefined (lookupBench x))
            newBenches = map findBench benches

        unless (null absent) $ putStrLn msg
        return matrix { groupBenches = newBenches }

prepareGroupMatrices :: Config -> CSV -> [String] -> IO (Int, [GroupMatrix])
prepareGroupMatrices cfg@Config{..} csvlines fields = do
    let (hdr, runs) =
              sanityCheckCSV csvlines
            & splitRuns
            & ensureIterField
    xs <- sequence $ map (readIterations hdr) runs
            & map (filterFields fields)
            -- & _filterCommonSubsets
            -- & map filterSamples
            & map foldBenchmark

    zip [0..] xs
        & map (splitGroup classifyBenchmark)
        & concat
        & sortGroups cfg
        >>= selectCommon
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

-- XXX put reportAnalyzed in reportColumns
data RawReport = RawReport
    { reportOutputFile :: Maybe FilePath
    , reportIdentifier :: String
    , reportRowIds     :: [String]
    , reportColumns    :: [ReportColumn]
    , reportAnalyzed   :: [[AnalyzedField]]
    , reportEstimators :: Maybe [[Estimator]]
    } deriving Show

getFieldMin :: Config -> Double -> String -> Double
getFieldMin cfg minval fieldName =
    case getFieldRange fieldName cfg of
        Nothing -> minval
        Just (minr, _) -> minr

scaleAnalyzedField :: RelativeUnit -> AnalyzedField -> AnalyzedField
scaleAnalyzedField (RelativeUnit _ mult) AnalyzedField{..} =
    AnalyzedField
    { analyzedMean = analyzedMean / mult
    , analyzedStdDev = analyzedStdDev / mult

    , analyzedMedian = analyzedMedian / mult
    , analyzedOutliers = analyzedOutliers
    , analyzedOutlierVar = analyzedOutlierVar
    , analyzedKDE = analyzedKDE
    , analyzedRegCoeff = case
        analyzedRegCoeff of
            Nothing -> Nothing
            Just Estimate{..} ->
                let ConfInt{..} = estError
                in Just $ Estimate
                    { estPoint = estPoint / mult
                    , estError = ConfInt
                        { confIntLDX = confIntLDX / mult
                        , confIntUDX = confIntUDX / mult
                        , confIntCL = confIntCL
                        }
                    }
    , analyzedRegRSq = analyzedRegRSq
    }

prepareGroupsReport :: Config
                    -> GroupStyle
                    -> Maybe FilePath
                    -> ReportType
                    -> Int
                    -> String
                    -> [GroupMatrix]
                    -> RawReport
prepareGroupsReport cfg@Config{..} style outfile rtype runs field matrices =
    -- XXX Determine the unit based the whole range of values across all columns
    let sortValues :: [String] -> [(String, a)] -> [a]
        sortValues bmarks vals =
            map (\name -> fromMaybe (error "bug") (lookup name vals)) bmarks

        unsortedCols = map (extractColumn field) matrices

        (estimators, transformedCols) =
            cmpTransformColumns rtype style estimator diffStrategy unsortedCols
        benchmarks = selectBenchmarksByField cfg matrices transformedCols
        sortedCols = map (sortValues benchmarks) transformedCols
        origSortedCols = map (sortValues benchmarks) unsortedCols

        mkColUnits :: [RelativeUnit]
        mkColUnits =
            let cols =
                    if style == Diff || style == PercentDiff
                    -- if we consider diff values as well here then the
                    -- units will change to potentially very small.
                    then [head sortedCols]
                    else sortedCols
                minVal = getFieldMin cfg (minimum $ concat cols) field
            in case (rtype, style) of
                -- In case of percentDiff in TextReport we use absolute
                -- values in the baseline column, so the unit is different.
                (TextReport, PercentDiff) ->
                    let unit = fieldUnits field minVal Absolute
                        punit = fieldUnits field 1 style -- % unit
                    in unit : replicate (length matrices - 1) punit
                (GraphicalChart, PercentDiff) | length matrices == 1 ->
                    [fieldUnits field minVal Absolute]
                _ -> let unit = fieldUnits field minVal style
                     in replicate (length matrices) unit

        mkColValues :: [[Double]]
        mkColValues =
            let applyUnit col (RelativeUnit _ multiplier) =
                    map (/multiplier) col
            in zipWith applyUnit sortedCols mkColUnits

        mkColNames :: [String]
        mkColNames =
                let withSuffix x =
                        groupName x ++
                            if runs > 1
                            then "(" ++ show (groupIndex x) ++ ")"
                            else ""
                    applyUnit name (RelativeUnit label _) =
                        name ++ inParens label
                in zipWith applyUnit (map withSuffix matrices) mkColUnits

        columns = getZipList $ ReportColumn
                    <$> ZipList mkColNames
                    <*> ZipList mkColUnits
                    <*> ZipList mkColValues

    in RawReport
            { reportOutputFile = outfile
            , reportIdentifier = field
            , reportRowIds     = benchmarks
            , reportColumns    = transformColumnNames style columns
            , reportAnalyzed   = zipWith (\x y -> map (scaleAnalyzedField x) y)
                                         mkColUnits origSortedCols
            , reportEstimators = estimators
            }

showStatusMessage :: Show a => Config -> String -> Maybe a -> IO ()
showStatusMessage cfg field outfile =
    let atitle = makeTitle field (diffString (presentation cfg)
                                 (diffStrategy cfg)) cfg
    in case outfile of
        Just path ->
            putStrLn $ "Creating chart "
                ++ "[" ++ atitle ++ "]"
                ++ " at "
                ++ show path
        Nothing -> return ()

reportComparingGroups
    :: GroupStyle
    -> FilePath
    -> Maybe FilePath
    -> ReportType
    -> Int
    -> Config
    -> (RawReport -> Config -> IO ())
    -> [GroupMatrix]
    -> String
    -> IO ()
reportComparingGroups style dir outputFile rtype runs cfg@Config{..} mkReport matrices field = do
    outfile <- case outputFile of
        Just file -> fmap Just $ prepareOutputFile dir rtype file
                                        estimator field
        Nothing -> return Nothing

    let rawReport = prepareGroupsReport cfg style outfile rtype runs field matrices
    showStatusMessage cfg field outfile
    mkReport rawReport cfg

-- Prepare report for a given group, the report would consist of multiple
-- field columns.
prepareFieldsReport :: Config
                 -> Maybe FilePath
                 -> GroupMatrix
                 -> RawReport
prepareFieldsReport cfg@Config{..} outfile group =
    let mkColNames :: [String]
        mkColNames = colNames $ groupMatrix group

        benchmarks = selectBenchmarksByGroup cfg group

        getBenchValues name =
              fromMaybe (error "bug") $
                lookup name (rowValues $ groupMatrix group)

        sortedCols = transpose $ map getBenchValues benchmarks
        minColValues = map (minimum . map (getAnalyzedValue estimator))
                           sortedCols

        mkColUnits :: [RelativeUnit]
        mkColUnits = map (\(x, v) -> getUnitByFieldName x (getFieldMin cfg v x))
                         (zip mkColNames minColValues)

        mkColValues :: [[Double]]
        mkColValues =
            let scaleCol (RelativeUnit _ multiplier) = map (/ multiplier)
            in  zipWith scaleCol mkColUnits
                    (map (map (getAnalyzedValue estimator)) sortedCols)

        addUnitLabel name (RelativeUnit label _) =
            if label /= []
            then name ++ inParens label
            else name
        withUnits xs = zipWith addUnitLabel xs mkColUnits

        columns = getZipList $ ReportColumn
                <$> ZipList (withUnits mkColNames)
                <*> ZipList mkColUnits
                <*> ZipList mkColValues

    in RawReport
            { reportOutputFile = outfile
            , reportIdentifier = groupName group
            , reportRowIds     = benchmarks
            , reportColumns    = columns
            , reportAnalyzed   = sortedCols
            , reportEstimators = Nothing
            }

reportPerGroup
    :: FilePath
    -> Maybe FilePath
    -> ReportType
    -> Config
    -> (RawReport -> Config -> IO ())
    -> GroupMatrix
    -> IO ()
reportPerGroup dir outputFile rtype cfg@Config{..} mkReport group = do
    outfile <- case outputFile of
        Just file -> fmap Just $ prepareOutputFile dir rtype file
                                        estimator (groupName group)
        Nothing -> return Nothing

    let rawReport = prepareFieldsReport cfg outfile group
    showStatusMessage cfg (groupName group) outfile
    mkReport rawReport cfg

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

showDiffStrategy :: DiffStrategy -> String
showDiffStrategy s =
    case s of
        SingleEstimator -> ""
        MinEstimator -> "using min estimator"

diffString :: Presentation -> DiffStrategy -> Maybe String
diffString style s =
    case style of
        Groups Diff        -> Just $ "Diff " ++ showDiffStrategy s
        Groups PercentDiff -> Just $ "Diff " ++ showDiffStrategy s
        _ -> Nothing

inParens :: String -> String
inParens str = "(" ++ str ++ ")"

showEstimator :: Estimator -> String
showEstimator est =
    case est of
        Mean       -> "Mean"
        Median     -> "Median"
        Regression -> "Regression Coeff."

makeTitle :: String -> Maybe String -> Config -> String
makeTitle field diff Config{..} =
       fromMaybe "" title
    ++ inParens field
    ++ inParens (showEstimator estimator)
    ++ case diff of
            Nothing -> ""
            Just str -> inParens str
