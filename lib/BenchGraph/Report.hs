-- |
-- Module      : BenchGraph.Report
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

module BenchGraph.Report
    (
      report
    ) where

import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Text.PrettyPrint.ANSI.Leijen
import Text.Printf (printf)
import Prelude hiding ((<$>))

import BenchGraph.Common

-- XXX in comparative reports render lower than baseline in green and higher
-- than baseline in red
genGroupReport :: RawReport -> Config -> IO ()
genGroupReport RawReport{..} cfg@Config{..} = do
    let diffStr =
            if length reportColumns > 1
            then diffString presentation
            else Nothing
    putStrLn $ makeTitle reportIdentifier diffStr cfg
    let benchcol  = "Benchmark" : reportRowIds
        groupcols =
            let firstCol : restCols = reportColumns
                colorCol ReportColumn{..} =
                    map (\x -> if x > 0 then dullred else dullgreen) colValues
                renderColored col =
                    let regular = renderGroupCol $ showCol col
                        colored = zipWith ($) (id : id : colorCol col) regular
                    in case presentation of
                        Groups Diff -> colored
                        Groups PercentDiff -> colored
                        _ -> regular
            in renderGroupCol (showFirstCol firstCol)
             : map renderColored restCols
        rows = foldl (zipWith (<+>)) (renderCol benchcol) groupcols
    putDoc $ vcat rows
    putStrLn "\n"

    where

    renderCol [] = error "Bug: header row missing"
    renderCol col@(h : rows) =
        let maxlen = maximum (map length col)
        in map (fill maxlen . text) (h : replicate maxlen '-' : rows)

    renderGroupCol [] = error
        "Bug: There has to be at least one column in raw report"
    renderGroupCol col@(h : rows) =
        let maxlen = maximum (map length col)
        in map (\x -> indent (maxlen - length x) $ text x)
               (h : replicate maxlen '-' : rows)

    showFirstCol ReportColumn{..} = colName : map (printf "%.2f") colValues
    showCol ReportColumn{..} = colName :
        let showDiff x =
                if x > 0
                then printf "+%.2f" x
                else printf "%.2f" x
        in case presentation of
                Groups Diff -> map showDiff colValues
                Groups PercentDiff -> map showDiff colValues
                _ -> map (printf "%.2f") colValues

-- | Presents the benchmark results in a CSV input file as text reports
-- according to the provided configuration.  The first parameter is the input
-- file name, the second parameter is the name prefix for the output SVG image
-- file(s). One or more output files may be generated depending on the
-- 'Presentation' setting.  When the second parameter is 'Nothing' the reports
-- are printed on the console. The last parameter is the configuration to
-- customize the report, you can start with 'defaultConfig' as the base and
-- override any of the fields that you may want to change.
--
-- For example:
--
-- @
-- report "bench-results.csv" Nothing 'defaultConfig'
-- @
--
-- @since 0.2.0
report :: FilePath -> Maybe FilePath -> Config -> IO ()
report inputFile outputFile cfg@Config{..} = do
    let dir = fromMaybe "." outputDir
    (csvlines, fields) <- prepareToReport inputFile cfg
    (runs, matrices) <- prepareGroupMatrices cfg csvlines fields
    case presentation of
        Groups style ->
            forM_ fields $
                reportComparingGroups style dir outputFile TextReport runs
                               cfg genGroupReport matrices
        Fields -> do
            forM_ matrices $
                reportPerGroup dir outputFile TextReport cfg genGroupReport
        Solo ->
            let funcs = map
                    (\mx -> reportComparingGroups Absolute dir
                        (fmap (++ "-" ++ groupName mx) outputFile)
                        TextReport runs cfg genGroupReport [mx])
                    matrices
             in sequence_ $ funcs <*> fields
