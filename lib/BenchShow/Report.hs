-- |
-- Module      : BenchShow.Report
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

module BenchShow.Report
    (
      report
    ) where

import Control.Applicative (ZipList(..))
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Statistics.Types (Estimate(..))
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Text.Printf (printf)

import BenchShow.Common
import BenchShow.Analysis

-- XXX in comparative reports render lower than baseline in green and higher
-- than baseline in red
genGroupReport :: RawReport -> Config -> IO ()
genGroupReport RawReport{..} cfg@Config{..} = do
    let diffStr =
            if length reportColumns > 1
            then diffString presentation diffStrategy
            else Nothing
    putStrLn $ makeTitle reportIdentifier diffStr cfg
    let benchcol  = "Benchmark" : reportRowIds
        groupcols =
            let firstCol : tailCols = reportColumns
                colorCol ReportColumn{..} =
                    let f x = case presentation of
                                Groups Diff ->
                                    if x > 0 then dullred else dullgreen
                                Groups PercentDiff ->
                                    if x > fromIntegral threshold
                                    then dullred
                                    else if x < (-1) * fromIntegral threshold
                                         then dullgreen
                                         else id
                                _ -> id
                    in map f colValues
                renderTailCols estimators col analyzed =
                    let regular = renderGroupCol $ showCol col Nothing analyzed
                        colored = zipWith ($) (id : id : colorCol col)
                                    $ renderGroupCol
                                    $ showCol col estimators analyzed
                    in case presentation of
                        Groups Diff        -> colored
                        Groups PercentDiff -> colored
                        _ -> regular
            in renderGroupCol (showFirstCol firstCol)
             : case reportEstimators of
                Just ests -> getZipList $
                            renderTailCols
                        <$> ZipList (map Just $ tail ests)
                        <*> ZipList tailCols
                        <*> ZipList (tail reportAnalyzed)
                Nothing ->  getZipList $
                            renderTailCols
                        <$> pure Nothing
                        <*> ZipList tailCols
                        <*> ZipList (tail reportAnalyzed)
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

    showEstimator est =
        case est of
            Mean       -> "(mean)"
            Median     -> "(medi)"
            Regression -> "(regr)"

    showEstVal estvals est =
        case est of
            Mean ->
                let sd = analyzedStdDev estvals
                    val = analyzedMean estvals
                in
                   if val /= 0
                   then printf "(%.2f)" $ sd / abs val
                   else ""
            Median ->
                let x = ovFraction $ analyzedOutlierVar estvals
                in printf "(%.2f)" x
            Regression ->
                case analyzedRegRSq estvals of
                    Just rsq -> printf "(%.2f)" (estPoint rsq)
                    Nothing -> ""

    showFirstCol ReportColumn{..} =
        let showVal = printf "%.2f"
            withEstimator val estvals =
                showVal val ++
                    if verbose
                    then showEstVal estvals estimator
                    else ""
            withEstVal =
                zipWith withEstimator colValues (head reportAnalyzed)
        in colName : withEstVal

    showCol ReportColumn{..} estimators analyzed = colName :
        let showVal val =
                let showDiff =
                        if val > 0
                        then printf "+%.2f" val
                        else printf "%.2f" val
                in case presentation of
                        Groups Diff        -> showDiff
                        Groups PercentDiff -> showDiff
                        _ -> printf "%.2f" val

            showEstAnnot est =
                case presentation of
                    Groups Diff        -> showEstimator est
                    Groups PercentDiff -> showEstimator est
                    _ -> ""

        in case estimators of
            Just ests ->
                let withAnnot val estvals est =
                           showVal val
                        ++ if verbose
                           then showEstVal estvals est
                                ++ showEstAnnot est
                           else ""
                in getZipList $
                        withAnnot
                    <$> ZipList colValues
                    <*> ZipList analyzed
                    <*> ZipList ests

            Nothing ->
                let withEstVal val estvals est =
                           showVal val
                        ++ if verbose then showEstVal estvals est else ""
                in getZipList $
                        withEstVal
                    <$> ZipList colValues
                    <*> ZipList analyzed
                    <*> pure estimator

-- | Presents the benchmark results in a CSV input file as text reports
-- according to the provided configuration.  The first parameter is the input
-- file name. The second parameter, when specified using 'Just', is the name
-- prefix for the output SVG image file(s). One or more output files may be
-- generated with the given prefix depending on the 'Presentation' setting.
-- When the second parameter is 'Nothing' the reports are printed on the
-- console. The last parameter is the configuration to customize the report,
-- you can start with 'defaultConfig' as the base and override any of the
-- fields that you may want to change.
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
    (runs, matrices) <- prepareGroupMatrices cfg inputFile csvlines fields
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
