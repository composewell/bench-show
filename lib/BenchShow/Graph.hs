-- |
-- Module      : BenchShow.Graph
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

module BenchShow.Graph
    (
      graph
    ) where

import Control.Arrow (second)
import Control.Monad (forM_, when)
import Control.Monad.Trans.State.Lazy (get, put)
import Data.Maybe (fromMaybe)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

import BenchShow.Common

-------------------------------------------------------------------------------
-- Benchmarking field specific handling
-------------------------------------------------------------------------------

-- XXX need the ability to specify Units in the scale
yindexes :: Maybe (Double, Double)
         -> Maybe FieldTick
         -> Double
         -> Maybe [Double]
yindexes fieldRange granularity multiplier =
    case (fieldRange, granularity) of
        (Just (rangeMin, rangeMax), Just g) ->
            let range = rangeMax - rangeMin
                (size, count) =
                    case g of
                        TickSize n ->
                            (fromIntegral n, round $ range / fromIntegral n)
                        TickCount n -> (range / fromIntegral n, n)
            in let size' = size / multiplier
                   rmin  = rangeMin / multiplier
               in Just $ take (count + 1) [rmin, rmin + size'..]
        _ -> Nothing

-------------------------------------------------------------------------------

transformColumns :: [ReportColumn] -> [ReportColumn]
transformColumns columns =
    if length columns == 1
    -- workaround for a bug that renders the plot badly when using
    -- a single cluster in the bar chart.
    then columns ++ [ReportColumn
            { colName = ""
            , colUnit = RelativeUnit "" 1
            , colValues = []
            }]
     else columns

genGroupGraph :: RawReport -> Config -> IO ()
genGroupGraph RawReport{..} cfg@Config{..} = do
    let outputFile  = fromMaybe undefined reportOutputFile
        fieldRange  = getFieldRange reportIdentifier cfg
        granularity = getFieldTick reportIdentifier cfg
        -- XXX assert that the unit for all columns is the same
        RelativeUnit ulabel multiplier = colUnit (head reportColumns)
        replaceMu 'μ' = 'u'
        replaceMu x = x
        unitLabel = map replaceMu ulabel
        columns = transformColumns reportColumns
        atitle = maybe "" (\f -> f reportIdentifier) mkTitle

    toFile def outputFile $ do
        layout_title .= atitle
        layout_title_style . font_size .= 25

        layout_x_axis . laxis_generate .=
            autoIndexAxis (map (map replaceMu . colName) columns)
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
                _axis_labels = map (map (second (++ " " ++ unitLabel)))
                                   (_axis_labels ad)
            }
        when (presentation /= Fields) $
            layout_y_axis . laxis_override .= modifyLabels

        case yindexes fieldRange granularity multiplier of
            Nothing -> return ()
            Just indexes ->
                layout_y_axis . laxis_override .= \_ ->
                    makeAxis (let f = floor :: Double -> Int
                              in map ((++ " " ++ unitLabel) . show . f))
                             (indexes, [], [])

        plot $ fmap plotBars $ bars reportRowIds
            $ (addIndexes $ map colValues columns)

-- | Presents the benchmark results in a CSV input file as graphical bar charts
-- according to the provided configuration.  The first parameter is the input
-- file name, the second parameter is the name prefix for the output SVG image
-- file(s). One or more output files may be generated depending on the
-- 'Presentation' setting.  The last parameter is the configuration to
-- customize the graph, you can start with 'defaultConfig' as the base and
-- override any of the fields that you may want to change.
--
-- For example:
--
-- @
-- graph "bench-results.csv" "output-graph" 'defaultConfig'
-- @
--
-- @since 0.2.0
graph :: FilePath -> FilePath -> Config -> IO ()
graph inputFile outputFile cfg@Config{..} = do
    let dir = fromMaybe "." outputDir
    (csvlines, fields) <- prepareToReport inputFile cfg
    (runs, matrices) <- prepareGroupMatrices cfg inputFile csvlines fields
    case presentation of
        Groups style ->
            forM_ fields $
                reportComparingGroups style dir (Just outputFile)
                                      GraphicalChart runs cfg
                                      genGroupGraph matrices
        Fields -> do
            forM_ matrices $
                reportPerGroup dir (Just outputFile) GraphicalChart
                               cfg genGroupGraph
        Solo ->
            let funcs = map
                    (\mx -> reportComparingGroups Absolute dir
                        (Just $ outputFile ++ "-" ++ groupName mx)
                        GraphicalChart runs cfg genGroupGraph [mx])
                    matrices
             in sequence_ $ funcs <*> fields
