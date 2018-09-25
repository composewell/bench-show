-- |
-- Module      : BenchGraph.Graph
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

module BenchGraph.Graph
    (
      graphCmp
    , bgraph
    ) where

import Control.Arrow (second)
import Control.Monad (forM_)
import Control.Monad.Trans.State.Lazy (get, put)
import Data.Maybe (fromMaybe)
import Text.CSV (CSV)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

import BenchGraph.Common

-------------------------------------------------------------------------------
-- Benchmarking field specific handling
-------------------------------------------------------------------------------

-- XXX need another scale on the right hand side for diff %
-- XXX need the ability to specify Units in the scale
yindexes :: String
         -> Maybe (Double, Double)
         -> Maybe Granularity
         -> Double
         -> Maybe [Double]
yindexes fieldName fieldRange granularity multiplier =
    case (fieldRange, granularity) of
        (Just (rangeMin, rangeMax), Just g) ->
            let range = rangeMax - rangeMin
                (size, count) =
                    case g of
                        GrainSize n ->
                            (fromIntegral n, round $ range / fromIntegral n)
                        GrainCount n -> (range / fromIntegral n, n)
            in case isTimeField fieldName of
                True ->
                    let size' = size/multiplier
                        rmin  = rangeMin/multiplier
                    in Just $ take (count + 1) [rmin, rmin + size'..]
                False -> Just $ take (count + 1) [rangeMin, rangeMin + size..]
        _ -> Nothing

-------------------------------------------------------------------------------

-- "values" is [(benchGroupName, [benchResult])]
-- benchResult contains results for each benchmark in "benchNames" in exactly
-- the same order.
genGroupGraph
    :: FilePath
    -> String
    -> String
    -> String
    -> Maybe (Double, Double)
    -> Maybe Granularity
    -> Double
    -> Config
    -> [String]
    -> [(String, [Maybe Double])]
    -> IO ()
genGroupGraph outputFile atitle units fieldName fieldRange granularity multiplier Config{..} benchNames values = do
    toFile def outputFile $ do
        layout_title .= atitle
        layout_title_style . font_size .= 25

        let vals = transformGroupValues comparisonStyle values

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

        case yindexes fieldName fieldRange granularity multiplier of
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

graphField :: FilePath -> FilePath -> CSV -> Config -> String -> IO ()
graphField dir outputFile csvlines cfg@Config{..} field =
    let fieldRange = getFieldRange field cfg
        granularity = getFieldGranularity field cfg
        atitle = makeTitle field cfg
    in do
        outfile <- prepareOutputFile dir outputFile atitle field
        (units, multiplier, bmNames, bmGroups) <-
            rawReportComparing csvlines field cfg
        genGroupGraph outfile atitle units field
            fieldRange granularity multiplier cfg bmNames bmGroups

-- | Generates comparative reports comparing all the benchgroups for each field
-- as filtered by 'sortBenchFields'.  The first parameter is an input file
-- containing CSV data as generated by @gauge --csv=bench-results.csv@ or a
-- similar output generated by @criterion@.  The second parameter is the name
-- prefix for the output files holding the output SVG image. One file is
-- generated for each field, the filename is generated by appending
-- @-feildname.svg@ to the filename prefix. The last parameter is the
-- configuration to customize the graph, you can start with 'defaultConfig' as
-- the base and override any of the fields that you may want to change.
--
-- For example:
--
-- @
-- graphCmp "bench-results.csv" "output-graph" 'defaultConfig'
-- @
--
-- @since 0.2.0
graphCmp :: FilePath -> FilePath -> Config -> IO ()
graphCmp inputFile outputFile cfg@Config{..} = do
    let dir = fromMaybe "." outputDir
    (csvlines, fields) <- prepareToReport inputFile cfg
    forM_ fields $ graphField dir outputFile csvlines cfg

-- | The first parameter is an input file containing CSV data as generated by
-- @gauge --csv=bench-results.csv@ or a similar output generated by
-- @criterion@.  The second parameter is the name of the output file containing
-- the graph SVG image. The third parameter is the name of the field that
-- should be plotted.  The field is matched with the fields in the header line
-- of the CSV input using a case insensitive match.  The last parameter is the
-- configuration to customize the graph, you can start with 'defaultConfig' as
-- the base and set any of the fields that you may want to change.
--
-- For example:
--
-- @
-- bgraph "bench-results.csv" "output-graph" "mean" 'defaultConfig'
-- @
--
-- @since 0.1.0
{-# DEPRECATED bgraph "Please use graphCmp instead" #-}
bgraph :: FilePath -> FilePath -> String -> Config -> IO ()
bgraph inputFile outputFile fieldName cfg@Config{..} = do
    let dir = fromMaybe "." outputDir
    (csvlines, _) <- prepareToReport inputFile cfg
    graphField dir outputFile csvlines cfg fieldName
