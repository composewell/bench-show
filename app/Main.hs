{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-deprecations #-}
#endif

-- |
-- Module      : Main
-- Copyright   : (c) 2019 Composewell Technologies
--               (c) 2019 Sanchayan Maity
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Main where

import Options.Applicative.Simple (addCommand, simpleOptions, simpleVersion)
import Paths_bench_show (version)

import Options.Applicative
import BenchShow

-- The command line interface provides subcommands for each task.  There are
-- some common options that apply to all subcommands. Subcommands may have
-- their own specific options as well.

-------------------------------------------------------------------------------
-- Common config options
-------------------------------------------------------------------------------

pVerbose :: Parser Bool
pVerbose = switch $
       long "verbose"
    <> short 'v'
    <> help "Provide more details in the report output"

pOutputDir :: Parser FilePath
pOutputDir = strOption
  ( long "output-dir"
    <> metavar "DIR"
    <> help "Default is current directory" )

pTitle :: Parser String
pTitle = strOption
  ( long "title"
    <> short 't'
    <> metavar "STRING"
    <> help "Title for the report" )

pFailureField :: Parser String
pFailureField = strOption
  ( long "failure-field"
    <> metavar "STRING"
    <> help "A field to scrutinize the threshold and fail on. Eg: 'allocated'" )

pTitleAnnotation :: Parser TitleAnnotation
pTitleAnnotation = option auto $
       long "title-annotations"
    <> help ("*TitleField*|TitleEstimator|TitleDiff")

pPresentation :: Parser Presentation
pPresentation = option auto $
       long "presentation"
    <> help ("Solo|Fields|*Groups* <*Absolute*|Diff|PercentDiff|Multiples")

pEstimator :: Parser Estimator
pEstimator = option auto $
       long "estimator"
    <> help ("*Median*|Mean|Regression")

pThreshold :: Parser Word
pThreshold = option auto $
       long "threshold"
    <> metavar "PERCENT"
    <> help "Min % diff (default 3) to flag regression or improvement"

pDiffStrategy :: Parser DiffStrategy
pDiffStrategy = option auto $
       long "diff-strategy"
    <> help ("*SingleEstimator*|MinEstimator")

pOmitBaseline :: Parser Bool
pOmitBaseline = switch $
       long "omit-baseline"
    <> help "omit the baseline column in relative comparisons"

-------------------------------------------------------------------------------
-- Build a Config parser for common options
-------------------------------------------------------------------------------

-- Specify a default value for a Maybe inside a functor
fMaybe :: Functor f => a -> f (Maybe a) -> f a
fMaybe a = fmap (maybe a id)

-- | parse an optional field with a default value taken from defaultConfig
parseOptional :: (Config -> a) -> Parser a -> Parser a
parseOptional def parser = fMaybe (def defaultConfig) (optional parser)

pConfig :: Parser Config
pConfig = Config
    <$> parseOptional verbose pVerbose
    <*> optional pOutputDir
    <*> pure Nothing
    <*> optional pTitle
    <*> many pTitleAnnotation
    <*> parseOptional presentation pPresentation
    <*> parseOptional estimator pEstimator
    <*> parseOptional threshold pThreshold
    <*> parseOptional failureField (optional pFailureField)
    <*> parseOptional diffStrategy pDiffStrategy
    <*> parseOptional omitBaseline pOmitBaseline
    <*> pure (selectFields defaultConfig)
    <*> pure (fieldRanges defaultConfig)
    <*> pure (fieldTicks defaultConfig)
    <*> pure (classifyBenchmark defaultConfig)
    <*> pure (selectGroups defaultConfig)
    <*> pure (selectBenchmarks defaultConfig)

-------------------------------------------------------------------------------
-- "report" subcommand
-------------------------------------------------------------------------------

data ReportOpts = ReportOpts
    { reportInput :: FilePath
    , reportOutput :: Maybe FilePath
    }

pInputFile :: Parser FilePath
pInputFile = argument str (metavar "INPUT-FILE.CSV")

pOutputFile :: Parser FilePath
pOutputFile = strOption
  (long "output"
  <> short 'o'
  <> help "Output file")

pReportOpts :: Parser ReportOpts
pReportOpts = ReportOpts
    <$> pInputFile
    <*> optional pOutputFile

cmdReport :: ReportOpts -> Config -> IO ()
cmdReport ReportOpts{..} cfg = report reportInput reportOutput cfg

-------------------------------------------------------------------------------
-- "graph" subcommand
-------------------------------------------------------------------------------

data GraphOpts = GraphOpts
    { graphInput :: FilePath
    , graphOutput :: FilePath
    }

pOutputArg :: Parser FilePath
pOutputArg = argument str (metavar "OUTPUT-FILE-PREFIX")

pGraphOpts :: Parser GraphOpts
pGraphOpts = GraphOpts
    <$> pInputFile
    <*> pOutputArg

cmdGraph :: GraphOpts -> Config -> IO ()
cmdGraph GraphOpts{..} cfg = graph graphInput graphOutput cfg

-------------------------------------------------------------------------------
-- Build and run a subcommand parser
-------------------------------------------------------------------------------

cmdLineParser :: Parser Config -> IO ()
cmdLineParser p = do
    (cfg, handler) <- simpleOptions $(simpleVersion version)
        ("Generate reports and graphs from gauge or criterion output.\n" ++
        "Default values are shown as *DEFAULT*.")
        "" p cmds

    handler cfg

    where cmds = do
            addCommand "report"
                       "Generate a text report"
                       cmdReport
                       pReportOpts
            addCommand "graph"
                       "Generate a graphical report"
                       cmdGraph
                       pGraphOpts

main :: IO ()
main = cmdLineParser pConfig
