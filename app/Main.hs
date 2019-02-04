{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import BenchShow
import Options.Applicative
import Data.Char (toLower)
import Data.Semigroup ((<>))

data OpType = Report | Graph FilePath

data CLIOptions = CLIOptions
  { cliConfig   :: Config
  , opType      :: OpType
  , inputFile   :: FilePath
  }

ograph :: Parser OpType
ograph = Graph <$> strOption
  ( long "gfile"
  <> short 'g'
  <> help "Provide a output file name for graph" )

oreport :: Parser OpType
oreport = flag' Report
  ( long "report"
  <> short 'r'
  <> help "Produce a report")

pOptype :: Parser OpType
pOptype = oreport <|> ograph

pInputFile :: Parser FilePath
pInputFile = strOption
  (long "ifile"
  <> short 'i'
  <> help "Input file")

pVerbose :: Parser Bool
pVerbose = switch ( long "verbose"
                    <> short 'v'
                    <> help "Provide more details in the report" )

pTitle :: Parser (Maybe String)
pTitle = optional $ strOption
  ( long "title"
    <> short 't'
    <> metavar "Report title"
    <> help "Report title, information like plotted field name etc." )

pTitleField :: Parser TitleAnnotation
pTitleField = flag' TitleField
  ( long "title-field"
    <> help "Additional annotation for title of report" )

pTitleEstimator :: Parser TitleAnnotation
pTitleEstimator = flag' TitleEstimator
  ( long "title-estimator"
    <> help "Additional annotation for title of report" )

pTitleDiff :: Parser TitleAnnotation
pTitleDiff = flag' TitleDiff
  ( long "title-diff"
    <> help "Additional annotation for title of report" )
  <|> pure TitleField

pTitleAnnotation :: Parser [TitleAnnotation]
pTitleAnnotation = (: []) <$> ( pTitleField
                                <|> pTitleEstimator
                                <|> pTitleDiff )

pOutputDir :: Parser (Maybe FilePath)
pOutputDir = optional $ strOption
  ( long "output-dir" <> short 'o'
    <> metavar "Output Directory"
    <> value "charts"
    <> help "Directory where output graph or report file should be placed" )

pGroupStyle :: Parser GroupStyle
pGroupStyle = flag' Absolute
  ( long "absolute"
    <> help "Show absolute field values for all groups" )
  <|> flag' Diff
  ( long "diff"
    <> help "Show baseline group values" )
  <|> flag' Percent
  ( long "percent"
    <> help "Show baseline group values as 100%" )
  <|> flag' PercentDiff
  ( long "percent-diff"
    <> help "Show baseline group values as % difference" )
  <|> pure Absolute

pPresentation :: Parser Presentation
pPresentation = flag' Solo
  ( long "solo" <> help "Solo Presentation")
  <|> Groups <$> pGroupStyle
  <|> flag' Fields
  ( long "fields" <> help "Fields Presentation" )
  <|> pure Solo

pEstimator :: Parser Estimator
pEstimator = flag' Median
  ( long "median"
    <> help "Report median, outliers & outlier variance" )
  <|> flag' Mean
  ( long "mean"
    <> help "Report mean & standard deviation" )
  <|> flag' Regression
  ( long "regression"
    <> help "Report coefficient of regression")
  <|> pure Median

pDiffStrategy :: Parser DiffStrategy
pDiffStrategy = flag' MinEstimator
  ( long "single-estimator"
    <> help "Use single estimator to compute difference" )
  <|> flag' MinEstimator
  ( long "min-estimator"
    <> help "Use mean, median & regression estimators" )
  <|> pure MinEstimator

pThreshold :: Parser Word
pThreshold = option auto ( long "threshold"
                          <> short 't'
                          <> value 3
                          <> help "Minimum % difference between two runs" )

pSelectFields :: Parser ([String] -> [String])
pSelectFields = pure (filter (flip elem
                             ["time", "mean", "maxrss"]. map toLower))

pFieldRanges :: Parser [(String, Double, Double)]
pFieldRanges = pure []

pFieldTicks :: Parser [(String, FieldTick)]
pFieldTicks = pure []

pClassifyBenchMark :: Parser (String -> Maybe (String, String))
pClassifyBenchMark = pure $ Just . ("default",)

pSelectGroups :: Parser ([(String, Int)] -> [(String, Int)])
pSelectGroups = pure id

pSelectBenchmarks :: Parser ((SortColumn -> Either String [(String, Double)])
                  -> [String])
pSelectBenchmarks = pure (\f -> either error (map fst) $ f (ColumnIndex 0))

pConfig :: Parser Config
pConfig = Config <$> pVerbose
          <*> pOutputDir <*> pTitle
          <*> pTitleAnnotation <*> pPresentation
          <*> pEstimator <*> pThreshold <*> pDiffStrategy
          <*> pSelectFields <*> pFieldRanges <*> pFieldTicks
          <*> pClassifyBenchMark <*> pSelectGroups <*> pSelectBenchmarks

pCliOptions :: Parser CLIOptions
pCliOptions = CLIOptions <$> pConfig <*> pOptype <*> pInputFile

opts :: ParserInfo CLIOptions
opts = info (pCliOptions <**> helper)
       (fullDesc <> progDesc "Bench Show CLI"
       <> header "Command line executable for bench-show")

main :: IO ()
main = do
  (CLIOptions cliConfig cliOptype cliInputFile) <- execParser opts

  case cliOptype of
    Report -> report cliInputFile Nothing cliConfig
    Graph outfile -> graph cliInputFile outfile cliConfig
