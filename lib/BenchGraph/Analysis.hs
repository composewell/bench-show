{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : BenchGraph.Analysis
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--               (c) 2018 Composewell Technologies
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC

module BenchGraph.Analysis
    ( OutlierEffect(..)
    , OutlierVariance(..)
    , countOutliers
    , AnalyzedField(..)
    , BenchmarkIterMatrix(..)
    , BenchmarkAnalyzedMatrix(..)
    , analyzeIterMatrix
    , analyzeBenchmark
    , isMaxField
    , rescaleIteration
    ) where

import Control.Applicative
import Data.Char (toLower)
import Data.Data (Data, Typeable)
import Data.Int (Int64)
import Data.List (transpose)
import Data.Traversable
import GHC.Generics (Generic)
import Statistics.Function (sort)
import Statistics.Quantile (weightedAvg)
import Statistics.Regression (bootstrapRegress, olsRegress)
import Statistics.Resampling (Estimator(..), resample)
import Statistics.Resampling.Bootstrap (bootstrapBCA)
import Statistics.Sample.KernelDensity (kde)
import Statistics.Types (Sample, Estimate(..), ConfInt(..), cl95, CL)
import System.Random.MWC (GenIO, createSystemRandom)
import Prelude hiding (sequence, mapM)

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

-------------------------------------------------------------------------------
-- Outliers
-------------------------------------------------------------------------------

-- | Outliers from sample data, calculated using the boxplot
-- technique.
data Outliers = Outliers {
      samplesSeen :: !Int64
    , lowSevere   :: !Int64
    -- ^ More than 3 times the interquartile range (IQR) below the
    -- first quartile.
    , lowMild     :: !Int64
    -- ^ Between 1.5 and 3 times the IQR below the first quartile.
    , highMild    :: !Int64
    -- ^ Between 1.5 and 3 times the IQR above the third quartile.
    , highSevere  :: !Int64
    -- ^ More than 3 times the IQR above the third quartile.
    } deriving (Eq, Show, Typeable, Data, Generic)

-- | A description of the extent to which outliers in the sample data
-- affect the sample mean and standard deviation.
data OutlierEffect = Unaffected -- ^ Less than 1% effect.
                   | Slight     -- ^ Between 1% and 10%.
                   | Moderate   -- ^ Between 10% and 50%.
                   | Severe     -- ^ Above 50% (i.e. measurements
                                -- are useless).
                     deriving (Eq, Ord, Show, Typeable, Data, Generic)

outliersEmpty :: Outliers
outliersEmpty = Outliers 0 0 0 0 0

addOutliers :: Outliers -> Outliers -> Outliers
addOutliers (Outliers s a b c d) (Outliers t w x y z) =
    Outliers (s+t) (a+w) (b+x) (c+y) (d+z)
{-# INLINE addOutliers #-}

-- | Analysis of the extent to which outliers in a sample affect its
-- standard deviation (and to some extent, its mean).
data OutlierVariance = OutlierVariance {
      ovEffect   :: !OutlierEffect
    -- ^ Qualitative description of effect.
    , ovDesc     :: !String
    -- ^ Brief textual description of effect.
    , ovFraction :: !Double
    -- ^ Quantitative description of effect (a fraction between 0 and 1).
    } deriving (Eq, Show, Typeable, Data, Generic)

-- | Classify outliers in a data set, using the boxplot technique.
classifyOutliers :: Sample -> Outliers
classifyOutliers sa = U.foldl' ((. outlier) . addOutliers) outliersEmpty ssa
    where outlier e = Outliers {
                        samplesSeen = 1
                      , lowSevere = if e <= loS && e < hiM then 1 else 0
                      , lowMild = if e > loS && e <= loM then 1 else 0
                      , highMild = if e >= hiM && e < hiS then 1 else 0
                      , highSevere = if e >= hiS && e > loM then 1 else 0
                      }
          !loS = q1 - (iqr * 3)
          !loM = q1 - (iqr * 1.5)
          !hiM = q3 + (iqr * 1.5)
          !hiS = q3 + (iqr * 3)
          q1   = weightedAvg 1 4 ssa
          q3   = weightedAvg 3 4 ssa
          ssa  = sort sa
          iqr  = q3 - q1

-- | Compute the extent to which outliers in the sample data affect
-- the sample mean and standard deviation.
outlierVariance
  :: Estimate ConfInt Double -- ^ Bootstrap estimate of sample mean.
  -> Estimate ConfInt Double -- ^ Bootstrap estimate of sample
                                 --   standard deviation.
  -> Double                      -- ^ Number of original iterations.
  -> OutlierVariance
outlierVariance µ σ a = OutlierVariance effect desc varOutMin
  where
    ( effect, desc ) | varOutMin < 0.01 = (Unaffected, "no")
                     | varOutMin < 0.1  = (Slight,     "slight")
                     | varOutMin < 0.5  = (Moderate,   "moderate")
                     | otherwise        = (Severe,     "severe")
    varOutMin = (minBy varOut 1 (minBy cMax 0 µgMin)) / σb2
    varOut c  = (ac / a) * (σb2 - ac * σg2) where ac = a - c
    σb        = estPoint σ
    µa        = estPoint µ / a
    µgMin     = µa / 2
    σg        = min (µgMin / 4) (σb / sqrt a)
    σg2       = σg * σg
    σb2       = σb * σb
    minBy f q r = min (f q) (f r)
    cMax x    = fromIntegral (floor (-2 * k0 / (k1 + sqrt det)) :: Int)
      where
        k1    = σb2 - a * σg2 + ad
        k0    = -a * ad
        ad    = a * d
        d     = k * k where k = µa - x
        det   = k1 * k1 - 4 * σg2 * k0

-- | Count the total number of outliers in a sample.
countOutliers :: Outliers -> Int64
countOutliers (Outliers _ a b c d) = a + b + c + d
{-# INLINE countOutliers #-}

-------------------------------------------------------------------------------
-- Linear regression
-------------------------------------------------------------------------------

resampleCount :: Int
resampleCount = 1000

confidence :: CL Double
confidence = cl95

regress
    :: GenIO
    -> [(Int, [Double])]
    -> IO [(Estimate ConfInt Double, Estimate ConfInt Double)]
regress randGen iterValues = do
    -- perform ordinary least squares regression for each field
    -- the only predictor we use is the number of iterations
    let iterVectors = [U.fromList $ map (fromIntegral . fst) iterValues]
        regressWithIters = bootstrapRegress randGen resampleCount
                                confidence olsRegress iterVectors

    res <- mapM regressWithIters
            $ map (U.fromList)
            $ transpose
            $ map snd iterValues
    return $ map (\(v,r2) -> (head (G.toList v), r2)) res

-------------------------------------------------------------------------------
-- Mean and std deviation by boostrap resampling
-------------------------------------------------------------------------------

estimateMeanAndStdDev
    :: GenIO
    -> [U.Vector Double]
    -> IO [(Estimate ConfInt Double, Estimate ConfInt Double)]
estimateMeanAndStdDev randGen vectors = do
    resamps <- mapM (resample randGen [Mean,StdDev] resampleCount) vectors
    return $ fmap (\[mn,dev] -> (mn, dev))
        $ getZipList
        $ bootstrapBCA confidence
            <$> ZipList vectors
            <*> ZipList resamps

-------------------------------------------------------------------------------
-- Statistical analysis of benchmark iterations
-------------------------------------------------------------------------------

-- By default the fields are considered "scaled" fields that is
-- they scale by iterations. However in case of maxrss field it is
-- a max value across the experiment and does not scale by
-- iterations, in this case we just need to take a mean or max
-- without scaling.
isMaxField :: String -> Bool
isMaxField fieldName = map toLower fieldName == "maxrss"

rescaleIteration :: [String] -> (Int, [Double]) -> [Double]
rescaleIteration fnames (iter, vals) =
    zipWith ($) (map ($ iter) foldFields) vals

    where

    getMeanOrMax fname i val =
        if isMaxField fname
        then val
        else val / fromIntegral i

    foldFields = map getMeanOrMax fnames

data AnalyzedField = AnalyzedField
    { analyzedMean       :: Estimate ConfInt Double
    , analyzedStdDev     :: Estimate ConfInt Double
    , analyzedRegCoeff   :: Estimate ConfInt Double
    , analyzedRegRSq     :: Estimate ConfInt Double
    , analyzedOutlierVar :: !OutlierVariance
    , analyzedOutliers   :: !Outliers
    , analyzedKDE        :: !(U.Vector Double, U.Vector Double)
    } deriving Show

-- XXX do not regress a max field
--
-- | Perform an analysis of a measurement.
analyzeBenchmark :: GenIO
                 -> [String]
                 -> [(Int, [Double])]
                 -> IO [AnalyzedField]
analyzeBenchmark randGen cols iterValues = do
    let sampleCnt = length iterValues
        vectors = map U.fromList
            $ transpose
            $ map (rescaleIteration cols) iterValues

    (coeffs, r2s) <- fmap unzip $ regress randGen iterValues
    (means, devs) <- fmap unzip $ estimateMeanAndStdDev randGen vectors
    let ovs = getZipList
                $ outlierVariance
                    <$> ZipList means
                    <*> ZipList devs
                    <*> pure (fromIntegral sampleCnt)
        outliers = getZipList $ classifyOutliers <$> ZipList vectors
        kdes = map (kde 128) vectors

    return $ getZipList $ AnalyzedField
        <$> ZipList means
        <*> ZipList devs
        <*> ZipList coeffs
        <*> ZipList r2s
        <*> ZipList ovs
        <*> ZipList outliers
        <*> ZipList kdes

data BenchmarkIterMatrix = BenchmarkIterMatrix
    { iterColNames  :: ![String]
    -- (Benchmark, [(iters, columns)])
    , iterRowValues :: ![(String, [(Int, [Double])])]
    } deriving Show

data BenchmarkAnalyzedMatrix = BenchmarkAnalyzedMatrix
    { anColNames  :: ![String]
    -- (Benchmark, [(iters, columns)])
    , anRowValues :: ![(String, [AnalyzedField])]
    } deriving Show

analyzeIterMatrix :: BenchmarkIterMatrix -> IO BenchmarkAnalyzedMatrix
analyzeIterMatrix BenchmarkIterMatrix {..} = do
    randGen <- createSystemRandom
    let analyzeIterVals = analyzeBenchmark randGen iterColNames
    rows <- mapM (\(s,xs) -> analyzeIterVals xs >>= return . (s,)) iterRowValues
    return $ BenchmarkAnalyzedMatrix
        { anColNames = iterColNames
        , anRowValues = rows
        }
