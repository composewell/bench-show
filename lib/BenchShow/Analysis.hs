{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : BenchShow.Analysis
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--               (c) 2018 Composewell Technologies
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC

module BenchShow.Analysis
    ( OutlierEffect(..)
    , OutlierVariance(..)
    , countOutliers
    , Estimator(..)
    , AnalyzedField(..)
    , getAnalyzedValue
    , BenchmarkMatrix(..)
    , BenchmarkIterMatrix(..)
    , foldBenchmark
    , filterSamples
    , isMaxField
    ) where

import Control.Applicative
import Data.Char (toLower)
import Data.Data (Data, Typeable)
import Data.Int (Int64)
import Data.List (elemIndex, transpose)
import Data.Maybe (fromMaybe)
import Data.Traversable
import GHC.Generics (Generic)
import Statistics.Function (sort)
import Statistics.Quantile (weightedAvg)
import Statistics.Regression (bootstrapRegress, olsRegress)
import Statistics.Resampling (resample)
import Statistics.Resampling.Bootstrap (bootstrapBCA)
import Statistics.Sample (mean, stdDev)
import Statistics.Sample.KernelDensity (kde)
import Statistics.Types (Sample, Estimate(..), ConfInt(..), cl95, CL)
import System.Random.MWC (GenIO, createSystemRandom)
import Prelude hiding (sequence, mapM)

import qualified Statistics.Resampling as St
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
    where outlier e = Outliers
                { samplesSeen = 1
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
  :: Double -- ^ mean
  -> Double -- ^ standard deviation.
  -> Double -- ^ Number of original iterations.
  -> OutlierVariance
outlierVariance µ σ a = OutlierVariance effect desc varOutMin
    where
    µa    = µ / a
    µgMin = µa / 2
    σg2   = σg * σg where σg = min (µgMin / 4) (σ / sqrt a)
    σ2    = σ * σ
    varOut c  = (ac / a) * (σ2 - ac * σg2) where ac = a - c
    cMax x    = fromIntegral (floor (-2 * k0 / (k1 + sqrt det)) :: Int)
        where
        ad = a * d
            where
            d = k * k
            k = µa - x
        k0    = -a * ad
        k1    = σ2 - a * σg2 + ad
        det   = k1 * k1 - 4 * σg2 * k0

    minBy f q r = min (f q) (f r)
    varOutMin = if σ2 == 0
                then 0
                else (minBy varOut 1 (minBy cMax 0 µgMin)) / σ2

    (effect, desc) | varOutMin < 0.01 = (Unaffected, "no")
                   | varOutMin < 0.1  = (Slight,     "slight")
                   | varOutMin < 0.5  = (Moderate,   "moderate")
                   | otherwise        = (Severe,     "severe")

-- | Count the total number of outliers in a sample.
countOutliers :: Outliers -> Int64
countOutliers (Outliers _ a b c d) = a + b + c + d
{-# INLINE countOutliers #-}

-------------------------------------------------------------------------------
-- Linear regression
-------------------------------------------------------------------------------

useRegression :: Bool
useRegression = True

useBootstrap :: Bool
useBootstrap = True

resampleCount :: Int
resampleCount = 1000

confidence :: CL Double
confidence = cl95

regress
    :: GenIO
    -> Int  -- index of the iters field, we return the coefficient of only the
            -- iters field
    -> [String] -- responder column names
    -> [([Double], [Double])]
    -> IO [Maybe (Estimate ConfInt Double, Estimate ConfInt Double)]
regress randGen i rcols samples = do
    -- perform ordinary least squares regression for each field
    -- the main predictor is the number of iterations
    let predVectors = map U.fromList $ transpose $ map fst samples
        regressWithIters = mapM (bootstrapRegress randGen resampleCount
                                confidence olsRegress predVectors)

    let avoidMaxFields name vec =
            if isMaxField name
            then Nothing
            else Just vec
    let respVectors = map U.fromList $ transpose $ map snd samples
    res <- mapM regressWithIters (zipWith avoidMaxFields rcols respVectors)
    return $ map (fmap (\(v,r2) -> ((G.toList v) !! i, r2))) res

-------------------------------------------------------------------------------
-- Mean and std deviation by boostrap resampling
-------------------------------------------------------------------------------

estimateMeanAndStdDev
    :: GenIO
    -> [U.Vector Double]
    -> IO [(Estimate ConfInt Double, Estimate ConfInt Double)]
estimateMeanAndStdDev randGen vectors = do
    let resamp = resample randGen [St.Mean, St.StdDev] resampleCount
    res <- mapM resamp vectors
    return $ fmap (\[mn,dev] -> (mn, dev))
        $ getZipList
        $ bootstrapBCA confidence
            <$> ZipList vectors
            <*> ZipList res

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

rescaleIteration :: Int -> [String] -> ([Double], [Double]) -> [Double]
rescaleIteration idx rcols (pvals, vals) =
    let iter = pvals !! idx
    in zipWith ($) (map ($ iter) foldFields) vals

    where

    getMeanOrMax fname i val =
        if isMaxField fname
        then val
        else val / i

    foldFields = map getMeanOrMax rcols

data AnalyzedField = AnalyzedField
    { analyzedMean       :: !Double
    , analyzedStdDev     :: !Double

    , analyzedMedian     :: !Double
    , analyzedOutliers   :: !Outliers
    , analyzedOutlierVar :: !OutlierVariance
    , analyzedKDE        :: !(U.Vector Double, U.Vector Double)

    , analyzedRegCoeff   :: Maybe (Estimate ConfInt Double)
    , analyzedRegRSq     :: Maybe (Estimate ConfInt Double)
    } deriving Show

-- | The statistical estimator used to arrive at a single value for a
-- benchmark when samples from multiple experiments are available.
--
-- @since 0.2.0
data Estimator =
      Median        -- ^ Report the median, outliers and outlier variance using
                    -- box-plot method. This is the most robust indicator
                    -- with respect to outliers when successive runs of
                    -- benchmarks are compared.
    | Mean          -- ^ Report the mean and the standard deviation from the
                    -- mean. This is less robust than median but more precise.
    | Regression    -- ^ Report the coefficient of regression, discarding the
                    -- constant factor, arrived at by linear regression using
                    -- ordinary least square method.  The R-square
                    -- goodness-of-fit estimate is also reported.  It works
                    -- better when larger number of samples are taken.  This
                    -- cannot be used when the number of samples is less than
                    -- 2, in that case a mean value is reported instead.
    deriving (Eq, Show)

getAnalyzedValue :: Estimator -> AnalyzedField -> Double
getAnalyzedValue estimator AnalyzedField{..} =
    case estimator of
        Median -> analyzedMedian
        Mean -> analyzedMean
        Regression ->
            case analyzedRegCoeff of
                Nothing -> analyzedMean
                Just x -> estPoint x

-- | Perform an analysis of a measurement.
analyzeBenchmark :: GenIO
                 -> [String]
                 -> [String]
                 -> [([Double], [Double])]
                 -> IO [AnalyzedField]
analyzeBenchmark randGen pcols rcols samples = do
    let sampleCnt = length samples
        i = fromMaybe (error "bug") $ elemIndex "iters" pcols
        vectors = map U.fromList
            $ transpose
            $ map (rescaleIteration i rcols) samples

    (coeffs, r2s) <-
        -- olsRegress fails if there are fewer samples than predictors
        if useRegression && length samples >= (length pcols + 1)
        then do
            let f (Just (x, y)) = (Just x, Just y)
                f Nothing = (Nothing, Nothing)
            fmap (unzip . map f) $ regress randGen i rcols samples
        else do
            let n = length rcols
            return (replicate n Nothing, replicate n Nothing)

    (means, devs) <-
        if useBootstrap
        then do
            (ms, ds) <- fmap unzip $ estimateMeanAndStdDev randGen vectors
            return (map estPoint ms, map estPoint ds)
        else do
            -- Even for max fields (e.g. maxrss) we take the mean
            let ms = map mean vectors
                ds = map stdDev vectors
            return (ms, ds)

    let len = U.length $ head vectors
        median v = (sort v) U.! (len `div` 2)
        medians = map median vectors
        outliers = getZipList $ classifyOutliers <$> ZipList vectors
        outlierVars = getZipList
                $ outlierVariance
                    <$> ZipList means
                    <*> ZipList devs
                    <*> pure (fromIntegral sampleCnt)
        kdes = map (kde 128) vectors

    return $ getZipList $ AnalyzedField
        <$> ZipList means
        <*> ZipList devs

        <*> ZipList medians
        <*> ZipList outliers
        <*> ZipList outlierVars
        <*> ZipList kdes

        <*> ZipList coeffs
        <*> ZipList r2s

-- predictor matrix
data BenchmarkIterMatrix = BenchmarkIterMatrix
    { iterPredColNames  :: ![String]  -- predictor column names
    , iterRespColNames  :: ![String]  -- responder column names
    -- (Benchmark, [(predictor columns, responder columns)])
    , iterRowValues :: ![(String, [([Double], [Double])])]
    } deriving Show

-- Stored in row major order
data BenchmarkMatrix = BenchmarkMatrix
    { colNames  :: ![String]
    , rowValues :: ![(String, [AnalyzedField])] -- (Benchmark, columns)
    } deriving Show

foldBenchmark :: BenchmarkIterMatrix -> IO BenchmarkMatrix
foldBenchmark BenchmarkIterMatrix{..} = do
    randGen <- createSystemRandom
    rows <- mapM (foldIters randGen) iterRowValues
    return $ BenchmarkMatrix
        { colNames = iterRespColNames
        , rowValues = rows
        }

    where

    foldIters randGen (name, vals) = do
            vals' <- analyzeBenchmark randGen iterPredColNames
                                      iterRespColNames vals
            return (name, vals')

-- take top samples
-- XXX take equivalent iterations across multiple groups
filterSamples :: BenchmarkIterMatrix -> BenchmarkIterMatrix
filterSamples matrix@BenchmarkIterMatrix{..} =
    matrix
        {-
        {
          iterRowValues = map filterIters iterRowValues
        }

    where

    iterIndex = fromMaybe undefined
        $ elemIndex "iters" (map (map toLower) iterPredColNames)
    nivcswIndex = fromMaybe undefined
        $ elemIndex "nivcsw" (map (map toLower) iterPredColNames)
    filterIters (name, vals) =
        let vals'' = take 50 $ reverse $ sortBy (comparing ((!! iterIndex) .  fst)) vals
        let vals' = filter (\(x,_) -> x !! nivcswIndex < 10) vals
            vals'' =
                if null vals'
                then trace "null after filter" vals
                else vals'
        in (name, vals'')
        -}
