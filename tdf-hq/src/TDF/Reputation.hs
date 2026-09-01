{-# LANGUAGE NamedFieldPuns #-}

-- | Deterministic, versioned primitives shared by the contextual-reputation
-- writer and the asynchronous aggregate worker.  This module deliberately has
-- no database dependency: the same input and formula version always produces
-- the same output, which makes simulations and historical replays auditable.
module TDF.Reputation
  ( FormulaVersion(..)
  , Confidence(..)
  , rankOrderCentroid
  , normalizeManualWeights
  , publicScore
  , confidenceFor
  , clamp
  ) where

newtype FormulaVersion = FormulaVersion String
  deriving (Eq, Show)

data Confidence = Forming | Low | Moderate | High
  deriving (Eq, Ord, Show)

-- | Rank-order centroid (ROC) weights.  A person provides only an ordinal
-- preference; ROC is the mean weight across all cardinal vectors compatible
-- with that order.  It is monotone, deterministic and totals exactly 100 once
-- represented as a final remainder rather than rounded independently.
rankOrderCentroid :: Int -> [Double]
rankOrderCentroid n
  | n <= 0 = []
  | otherwise = prefix <> [100 - sum prefix]
  where
    raw rank = 100 * sum [1 / fromIntegral position | position <- [rank .. n]] / fromIntegral n
    prefix = map raw [1 .. n - 1]

-- | Advanced mode accepts non-negative inputs but keeps the same invariant as
-- ROC: a higher-ranked category cannot receive a lower normalized weight.
normalizeManualWeights :: [Double] -> Either String [Double]
normalizeManualWeights values
  | null values = Right []
  | any (< 0) values = Left "weights must be non-negative"
  | not (and (zipWith (>=) values (drop 1 values))) = Left "weights must respect category priority"
  | total <= 0 = Left "at least one weight must be positive"
  | otherwise = Right (prefix <> [100 - sum prefix])
  where
    total = sum values
    normalized = map (100 *) (map (/ total) values)
    prefix = take (length values - 1) normalized

-- | Public reputation uses a conservative beta prior centred at 50.  The
-- caller supplies only verified, risk-cleared, time-decayed observations in
-- the inclusive 0..100 range.  A single reviewer is capped upstream; this
-- shrinkage prevents small samples from looking precise.
publicScore :: Double -> Double -> [Double] -> Double
publicScore priorStrength priorMean observations
  | priorStrength <= 0 = error "priorStrength must be positive"
  | otherwise = clamp 0 100 ((priorStrength * clamp 0 100 priorMean + sum clipped) / denominator)
  where
    clipped = map (clamp 0 100) observations
    denominator = priorStrength + fromIntegral (length clipped)

confidenceFor :: Int -> Confidence
confidenceFor n
  | n < 3 = Forming
  | n < 8 = Low
  | n < 25 = Moderate
  | otherwise = High

clamp :: Ord a => a -> a -> a -> a
clamp lower upper = max lower . min upper
