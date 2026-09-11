{-# LANGUAGE NamedFieldPuns #-}

-- | Pure, versioned commercial-reputation calculations for merchandise
-- stores. Keeping this module free of database and HTTP concerns makes the
-- formula reproducible in backfills, sensitivity tests and incident reviews.
module TDF.Merch.Reputation
  ( CommercialFormula(..)
  , CommercialConfidence(..)
  , OperationalResponsibility(..)
  , VerifiedRating(..)
  , OperationalObservation(..)
  , initialCommercialFormula
  , validateCommercialFormula
  , temporalWeight
  , bayesianRating
  , commercialStoreScore
  , commercialConfidence
  , roundPublicRating
  , boundedReputationRankingContribution
  ) where

data CommercialFormula = CommercialFormula
  { formulaVersion :: String
  , priorMean :: Double
  , priorStrength :: Double
  , reviewWeight :: Double
  , operationalWeight :: Double
  , halfLifeDays :: Double
  , minimumEvaluableOrders :: Int
  , limitedEvidenceReviewCount :: Int
  , strongEvidenceReviewCount :: Int
  , rankingContributionCap :: Double
  } deriving (Eq, Show)

data CommercialConfidence
  = NewStore
  | LimitedEvidence
  | ModerateEvidence
  | StrongEvidence
  deriving (Eq, Ord, Show)

data OperationalResponsibility
  = SellerResponsible
  | CourierResponsible
  | BuyerResponsible
  | PlatformResponsible
  | UnknownResponsibility
  deriving (Eq, Show)

data VerifiedRating = VerifiedRating
  { ratingValue :: Double
  , ratingAgeDays :: Double
  } deriving (Eq, Show)

data OperationalObservation = OperationalObservation
  { operationalOutcome :: Double
  , operationalEvidenceQuality :: Double
  , operationalAgeDays :: Double
  , operationalResponsibility :: OperationalResponsibility
  } deriving (Eq, Show)

-- | Initial governed formula. Buyer experience remains dominant; operational
-- evidence can move the result by at most fifteen percent. The five-order
-- threshold controls publication, not the Bayesian prior.
initialCommercialFormula :: CommercialFormula
initialCommercialFormula = CommercialFormula
  { formulaVersion = "merch-commercial-bayes-v1"
  , priorMean = 3.5
  , priorStrength = 5
  , reviewWeight = 0.85
  , operationalWeight = 0.15
  , halfLifeDays = 730
  , minimumEvaluableOrders = 5
  , limitedEvidenceReviewCount = 10
  , strongEvidenceReviewCount = 30
  , rankingContributionCap = 0.12
  }

validateCommercialFormula :: CommercialFormula -> Either String CommercialFormula
validateCommercialFormula formula@CommercialFormula
  { priorMean
  , priorStrength
  , reviewWeight
  , operationalWeight
  , halfLifeDays
  , minimumEvaluableOrders
  , limitedEvidenceReviewCount
  , strongEvidenceReviewCount
  , rankingContributionCap
  }
  | priorMean < 1 || priorMean > 5 = Left "prior mean must be between 1 and 5"
  | priorStrength <= 0 = Left "prior strength must be positive"
  | reviewWeight < 0.8 = Left "buyer reviews must retain at least 80% influence"
  | operationalWeight < 0 || operationalWeight > 0.2 =
      Left "operational influence must stay between 0% and 20%"
  | abs (reviewWeight + operationalWeight - 1) > 0.000001 =
      Left "review and operational weights must total 1"
  | halfLifeDays < 365 = Left "temporal decay must remain moderate"
  | minimumEvaluableOrders < 5 = Left "numeric publication requires at least five orders"
  | limitedEvidenceReviewCount <= 0
      || strongEvidenceReviewCount <= limitedEvidenceReviewCount =
      Left "confidence evidence thresholds must be positive and increasing"
  | rankingContributionCap < 0 || rankingContributionCap > 0.2 =
      Left "ranking influence must stay between 0% and 20%"
  | otherwise = Right formula

temporalWeight :: CommercialFormula -> Double -> Double
temporalWeight CommercialFormula{halfLifeDays} ageDays =
  0.5 ** (max 0 ageDays / halfLifeDays)

-- | Every verified order contributes equally before moderate time decay. No
-- purchase amount is accepted by this API, so basket value cannot buy weight.
bayesianRating :: CommercialFormula -> [VerifiedRating] -> Maybe Double
bayesianRating _ [] = Nothing
bayesianRating formula@CommercialFormula{priorMean, priorStrength} ratings =
  Just ((priorMean * priorStrength + weightedTotal) / (priorStrength + totalWeight))
  where
    weightedRatings =
      [ (clamp 1 5 ratingValue, temporalWeight formula ratingAgeDays)
      | VerifiedRating{ratingValue, ratingAgeDays} <- ratings
      ]
    weightedTotal = sum [value * weight | (value, weight) <- weightedRatings]
    totalWeight = sum (map snd weightedRatings)

-- | Courier-, buyer- and platform-attributed evidence never penalizes the
-- seller. When no attributable operational evidence exists, the buyer score
-- stands on its own rather than receiving an invented neutral value.
commercialStoreScore
  :: CommercialFormula
  -> Int
  -> [VerifiedRating]
  -> [OperationalObservation]
  -> Maybe Double
commercialStoreScore formula@CommercialFormula
  { reviewWeight
  , operationalWeight
  , minimumEvaluableOrders
  } evaluableOrders ratings observations
  | evaluableOrders < minimumEvaluableOrders = Nothing
  | otherwise = do
      reviewScore <- bayesianRating formula ratings
      case sellerOperationalScore of
        Nothing -> pure (roundPublicRating reviewScore)
        Just operationalScore -> pure . roundPublicRating $
          reviewWeight * reviewScore + operationalWeight * operationalScore
  where
    attributable =
      [ ( clamp 0 1 operationalOutcome
        , clamp 0 1 operationalEvidenceQuality
            * temporalWeight formula operationalAgeDays
        )
      | OperationalObservation
          { operationalOutcome
          , operationalEvidenceQuality
          , operationalAgeDays
          , operationalResponsibility = SellerResponsible
          } <- observations
      ]
    evidenceWeight = sum (map snd attributable)
    sellerOperationalScore
      | evidenceWeight <= 0 = Nothing
      | otherwise = Just
          (1 + 4 * sum [outcome * weight | (outcome, weight) <- attributable] / evidenceWeight)

commercialConfidence :: CommercialFormula -> Int -> Int -> CommercialConfidence
commercialConfidence CommercialFormula
  {minimumEvaluableOrders,limitedEvidenceReviewCount,strongEvidenceReviewCount}
  evaluableOrders evidenceCount
  | evaluableOrders < minimumEvaluableOrders = NewStore
  | evidenceCount < limitedEvidenceReviewCount = LimitedEvidence
  | evidenceCount < strongEvidenceReviewCount = ModerateEvidence
  | otherwise = StrongEvidence

roundPublicRating :: Double -> Double
roundPublicRating value = fromIntegral (floor (clamp 1 5 value * 10 + 0.5) :: Int) / 10

-- | Reputation is one bounded ranking input. The caller supplies the
-- normalized reputation signal; this function cannot let it exceed the
-- governed contribution cap.
boundedReputationRankingContribution :: CommercialFormula -> Double -> Double
boundedReputationRankingContribution CommercialFormula{rankingContributionCap} signal =
  rankingContributionCap * clamp 0 1 signal

clamp :: Ord a => a -> a -> a -> a
clamp lower upper = max lower . min upper
