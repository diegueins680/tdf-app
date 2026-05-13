{-# LANGUAGE OverloadedStrings #-}

module TDF.Pipelines
  ( pipelineStages
  , pipelineTypeSlug
  , parsePipelineType
  , canonicalStage
  , defaultStage
  ) where

import           Data.Char
  ( GeneralCategory (Format, LineSeparator, ParagraphSeparator)
  , generalCategory
  , isAlphaNum
  , isControl
  )
import           Data.List      (find)
import           Data.Text      (Text)
import qualified Data.Text      as T

import           TDF.Models     (ServiceKind(..))

pipelineStages :: ServiceKind -> [Text]
pipelineStages Recording =
  [ "Inquiry"
  , "Quoted"
  , "Scheduled"
  , "In Session"
  , "Editing"
  , "Approved"
  , "Delivered"
  , "Closed"
  ]
pipelineStages Mixing =
  [ "Brief"
  , "Prep"
  , "v1 Sent"
  , "Revisions"
  , "Approved"
  , "Delivered"
  ]
pipelineStages Mastering =
  [ "Brief"
  , "v1"
  , "Revisions"
  , "Approved"
  , "DDP Delivered"
  ]
pipelineStages Rehearsal =
  [ "Booked"
  , "In Use"
  , "Completed"
  , "No-show"
  ]
pipelineStages Classes =
  [ "Enrolled"
  , "Scheduled"
  , "Attended"
  , "Make-up Needed"
  , "Completed"
  ]
pipelineStages EventProduction =
  [ "Lead"
  , "Proposal"
  , "Confirmed"
  , "Pre-Prod"
  , "Onsite"
  , "Post-Prod"
  , "Settled"
  ]

pipelineTypeSlug :: ServiceKind -> Text
pipelineTypeSlug Recording       = "recording"
pipelineTypeSlug Mixing          = "mixing"
pipelineTypeSlug Mastering       = "mastering"
pipelineTypeSlug Rehearsal       = "rehearsal"
pipelineTypeSlug Classes         = "classes"
pipelineTypeSlug EventProduction = "eventproduction"

parsePipelineType :: Text -> Maybe ServiceKind
parsePipelineType raw =
  case normalise raw of
    Nothing                -> Nothing
    Just "recording"       -> Just Recording
    Just "mixing"          -> Just Mixing
    Just "mastering"       -> Just Mastering
    Just "rehearsal"       -> Just Rehearsal
    Just "classes"         -> Just Classes
    Just "eventproduction" -> Just EventProduction
    _                      -> Nothing

canonicalStage :: ServiceKind -> Text -> Maybe Text
canonicalStage kind raw = do
  target <- normalise raw
  fmap fst . find (\(_, norm) -> norm == target) $
    map (\stage -> (stage, T.toLower (T.filter isAlphaNum stage))) (pipelineStages kind)

normalise :: Text -> Maybe Text
normalise raw
  | T.any isUnsafePipelineCanonicalChar raw = Nothing
  | otherwise = Just (T.toLower (T.filter isAlphaNum raw))

isUnsafePipelineCanonicalChar :: Char -> Bool
isUnsafePipelineCanonicalChar ch =
  isControl ch || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

defaultStage :: ServiceKind -> Text
defaultStage kind =
  case pipelineStages kind of
    (x:_) -> x
    []    -> "New"
