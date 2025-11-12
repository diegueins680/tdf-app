{-# LANGUAGE OverloadedStrings #-}

module TDF.Pipelines
  ( pipelineStages
  , pipelineTypeSlug
  , parsePipelineType
  , canonicalStage
  , defaultStage
  ) where

import           Data.Char      (isAlphaNum)
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
    "recording"       -> Just Recording
    "mixing"          -> Just Mixing
    "mastering"       -> Just Mastering
    "rehearsal"       -> Just Rehearsal
    "classes"         -> Just Classes
    "eventproduction" -> Just EventProduction
    _                 -> Nothing
  where
    normalise = T.toLower . T.filter isAlphaNum

canonicalStage :: ServiceKind -> Text -> Maybe Text
canonicalStage kind raw =
  let target = normalise raw
  in fmap fst . find (\(_, norm) -> norm == target) $
       map (\stage -> (stage, normalise stage)) (pipelineStages kind)
  where
    normalise = T.toLower . T.filter isAlphaNum

defaultStage :: ServiceKind -> Text
defaultStage kind =
  case pipelineStages kind of
    (x:_) -> x
    []    -> "New"
