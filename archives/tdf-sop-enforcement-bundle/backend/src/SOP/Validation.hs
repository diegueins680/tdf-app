
{-# LANGUAGE OverloadedStrings #-}
module SOP.Validation
  ( Requirement(..)
  , Stage(..)
  , Missing(..)
  , listMissing
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

data Stage = Ready | InProgress | QC | Schedule | Published
  deriving (Eq, Show, Read)

data Requirement = Requirement
  { reqKey   :: Text
  , reqLabel :: Text
  } deriving (Eq, Show)

newtype Missing = Missing { unMissing :: [Requirement] }
  deriving (Eq, Show)

-- listMissing should query DB:
--  1) fetch all SopRequirement for (projectType, stage)
--  2) for each, check if SopEvidence exists for (projectId, reqKey)
-- Here we return [] as placeholder.
listMissing :: Int -> Text -> Stage -> IO Missing
listMissing _projectId _ptype _stage = pure (Missing [])
