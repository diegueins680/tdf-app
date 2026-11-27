{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- | Shared filter options (TAX-FILTER-001) and artist genre suggestions
-- (TAX-GENRE-SUGGESTION-001).
module TDF.API.Taxonomy
  ( TaxonomyPublicAPI
  , TaxonomyProtectedAPI
  , GenreSuggestionRequest(..)
  , GenreSuggestionApproval(..)
  , GenreSuggestionRejection(..)
  , GenreSuggestionMerge(..)
  ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Servant

import TDF.UUIDInstances ()

data GenreSuggestionRequest = GenreSuggestionRequest
  { nameEs :: Text
  , nameEn :: Text
  , parentGenreId :: Maybe UUID
  , confirmDistinct :: Maybe Bool
  } deriving (Show, Generic)
instance FromJSON GenreSuggestionRequest
instance ToJSON GenreSuggestionRequest

data GenreSuggestionApproval = GenreSuggestionApproval
  { approvedNameEs :: Text
  , approvedNameEn :: Text
  , approvedParentGenreId :: Maybe UUID
  } deriving (Show, Generic)
instance FromJSON GenreSuggestionApproval
instance ToJSON GenreSuggestionApproval

newtype GenreSuggestionRejection = GenreSuggestionRejection
  { rejectionNote :: Text
  } deriving (Show, Generic)
instance FromJSON GenreSuggestionRejection
instance ToJSON GenreSuggestionRejection

newtype GenreSuggestionMerge = GenreSuggestionMerge
  { mergeIntoGenreId :: UUID
  } deriving (Show, Generic)
instance FromJSON GenreSuggestionMerge
instance ToJSON GenreSuggestionMerge

-- | Public: every option a filter can show, with both names, so a shared link
-- (taxonomy ids) renders in either language.
type TaxonomyPublicAPI =
  "taxonomy" :> "filter-options" :> Get '[JSON] Value

type TaxonomyProtectedAPI =
       "genres" :> "suggestions" :> "check" :> ReqBody '[JSON] GenreSuggestionRequest :> Post '[JSON] Value
  :<|> "genres" :> "suggestions" :> ReqBody '[JSON] GenreSuggestionRequest :> Post '[JSON] Value
  :<|> "genres" :> "suggestions" :> "mine" :> Get '[JSON] [Value]
  :<|> "admin" :> "genre-suggestions" :> QueryParam "status" Text :> Get '[JSON] [Value]
  :<|> "admin" :> "genre-suggestions" :> Capture "suggestionId" UUID :> "approve"
         :> ReqBody '[JSON] GenreSuggestionApproval :> Post '[JSON] Value
  :<|> "admin" :> "genre-suggestions" :> Capture "suggestionId" UUID :> "reject"
         :> ReqBody '[JSON] GenreSuggestionRejection :> Post '[JSON] Value
  :<|> "admin" :> "genre-suggestions" :> Capture "suggestionId" UUID :> "merge"
         :> ReqBody '[JSON] GenreSuggestionMerge :> Post '[JSON] Value
