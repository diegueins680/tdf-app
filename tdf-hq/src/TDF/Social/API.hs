{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module TDF.Social.API (SocialV2API, Command(..), Preferences(..)) where

import Control.Monad (unless)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Int (Int64)
import Data.Text (Text)
import Servant

data Command = Command Text Int64 Text deriving (Eq, Show)
instance FromJSON Command where
  parseJSON = withObject "Social command" $ \o -> do
    unless (all (`elem` ["operation", "expectedRevision", "requestKey"]) (KM.keys o)) $
      fail "Unknown social command field"
    Command <$> o .: "operation" <*> o .: "expectedRevision" <*> o .: "requestKey"
instance ToJSON Command where
  toJSON (Command op rev key) = object
    ["operation" .= op, "expectedRevision" .= rev, "requestKey" .= key]

data Preferences = Preferences Bool Bool Int64 deriving (Eq, Show)
instance FromJSON Preferences where
  parseJSON = withObject "Social preferences" $ \o -> do
    unless (all (`elem` ["discoverable", "personalized", "expectedRevision"]) (KM.keys o)) $
      fail "Unknown social preference field"
    Preferences <$> o .: "discoverable" <*> o .: "personalized" <*> o .: "expectedRevision"
instance ToJSON Preferences where
  toJSON (Preferences discover personalize rev) = object
    ["discoverable" .= discover, "personalized" .= personalize, "expectedRevision" .= rev]

-- Additive experimental contract; JSON results are documented in OpenAPI.
type SocialV2API = "v2" :>
  ( "me" :> Get '[JSON] Value
  :<|> "relationships" :> Capture "partyId" Int64 :> Get '[JSON] Value
  :<|> "relationships" :> Capture "partyId" Int64 :> ReqBody '[JSON] Command :> Post '[JSON] Value
  :<|> "preferences" :> ReqBody '[JSON] Preferences :> Put '[JSON] Value
  :<|> "following" :> QueryParam "cursor" Int64 :> QueryParam "limit" Int :> Get '[JSON] Value
  :<|> "discover" :> QueryParam "limit" Int :> Get '[JSON] Value
  )
