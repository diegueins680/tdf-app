{-# LANGUAGE DeriveGeneric #-}
module TDF.Contracts.Types where
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)

data Contract = Contract {
  kind :: Text,
  json_payload :: Value
} deriving (Show, Generic)
instance FromJSON Contract
instance ToJSON Contract
