{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module TDF.Interactions.API
  ( InteractionsAPI, PublicInteractionsAPI, CommandRequest(..), InteractionResponse, ReadDiscussion, BlockRequest(..) ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant

data CommandRequest = CommandRequest
  { requestKey :: Text
  , command :: Value
  } deriving (Eq, Show, Generic)
instance FromJSON CommandRequest where
  parseJSON = genericParseJSON defaultOptions { rejectUnknownFields = True }
instance ToJSON CommandRequest

data BlockRequest = BlockRequest
  { blockRequestKey :: Text
  , blocked :: Bool
  , expectedVersion :: Integer
  } deriving (Eq, Show, Generic)
instance FromJSON BlockRequest where
  parseJSON = genericParseJSON defaultOptions { rejectUnknownFields = True }
instance ToJSON BlockRequest

type InteractionResponse = Headers '[Header "Cache-Control" Text] Value

type ReadDiscussion =
       Get '[JSON] InteractionResponse
  :<|> "comments" :> QueryParam "root" Text :> QueryParam "cursor" Text
         :> QueryParam "sort" Text :> QueryParam "limit" Int :> Get '[JSON] InteractionResponse
  :<|> "comments" :> Capture "commentId" Text :> Get '[JSON] InteractionResponse
  :<|> "reactors" :> QueryParam "cursor" Text :> QueryParam "limit" Int
         :> Get '[JSON] InteractionResponse

type InteractionsAPI = "interactions" :>
  ( "targets" :> Capture "kind" Text :> Capture "entityKey" Text :> ReadDiscussion
  :<|> "targets" :> Capture "targetId" Text :> "commands"
       :> ReqBody '[JSON] CommandRequest :> Post '[JSON] InteractionResponse
  :<|> "resolve" :> Capture "destinationKind" Text :> Capture "destinationId" Text :> Get '[JSON] InteractionResponse
  :<|> "blocks" :> Capture "partyId" Text :>
       (Get '[JSON] InteractionResponse :<|> ReqBody '[JSON] BlockRequest :> Put '[JSON] InteractionResponse)
  :<|> "blocked-accounts" :> QueryParam "cursor" Integer :> QueryParam "limit" Int :> Get '[JSON] InteractionResponse
  :<|> "reports" :> QueryParam "cursor" Text :> QueryParam "limit" Int :> Get '[JSON] InteractionResponse
  :<|> "preferences" :> (Get '[JSON] InteractionResponse :<|> ReqBody '[JSON] Value :> Put '[JSON] InteractionResponse)
  :<|> "moderation" :> Capture "targetId" Text :> QueryParam "cursor" Text :> QueryParam "limit" Int :> Get '[JSON] InteractionResponse
  )
type PublicInteractionsAPI = "public" :> "interactions" :>
  ( "targets" :> Capture "kind" Text :> Capture "entityKey" Text :> ReadDiscussion
  :<|> "resolve" :> Capture "destinationKind" Text :> Capture "destinationId" Text :> Get '[JSON] InteractionResponse
  )
