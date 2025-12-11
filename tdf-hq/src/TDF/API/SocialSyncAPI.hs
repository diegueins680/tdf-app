{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.SocialSyncAPI where

import           Data.Text (Text)
import           Servant

import           TDF.DTO.SocialSyncDTO

type SocialSyncAPI =
  "social-sync" :>
    ( "ingest" :> ReqBody '[JSON] SocialSyncIngestRequest :> Post '[JSON] SocialSyncIngestResponse
 :<|> "posts"
        :> QueryParam "platform" Text
        :> QueryParam "artistPartyId" Text
        :> QueryParam "artistProfileId" Text
        :> QueryParam "tag" Text
        :> QueryParam "limit" Int
        :> Get '[JSON] [SocialSyncPostDTO]
    )
