{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Radio
  ( RadioAPI
  ) where

import           Data.Text     (Text)
import           Data.Int      (Int64)
import           Servant

import           TDF.API.Types (RadioStreamDTO, RadioStreamUpsert, RadioPresenceDTO, RadioPresenceUpsert, RadioImportRequest,
                                RadioImportResult, RadioMetadataRefreshRequest, RadioMetadataRefreshResult)

type RadioAPI =
  "radio" :>
    ( "streams"
        :> QueryParam "country" Text
        :> QueryParam "genre" Text
        :> Get '[JSON] [RadioStreamDTO]
   :<|> "streams"
        :> "active"
        :> ReqBody '[JSON] RadioStreamUpsert
        :> Post '[JSON] RadioStreamDTO
   :<|> "streams"
        :> "import"
        :> ReqBody '[JSON] RadioImportRequest
        :> Post '[JSON] RadioImportResult
   :<|> "streams"
         :> "refresh-metadata"
         :> ReqBody '[JSON] RadioMetadataRefreshRequest
         :> Post '[JSON] RadioMetadataRefreshResult
   :<|> "presence"
         :> Get '[JSON] (Maybe RadioPresenceDTO)
   :<|> "presence"
         :> ReqBody '[JSON] RadioPresenceUpsert
         :> Post '[JSON] RadioPresenceDTO
   :<|> "presence"
         :> Delete '[JSON] NoContent
   :<|> "presence"
         :> Capture "partyId" Int64
         :> Get '[JSON] (Maybe RadioPresenceDTO)
    )
