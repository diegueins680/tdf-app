{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Radio
  ( RadioAPI
  ) where

import           Data.Int      (Int64)
import           Data.Text     (Text)
import           Data.UUID     (UUID)
import           Servant

import           TDF.API.Types (RadioAutoStopOptionsDTO, RadioStreamDTO, RadioStreamUpsert, RadioPresenceDTO, RadioPresenceUpsert, RadioImportRequest,
                                RadioImportResult, RadioMetadataRefreshRequest, RadioMetadataRefreshResult,
                                RadioNowPlayingRequest, RadioNowPlayingResult, RadioTransmissionRequest, RadioTransmissionInfo)

type RadioAPI =
   "radio" :>
    ( "auto-stop-options"
        :> QueryParam "locale" Text
        :> Get '[JSON] RadioAutoStopOptionsDTO
   :<|> "streams"
        :> QueryParam "countryId" UUID
        :> QueryParam "genreId" UUID
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
   :<|> "streams"
         :> "now-playing"
         :> ReqBody '[JSON] RadioNowPlayingRequest
         :> Post '[JSON] RadioNowPlayingResult
   :<|> "transmissions"
         :> ReqBody '[JSON] RadioTransmissionRequest
         :> Post '[JSON] RadioTransmissionInfo
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
