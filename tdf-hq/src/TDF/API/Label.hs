{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Label where

import           Data.Text (Text)
import           Servant
import           Data.Int (Int64)
import           TDF.API.Types

type LabelAPI =
       "tracks" :> QueryParam "ownerId" Int64 :> Get '[JSON] [LabelTrackDTO]
  :<|> "tracks" :> ReqBody '[JSON] LabelTrackCreate :> Post '[JSON] LabelTrackDTO
  :<|> "tracks" :> Capture "id" Text :> ReqBody '[JSON] LabelTrackUpdate :> Patch '[JSON] LabelTrackDTO
  :<|> "tracks" :> Capture "id" Text :> DeleteNoContent
