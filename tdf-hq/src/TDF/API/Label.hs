{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Label where

import           Data.Text (Text)
import           Servant
import           TDF.API.Types

type LabelAPI =
       "tracks" :> Get '[JSON] [LabelTrackDTO]
  :<|> "tracks" :> ReqBody '[JSON] LabelTrackCreate :> Post '[JSON] LabelTrackDTO
  :<|> "tracks" :> Capture "id" Text :> ReqBody '[JSON] LabelTrackUpdate :> Patch '[JSON] LabelTrackDTO
  :<|> "tracks" :> Capture "id" Text :> DeleteNoContent
