{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Bands where

import           Data.Text      (Text)
import           Servant

import           TDF.API.Types  (BandCreate, BandDTO, BandOptionsDTO, Page)

type BandsAPI =
  "bands" :>
    ( QueryParam "page" Int :> QueryParam "pageSize" Int :> Get '[JSON] (Page BandDTO)
 :<|> ReqBody '[JSON] BandCreate :> PostCreated '[JSON] BandDTO
 :<|> "options" :> Get '[JSON] BandOptionsDTO
 :<|> Capture "id" Text :> Get '[JSON] BandDTO
    )
