{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Rooms where

import           Data.Text (Text)
import           Servant

import           TDF.API.Types

type RoomsPublicAPI =
       "rooms" :> "public" :> Get '[JSON] [RoomDTO]

type RoomsAPI =
       "rooms" :> Get '[JSON] [RoomDTO]
  :<|> "rooms" :> ReqBody '[JSON] RoomCreate :> PostCreated '[JSON] RoomDTO
  :<|> "rooms" :> Capture "id" Text :> ReqBody '[JSON] RoomUpdate :> Patch '[JSON] RoomDTO
