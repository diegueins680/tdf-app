{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Sessions where

import           Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import           Servant

import           TDF.API.Types

type SessionsAPI =
  "sessions" :>
    (    QueryParam "page" Int
      :> QueryParam "pageSize" Int
      :> Get '[JSON] (Page SessionDTO)
  :<|> ReqBody '[JSON] SessionCreate :> PostCreated '[JSON] SessionDTO
  :<|> "options" :> Get '[JSON] SessionOptionsDTO
  :<|> Capture "id" Text :> Get '[JSON] SessionDTO
  :<|> Capture "id" Text :> ReqBody '[JSON] SessionUpdate :> Patch '[JSON] SessionDTO
  :<|> Capture "id" Text :> "input-list" :> Get '[JSON] [SessionInputRow]
  :<|> Capture "id" Text :> "input-list.pdf"
        :> Get '[OctetStream] (Headers '[Header "Content-Disposition" Text] BL.ByteString)
    )
