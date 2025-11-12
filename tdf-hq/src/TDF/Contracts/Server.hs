{-# LANGUAGE OverloadedStrings #-}
module TDF.Contracts.Server where
import Servant
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import Data.Text (Text)
import TDF.Contracts.API

server :: Server ContractsAPI
server = createH :<|> pdfH :<|> sendH
  where
    createH :: A.Value -> Handler A.Value
    createH v = pure (A.object ["status" A..= ("created" :: Text), "id" A..= ("uuid-demo" :: Text)])

    pdfH :: Text -> Handler BL.ByteString
    pdfH _ = pure BL.empty -- TODO: call latex renderer

    sendH :: Text -> A.Value -> Handler A.Value
    sendH _ _ = pure (A.object ["status" A..= ("sent" :: Text)])
