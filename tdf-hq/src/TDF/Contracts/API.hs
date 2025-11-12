{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module TDF.Contracts.API where
import Servant
import Data.Text (Text)
import Data.Aeson (Value)

type ContractsAPI = 
       "contracts" :> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> "contracts" :> Capture "id" Text :> "pdf" :> Get '[OctetStream] ByteString
  :<|> "contracts" :> Capture "id" Text :> "send" :> ReqBody '[JSON] Value :> Post '[JSON] Value
