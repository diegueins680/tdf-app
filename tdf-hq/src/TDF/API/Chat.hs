{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module TDF.API.Chat (ChatAPI) where

import Data.Int (Int64)
import Servant
import TDF.DTO (ChatThreadDTO, ChatMessageDTO, ChatSendMessageRequest)

-- Existing wire contract, shared by the main server and focused HTTP verification.
type ChatAPI =
       "chat" :> "threads" :> Get '[JSON] [ChatThreadDTO]
  :<|> "chat" :> "threads" :> "dm" :> Capture "otherPartyId" Int64 :> Post '[JSON] ChatThreadDTO
  :<|> "chat" :> "threads" :> Capture "threadId" Int64 :> "messages"
         :> QueryParam "limit" Int
         :> QueryParam "beforeId" Int64
         :> QueryParam "afterId" Int64
         :> Get '[JSON] [ChatMessageDTO]
  :<|> "chat" :> "threads" :> Capture "threadId" Int64 :> "messages"
         :> ReqBody '[JSON] ChatSendMessageRequest
         :> Post '[JSON] ChatMessageDTO
