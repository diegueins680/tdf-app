
{-# LANGUAGE OverloadedStrings #-}
module SOP.GitHubDispatch (dispatchUpload) where

import           Control.Exception (try)
import           Data.Aeson
import           Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as BL
import           Data.Text (Text)
import           Network.HTTP.Simple
import           System.Environment (getEnv)

-- client_payload must include the YT metadata (file ids, title, description, etc.)
dispatchUpload :: Value -> IO (Either String ())
dispatchUpload payload = do
  -- Env vars:
  --   GH_REPO_TOKEN: GitHub PAT with repo:write
  --   GH_DISPATCH_REPO: e.g. "diegueins680/TDF" (repo where workflow lives)
  token <- getEnv "GH_REPO_TOKEN"
  repo  <- getEnv "GH_DISPATCH_REPO"
  let url = "https://api.github.com/repos/" <> repo <> "/dispatches"
  let body = object
        [ "event_type"     .= String "upload_youtube"
        , "client_payload" .= payload
        ]
  initReq <- parseRequest url
  let req = setRequestMethod "POST"
          $ setRequestHeader "Authorization" ["token " <> pack token]
          $ setRequestHeader "Accept" ["application/vnd.github+json"]
          $ setRequestHeader "User-Agent" ["tdf-sop-bot"]
          $ setRequestBodyLBS (encode body)
          $ initReq
  resE <- try (httpBS req) :: IO (Either HttpException (Response BL.ByteString))
  case resE of
    Left e  -> pure (Left ("HTTP error: " <> show e))
    Right _ -> pure (Right ())
