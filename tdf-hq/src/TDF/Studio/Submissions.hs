{-# LANGUAGE OverloadedStrings #-}
module TDF.Studio.Submissions (studioIntakeLimits, studioOperation) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Aeson (Value(..), decodeStrict', encode, object, (.=))
import qualified Data.Aeson.KeyMap as AKeyMap
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS8
import Network.HTTP.Types.Status (status400, status403, status413)
import qualified Network.Wai as Wai
import Servant
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

studioOperation :: String -> Value -> ReaderT env Handler Value
studioOperation op payload = do
  result <- liftIO $ readProcessWithExitCode "python3" ["studio/submissions.py",op]
    (T.unpack (TE.decodeUtf8 (BL.toStrict (encode payload))))
  case result of
    (ExitSuccess,out,_) | Just (Object response) <- decodeStrict' (TE.encodeUtf8 (T.pack out)) ->
      case AKeyMap.lookup "ok" response of
        Just value -> pure value
        Nothing -> do
          let code = AKeyMap.lookup "status" response
              base = case code of
                Just (Number 403) -> err403
                Just (Number 404) -> err404
                Just (Number 409) -> err409
                Just (Number 413) -> err413
                Just (Number 422) -> err422
                Just (Number 429) -> err429
                _ -> err400
          throwError base { errBody = encode (Object response) }
    _ -> throwError err503

-- Reject oversized or uninvited multipart requests before Servant spools any files.
studioIntakeLimits :: env -> Wai.Middleware
studioIntakeLimits env next req respond
  | Wai.requestMethod req /= "POST" = next req respond
  | "studio-submissions" : route <- Wai.pathInfo req = do
      let upload = take 1 route == ["upload"]
          limit = if upload then 25 * 1024 * 1024 + 16384 else 4096
          deny code = respond (Wai.responseLBS code [("Cache-Control","no-store")] "")
      case Wai.requestBodyLength req of
        Wai.KnownLength bytes | bytes <= limit ->
          if upload then case lookup "Authorization" (Wai.requestHeaders req) >>= BS8.stripPrefix "Bearer " of
            Just token | BS8.length token == 43 -> do
              auth <- runHandler (runReaderT (studioOperation "authorize" (object ["token" .= BS8.unpack token])) env)
              either (const (deny status403)) (const (next req respond)) auth
            _ -> deny status403
          else next req respond
        Wai.ChunkedBody -> deny status400
        _ -> deny status413
  | otherwise = next req respond
