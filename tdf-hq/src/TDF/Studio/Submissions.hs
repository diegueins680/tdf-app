{-# LANGUAGE OverloadedStrings #-}
module TDF.Studio.Submissions (studioOperation) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Data.Aeson (Value(..), decodeStrict', encode)
import qualified Data.Aeson.KeyMap as AKeyMap
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
