{-# LANGUAGE OverloadedStrings #-}
module TDF.Services.InstagramMessaging
  ( sendInstagramText
  , sendInstagramTextWithContext
  ) where

import           Control.Exception (SomeException, try)
import           Data.Aeson (encode, object, (.=))
import           Data.List (nub)
import           Data.Maybe (catMaybes, maybeToList)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Client (Manager, Request(..), RequestBody(..), Response, httpLbs, newManager, parseRequest, responseBody, responseStatus)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types.Header (hAuthorization)
import           Network.HTTP.Types.Status (statusCode)

import           TDF.Config (AppConfig(..))

sendInstagramText :: AppConfig -> Text -> Text -> IO (Either Text Text)
sendInstagramText cfg recipientId body =
  sendInstagramTextWithContext cfg Nothing Nothing recipientId body

data InstagramAttemptSource = InstagramAttemptSource
  { iasLabel     :: Text
  , iasToken     :: Text
  , iasAccountId :: Maybe Text
  }

data InstagramAttempt = InstagramAttempt
  { iaLabel :: Text
  , iaToken :: Text
  , iaUrl   :: Text
  }

sendInstagramTextWithContext :: AppConfig -> Maybe Text -> Maybe Text -> Text -> Text -> IO (Either Text Text)
sendInstagramTextWithContext cfg mTokenOverride mAccountIdOverride recipientId body =
  case buildAttempts cfg mTokenOverride mAccountIdOverride of
    [] -> pure (Left "INSTAGRAM_MESSAGING_TOKEN no configurado")
    attempts -> do
      manager <- newManager tlsManagerSettings
      runAttempts manager recipientId body attempts []

nonEmptyText :: Text -> Maybe Text
nonEmptyText raw =
  let trimmed = T.strip raw
  in if T.null trimmed then Nothing else Just trimmed

buildAttempts :: AppConfig -> Maybe Text -> Maybe Text -> [InstagramAttempt]
buildAttempts cfg mTokenOverride mAccountIdOverride =
  let base = T.dropWhileEnd (== '/') (instagramMessagingApiBase cfg)
      sources = nubSources (catMaybes
        [ buildSource "connected asset token" (mTokenOverride >>= nonEmptyText) (mAccountIdOverride >>= nonEmptyText)
        , buildSource "configured fallback token" (instagramMessagingToken cfg >>= nonEmptyText) (instagramMessagingAccountId cfg >>= nonEmptyText)
        ])
  in nubAttempts (concatMap (sourceAttempts base) sources)
  where
    buildSource label mToken mAccountId =
      InstagramAttemptSource label <$> mToken <*> pure mAccountId

nubSources :: [InstagramAttemptSource] -> [InstagramAttemptSource]
nubSources =
  nubByText (\src -> iasLabel src <> "|" <> iasToken src <> "|" <> maybe "" id (iasAccountId src))

nubAttempts :: [InstagramAttempt] -> [InstagramAttempt]
nubAttempts =
  nubByText (\attempt -> iaLabel attempt <> "|" <> iaToken attempt <> "|" <> iaUrl attempt)

nubByText :: (a -> Text) -> [a] -> [a]
nubByText toKey = go []
  where
    go _ [] = []
    go seen (x:xs) =
      let key = toKey x
      in if key `elem` seen
        then go seen xs
        else x : go (key : seen) xs

sourceAttempts :: Text -> InstagramAttemptSource -> [InstagramAttempt]
sourceAttempts base source =
  let urls = nub (map (\accountId -> base <> "/" <> accountId <> "/messages") (maybeToList (iasAccountId source)) <> [base <> "/me/messages"])
  in map (\urlTxt -> InstagramAttempt (iasLabel source) (iasToken source) urlTxt) urls

runAttempts :: Manager -> Text -> Text -> [InstagramAttempt] -> [Text] -> IO (Either Text Text)
runAttempts _ _ _ [] [] = pure (Left "Instagram messaging failed without details")
runAttempts _ _ _ [] errors = pure (Left (T.intercalate " | " (reverse errors)))
runAttempts manager recipientId body (attempt:rest) errors = do
  result <- sendToEndpoint manager (iaToken attempt) recipientId body (iaUrl attempt)
  case result of
    Right payload -> pure (Right payload)
    Left err ->
      let labelledError = "Send failed via " <> iaLabel attempt <> " at " <> iaUrl attempt <> ": " <> err
      in runAttempts manager recipientId body rest (labelledError : errors)

sendToEndpoint
  :: Manager
  -> Text
  -> Text
  -> Text
  -> Text
  -> IO (Either Text Text)
sendToEndpoint manager token recipientId body urlTxt = do
  reqE <- try (parseRequest (T.unpack urlTxt)) :: IO (Either SomeException Request)
  case reqE of
    Left err -> pure (Left (T.pack (show err)))
    Right req0 -> do
      let req = req0
            { method = "POST"
            , requestHeaders =
                [ ("Content-Type", "application/json")
                , (hAuthorization, BS.pack ("Bearer " <> T.unpack token))
                ]
            , requestBody = RequestBodyLBS (encode payload)
            }
          payload = object
            [ "recipient" .= object [ "id" .= recipientId ]
            , "message" .= object [ "text" .= body ]
            , "messaging_type" .= ("RESPONSE" :: Text)
            ]
      respE <- try (httpLbs req manager) :: IO (Either SomeException (Response BL.ByteString))
      pure $ case respE of
        Left err -> Left (T.pack (show err))
        Right resp ->
          let status = statusCode (responseStatus resp)
              bodyTxt = TE.decodeUtf8 (BL.toStrict (responseBody resp))
          in if status >= 200 && status < 300
               then Right bodyTxt
               else Left ("HTTP " <> T.pack (show status) <> ": " <> bodyTxt)
