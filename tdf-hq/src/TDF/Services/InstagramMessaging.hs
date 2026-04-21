{-# LANGUAGE OverloadedStrings #-}
module TDF.Services.InstagramMessaging
  ( sendInstagramText
  , sendInstagramTextWithContext
  ) where

import           Control.Exception (SomeException, try)
import           Data.Aeson (encode, object, (.=))
import           Data.Char (isControl, isSpace)
import           Data.Maybe (isJust)
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
  , iasAccountId :: Text
  }

data InstagramAttempt = InstagramAttempt
  { iaLabel :: Text
  , iaToken :: Text
  , iaUrl   :: Text
  }

sendInstagramTextWithContext :: AppConfig -> Maybe Text -> Maybe Text -> Text -> Text -> IO (Either Text Text)
sendInstagramTextWithContext cfg mTokenOverride mAccountIdOverride recipientId body =
  case validateInstagramMessagePayload recipientId body of
    Left err -> pure (Left err)
    Right (cleanRecipientId, cleanBody) ->
      case buildAttempts cfg mTokenOverride mAccountIdOverride of
        Left err ->
          pure (Left err)
        Right attempts -> do
          manager <- newManager tlsManagerSettings
          runAttempts manager cleanRecipientId cleanBody attempts []

nonEmptyText :: Text -> Maybe Text
nonEmptyText raw =
  let trimmed = T.strip raw
  in if T.null trimmed then Nothing else Just trimmed

validateInstagramMessagePayload :: Text -> Text -> Either Text (Text, Text)
validateInstagramMessagePayload rawRecipientId rawBody = do
  recipientId <- validateInstagramRecipientId rawRecipientId
  body <- maybe (Left "Instagram message body requerido") Right (nonEmptyText rawBody)
  pure (recipientId, body)

validateInstagramRecipientId :: Text -> Either Text Text
validateInstagramRecipientId rawRecipientId =
  case nonEmptyText rawRecipientId of
    Nothing ->
      Left "Instagram recipient id requerido"
    Just recipientId
      | T.any isSpace recipientId ->
          Left "Instagram recipient id must not contain whitespace"
      | T.any isControl recipientId ->
          Left "Instagram recipient id must not contain control characters"
      | T.length recipientId > 256 ->
          Left "Instagram recipient id must be 256 characters or fewer"
      | otherwise ->
          Right recipientId

buildAttempts :: AppConfig -> Maybe Text -> Maybe Text -> Either Text [InstagramAttempt]
buildAttempts cfg mTokenOverride mAccountIdOverride =
  let base = T.dropWhileEnd (== '/') (instagramMessagingApiBase cfg)
  in do
    source <-
      if hasExplicitMessagingContext mTokenOverride mAccountIdOverride
        then buildSource
          "connected asset token"
          "Instagram connected asset token"
          "Instagram connected asset account id"
          mTokenOverride
          mAccountIdOverride
        else buildSource
          "configured fallback token"
          "INSTAGRAM_MESSAGING_TOKEN"
          "INSTAGRAM_MESSAGING_ACCOUNT_ID"
          (instagramMessagingToken cfg)
          (instagramMessagingAccountId cfg)
    pure (nubAttempts (sourceAttempts base source))
  where
    buildSource attemptLabel tokenLabel accountIdLabel mToken mAccountId =
      InstagramAttemptSource attemptLabel
        <$> validateInstagramBearerToken tokenLabel mToken
        <*> validateInstagramAccountId accountIdLabel mAccountId

validateInstagramBearerToken :: Text -> Maybe Text -> Either Text Text
validateInstagramBearerToken label mRawToken =
  case mRawToken >>= nonEmptyText of
    Nothing ->
      Left (label <> " no configurado")
    Just token
      | T.any invalidHeaderValueChar token ->
          Left (label <> " must not contain whitespace or control characters")
      | otherwise ->
          Right token

validateInstagramAccountId :: Text -> Maybe Text -> Either Text Text
validateInstagramAccountId label mRawAccountId =
  case mRawAccountId >>= nonEmptyText of
    Nothing ->
      Left (label <> " no configurado")
    Just accountId
      | T.length accountId > maxInstagramAccountIdChars
          || T.any (not . isGraphNodeIdChar) accountId ->
          Left
            ( label
                <> " must be a Graph node id using only ASCII letters, numbers, "
                <> "'.', '_' or '-' (128 chars max)"
            )
      | otherwise ->
          Right accountId

hasExplicitMessagingContext :: Maybe Text -> Maybe Text -> Bool
hasExplicitMessagingContext mTokenOverride mAccountIdOverride =
  isJust (mTokenOverride >>= nonEmptyText)
    || isJust (mAccountIdOverride >>= nonEmptyText)

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
  [ InstagramAttempt
      (iasLabel source)
      (iasToken source)
      (base <> "/" <> iasAccountId source <> "/messages")
  ]

invalidHeaderValueChar :: Char -> Bool
invalidHeaderValueChar ch = isSpace ch || isControl ch

isGraphNodeIdChar :: Char -> Bool
isGraphNodeIdChar ch =
  (ch >= 'a' && ch <= 'z')
    || (ch >= 'A' && ch <= 'Z')
    || (ch >= '0' && ch <= '9')
    || ch `elem` ("._-" :: String)

maxInstagramAccountIdChars :: Int
maxInstagramAccountIdChars = 128

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
