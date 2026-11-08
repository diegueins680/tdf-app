{-# LANGUAGE OverloadedStrings #-}
-- Canonical application OAuth client configuration shared by exchange and refresh.
-- A personal refresh uses one complete validated client pair, never a service token.
module TDF.DriveOAuthClient
  ( resolveDriveClientCreds
  , validateOptionalDriveClientId
  , validateOptionalDriveClientCredential
  , maxDriveOAuthTokenChars
  , isHiddenDriveOAuthTokenChar
  ) where

import Data.Char (GeneralCategory(Format, LineSeparator, ParagraphSeparator),
                  generalCategory, isAscii, isControl, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Servant (ServerError(..), err503)
import TDF.Config (normalizeConfiguredGoogleClientId)

maxDriveOAuthTokenChars :: Int
maxDriveOAuthTokenChars = 4096

isHiddenDriveOAuthTokenChar :: Char -> Bool
isHiddenDriveOAuthTokenChar ch =
  generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

resolveDriveClientCreds
  :: Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Either ServerError (Text, Text)
resolveDriveClientCreds
  rawDriveClientId
  rawDriveClientSecret
  rawGoogleClientId
  rawGoogleClientSecret = do
  mDriveClientId <- validateOptionalDriveClientId "DRIVE_CLIENT_ID" rawDriveClientId
  mDriveClientSecret <-
    validateOptionalDriveClientCredential "DRIVE_CLIENT_SECRET" rawDriveClientSecret
  mGoogleClientId <- validateOptionalDriveClientId "GOOGLE_CLIENT_ID" rawGoogleClientId
  mGoogleClientSecret <-
    validateOptionalDriveClientCredential "GOOGLE_CLIENT_SECRET" rawGoogleClientSecret
  case (mDriveClientId, mDriveClientSecret) of
    (Just cid, Just secret) -> Right (cid, secret)
    (Just _, Nothing) -> Left partialDriveCredsError
    (Nothing, Just _) -> Left partialDriveCredsError
    (Nothing, Nothing) ->
      case (mGoogleClientId, mGoogleClientSecret) of
        (Just cid, Just secret) -> Right (cid, secret)
        (Just _, Nothing) -> Left partialGoogleCredsError
        (Nothing, Just _) -> Left partialGoogleCredsError
        (Nothing, Nothing) -> Left missingDriveCredsError
  where
    partialDriveCredsError =
      err503
        { errBody =
            "Google Drive no configurado: set both DRIVE_CLIENT_ID and DRIVE_CLIENT_SECRET, " <>
            "or unset both to use GOOGLE_CLIENT_ID/GOOGLE_CLIENT_SECRET fallback."
        }
    partialGoogleCredsError =
      err503
        { errBody =
            "Google Drive no configurado: GOOGLE_CLIENT_ID and GOOGLE_CLIENT_SECRET " <>
            "must both be set for fallback credentials."
        }
    missingDriveCredsError =
      err503
        { errBody =
            "Google Drive no configurado (faltan DRIVE_CLIENT_ID/DRIVE_CLIENT_SECRET o " <>
            "GOOGLE_CLIENT_ID/GOOGLE_CLIENT_SECRET)."
        }

validateOptionalDriveClientId :: Text -> Maybe Text -> Either ServerError (Maybe Text)
validateOptionalDriveClientId envName rawClientId =
  case rawClientId of
    Nothing -> Right Nothing
    Just clientId ->
      if T.null (T.strip clientId)
        then
          Left err503
            { errBody =
                BL.fromStrict . TE.encodeUtf8 $
                  envName <> " is configured but blank"
            }
        else
          case normalizeConfiguredGoogleClientId (T.unpack envName) (T.unpack clientId) of
            Left msg ->
              Left err503
                { errBody = BL.fromStrict (TE.encodeUtf8 (T.pack msg)) }
            Right normalized -> Right normalized

validateOptionalDriveClientCredential :: Text -> Maybe Text -> Either ServerError (Maybe Text)
validateOptionalDriveClientCredential envName rawCredential =
  case rawCredential of
    Nothing -> Right Nothing
    Just raw
      | T.null credential ->
          Left err503
            { errBody =
                BL.fromStrict . TE.encodeUtf8 $
                  envName <> " is configured but blank"
            }
      | T.any (\ch -> isControl ch || isSpace ch) credential ->
          Left err503
            { errBody =
                BL.fromStrict . TE.encodeUtf8 $
                  envName <> " must not contain control characters or whitespace"
            }
      | T.any isHiddenDriveOAuthTokenChar credential ->
          Left err503
            { errBody =
                BL.fromStrict . TE.encodeUtf8 $
                  envName <> " must not contain hidden formatting characters"
            }
      | T.any (not . isAscii) credential ->
          Left err503
            { errBody =
                BL.fromStrict . TE.encodeUtf8 $
                  envName <> " must contain only ASCII characters"
            }
      | T.length credential > maxDriveOAuthTokenChars ->
          Left err503
            { errBody =
                BL.fromStrict . TE.encodeUtf8 $
                  envName <> " must be 4096 characters or fewer"
            }
      | otherwise -> Right (Just credential)
      where
        credential = T.strip raw
