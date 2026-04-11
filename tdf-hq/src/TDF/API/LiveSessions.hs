{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.LiveSessions
  ( LiveSessionsAPI
  , LiveSessionIntakePayload(..)
  , LiveSessionMusicianPayload(..)
  , LiveSessionSongPayload(..)
  ) where

import           Data.Aeson               (FromJSON(..), Object, Value, eitherDecodeStrict', withObject, (.:?))
import qualified Data.Aeson.Key           as AesonKey
import           Data.Aeson.Types         (Parser)
import           Data.Char                (isAsciiLower, isDigit, isSpace)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (encodeUtf8)
import           Data.Time                (Day)
import           Data.Time.Format         (defaultTimeLocale, parseTimeM)
import           GHC.Generics             (Generic)
import           Servant
import           Servant.Multipart        ( FileData
                                          , FromMultipart(..)
                                          , Input(..)
                                          , MultipartData(inputs, files)
                                          , MultipartForm
                                          , Tmp
                                          , fdInputName
                                          )
type LiveSessionsAPI =
  "live-sessions" :>
    ( "intake" :> MultipartForm Tmp LiveSessionIntakePayload :> Post '[JSON] NoContent
    )

data LiveSessionMusicianPayload = LiveSessionMusicianPayload
  { lsmPartyId    :: Maybe Int
  , lsmName       :: Text
  , lsmEmail      :: Maybe Text
  , lsmInstrument :: Maybe Text
  , lsmRole       :: Maybe Text
  , lsmNotes      :: Maybe Text
  , lsmIsExisting :: Bool
  } deriving (Show, Generic)

instance FromJSON LiveSessionMusicianPayload where
  parseJSON = withAliasedObject "LiveSessionMusicianPayload" $ \obj ->
    LiveSessionMusicianPayload
      <$> parseAliasedOptionalField obj "partyId" "lsmPartyId"
      <*> parseAliasedRequiredField obj "name" "lsmName"
      <*> parseAliasedOptionalField obj "email" "lsmEmail"
      <*> parseAliasedOptionalField obj "instrument" "lsmInstrument"
      <*> parseAliasedOptionalField obj "role" "lsmRole"
      <*> parseAliasedOptionalField obj "notes" "lsmNotes"
      <*> parseAliasedRequiredField obj "isExisting" "lsmIsExisting"

data LiveSessionSongPayload = LiveSessionSongPayload
  { lssTitle     :: Text
  , lssBpm       :: Maybe Int
  , lssSongKey   :: Maybe Text
  , lssLyrics    :: Maybe Text
  , lssSortOrder :: Maybe Int
  } deriving (Show, Generic)

instance FromJSON LiveSessionSongPayload where
  parseJSON = withAliasedObject "LiveSessionSongPayload" $ \obj ->
    LiveSessionSongPayload
      <$> parseAliasedRequiredField obj "title" "lssTitle"
      <*> parseAliasedOptionalField obj "bpm" "lssBpm"
      <*> parseAliasedOptionalField obj "songKey" "lssSongKey"
      <*> parseAliasedOptionalField obj "lyrics" "lssLyrics"
      <*> parseAliasedOptionalField obj "sortOrder" "lssSortOrder"

data LiveSessionIntakePayload = LiveSessionIntakePayload
  { lsiBandName     :: Text
  , lsiBandDescription :: Maybe Text
  , lsiPrimaryGenre :: Maybe Text
  , lsiInputList    :: Maybe Text
  , lsiContactEmail :: Maybe Text
  , lsiContactPhone :: Maybe Text
  , lsiSessionDate  :: Maybe Day
  , lsiAvailability :: Maybe Text
  , lsiAcceptedTerms :: Bool
  , lsiTermsVersion :: Maybe Text
  , lsiMusicians    :: [LiveSessionMusicianPayload]
  , lsiSetlist      :: [LiveSessionSongPayload]
  , lsiRider        :: Maybe (FileData Tmp)
  } deriving (Show, Generic)

instance FromMultipart Tmp LiveSessionIntakePayload where
  fromMultipart multipart = do
    bandName <- lookupText "bandName" multipart
    bandDescription <- optionalText "bandDescription" multipart
    primaryGenre <- optionalText "primaryGenre" multipart
    inputList <- optionalText "inputList" multipart
    contactEmail <- optionalEmail "contactEmail" multipart
    contactPhone <- optionalText "contactPhone" multipart
    riderFile <- lookupFile "rider" multipart
    sessionDate <- optionalDay "sessionDate" multipart
    availability <- optionalText "availability" multipart
    acceptedTerms <- optionalBool "acceptedTerms" multipart
    termsVersion <- optionalText "termsVersion" multipart
    musiciansTxt <- lookupText "musicians" multipart
    musicians <- decodeMusicians musiciansTxt
    setlistTxt <- optionalText "setlist" multipart
    setlist <- maybe (Right []) decodeSetlist setlistTxt
    if acceptedTerms && maybe True T.null termsVersion
      then Left "Missing field: termsVersion is required when acceptedTerms is true"
      else
        pure LiveSessionIntakePayload
          { lsiBandName     = T.strip bandName
          , lsiBandDescription = bandDescription
          , lsiPrimaryGenre = primaryGenre
          , lsiInputList    = inputList
          , lsiContactEmail = contactEmail
          , lsiContactPhone = contactPhone
          , lsiSessionDate  = sessionDate
          , lsiAvailability = availability
          , lsiAcceptedTerms = acceptedTerms
          , lsiTermsVersion = termsVersion
          , lsiMusicians    = musicians
          , lsiSetlist      = setlist
          , lsiRider        = riderFile
          }
    where
      lookupText name mp =
        case lookupSingleInput name mp of
          Left err -> Left err
          Right Nothing -> Left ("Missing field: " <> T.unpack name)
          Right (Just val) -> Right (inputValueText val)
      optionalText name mp =
        case lookupSingleInput name mp of
          Left err -> Left err
          Right Nothing  -> Right Nothing
          Right (Just inp) -> Right (normalizeOptionalText (inputValueText inp))
      optionalEmail name mp =
        case lookupSingleInput name mp of
          Left err -> Left err
          Right Nothing  -> Right Nothing
          Right (Just inp) -> validateOptionalEmailText name (inputValueText inp)
      lookupFile = lookupSingleFile
      optionalDay name mp =
        case lookupSingleInput name mp of
          Left err -> Left err
          Right Nothing  -> Right Nothing
          Right (Just inp) ->
            let txt = T.strip (inputValueText inp)
            in if T.null txt
                 then Right Nothing
                 else fmap Just (readMaybeDay txt)
      optionalBool name mp =
        case lookupSingleInput name mp of
          Left err -> Left err
          Right Nothing -> Right False
          Right (Just inp) -> parseBoolField name (inputValueText inp)

      decodeMusicians txt =
        case eitherDecodeStrict' (encodeUtf8 txt) of
          Left err -> Left ("Invalid musicians payload: " <> err)
          Right xs ->
            case traverse validateMusician xs of
              Left err -> Left ("Invalid musicians payload: " <> err)
              Right validated -> Right validated
      decodeSetlist txt =
        case eitherDecodeStrict' (encodeUtf8 txt) of
          Left err -> Left ("Invalid setlist payload: " <> err)
          Right xs -> Right xs

      validateMusician musician = do
        normalizedEmail <-
          maybe
            (Right Nothing)
            (validateOptionalEmailText "musician email")
            (lsmEmail musician)
        let normalizedMusician =
              musician
                { lsmName = T.strip (lsmName musician)
                , lsmEmail = normalizedEmail
                , lsmInstrument = lsmInstrument musician >>= normalizeOptionalText
                , lsmRole = lsmRole musician >>= normalizeOptionalText
                , lsmNotes = lsmNotes musician >>= normalizeOptionalText
                }
            noReferenceProvided =
              lsmPartyId normalizedMusician == Nothing
                && T.null (lsmName normalizedMusician)
                && maybe True T.null (lsmEmail normalizedMusician)
        if maybe False (<= 0) (lsmPartyId musician)
          then Left "musician partyId must be a positive integer"
          else if noReferenceProvided
            then Left "each musician must include a non-blank name, email, or partyId"
            else Right normalizedMusician

      normalizeOptionalText :: Text -> Maybe Text
      normalizeOptionalText raw =
        let trimmed = T.strip raw
        in if T.null trimmed then Nothing else Just trimmed

      validateOptionalEmailText :: Text -> Text -> Either String (Maybe Text)
      validateOptionalEmailText fieldName raw =
        case normalizeOptionalText raw of
          Nothing -> Right Nothing
          Just emailVal ->
            let normalized = T.toLower emailVal
            in if isValidEmail normalized
                 then Right (Just normalized)
                 else
                   Left
                     ( "Invalid field: "
                         <> T.unpack fieldName
                         <> " must be a valid email address"
                     )

      isValidEmail :: Text -> Bool
      isValidEmail candidate =
        case T.splitOn "@" candidate of
          [localPart, domain] ->
            not (T.null localPart)
              && not (T.null domain)
              && not (T.any isSpace candidate)
              && not (T.isPrefixOf "." domain)
              && not (T.isSuffixOf "." domain)
              && T.isInfixOf "." domain
              && all isValidEmailDomainLabel (T.splitOn "." domain)
          _ -> False

      isValidEmailDomainLabel :: Text -> Bool
      isValidEmailDomainLabel label =
        not (T.null label)
          && not (T.isPrefixOf "-" label)
          && not (T.isSuffixOf "-" label)
          && T.all isValidEmailDomainChar label

      isValidEmailDomainChar :: Char -> Bool
      isValidEmailDomainChar c = isAsciiLower c || isDigit c || c == '-'

      parseBoolField :: Text -> Text -> Either String Bool
      parseBoolField fieldName raw =
        case T.toLower (T.strip raw) of
          "true" -> Right True
          "1" -> Right True
          "yes" -> Right True
          "on" -> Right True
          "si" -> Right True
          "sí" -> Right True
          "false" -> Right False
          "0" -> Right False
          "no" -> Right False
          "off" -> Right False
          _ -> Left ("Invalid field: " <> T.unpack fieldName <> " must be a boolean")

      lookupSingleInput name mp =
        case filter (\(Input nm _) -> nm == name) (inputs mp) of
          [] -> Right Nothing
          [x] -> Right (Just x)
          _ -> Left ("Duplicate field: " <> T.unpack name)

      lookupSingleFile name mp =
        case [file | file <- files mp, fdInputName file == name] of
          [] -> Right Nothing
          [file] -> Right (Just file)
          _ -> Left ("Duplicate file field: " <> T.unpack name)

      inputValueText :: Input -> Text
      inputValueText (Input _ value) = value

readMaybeDay :: Text -> Either String Day
readMaybeDay txt =
  maybe
    (Left "Invalid date format for sessionDate (expected YYYY-MM-DD)")
    Right
    (parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack txt) :: Maybe Day)

withAliasedObject :: String -> (Object -> Parser a) -> Value -> Parser a
withAliasedObject = withObject

parseAliasedRequiredField
  :: (Eq a, FromJSON a)
  => Object
  -> Text
  -> Text
  -> Parser a
parseAliasedRequiredField obj canonicalField legacyField = do
  canonicalValue <- obj .:? AesonKey.fromText canonicalField
  legacyValue <- obj .:? AesonKey.fromText legacyField
  case (canonicalValue, legacyValue) of
    (Just canonical, Just legacy)
      | canonical == legacy -> pure canonical
      | otherwise -> fail conflictingMessage
    (Just canonical, Nothing) -> pure canonical
    (Nothing, Just legacy) -> pure legacy
    (Nothing, Nothing) -> fail missingMessage
  where
    conflictingMessage =
      "Conflicting fields: "
        <> T.unpack canonicalField
        <> " and "
        <> T.unpack legacyField
        <> " must match when both are provided"
    missingMessage = "Missing required field: " <> T.unpack canonicalField

parseAliasedOptionalField
  :: (Eq a, FromJSON a)
  => Object
  -> Text
  -> Text
  -> Parser (Maybe a)
parseAliasedOptionalField obj canonicalField legacyField = do
  canonicalValue <- obj .:? AesonKey.fromText canonicalField
  legacyValue <- obj .:? AesonKey.fromText legacyField
  case (canonicalValue, legacyValue) of
    (Just canonical, Just legacy)
      | canonical == legacy -> pure (Just canonical)
      | otherwise -> fail conflictingMessage
    (Just canonical, Nothing) -> pure (Just canonical)
    (Nothing, Just legacy) -> pure (Just legacy)
    (Nothing, Nothing) -> pure Nothing
  where
    conflictingMessage =
      "Conflicting fields: "
        <> T.unpack canonicalField
        <> " and "
        <> T.unpack legacyField
        <> " must match when both are provided"
