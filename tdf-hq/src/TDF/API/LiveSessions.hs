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

import           Data.Aeson               (FromJSON, eitherDecodeStrict')
import           Data.List                (find)
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

instance FromJSON LiveSessionMusicianPayload

data LiveSessionSongPayload = LiveSessionSongPayload
  { lssTitle     :: Text
  , lssBpm       :: Maybe Int
  , lssSongKey   :: Maybe Text
  , lssLyrics    :: Maybe Text
  , lssSortOrder :: Maybe Int
  } deriving (Show, Generic)

instance FromJSON LiveSessionSongPayload

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
    contactEmail <- optionalText "contactEmail" multipart
    contactPhone <- optionalText "contactPhone" multipart
    let riderFile    = lookupFile "rider" multipart
    sessionDate <- optionalDay "sessionDate" multipart
    availability <- optionalText "availability" multipart
    acceptedTerms <- optionalBool "acceptedTerms" multipart
    termsVersion <- optionalText "termsVersion" multipart
    musiciansTxt <- lookupText "musicians" multipart
    musicians <- decodeMusicians musiciansTxt
    setlistTxt <- optionalText "setlist" multipart
    setlist <- maybe (Right []) decodeSetlist setlistTxt
    pure LiveSessionIntakePayload
      { lsiBandName     = T.strip bandName
      , lsiBandDescription = fmap T.strip bandDescription
      , lsiPrimaryGenre = fmap T.strip primaryGenre
      , lsiInputList    = fmap T.strip inputList
      , lsiContactEmail = fmap T.strip contactEmail
      , lsiContactPhone = fmap T.strip contactPhone
      , lsiSessionDate  = sessionDate
      , lsiAvailability = fmap T.strip availability
      , lsiAcceptedTerms = acceptedTerms
      , lsiTermsVersion = fmap T.strip termsVersion
      , lsiMusicians    = musicians
      , lsiSetlist      = setlist
      , lsiRider        = riderFile
      }
    where
      lookupInputByName name mp =
        find (\(Input nm _) -> nm == name) (inputs mp)
      lookupText name mp =
        maybe (Left ("Missing field: " <> T.unpack name)) (Right . inputValueText) (lookupInputByName name mp)
      optionalText name mp =
        case lookupInputByName name mp of
          Nothing  -> Right Nothing
          Just inp -> Right (Just (inputValueText inp))
      lookupFile name mp = lookup name [(fdInputName f, f) | f <- files mp]
      optionalDay name mp =
        case lookupInputByName name mp of
          Nothing  -> Right Nothing
          Just inp ->
            let txt = T.strip (inputValueText inp)
            in if T.null txt
                 then Right Nothing
                 else fmap Just (readMaybeDay txt)
      optionalBool name mp =
        case lookupInputByName name mp of
          Nothing -> Right False
          Just inp ->
            let txt = T.toLower (T.strip (inputValueText inp))
            in Right (txt `elem` ["true", "1", "yes", "on", "si", "sí"])

      decodeMusicians txt =
        case eitherDecodeStrict' (encodeUtf8 txt) of
          Left err -> Left ("Invalid musicians payload: " <> err)
          Right xs -> Right xs
      decodeSetlist txt =
        case eitherDecodeStrict' (encodeUtf8 txt) of
          Left err -> Left ("Invalid setlist payload: " <> err)
          Right xs -> Right xs

      inputValueText :: Input -> Text
      inputValueText (Input _ value) = value

readMaybeDay :: Text -> Either String Day
readMaybeDay txt =
  maybe
    (Left "Invalid date format for sessionDate (expected YYYY-MM-DD)")
    Right
    (parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack txt) :: Maybe Day)
