{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.LiveSessions
  ( LiveSessionsAPI
  , LiveSessionIntakePayload(..)
  , LiveSessionMusicianPayload(..)
  ) where

import           Data.Aeson           (FromJSON, eitherDecodeStrict')
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Time            (Day)
import           Data.Time.Format     (defaultTimeLocale, parseTimeM)
import           GHC.Generics         (Generic)
import           Servant
import           Servant.Multipart
import           Data.Text.Encoding   (encodeUtf8)

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

data LiveSessionIntakePayload = LiveSessionIntakePayload
  { lsiBandName     :: Text
  , lsiContactEmail :: Maybe Text
  , lsiContactPhone :: Maybe Text
  , lsiSessionDate  :: Maybe Day
  , lsiMusicians    :: [LiveSessionMusicianPayload]
  , lsiRider        :: Maybe (FileData Tmp)
  } deriving (Show, Generic)

instance FromMultipart Tmp LiveSessionIntakePayload where
  fromMultipart multipart = do
    bandName <- lookupText "bandName" multipart
    let contactEmail = optionalText "contactEmail" multipart
        contactPhone = optionalText "contactPhone" multipart
        sessionDate  = optionalDay "sessionDate" multipart
        riderFile    = lookupFile "rider" multipart
    musiciansTxt <- lookupText "musicians" multipart
    musicians <- decodeMusicians musiciansTxt
    pure LiveSessionIntakePayload
      { lsiBandName     = T.strip bandName
      , lsiContactEmail = fmap T.strip contactEmail
      , lsiContactPhone = fmap T.strip contactPhone
      , lsiSessionDate  = sessionDate
      , lsiMusicians    = musicians
      , lsiRider        = riderFile
      }
    where
      lookupText name mp = maybe (Left ("Missing field: " <> T.unpack name)) (Right . inputValue) (lookupInput name mp)
      optionalText name mp = fmap T.strip (inputValue <$> lookupInput name mp)
      lookupFile name mp = lookup name [(fdInputName f, f) | f <- files mp]
      optionalDay name mp = do
        raw <- optionalText name mp
        case raw of
          Nothing -> Nothing
          Just txt | T.null txt -> Nothing
                   | otherwise ->
                       case readMaybeDay txt of
                         Left _  -> Nothing
                         Right d -> Just d

      decodeMusicians txt =
        case eitherDecodeStrict' (encodeUtf8 txt) of
          Left err -> Left ("Invalid musicians payload: " <> err)
          Right xs -> Right xs

readMaybeDay :: Text -> Either String Day
readMaybeDay txt =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack txt) of
    Just d  -> Right d
    Nothing -> Left "Invalid date"
