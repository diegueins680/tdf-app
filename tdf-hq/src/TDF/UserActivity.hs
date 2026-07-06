{-# LANGUAGE OverloadedStrings #-}

module TDF.UserActivity
  ( listRecentUserActivities
  , recordUserActivity
  , redactActivityValue
  , validateUserActivityLimit
  ) where

import           Control.Applicative    ((<|>))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson             as Aeson
import           Data.Aeson             (Value(..), decodeStrict')
import qualified Data.Aeson.Key         as Key
import qualified Data.Aeson.KeyMap      as KeyMap
import qualified Data.ByteString.Lazy   as BL
import           Data.Char              (isControl, isSpace)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Data.Time              (getCurrentTime)
import           Database.Persist       ( Entity(..)
                                        , SelectOpt(..)
                                        , get
                                        , insert_
                                        , selectList
                                        , (==.)
                                        )
import           Database.Persist.Sql   (SqlPersistT, fromSqlKey)
import           Servant                (ServerError, err400, errBody)

import           TDF.DTO                (UserActivityDTO(..))
import           TDF.Models

recordUserActivity
  :: Maybe PartyId
  -> Text
  -> Text
  -> Text
  -> Maybe Value
  -> SqlPersistT IO ()
recordUserActivity actorId entity entityId action metadata = do
  now <- liftIO getCurrentTime
  insert_ AuditLog
    { auditLogActorId = actorId
    , auditLogEntity = normalizeActivityText entity
    , auditLogEntityId = normalizeActivityText entityId
    , auditLogAction = normalizeActivityText action
    , auditLogDiff = encodeActivityMetadata <$> metadata
    , auditLogCreatedAt = now
    }

listRecentUserActivities :: Int -> SqlPersistT IO [UserActivityDTO]
listRecentUserActivities limit = do
  rows <- selectList [] [Desc AuditLogCreatedAt, Desc AuditLogId, LimitTo limit]
  mapM activityToDTO rows

validateUserActivityLimit :: Maybe Int -> Either ServerError Int
validateUserActivityLimit rawLimit =
  case rawLimit of
    Nothing -> Right defaultActivityLimit
    Just limit
      | limit < 1 ->
          Left err400 { errBody = "activity limit must be at least 1" }
      | limit > maxActivityLimit ->
          Left err400 { errBody = "activity limit must be 1000 or fewer" }
      | otherwise ->
          Right limit

activityToDTO :: Entity AuditLog -> SqlPersistT IO UserActivityDTO
activityToDTO (Entity key activity) = do
  (actorName, usernames, roles) <- resolveActor (auditLogActorId activity)
  pure UserActivityDTO
    { uaId = fromSqlKey key
    , uaCreatedAt = auditLogCreatedAt activity
    , uaActorPartyId = fmap fromSqlKey (auditLogActorId activity)
    , uaActorName = actorName
    , uaActorUsernames = usernames
    , uaActorRoles = roles
    , uaEntity = auditLogEntity activity
    , uaEntityId = auditLogEntityId activity
    , uaAction = auditLogAction activity
    , uaMetadata = auditLogDiff activity >>= decodeActivityMetadata
    }

resolveActor :: Maybe PartyId -> SqlPersistT IO (Text, [Text], [Text])
resolveActor Nothing =
  pure ("Sistema / sin autenticar", [], [])
resolveActor (Just partyKey) = do
  mParty <- get partyKey
  creds <- selectList
    [ UserCredentialPartyId ==. partyKey
    , UserCredentialActive ==. True
    ]
    [Asc UserCredentialUsername]
  roles <- selectList
    [ PartyRolePartyId ==. partyKey
    , PartyRoleActive ==. True
    ]
    [Asc PartyRoleRole]
  let actorName =
        case mParty of
          Nothing -> "Usuario desconocido"
          Just party ->
            fromMaybe
              "Usuario sin nombre"
              (cleanDisplayName (partyDisplayName party) <|> (partyLegalName party >>= cleanDisplayName))
      usernames =
        dedupeText (map (userCredentialUsername . entityVal) creds)
      roleLabels =
        dedupeText (map (roleToText . partyRoleRole . entityVal) roles)
  pure (actorName, usernames, roleLabels)

cleanDisplayName :: Text -> Maybe Text
cleanDisplayName raw =
  let cleaned = T.strip raw
  in if T.null cleaned then Nothing else Just cleaned

normalizeActivityText :: Text -> Text
normalizeActivityText raw =
  let cleaned = T.filter (not . isUnsafeActivityTextChar) (T.strip raw)
  in if T.null cleaned then "unknown" else T.take maxActivityTextChars cleaned

isUnsafeActivityTextChar :: Char -> Bool
isUnsafeActivityTextChar ch =
  isControl ch

encodeActivityMetadata :: Value -> Text
encodeActivityMetadata =
  TE.decodeUtf8 . BL.toStrict . Aeson.encode . redactActivityValue

decodeActivityMetadata :: Text -> Maybe Value
decodeActivityMetadata raw =
  redactActivityValue <$> decodeStrict' (TE.encodeUtf8 raw)

redactActivityValue :: Value -> Value
redactActivityValue (Object obj) =
  Object (KeyMap.mapWithKey redactField obj)
  where
    redactField key value
      | isSensitiveActivityKey (Key.toText key) = String "[redacted]"
      | otherwise = redactActivityValue value
redactActivityValue (Array items) =
  Array (fmap redactActivityValue items)
redactActivityValue value = value

isSensitiveActivityKey :: Text -> Bool
isSensitiveActivityKey rawKey =
  any (`T.isInfixOf` key)
    [ "authorization"
    , "body"
    , "email"
    , "message"
    , "password"
    , "phone"
    , "secret"
    , "text"
    , "token"
    , "whatsapp"
    ]
  where
    key = T.toLower (T.filter (not . isSpace) rawKey)

dedupeText :: [Text] -> [Text]
dedupeText = go []
  where
    go _ [] = []
    go seen (value:rest)
      | T.null cleaned = go seen rest
      | T.toCaseFold cleaned `elem` seen = go seen rest
      | otherwise = cleaned : go (T.toCaseFold cleaned : seen) rest
      where
        cleaned = T.strip value

defaultActivityLimit :: Int
defaultActivityLimit = 200

maxActivityLimit :: Int
maxActivityLimit = 1000

maxActivityTextChars :: Int
maxActivityTextChars = 160
