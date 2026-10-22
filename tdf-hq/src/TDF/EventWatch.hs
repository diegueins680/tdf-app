{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- Deliberately separate from interactive sessions and the general protected API.
module TDF.EventWatch
  ( EventWatchAPI, EventWatchPrincipal, authHandler, eventWatchServer
  , TokenRequest(..), TokenResponse(..), ImportRequest(..), ImportResponse(..)
  , secretDigest, validClientId, sessionValid, tokenTTL
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Control.Exception.Safe (tryAny)
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.Random (getRandomBytes)
import Data.Aeson (FromJSON(..), ToJSON, defaultOptions, genericParseJSON, rejectUnknownFields)
import Data.ByteArray (constEq)
import Data.ByteArray.Encoding (Base(Base16), convertToBase)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Int (Int64)
import Database.Persist (Entity(..), PersistValue(..), SelectOpt(..), getJust, selectList, (==.))
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawExecute, rawSql,
                             runSqlPool, transactionUndo)
import GHC.Generics (Generic)
import Network.Wai (Request, requestHeaders)
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

import TDF.Auth (parseBearerAuthorizationHeader)
import TDF.DB (Env(..))
import TDF.DTO.EventResearchDTO
import TDF.Models.SocialEventsModels
import qualified TDF.Server.EventResearch as Research

type NoStore a = Headers '[Header "Cache-Control" T.Text, Header "Pragma" T.Text] a
type EventWatchAPI = "integrations" :> "event-watch" :>
  ( "token" :> ReqBody '[JSON] TokenRequest :> Post '[JSON] (NoStore TokenResponse)
    :<|> AuthProtect "event-watch" :>
      ( "candidates" :> Get '[JSON] (NoStore [EventResearchCandidateDTO])
        :<|> "import" :> ReqBody '[JSON] ImportRequest
          :> Put '[JSON] (NoStore ImportResponse)
      )
  )

data TokenRequest = TokenRequest
  { grant_type :: T.Text, client_id :: T.Text, client_secret :: T.Text }
  deriving (Generic)
instance FromJSON TokenRequest where
  parseJSON = genericParseJSON defaultOptions{rejectUnknownFields=True}

data TokenResponse = TokenResponse
  { access_token :: T.Text, token_type :: T.Text, expires_in :: Int, scope :: T.Text }
  deriving (Generic)
instance ToJSON TokenResponse

data ImportRequest = ImportRequest
  { runKey :: T.Text
  , candidate :: EventResearchCandidateWriteDTO
  , materialize :: Bool
  , publish :: Bool
  } deriving (Generic)
instance FromJSON ImportRequest where
  parseJSON = genericParseJSON defaultOptions{rejectUnknownFields=True}

data ImportResponse = ImportResponse
  { candidateRecord :: EventResearchCandidateDTO
  , eventLink :: Maybe EventResearchMaterializationDTO
  } deriving (Generic)
instance ToJSON ImportResponse

-- Hash only; neither the raw token nor the client secret enters a principal/log.
newtype EventWatchPrincipal = EventWatchPrincipal T.Text
type instance AuthServerData (AuthProtect "event-watch") = EventWatchPrincipal

data Client = Client T.Text T.Text Bool UTCTime -- id, audit Party, publication, expiry

tokenTTL :: NominalDiffTime
tokenTTL = 900

secretDigest :: T.Text -> T.Text
secretDigest secret = T.pack (show (hash (TE.encodeUtf8 secret) :: Digest SHA256))

validClientId :: T.Text -> Bool
validClientId value = T.length value >= 3 && T.length value <= 48
  && maybe False (asciiAlphaNum . fst) (T.uncons value)
  && T.all (\c -> asciiAlphaNum c || c == '_' || c == '-') value
  where asciiAlphaNum c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

sessionValid :: UTCTime -> UTCTime -> UTCTime -> UTCTime -> Bool
sessionValid now issued expiry clientExpiry = issued <= now && now < expiry
  && expiry <= addUTCTime tokenTTL issued && now < clientExpiry

noStore :: a -> NoStore a
noStore = addHeader ("no-store" :: T.Text) . addHeader ("no-cache" :: T.Text)

-- Auth failures intentionally reveal no identity, secret or SQL details.
denied :: ServerError
denied = err401{errBody="Invalid or expired service credentials",
  errHeaders=[("Cache-Control","no-store"), ("WWW-Authenticate","Bearer")]}

runServiceDB :: Env -> SqlPersistT IO (Either ServerError a) -> Handler a
runServiceDB env action = do
  result <- liftIO $ tryAny $ runSqlPool (do
    outcome <- action
    case outcome of
      Left _ -> transactionUndo >> pure outcome
      Right _ -> pure outcome) (envPool env)
  either (const (throwError err503{errBody="Event watch unavailable"}))
    (either throwError pure) result

-- Client first, then session: rotation/revocation takes a conflicting client
-- lock, and cannot commit in the middle of an admitted import transaction.
withSession :: EventWatchPrincipal -> (Client -> SqlPersistT IO (Either ServerError a))
  -> SqlPersistT IO (Either ServerError a)
withSession (EventWatchPrincipal digest) action = do
  clients <- rawSql
    "SELECT c.client_id,c.actor_party_id::text,c.allow_publish,c.expires_at FROM event_watch_client c JOIN event_watch_session s ON s.client_id=c.client_id WHERE s.token_hash=? AND c.active FOR SHARE OF c"
    [PersistText digest]
  case clients of
    [(Single ident, Single actor, Single canPublish, Single clientExpiry)] -> do
      sessions <- rawSql
        "SELECT issued_at,expires_at FROM event_watch_session WHERE token_hash=? AND client_id=? FOR SHARE"
        [PersistText digest, PersistText ident]
      now <- liftIO getCurrentTime
      case sessions of
        [(Single issued, Single expiry)] | sessionValid now issued expiry clientExpiry -> do
          outcome <- action (Client ident actor canPublish clientExpiry)
          finished <- liftIO getCurrentTime
          -- Locks prevent revocation races; this second clock check prevents a
          -- request that waited on an event lock from committing after expiry.
          pure $ if sessionValid finished issued expiry clientExpiry
            then outcome else Left denied
        _ -> pure (Left denied)
    _ -> pure (Left denied)

authHandler :: Env -> AuthHandler Request EventWatchPrincipal
authHandler env = mkAuthHandler $ \request -> do
  -- Cookies and interactive tokens are never accepted in this lane.
  raw <- case [v | (k,v) <- requestHeaders request, k == "Authorization"] of
    [value] -> case TE.decodeUtf8' value of
      Left _ -> throwError denied
      Right header -> either (const (throwError denied)) pure
        (parseBearerAuthorizationHeader header)
    _ -> throwError denied
  unless (T.length raw == 64 && T.all (`elem` ("0123456789abcdef" :: String)) raw)
    (throwError denied)
  let principal = EventWatchPrincipal (secretDigest raw)
  runServiceDB env (withSession principal (const (pure (Right principal))))

eventWatchServer :: ServerT EventWatchAPI (ReaderT Env Handler)
eventWatchServer = exchange :<|> protected
  where
    exchange request = do
      env <- ask
      unless (request.grant_type == "client_credentials" && validClientId request.client_id
              && T.length request.client_secret == 64
              && T.all (`elem` ("0123456789abcdef" :: String)) request.client_secret)
        (throwError denied)
      bytes <- liftIO (getRandomBytes 32 :: IO ByteString)
      let token = TE.decodeUtf8 (convertToBase Base16 bytes)
      response <- lift $ runServiceDB env $ do
        rows <- rawSql
          "SELECT secret_hash,allow_publish,expires_at FROM event_watch_client WHERE client_id=? AND active FOR UPDATE"
          [PersistText request.client_id]
        now <- liftIO getCurrentTime
        case rows of
          [(Single digest, Single canPublish, Single expires)]
            | TE.encodeUtf8 digest `constEq` TE.encodeUtf8 (secretDigest request.client_secret)
            , now < expires -> do
                let untilTime = min expires (addUTCTime tokenTTL now)
                -- Bound retained rows even when a job exchanges on every run.
                rawExecute "DELETE FROM event_watch_session WHERE client_id=? AND expires_at<=?"
                  [PersistText request.client_id, PersistUTCTime now]
                rawExecute "DELETE FROM event_watch_session WHERE token_hash IN (SELECT token_hash FROM event_watch_session WHERE client_id=? ORDER BY issued_at DESC,token_hash OFFSET 3)"
                  [PersistText request.client_id]
                rawExecute "INSERT INTO event_watch_session(token_hash,client_id,issued_at,expires_at) VALUES (?,?,?,?)"
                  [PersistText (secretDigest token), PersistText request.client_id,
                   PersistUTCTime now, PersistUTCTime untilTime]
                pure $ Right $ TokenResponse token "Bearer"
                  (floor (diffUTCTime untilTime now))
                  (if canPublish then "event-watch:write event-watch:publish" else "event-watch:write")
          _ -> pure (Left denied)
      pure (noStore response)

    protected principal = listCandidates principal :<|> importEvent principal

    listCandidates principal = do
      env <- ask
      result <- lift $ runServiceDB env $ withSession principal $ \(Client ident _ _ _) -> do
        rows <- selectList [EventResearchCandidateProvider ==. providerFor ident]
          [Desc EventResearchCandidateVerifiedAt, LimitTo 500]
        pure $ either (const (Left err503)) Right (traverse Research.candidateEntityToDTO rows)
      pure (noStore result)

    importEvent principal request = do
      env <- ask
      result <- lift $ runServiceDB env $ withSession principal $ \(Client ident actor canPublish _) ->
        if request.publish && (not canPublish || not request.materialize)
          then pure (Left err403{errBody="Publication scope and materialization required"})
          else if not (validRunKey request.runKey)
            then pure (Left err400{errBody="Invalid run key"})
            else do
              -- One client cannot select another client's run/provider/source.
              -- Serialize retries; all candidate, entity and audit writes commit
              -- together, without the admin lane's intermediate transactionSave.
              _ <- rawSql "SELECT 1::bigint FROM pg_advisory_xact_lock(hashtextextended(?,0))"
                [PersistText ("event-watch:" <> ident)] :: SqlPersistT IO [Single Int64]
              now <- liftIO getCurrentTime
              Entity runId _ <- Research.createRunDb actor
                ("event-watch:" <> ident <> ":" <> request.runKey) True Nothing now
              let scoped = request.candidate
                    { erCandidateProvider=providerFor ident
                    , erCandidateRunId=T.pack (show (fromSqlKey runId))
                    , erCandidateSourceId=Nothing }
              case Research.validateEventResearchCandidate scoped of
                Left _ -> pure (Left err400{errBody="Invalid event candidate; check source evidence and required fields"})
                Right validated
                  | validated.erCandidateCountryCode /= "EC" -> pure (Left err403)
                  | otherwise -> do
                      saved <- Research.upsertCandidateDb validated now
                      case saved of
                        Left failure -> pure (Left failure)
                        Right entity@(Entity candidateId _) ->
                          case Research.candidateEntityToDTO entity of
                            Left _ -> pure (Left err503)
                            Right dto -> if not request.materialize
                              then pure (Right (ImportResponse dto Nothing))
                              else do
                                link <- Research.materializeCandidateDb actor candidateId runId
                                  (EventResearchMaterializationRequestDTO
                                    (T.pack (show (fromSqlKey runId))) request.publish) now
                                case link of
                                  Left failure -> pure (Left failure)
                                  Right value -> do
                                    updated <- getJust candidateId
                                    pure $ case Research.candidateEntityToDTO (Entity candidateId updated) of
                                      Left _ -> Left err503
                                      Right current -> Right (ImportResponse current (Just value))
      pure (noStore result)

providerFor :: T.Text -> T.Text
providerFor ident = "event-watch:" <> ident

validRunKey :: T.Text -> Bool
validRunKey value = not (T.null value) && T.length value <= 80
  && T.all (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
                || (c >= '0' && c <= '9') || c `elem` ("._-" :: String)) value

