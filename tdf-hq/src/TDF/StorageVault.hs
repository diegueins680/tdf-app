{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- Delegated credentials never leave this server. Handles are identifiers, not
-- capabilities: every operation also requires the current owning TDF session.
module TDF.StorageVault
  ( sealCredential, openCredential, credentialBinding, isVaultHandle
  , storeConnection, resolveConnection, refreshConnection, revokeConnection
  ) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import qualified Control.Exception.Safe as Safe
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types
  ( AEADMode(AEAD_GCM), AuthTag(..), aeadInit, aeadSimpleDecrypt
  , aeadSimpleEncrypt, cipherInit )
import Crypto.Error (CryptoFailable(..))
import Crypto.Random (getRandomBytes)
import Data.Aeson (FromJSON(..), ToJSON(..), decodeStrict', encode, object, withObject, (.:), (.:?), (.=))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, addUTCTime, getCurrentTime, diffUTCTime)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawExecute, rawSql, runSqlPool)
import Network.HTTP.Client
  ( Manager, Request(..), RequestBody(..), httpLbs, parseRequest
  , responseBody, responseStatus, responseTimeoutMicro )
import Network.HTTP.Types (renderSimpleQuery, statusCode)
import Servant
import System.Environment (lookupEnv)

import TDF.API.Types (DriveTokenResponse(..))
import TDF.Auth (AuthedUser(..), withCurrentAuthSession)
import TDF.DB (Env(..))

type AppM = ReaderT Env Handler

data Credential = Credential Text (Maybe Text)
instance Show Credential where show _ = "<delegated-credential>"
instance ToJSON Credential where
  toJSON (Credential access refresh) = object ["access" .= access, "refresh" .= refresh]
instance FromJSON Credential where
  parseJSON = withObject "Credential" $ \o -> Credential <$> o .: "access" <*> o .:? "refresh"

credentialBinding :: Int64 -> Text -> Text
credentialBinding owner identifier = "tdf:google-drive:v1:" <> T.pack (show owner) <> ":" <> identifier

cryptoResult :: CryptoFailable a -> Either Text a
cryptoResult (CryptoPassed value) = Right value
cryptoResult (CryptoFailed _) = Left "storage_vault_unavailable"

-- Version || fresh 96-bit nonce || 128-bit authentication tag || ciphertext.
-- AAD prevents transplanting ciphertext to a different account or connection.
sealCredential :: BS.ByteString -> BS.ByteString -> Text -> BS.ByteString -> Either Text BS.ByteString
sealCredential key nonce binding plaintext = do
  unless (BS.length key == 32 && BS.length nonce == 12) (Left "storage_vault_unavailable")
  cipher <- cryptoResult (cipherInit key :: CryptoFailable AES256)
  aead <- cryptoResult (aeadInit AEAD_GCM cipher nonce)
  let (tag, ciphertext) = aeadSimpleEncrypt aead (TE.encodeUtf8 binding) plaintext 16
  pure (BS.singleton 1 <> nonce <> BA.convert tag <> ciphertext)

openCredential :: BS.ByteString -> Text -> BS.ByteString -> Either Text BS.ByteString
openCredential key binding sealed = do
  unless (BS.length key == 32 && BS.length sealed >= 29 && BS.take 1 sealed == BS.singleton 1)
    (Left "storage_vault_unavailable")
  cipher <- cryptoResult (cipherInit key :: CryptoFailable AES256)
  aead <- cryptoResult (aeadInit AEAD_GCM cipher (BS.take 12 (BS.drop 1 sealed)))
  let tag = AuthTag (BA.convert (BS.take 16 (BS.drop 13 sealed)))
  maybe (Left "storage_vault_unavailable") Right
    (aeadSimpleDecrypt aead (TE.encodeUtf8 binding) (BS.drop 29 sealed) tag)

isVaultHandle :: Text -> Bool
isVaultHandle handle = maybe False (const True) (handleId handle)

handleId :: Text -> Maybe Text
handleId handle = do
  raw <- T.stripPrefix "tdf-vault-v1_" handle
  uuid <- UUID.fromText raw
  if UUID.toText uuid == raw then Just raw else Nothing

failure :: ServerError -> Text -> ServerError
failure base code = base
  { errBody = encode (object ["code" .= code])
  , errHeaders = [("Content-Type", "application/json"), ("Cache-Control", "no-store")]
  }

vaultKey :: AppM BS.ByteString
vaultKey = do
  raw <- liftIO (lookupEnv "CONTENT_STORAGE_VAULT_KEY")
  case raw of
    Just value | length value == 64 -> case B16.decode (TE.encodeUtf8 (T.pack value)) of
      Right key | BS.length key == 32 -> pure key
      _ -> unavailable
    _ -> unavailable
  where unavailable = throwError (failure err503 "storage_vault_unavailable")

runOwned :: AuthedUser -> SqlPersistT IO a -> AppM a
runOwned user action = do
  pool <- asks envPool
  result <- liftIO $ Safe.tryAny $ runSqlPool (withCurrentAuthSession user action) pool
  case result of
    Right (Just value) -> pure value
    Right Nothing -> throwError err401
    Left _ -> throwError (failure err503 "storage_vault_unavailable")

descriptor :: Text -> Int -> DriveTokenResponse
descriptor identifier seconds = DriveTokenResponse
  { accessToken = "tdf-vault-v1_" <> identifier
  , refreshToken = Just ("tdf-vault-v1_" <> identifier)
  , expiresIn = max 1 seconds
  , tokenType = Just "TDFConnection"
  }

storeConnection :: AuthedUser -> Text -> Maybe Text -> Int -> AppM DriveTokenResponse
storeConnection user access refresh seconds = do
  key <- vaultKey
  identifier <- UUID.toText <$> liftIO nextRandom
  nonce <- liftIO (getRandomBytes 12)
  let owner = fromSqlKey (auPartyId user)
  sealed <- either (throwError . failure err503) pure
    (sealCredential key nonce (credentialBinding owner identifier) (BL.toStrict (encode (Credential access refresh))))
  now <- liftIO getCurrentTime
  runOwned user $ do
    -- Serialize reconnects even when no previous connection exists. A reconnect
    -- erases the previous credential; a late refresh cannot resurrect it.
    _ <- rawSql "SELECT id FROM party WHERE id=? FOR UPDATE" [PersistInt64 owner] :: SqlPersistT IO [Single Int64]
    rawExecute "DELETE FROM content_storage_credential WHERE owner_party_id=?" [PersistInt64 owner]
    rawExecute "INSERT INTO content_storage_credential(id,owner_party_id,sealed,expires_at) VALUES(?::uuid,?,?,?)"
      [PersistText identifier, PersistInt64 owner, PersistByteString sealed, PersistUTCTime (addUTCTime (fromIntegral seconds) now)]
  pure (descriptor identifier seconds)

-- The database lock covers provider refresh and ciphertext replacement. This is
-- single-flight across replicas, and disconnect waits for in-flight refresh.
loadConnection :: Manager -> AuthedUser -> Text -> AppM (Text, Int)
loadConnection manager user handle = do
  identifier <- maybe (throwError (failure err409 "storage_reconnect_required")) pure (handleId handle)
  key <- vaultKey
  result <- runOwned user $ do
    rows <- rawSql "SELECT sealed,expires_at FROM content_storage_credential WHERE id=?::uuid AND owner_party_id=? FOR UPDATE"
      [PersistText identifier,PersistInt64 (fromSqlKey (auPartyId user))]
      :: SqlPersistT IO [(Single BS.ByteString, Single UTCTime)]
    now <- liftIO getCurrentTime
    case rows of
      [(Single sealed,Single expiry)] -> case openCredential key (credentialBinding (fromSqlKey (auPartyId user)) identifier) sealed >>= maybe (Left "storage_vault_unavailable") Right . decodeStrict' of
        Left code -> pure (Left (failure err503 code))
        Right credential@(Credential access refresh)
          | expiry > addUTCTime 60 now -> pure (Right (access, floor (diffUTCTime expiry now)))
          | otherwise -> do
              refreshed <- liftIO (refreshProvider manager credential)
              case refreshed of
                Left err -> pure (Left err)
                Right (Credential nextAccess nextRefresh, seconds) -> do
                  nonce <- liftIO (getRandomBytes 12)
                  case sealCredential key nonce (credentialBinding (fromSqlKey (auPartyId user)) identifier)
                       (BL.toStrict (encode (Credential nextAccess (nextRefresh `orElse` refresh)))) of
                    Left code -> pure (Left (failure err503 code))
                    Right replacement -> do
                      completed <- liftIO getCurrentTime
                      rawExecute "UPDATE content_storage_credential SET sealed=?,expires_at=?,revision=revision+1 WHERE id=?::uuid AND owner_party_id=?"
                        [PersistByteString replacement,PersistUTCTime (addUTCTime (fromIntegral seconds) completed),PersistText identifier,PersistInt64 (fromSqlKey (auPartyId user))]
                      pure (Right (nextAccess,seconds))
      _ -> pure (Left (failure err409 "storage_reconnect_required"))
  either throwError pure result
  where orElse (Just value) _ = Just value
        orElse Nothing fallback = fallback

resolveConnection :: Manager -> AuthedUser -> Text -> AppM Text
resolveConnection manager user handle = fst <$> loadConnection manager user handle

refreshConnection :: Manager -> AuthedUser -> Text -> AppM DriveTokenResponse
refreshConnection manager user handle = do
  (_,seconds) <- loadConnection manager user handle
  identifier <- maybe (throwError err409) pure (handleId handle)
  pure (descriptor identifier seconds)

revokeConnection :: AuthedUser -> AppM NoContent
revokeConnection user = do
  runOwned user $ do
    _ <- rawSql "SELECT id FROM party WHERE id=? FOR UPDATE" [PersistInt64 (fromSqlKey (auPartyId user))] :: SqlPersistT IO [Single Int64]
    rawExecute "DELETE FROM content_storage_credential WHERE owner_party_id=?" [PersistInt64 (fromSqlKey (auPartyId user))]
  pure NoContent

data ProviderToken = ProviderToken Text (Maybe Text) Int
instance FromJSON ProviderToken where
  parseJSON = withObject "ProviderToken" $ \o -> do
    access <- o .: "access_token"
    refresh <- o .:? "refresh_token"
    seconds <- o .: "expires_in"
    when (T.null access || T.length access > 4096 || seconds <= 0 || seconds > (86400 :: Int)) (fail "Invalid credential")
    pure (ProviderToken access refresh seconds)

refreshProvider :: Manager -> Credential -> IO (Either ServerError (Credential, Int))
refreshProvider _ (Credential _ Nothing) = pure (Left (failure err409 "storage_reconnect_required"))
refreshProvider manager (Credential _ (Just refresh)) = do
  result <- Safe.tryAny $ do
    client <- lookupEnv "DRIVE_CLIENT_ID"
    secret <- lookupEnv "DRIVE_CLIENT_SECRET"
    case (client,secret) of
      (Just cid,Just cs) | not (null cid || null cs) -> do
        request <- parseRequest "https://oauth2.googleapis.com/token"
        response <- httpLbs request
          { method = "POST", redirectCount = 0, responseTimeout = responseTimeoutMicro 30000000
          , requestHeaders = [("Content-Type","application/x-www-form-urlencoded")]
          , requestBody = RequestBodyBS (renderSimpleQuery False
              [("client_id",TE.encodeUtf8 (T.pack cid)),("client_secret",TE.encodeUtf8 (T.pack cs))
              ,("refresh_token",TE.encodeUtf8 refresh),("grant_type","refresh_token")])
          } manager
        if statusCode (responseStatus response) == 400 || statusCode (responseStatus response) == 401
          then pure (Left (failure err409 "storage_reconnect_required"))
          else if statusCode (responseStatus response) /= 200
            then pure (Left (failure err503 "storage_provider_unavailable"))
            else case decodeStrict' (BL.toStrict (responseBody response)) of
              Just (ProviderToken access next seconds) -> pure (Right (Credential access next,seconds))
              Nothing -> pure (Left (failure err502 "storage_provider_unavailable"))
      _ -> pure (Left (failure err503 "storage_vault_unavailable"))
  pure (either (const (Left (failure err503 "storage_provider_unavailable"))) id result)
