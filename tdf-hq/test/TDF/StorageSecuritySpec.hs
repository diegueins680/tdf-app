{-# LANGUAGE OverloadedStrings #-}
module TDF.StorageSecuritySpec (spec) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Control.Monad.Reader (runReaderT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool)
import Servant ((:<|>)(..), getResponse, ServerError(..))
import Servant.Multipart (FileData(..))
import Servant.Server (runHandler)
import System.Directory (createDirectoryIfMissing, createFileLink, getCurrentDirectory, setCurrentDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import TDF.API.Types (DriveUploadDTO(..), DriveTokenResponse(..))
import TDF.API.Drive (DriveUploadForm(..))
import TDF.Auth (loadAuthedUser)
import TDF.Config (loadConfig, assetsRootDir)
import TDF.ContentStorage
import TDF.DB (Env(..), makePool, sharedTlsManager)
import TDF.StorageVault
import TDF.Server (mkApp, driveUploadServer, privateDrivePermissionsSafe, resolveDriveClientCreds)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (parseSimpleQuery)
import Network.Wai.Handler.Warp (testWithApplication)
import Data.Aeson (encode, object, (.=))
import qualified Data.Text.Encoding as TE

spec :: Spec
spec = do
  describe "storage-security authenticated encryption" $ do
    let key = BS.replicate 32 42
        nonce = BS.replicate 12 7
        binding = credentialBinding 1 "connection-a"
        plaintext = "synthetic-provider-secret"
        Right sealed = sealCredential key nonce binding plaintext
    it "round trips while excluding plaintext from the envelope" $ do
      openCredential key binding sealed `shouldBe` Right plaintext
      (plaintext `BS.isInfixOf` sealed) `shouldBe` False
    it "rejects a different owner, connection, key, or truncated envelope" $ do
      openCredential key (credentialBinding 2 "connection-a") sealed `shouldSatisfy` isLeft
      openCredential key (credentialBinding 1 "connection-b") sealed `shouldSatisfy` isLeft
      openCredential (BS.replicate 32 43) binding sealed `shouldSatisfy` isLeft
      forM_ [0..28] $ \size -> openCredential key binding (BS.take size sealed) `shouldSatisfy` isLeft
    it "rejects tampering at every byte, including version, nonce and tag" $
      forM_ [0..BS.length sealed-1] $ \offset -> do
        let altered = BS.take offset sealed <> BS.singleton (BS.index sealed offset + 1) <> BS.drop (offset+1) sealed
        openCredential key binding altered `shouldSatisfy` isLeft
    it "requires a 256-bit key and a 96-bit nonce" $ do
      sealCredential (BS.replicate 31 0) nonce binding plaintext `shouldSatisfy` isLeft
      sealCredential key (BS.replicate 11 0) binding plaintext `shouldSatisfy` isLeft
    it "rejects provider credentials and malformed handles" $ do
      isVaultHandle "raw-google-token" `shouldBe` False
      isVaultHandle "tdf-vault-v1_00000000-0000-4000-8000-000000000001" `shouldBe` True
    it "distinguishes revoked personal consent from operator client and provider failures" $ do
      errHTTPCode (classifyRefreshFailure 400 (Just "invalid_grant")) `shouldBe` 409
      errHTTPCode (classifyRefreshFailure 400 (Just "invalid_client")) `shouldBe` 503
      errHTTPCode (classifyRefreshFailure 401 (Just "invalid_client")) `shouldBe` 503
      errHTTPCode (classifyRefreshFailure 500 (Just "invalid_grant")) `shouldBe` 503
      errHTTPCode (classifyRefreshFailure 400 Nothing) `shouldBe` 503
    it "only recognizes exact local or canonical private object references" $ do
      let identifier = "00000000-0000-4000-8000-000000000001"
          api = "https://api.tdfrecords.net"
      storageFileIdFromUrl api (api <> "/storage/files/" <> identifier) `shouldBe` Right (UUID.fromText identifier)
      forM_ ["https://evil.test/storage/files/" <> identifier, api <> "/storage/files/" <> identifier <> "?public=true", "/storage/files/../secret"] $ \url ->
        storageFileIdFromUrl api url `shouldSatisfy` isLeft
    it "rejects overlapping static roots and symbolic links escaping the public mount" $
      withSystemTempDirectory "tdf-static-boundary" $ \root -> do
        original <- getCurrentDirectory
        bracket (setCurrentDirectory root) (const (setCurrentDirectory original)) $ \_ -> do
          createDirectoryIfMissing True "uploads/content-storage"
          createDirectoryIfMissing True "public"
          BS.writeFile "uploads/content-storage/secret" "private"
          createFileLink "../uploads/content-storage/secret" "public/link"
          publicRootSafe "uploads" `shouldReturn` False
          publicRootSafe "uploads/content-storage" `shouldReturn` False
          publicRequestSafe "public" ["link"] `shouldReturn` False
    it "denies unknown, broad, foreign-user and incomplete personal provider ACLs" $ do
      let permission identifier kind role = object ["id" .= (identifier :: T.Text),"type" .= (kind :: T.Text),"role" .= (role :: T.Text)]
          own = permission "owner" "user" "owner"
      privateDrivePermissionsSafe "owner" (object ["permissions" .= [own]]) `shouldBe` True
      forM_ [permission "anyone" "anyone" "reader", permission "domain" "domain" "reader", permission "other" "user" "reader"] $ \broad ->
        privateDrivePermissionsSafe "owner" (object ["permissions" .= [own,broad]]) `shouldBe` False
      privateDrivePermissionsSafe "owner" (object ["permissions" .= [own],"nextPageToken" .= ("more" :: T.Text)]) `shouldBe` False
      privateDrivePermissionsSafe "owner" (object []) `shouldBe` False

  describe "storage-security OAuth refresh request" $ do
    let valid =
          [ ("normalizes a complete Drive pair before refreshing", Just "  drive-client\n", Just "\tdrive-secret  ", Just "google-client", Just "google-secret")
          , ("normalizes the complete Google fallback before refreshing", Nothing, Nothing, Just " google-client\n", Just "\tgoogle-secret ")
          , ("prefers a complete Drive pair without mixing fallback values", Just "drive-client", Just "drive-secret", Just "google-client", Just "google-secret")
          , ("permits a complete Drive pair with no fallback configured", Just "drive-client", Just "drive-secret", Nothing, Nothing)
          ]
        invalid =
          [ ("rejects an explicitly blank Drive client id", Just " ", Just "drive-secret", Just "google-client", Just "google-secret")
          , ("rejects an explicitly blank Drive secret", Just "drive-client", Just "\t", Just "google-client", Just "google-secret")
          , ("rejects internal whitespace in the Drive client id", Just "drive client", Just "drive-secret", Nothing, Nothing)
          , ("rejects control characters in the Drive secret", Just "drive-client", Just "drive\NULsecret", Nothing, Nothing)
          , ("rejects hidden formatting in the Drive secret", Just "drive-client", Just "drive\x202E\&secret", Nothing, Nothing)
          , ("rejects non-ASCII Drive secrets", Just "drive-client", Just "secr\233t", Nothing, Nothing)
          , ("rejects oversized Drive secrets", Just "drive-client", Just (T.replicate 4097 "a"), Nothing, Nothing)
          , ("rejects path characters in the Drive client id", Just "drive/client", Just "drive-secret", Nothing, Nothing)
          , ("rejects a Drive id without its secret instead of mixing Google fallback", Just "drive-client", Nothing, Just "google-client", Just "google-secret")
          , ("rejects a Drive secret without its id instead of mixing Google fallback", Nothing, Just "drive-secret", Just "google-client", Just "google-secret")
          , ("rejects a partial Google fallback id", Nothing, Nothing, Just "google-client", Nothing)
          , ("rejects a partial Google fallback secret", Nothing, Nothing, Nothing, Just "google-secret")
          , ("validates an unused Google fallback id even with a complete Drive pair", Just "drive-client", Just "drive-secret", Just "bad/google-client", Just "google-secret")
          , ("validates an unused Google fallback secret even with a complete Drive pair", Just "drive-client", Just "drive-secret", Just "google-client", Just "google secret")
          , ("rejects absent application client credentials", Nothing, Nothing, Nothing, Nothing)
          , ("rejects an explicitly blank Google fallback id", Nothing, Nothing, Just " ", Just "google-secret")
          , ("rejects an explicitly blank Google fallback secret", Nothing, Nothing, Just "google-client", Just " ")
          ]
    forM_ valid $ \(name, driveId, driveSecret, googleId, googleSecret) -> it name $ do
      expected <- either (const (expectationFailure "Expected canonical valid credentials" >> fail "invalid fixture")) pure
        (resolveDriveClientCreds driveId driveSecret googleId googleSecret)
      prepared <- buildDriveRefreshRequest driveId driveSecret googleId googleSecret "synthetic-refresh"
      case prepared of
        Left _ -> expectationFailure "Expected a canonical refresh request"
        Right request -> do
          HTTP.method request `shouldBe` "POST"
          HTTP.secure request `shouldBe` True
          HTTP.host request `shouldBe` "oauth2.googleapis.com"
          HTTP.port request `shouldBe` 443
          HTTP.path request `shouldBe` "/token"
          HTTP.queryString request `shouldBe` ""
          HTTP.redirectCount request `shouldBe` 0
          HTTP.responseTimeout request `shouldBe` HTTP.responseTimeoutMicro 30000000
          HTTP.requestHeaders request `shouldBe` [("Content-Type", "application/x-www-form-urlencoded")]
          case HTTP.requestBody request of
            HTTP.RequestBodyBS body -> parseSimpleQuery body `shouldBe`
              [("client_id", TE.encodeUtf8 (fst expected)), ("client_secret", TE.encodeUtf8 (snd expected)),
               ("refresh_token", "synthetic-refresh"), ("grant_type", "refresh_token")]
            _ -> expectationFailure "Unexpected refresh request body constructor"
    forM_ invalid $ \(name, driveId, driveSecret, googleId, googleSecret) -> it name $ do
      resolveDriveClientCreds driveId driveSecret googleId googleSecret `shouldSatisfy` isLeft
      prepared <- buildDriveRefreshRequest driveId driveSecret googleId googleSecret "synthetic-refresh"
      case prepared of
        Right _ -> expectationFailure "Invalid configuration must fail before constructing a provider request"
        Left err -> do
          errHTTPCode err `shouldBe` 503
          errBody err `shouldBe` encode (object ["code" .= ("storage_vault_unavailable" :: T.Text)])
          errHeaders err `shouldBe` [("Content-Type", "application/json"), ("Cache-Control", "no-store")]

  database <- runIO (lookupEnv "TDF_STORAGE_SECURITY_DATABASE_URL")
  case database of
    Nothing -> pure () -- Executed by the dedicated PostgreSQL runner, not a pass here.
    Just url -> describe "storage-security PostgreSQL handlers" $
      it "isolates accounts, binds live readers, fences revoked sessions and rejects corrupt bytes" $
        withStorageEnvironment $ withSystemTempDirectory "tdf-storage-security" $ \root -> do
          original <- getCurrentDirectory
          bracket (setCurrentDirectory root) (const (setCurrentDirectory original)) $ \_ -> do
            cfg <- loadConfig
            createDirectoryIfMissing True (root </> "public")
            pool <- makePool (BS8.pack url)
            let env = Env pool (cfg { assetsRootDir = root </> "public" })
                run action = runHandler (runReaderT action env)
                db action = runSqlPool action pool
                httpRead token path = testWithApplication (pure (mkApp env)) $ \port -> do
                  manager <- HTTP.newManager HTTP.defaultManagerSettings
                  request <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> T.unpack path)
                  HTTP.httpLbs request
                    { HTTP.requestHeaders = [("Authorization","Bearer " <> value) | value <- maybe [] pure token]
                    } manager
                expectHttpDenial token path code = do
                  response <- httpRead token path
                  statusCode (HTTP.responseStatus response) `shouldBe` code
                  (binaryMarker `BS.isInfixOf` BL.toStrict (HTTP.responseBody response)) `shouldBe` False
                binaryMarker = "private attachment bytes"
                expectCode code result = case result of
                  Left err -> errHTTPCode err `shouldBe` code
                  Right _ -> expectationFailure "Expected denial"
            owner <- fromJust <$> db (loadAuthedUser "storage-owner-a")
            other <- fromJust <$> db (loadAuthedUser "storage-owner-b")
            admin <- fromJust <$> db (loadAuthedUser "storage-admin")
            setEnv "CONTENT_STORAGE_VAULT_KEY" (replicate 64 'a')
            run (storeConnection owner "bad\r\ntoken" Nothing 3600) >>= expectCode 502
            connected <- run (storeConnection owner "synthetic-access-secret" (Just "synthetic-refresh-secret") 3600)
            handle <- case connected of
              Right dto -> pure (accessToken dto)
              Left _ -> expectationFailure "Connection failed" >> pure ""
            run (resolveConnection sharedTlsManager owner handle) >>= (`shouldBe` Right "synthetic-access-secret")
            run (resolveConnection sharedTlsManager other handle) >>= expectCode 409
            sealedRows <- db (rawSql "SELECT position(convert_to('synthetic-access-secret','UTF8') in sealed)=0 FROM content_storage_credential" [] :: SqlPersistT IO [Single Bool])
            sealedRows `shouldBe` [Single True]
            run (revokeConnection owner) >>= \result -> case result of
              Right _ -> pure ()
              Left _ -> expectationFailure "Disconnect failed"
            run (resolveConnection sharedTlsManager owner handle) >>= expectCode 409
            unsetEnv "CONTENT_STORAGE_VAULT_KEY"

            let binary = "private attachment bytes"
                source = root </> "original.bin"
                file = FileData "file" "evidence.bin" "application/octet-stream" source
                key = T.replicate 64 "a"
            BS.writeFile source binary
            unsetEnv "CONTENT_STORAGE_UPLOADS_ENABLED"
            run (storeManagedUpload owner file "evidence.bin" "application/octet-stream" "private" key) >>= expectCode 503
            let legacyForm = DriveUploadForm file Nothing Nothing Nothing (Just "server") Nothing Nothing Nothing
            run (driveUploadServer admin Nothing legacyForm) >>= expectCode 503
            setEnv "CONTENT_STORAGE_UPLOADS_ENABLED" "true"
            run (driveUploadServer admin Nothing legacyForm) >>= expectCode 409
            uploaded <- run (storeManagedUpload owner file "evidence.bin" "application/octet-stream" "private" key)
            identifier <- case uploaded of
              Right dto -> pure (fromJust (UUID.fromText (duFileId dto)))
              Left _ -> expectationFailure "Upload failed" >> pure UUID.nil
            let (_ :<|> _ :<|> _ :<|> readOwner :<|> deleteOwner) = contentStorageServer owner
                (_ :<|> _ :<|> _ :<|> readOther :<|> _) = contentStorageServer other
                (_ :<|> _ :<|> _ :<|> readAdmin :<|> _) = contentStorageServer admin
                relative = "/storage/files/" <> UUID.toText identifier
            run (readOwner identifier) >>= \result -> case result of
              Right headers -> getResponse headers `shouldBe` BL.fromStrict binary
              Left _ -> expectationFailure "Owner read denied"
            ownerResponse <- httpRead (Just "storage-owner-a") relative
            statusCode (HTTP.responseStatus ownerResponse) `shouldBe` 200
            HTTP.responseBody ownerResponse `shouldBe` BL.fromStrict binary
            lookup "Cache-Control" (HTTP.responseHeaders ownerResponse) `shouldBe` Just "private, no-store"
            lookup "X-Content-Type-Options" (HTTP.responseHeaders ownerResponse) `shouldBe` Just "nosniff"
            expectHttpDenial Nothing relative 401
            expectHttpDenial (Just "storage-owner-b") relative 404
            expectHttpDenial (Just "storage-admin") relative 404
            -- A copied/uncommitted filename still cannot pass public admission.
            let publicName = UUID.toText identifier <> ".png"
            createDirectoryIfMissing True (root </> "public/content")
            BS.writeFile (root </> "public/content" </> T.unpack publicName) binary
            createDirectoryIfMissing True (root </> "public/inventory")
            createFileLink (root </> "uploads/content-storage" </> T.unpack (UUID.toText identifier))
              (root </> "public/inventory/private-link")
            forM_ ["/assets/serve/content/" <> publicName,"/inventory/private-link",
                   "/assets/serve/inventory/private-link","/assets/serve/.content-storage-staging/secret"] $ \path ->
              expectHttpDenial Nothing path 404
            run (readOther identifier) >>= expectCode 404
            run (readAdmin identifier) >>= expectCode 404
            publicObjectAllowed env ["content",UUID.toText identifier <> ".png"] `shouldReturn` False
            denied <- db (bindReference other "https://api.tdfrecords.net" "course_receipt" 1 (Just relative))
            denied `shouldSatisfy` isLeft
            db $ do
              rawExecute "INSERT INTO course_registration_receipt(id) VALUES(1)" []
              requireReference owner "https://api.tdfrecords.net" "course_receipt" 1 (Just relative)
            run (readAdmin identifier) >>= \result -> case result of
              Right headers -> getResponse headers `shouldBe` BL.fromStrict binary
              Left _ -> expectationFailure "Authorized resource reader denied"
            authorizedResponse <- httpRead (Just "storage-admin") relative
            statusCode (HTTP.responseStatus authorizedResponse) `shouldBe` 200
            HTTP.responseBody authorizedResponse `shouldBe` BL.fromStrict binary
            run (deleteOwner identifier) >>= expectCode 409
            -- Role revocation after the AuthedUser was captured must still deny.
            db (rawExecute "UPDATE role_permission SET active=false" [])
            run (readAdmin identifier) >>= expectCode 404
            expectHttpDenial (Just "storage-admin") relative 404
            db (rawExecute "DELETE FROM course_registration_receipt WHERE id=1" [])
            run (readAdmin identifier) >>= expectCode 404
            BS.writeFile (root </> "uploads/content-storage" </> T.unpack (UUID.toText identifier)) "corrupt"
            run (readOwner identifier) >>= expectCode 503
            db (rawExecute "UPDATE api_token SET active=false WHERE token='storage-owner-a'" [])
            run (readOwner identifier) >>= expectCode 401
            expectHttpDenial (Just "storage-owner-a") relative 401
            unsetEnv "CONTENT_STORAGE_UPLOADS_ENABLED"

withStorageEnvironment :: IO a -> IO a
withStorageEnvironment action = bracket
  (mapM lookupEnv names)
  (\values -> forM_ (zip names values) $ \(name,value) -> maybe (unsetEnv name) (setEnv name) value)
  (const action)
  where names = ["CONTENT_STORAGE_VAULT_KEY","CONTENT_STORAGE_UPLOADS_ENABLED"]
