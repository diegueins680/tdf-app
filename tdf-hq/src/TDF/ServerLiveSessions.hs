{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TDF.ServerLiveSessions
  ( liveSessionsServer
  ) where

import           Control.Monad              (forM_, unless, void, when)
import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, asks)
import           Crypto.BCrypt              (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as TE
import           Data.Time                  (Day, UTCTime, getCurrentTime)
import           Data.UUID                  (toText)
import           Data.UUID.V4               (nextRandom)
import           Database.Persist
import           Database.Persist.Sql       (SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)
import           Servant
import           Servant.Multipart          (FileData(..), Tmp(..))
import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath            ((</>), takeFileName)
import qualified Data.ByteString.Lazy       as BL

import           TDF.API.LiveSessions
import           TDF.Auth                   (AuthedUser, auPartyId)
import           TDF.DB                     (Env(..))
import           TDF.Models
import qualified TDF.Models                 as M
import qualified TDF.ModelsExtra           as ME

liveSessionsServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT LiveSessionsAPI m
liveSessionsServer user = intakeHandler
  where
    intakeHandler payload = do
      let bandName = T.strip (lsiBandName payload)
      when (T.null bandName) $
        throwError err400 { errBody = "bandName is required" }

      now <- liftIO getCurrentTime
      riderPath <- liftIO $ traverse storeRiderFile (lsiRider payload)

      partyKeys <- mapM (ensureMusician now) (lsiMusicians payload)

      intakeId <- withPool $ insert ME.LiveSessionIntake
        { ME.liveSessionIntakeBandName     = bandName
        , ME.liveSessionIntakeContactEmail = T.strip <$> lsiContactEmail payload
        , ME.liveSessionIntakeContactPhone = T.strip <$> lsiContactPhone payload
        , ME.liveSessionIntakeSessionDate  = lsiSessionDate payload
        , ME.liveSessionIntakeRiderPath    = riderPath
        , ME.liveSessionIntakeCreatedBy    = Just (auPartyId user)
        , ME.liveSessionIntakeCreatedAt    = now
        }

      withPool $
        forM_ (zip partyKeys (lsiMusicians payload)) $ \(partyKey, m) ->
          insert_ ME.LiveSessionMusician
            { ME.liveSessionMusicianIntakeId   = intakeId
            , ME.liveSessionMusicianPartyId    = partyKey
            , ME.liveSessionMusicianName       = lsmName m
            , ME.liveSessionMusicianEmail      = lsmEmail m
            , ME.liveSessionMusicianInstrument = lsmInstrument m
            , ME.liveSessionMusicianRole       = lsmRole m
            , ME.liveSessionMusicianNotes      = lsmNotes m
            , ME.liveSessionMusicianIsExisting = lsmIsExisting m
            }

      pure NoContent

    ensureMusician :: UTCTime -> LiveSessionMusicianPayload -> m (Key Party)
    ensureMusician now LiveSessionMusicianPayload{..} = do
      let mEmail = T.strip <$> lsmEmail
          trimmedName = T.strip lsmName
      partyKey <- case lsmPartyId of
        Just pidInt -> do
          let key = toSqlKey (fromIntegral pidInt)
          exists <- withPool $ get key
          case exists of
            Nothing -> throwError err400 { errBody = "Referenced party not found" }
            Just _  -> pure key
        Nothing -> do
          foundByEmail <- case mEmail of
            Nothing -> pure Nothing
            Just e | T.null e -> pure Nothing
                   | otherwise -> withPool $ selectFirst [M.PartyPrimaryEmail ==. Just e] []
          found <- case foundByEmail of
            Just ent -> pure (Just ent)
            Nothing ->
              if T.null trimmedName
                then pure Nothing
                else withPool $ selectFirst [M.PartyDisplayName ==. trimmedName] []
          case found of
            Just ent -> pure (entityKey ent)
            Nothing -> withPool $
              insert Party
                { partyLegalName        = Nothing
                , partyDisplayName      = if T.null trimmedName then "Músico Live Session" else trimmedName
                , partyIsOrg            = False
                , partyTaxId            = Nothing
                , partyPrimaryEmail     = mEmail
                , partyPrimaryPhone     = Nothing
                , partyWhatsapp         = Nothing
                , partyInstagram        = Nothing
                , partyEmergencyContact = Nothing
                , partyNotes            = Just (fromMaybe "" lsmInstrument)
                , partyCreatedAt        = now
                }

      when (partyKey == toSqlKey 0) $
        throwError err400 { errBody = "Invalid party reference" }
      when (not (maybe True T.null mEmail)) $
        withPool $ update partyKey [M.PartyPrimaryEmail =. mEmail]
      withPool $ ensureArtistRole partyKey
      withPool $ ensureUserAccount partyKey mEmail
      pure partyKey

    ensureArtistRole :: PartyId -> SqlPersistT IO ()
    ensureArtistRole pid = do
      _ <- upsert (PartyRole pid Artist True) [PartyRoleActive =. True]
      pure ()

    ensureUserAccount :: PartyId -> Maybe Text -> SqlPersistT IO ()
    ensureUserAccount pid mEmail = do
      existing <- selectFirst [UserCredentialPartyId ==. pid, UserCredentialActive ==. True] []
      case existing of
        Just _  -> pure ()
        Nothing -> do
          baseUsername <- pure $ case mEmail of
            Just email | not (T.null email) -> T.toLower email
            _ -> "livesession-" <> T.pack (show (fromSqlKey pid))
          username <- generateUniqueUsername baseUsername
          tempPwd <- liftIO randomPassword
          hashed <- liftIO (hashPasswordText tempPwd)
          _ <- insert UserCredential
            { userCredentialPartyId      = pid
            , userCredentialUsername     = username
            , userCredentialPasswordHash = hashed
            , userCredentialActive       = True
            }
          applyRoles pid [Artist]

    applyRoles :: PartyId -> [RoleEnum] -> SqlPersistT IO ()
    applyRoles pid rolesList =
      forM_ rolesList $ \role ->
        void $ upsert (PartyRole pid role True) [PartyRoleActive =. True]

    generateUniqueUsername :: Text -> SqlPersistT IO Text
    generateUniqueUsername base = do
      conflict <- getBy (UniqueCredentialUsername base)
      case conflict of
        Nothing -> pure base
        Just _  -> do
          suffix <- liftIO (T.pack . show . toText <$> nextRandom)
          let candidate = T.take 60 (base <> "-" <> suffix)
          generateUniqueUsername candidate

    hashPasswordText :: Text -> IO Text
    hashPasswordText pwd = do
      let raw = TE.encodeUtf8 pwd
      mHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy raw
      case mHash of
        Nothing   -> fail "Failed to hash password"
        Just hash -> pure (TE.decodeUtf8 hash)

    randomPassword :: IO Text
    randomPassword = toText <$> nextRandom

    storeRiderFile :: FileData Tmp -> IO Text
    storeRiderFile FileData{..} = do
      let safeName = sanitize (T.pack (takeFileName fdFileName))
      token <- toText <$> nextRandom
      let destDir  = "uploads/live-sessions"
          destPath = destDir </> T.unpack token <> "-" <> T.unpack safeName
      createDirectoryIfMissing True destDir
      BL.readFile fdFilePath >>= BL.writeFile destPath
      pure (T.pack destPath)

    sanitize :: Text -> Text
    sanitize = T.filter (\c -> c /= '/' && c /= '\\')

withPool
  :: (MonadReader Env m, MonadIO m)
  => SqlPersistT IO a
  -> m a
withPool action = do
  pool <- asks envPool
  liftIO (runSqlPool action pool)
