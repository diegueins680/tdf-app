{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TDF.Server where

import Servant
import Database.Persist
import Database.Persist.Postgresql (ConnectionPool, runSqlPool)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Exception (SomeException, try, displayException)
import Data.Maybe (catMaybes)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version (showVersion)
import Development.GitRev (gitHash, gitCommitDate)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Database.Persist.Sql (rawExecute, rawSql, toSqlKey, fromSqlKey, Single (..), SqlPersistT)
import qualified TDF.Models as Models
import qualified TDF.DTO as DTO
import TDF.DB
import qualified TDF.API as API
import qualified Paths_tdf_hq as Paths
import Control.Monad (when)

currentVersionText :: T.Text
currentVersionText = T.pack $ showVersion Paths.version

-- Application monad that carries database connection pool
type AppM = ReaderT ConnectionPool Handler

-- Convert database entities to DTOs
entityToUserDTO :: Entity Models.Party -> [Models.PartyRole] -> DTO.UserDTO
entityToUserDTO (Entity partyId party) roles = DTO.UserDTO
    { DTO.userId = fromIntegral $ fromSqlKey partyId
    , DTO.userName = Models.partyDisplayName party
    , DTO.userEmail = Models.partyPrimaryEmail party
    , DTO.userPhone = Models.partyPrimaryPhone party
    , DTO.userRoles = roles
    , DTO.userStatus = Models.partyStatus party
    , DTO.userCreatedAt = Models.partyCreatedAt party
    }

-- Server implementation
usersServer :: ServerT API.UsersAPI AppM
usersServer = getUsers
         :<|> getUserRoles'
         :<|> updateUserRoles'

partyEntityToDTO :: Entity Models.Party -> DTO.PartyDTO
partyEntityToDTO (Entity pid party) = DTO.PartyDTO
    { DTO.partyId = fromIntegral $ fromSqlKey pid
    , DTO.partyLegalName = Models.partyLegalName party
    , DTO.partyDisplayName = Models.partyDisplayName party
    , DTO.partyIsOrg = Models.partyIsOrg party
    , DTO.partyTaxId = Models.partyTaxId party
    , DTO.partyPrimaryEmail = Models.partyPrimaryEmail party
    , DTO.partyPrimaryPhone = Models.partyPrimaryPhone party
    , DTO.partyWhatsapp = Models.partyWhatsapp party
    , DTO.partyInstagram = Models.partyInstagram party
    , DTO.partyEmergencyContact = Models.partyEmergencyContact party
    , DTO.partyNotes = Models.partyNotes party
    }

bookingEntityToDTO :: Entity Models.Booking -> DTO.BookingDTO
bookingEntityToDTO (Entity bid booking) = DTO.BookingDTO
    { DTO.bookingId = fromIntegral $ fromSqlKey bid
    , DTO.bookingTitle = Models.bookingTitle booking
    , DTO.bookingStartsAt = T.pack $ iso8601Show $ Models.bookingStartTime booking
    , DTO.bookingEndsAt = T.pack $ iso8601Show $ Models.bookingEndTime booking
    , DTO.bookingStatusText = T.pack $ show $ Models.bookingStatus booking
    , DTO.bookingNotes = Models.bookingNotes booking
    }

parseIsoTime :: T.Text -> Either T.Text UTCTime
parseIsoTime txt = case iso8601ParseM (T.unpack txt) :: Maybe UTCTime of
    Just t  -> Right t
    Nothing -> Left $ "Invalid ISO8601 timestamp: " <> txt

parseBookingStatus :: T.Text -> Either T.Text Models.BookingStatus
parseBookingStatus txt = maybe (Left $ "Invalid booking status: " <> txt) Right (readMaybe $ T.unpack txt)

-- Get all users with their roles
getUsers :: AppM [DTO.UserDTO]
getUsers = do
    pool <- ask
    users <- liftIO $ runSqlPool getUsersWithRoles pool
    return $ map (uncurry entityToUserDTO) users

-- Get roles for a specific user
getUserRoles' :: Int -> AppM [Models.PartyRole]
getUserRoles' userId = do
    pool <- ask
    let partyId = toSqlKey $ fromIntegral userId
    liftIO $ runSqlPool (getUserRoles partyId) pool

-- Update roles for a user
updateUserRoles' :: Int -> DTO.UserRoleUpdateDTO -> AppM ()
updateUserRoles' userId roleUpdate = do
    pool <- ask
    let partyId = toSqlKey $ fromIntegral userId
    liftIO $ runSqlPool (updateUserRoles partyId (DTO.roles roleUpdate) Nothing) pool

-- Version information handler
versionHandler :: Handler DTO.VersionDTO
versionHandler = pure buildVersionInfo

buildVersionInfo :: DTO.VersionDTO
buildVersionInfo = DTO.VersionDTO
    { DTO.name = "tdf-hq"
    , DTO.versionField = currentVersionText
    , DTO.commit = Just (T.pack $(gitHash))
    , DTO.buildTime = Just (T.pack $(gitCommitDate))
    }

-- Health check handler
healthHandler :: ConnectionPool -> Handler DTO.HealthDTO
healthHandler pool = do
    liftIO $ appendFile "health.log" "[health] probe invoked\n"
    dbStatus <- liftIO $ try (runSqlPool pingDatabase pool) :: Handler (Either SomeException [Single Int])
    case dbStatus of
        Left err -> do
            liftIO $ appendFile "health.log" $ "[health] database check failed: " <> displayException err <> "\n"
            pure degraded
        Right [Single 1] -> do
            liftIO $ appendFile "health.log" "[health] database check succeeded\n"
            pure ok
        Right unexpected -> do
            liftIO $ appendFile "health.log" $ "[health] unexpected DB response: " <> show unexpected <> "\n"
            pure degraded
  where
    pingDatabase =
        rawSql "SELECT 1" [] :: SqlPersistT IO [Single Int]
    ok = DTO.HealthDTO { DTO.status = "ok", DTO.version = Just currentVersionText }
    degraded = DTO.HealthDTO { DTO.status = "degraded", DTO.version = Just currentVersionText }

-- Main server
server :: ConnectionPool -> Server API.API
server pool =
    versionHandler
    :<|> healthHandler pool
    :<|> partiesServer
    :<|> bookingsServer
    :<|> hoistServer API.usersApi (`runReaderT` pool) usersServer
  where
    partiesServer =
           listPartiesH
      :<|> createPartyH
      :<|> getPartyH
      :<|> updatePartyH
      :<|> addRoleH

    bookingsServer =
           listBookingsH
      :<|> createBookingH

    listPartiesH :: Handler [DTO.PartyDTO]
    listPartiesH = do
        parties <- liftIO $ runSqlPool TDF.DB.listParties pool
        pure $ map partyEntityToDTO parties

    createPartyH :: DTO.PartyCreateDTO -> Handler DTO.PartyDTO
    createPartyH body = do
        now <- liftIO getCurrentTime
        let party = Models.Party
                { Models.partyDisplayName = DTO.cDisplayName body
                , Models.partyLegalName = DTO.cLegalName body
                , Models.partyIsOrg = DTO.cIsOrg body
                , Models.partyPrimaryEmail = DTO.cPrimaryEmail body
                , Models.partyPrimaryPhone = DTO.cPrimaryPhone body
                , Models.partyWhatsapp = DTO.cWhatsapp body
                , Models.partyInstagram = DTO.cInstagram body
                , Models.partyTaxId = DTO.cTaxId body
                , Models.partyEmergencyContact = DTO.cEmergencyContact body
                , Models.partyNotes = DTO.cNotes body
                , Models.partyStatus = Models.Active
                , Models.partyCreatedAt = now
                , Models.partyUpdatedAt = now
                }
        entity <- liftIO $ runSqlPool (insertParty party) pool
        pure $ partyEntityToDTO entity

    getPartyH :: Int -> Handler DTO.PartyDTO
    getPartyH pid = do
        mParty <- liftIO $ runSqlPool (TDF.DB.getParty partyKey) pool
        maybe (throwError err404) (pure . partyEntityToDTO) mParty
      where
        partyKey = toSqlKey (fromIntegral pid)

    updatePartyH :: Int -> DTO.PartyUpdateDTO -> Handler DTO.PartyDTO
    updatePartyH pid body = do
        now <- liftIO getCurrentTime
        let updates = catMaybes
                [ fmap (Models.PartyDisplayName =.) (DTO.uDisplayName body)
                , fmap (Models.PartyIsOrg =.) (DTO.uIsOrg body)
                , fmap (\v -> Models.PartyLegalName =. Just v) (DTO.uLegalName body)
                , fmap (\v -> Models.PartyPrimaryEmail =. Just v) (DTO.uPrimaryEmail body)
                , fmap (\v -> Models.PartyPrimaryPhone =. Just v) (DTO.uPrimaryPhone body)
                , fmap (\v -> Models.PartyWhatsapp =. Just v) (DTO.uWhatsapp body)
                , fmap (\v -> Models.PartyInstagram =. Just v) (DTO.uInstagram body)
                , fmap (\v -> Models.PartyTaxId =. Just v) (DTO.uTaxId body)
                , fmap (\v -> Models.PartyEmergencyContact =. Just v) (DTO.uEmergencyContact body)
                , fmap (\v -> Models.PartyNotes =. Just v) (DTO.uNotes body)
                ]
            allUpdates = updates ++ [Models.PartyUpdatedAt =. now]
        mUpdated <- liftIO $ runSqlPool (updateParty partyKey allUpdates) pool
        maybe (throwError err404) (pure . partyEntityToDTO) mUpdated
      where
        partyKey = toSqlKey (fromIntegral pid)

    addRoleH :: Int -> Models.PartyRole -> Handler NoContent
    addRoleH pid role = do
        liftIO $ runSqlPool (addUserRole partyKey role Nothing) pool
        pure NoContent
      where
        partyKey = toSqlKey (fromIntegral pid)

    listBookingsH :: Handler [DTO.BookingDTO]
    listBookingsH = do
        bookings <- liftIO $ runSqlPool TDF.DB.listBookings pool
        pure $ map bookingEntityToDTO bookings

    createBookingH :: DTO.BookingCreateDTO -> Handler DTO.BookingDTO
    createBookingH body = do
        start <- either throwIsoError pure $ parseIsoTime (DTO.cbStartsAt body)
        end <- either throwIsoError pure $ parseIsoTime (DTO.cbEndsAt body)
        status' <- either throwStatusError pure $ parseBookingStatus (DTO.cbStatus body)
        now <- liftIO getCurrentTime
        let booking = Models.Booking
                { Models.bookingTitle = DTO.cbTitle body
                , Models.bookingPartyId = Nothing
                , Models.bookingResourceId = Nothing
                , Models.bookingServiceType = Nothing
                , Models.bookingStartTime = start
                , Models.bookingEndTime = end
                , Models.bookingStatus = status'
                , Models.bookingNotes = DTO.cbNotes body
                , Models.bookingCreatedAt = now
                }
        entity <- liftIO $ runSqlPool (insertBooking booking) pool
        pure $ bookingEntityToDTO entity
      where
        throwIsoError :: T.Text -> Handler a
        throwIsoError msg = throwError err400 { errBody = BL.fromStrict $ TE.encodeUtf8 msg }
        throwStatusError :: T.Text -> Handler a
        throwStatusError msg = throwError err400 { errBody = BL.fromStrict $ TE.encodeUtf8 msg }
