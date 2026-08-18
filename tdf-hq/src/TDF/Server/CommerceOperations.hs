{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.CommerceOperations
  ( commerceOperationsServer
  , validateProviderEventReplayReason
  ) where

import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask)
import           Data.Char (isControl)
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (getCurrentTime)
import           Database.Persist.Sql (fromSqlKey, runSqlPool)
import           Servant

import           TDF.API.CommerceOperations
import           TDF.Auth (AuthedUser(..), hasStrictAdminAccess)
import qualified TDF.Commerce.ProviderEventStore as ProviderEvent
import           TDF.DB (Env(..))

type AppM = ReaderT Env Handler

commerceOperationsServer
  :: AuthedUser
  -> ServerT CommerceOperationsAPI AppM
commerceOperationsServer user =
       (\status limit offset -> requireAccess *> listProviderEventsHandler status limit offset)
  :<|> (\eventId request -> requireAccess *> replayProviderEventHandler user eventId request)
  where
    requireAccess = unless (hasStrictAdminAccess user) $
      throwError err403 { errBody = "Strict Admin access required" }

listProviderEventsHandler
  :: Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> AppM [CommerceProviderEventDTO]
listProviderEventsHandler rawStatus rawLimit rawOffset = do
  Env{..} <- ask
  status <- either (throwError . badRequest) pure (validateProviderEventStatus rawStatus)
  let limit = min 100 (max 1 (maybe 50 id rawLimit))
      offset = min 10000 (max 0 (maybe 0 id rawOffset))
  records <- liftIO $ flip runSqlPool envPool $
    ProviderEvent.listProviderEvents status limit offset
  pure (map providerEventToDTO records)

replayProviderEventHandler
  :: AuthedUser
  -> Text
  -> CommerceProviderEventReplayCreate
  -> AppM CommerceProviderEventDTO
replayProviderEventHandler user rawEventId CommerceProviderEventReplayCreate{..} = do
  Env{..} <- ask
  eventRef <- either (throwError . badRequest) pure $
    ProviderEvent.parseProviderEventReference rawEventId
  reason <- either (throwError . badRequest) pure $
    validateProviderEventReplayReason cperReason
  now <- liftIO getCurrentTime
  result <- liftIO $ flip runSqlPool envPool $
    ProviderEvent.requeueDeadLetterProviderEvent
      eventRef (fromSqlKey (auPartyId user) :: Int64) reason now
  case result of
    Left ProviderEvent.ProviderEventNotFound ->
      throwError err404 { errBody = "Provider event not found" }
    Left (ProviderEvent.ProviderEventReplayConflict status) ->
      throwError err409
        { errBody = BL.fromStrict (TE.encodeUtf8
            ("Only dead-letter provider events can be replayed; current status is " <> status)) }
    Right record -> pure (providerEventToDTO record)

validateProviderEventReplayReason :: Text -> Either Text Text
validateProviderEventReplayReason rawReason =
  let reason = T.strip rawReason
  in if T.length reason < 8 || T.length reason > 500
       then Left "Replay reason must contain 8 to 500 characters"
       else if T.any isControl reason
         then Left "Replay reason contains unsupported control characters"
         else Right reason

validateProviderEventStatus :: Maybe Text -> Either Text (Maybe Text)
validateProviderEventStatus Nothing = Right Nothing
validateProviderEventStatus (Just rawStatus)
  | status `elem` allowedStatuses = Right (Just status)
  | otherwise = Left "Unsupported provider event status filter"
  where
    status = T.toLower (T.strip rawStatus)
    allowedStatuses =
      [ "pending", "processing", "processed", "retry", "dead_letter", "ignored" ]

providerEventToDTO :: ProviderEvent.ProviderEventRecord -> CommerceProviderEventDTO
providerEventToDTO ProviderEvent.ProviderEventRecord{..} = CommerceProviderEventDTO
  { cpeId = perId
  , cpeProvider = perProvider
  , cpeEnvironment = perEnvironment
  , cpeProviderEventId = perProviderEventId
  , cpeEventType = perEventType
  , cpeProviderResourceId = perProviderResourceId
  , cpeStatus = perStatus
  , cpeAttemptCount = perAttemptCount
  , cpeCheckoutId = perCheckoutId
  , cpePaymentAttemptId = perPaymentAttemptId
  , cpeRefundId = perRefundId
  , cpeReceivedAt = perReceivedAt
  , cpeProviderCreatedAt = perProviderCreatedAt
  , cpeProcessingStartedAt = perProcessingStartedAt
  , cpeLastAttemptAt = perLastAttemptAt
  , cpeNextAttemptAt = perNextAttemptAt
  , cpeProcessedAt = perProcessedAt
  , cpeErrorSummary = perErrorSummary
  }

badRequest :: Text -> ServerError
badRequest message = err400 { errBody = BL.fromStrict (TE.encodeUtf8 message) }
