{-# LANGUAGE OverloadedStrings #-}

-- | Musician (participant) ticket tiers: a tier whose code is 'MUSICO'. When
-- tickets from such a tier are issued for a paid order, the buyer is listed
-- among the event's artists, with a basic artist profile created for them if
-- they have none. The tier code is the marker, so no schema change is needed.
--
-- Runs inside the issuance transaction, so the listing commits or rolls back
-- with the tickets themselves. Guest buyers (no party) are left unlisted.
module TDF.Ticketing.ParticipantArtist
    ( registerParticipantArtistForOrder
    , isParticipantTierCode
    , participantTierCode
    , participantArtistName
    , maxListedEventArtists
    ) where

import Control.Monad (void, when)
import Data.Char (isControl, isDigit)
import Data.Int (Int64)
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.SqlBackend (getRDBMS)
import Text.Read (readMaybe)

import TDF.Models.SocialEventsModels

-- | Code that marks a ticket tier as the musician tier of its event.
participantTierCode :: T.Text
participantTierCode = "MUSICO"

isParticipantTierCode :: T.Text -> Bool
isParticipantTierCode code = T.toUpper (T.strip code) == participantTierCode

-- | Same ceiling the event editor enforces for an event's artist list.
maxListedEventArtists :: Int
maxListedEventArtists = 50

registerParticipantArtistForOrder :: UTCTime -> EventTicketOrder -> SqlPersistT IO ()
registerParticipantArtistForOrder now order = do
    mTier <- get (eventTicketOrderTierId order)
    let participantTier = maybe False (isParticipantTierCode . eventTicketTierCode) mTier
    case (participantTier, eventTicketOrderBuyerPartyId order >>= parsePartyId) of
        (True, Just partyIdValue) -> do
            let eventKey = eventTicketOrderEventId order
            -- Lock the event before the buyer, the order the event editor uses
            -- (event row, then artist owners). Two musician orders for one
            -- event, or an order and an edit, then check and change the lineup
            -- one at a time, so the artist ceiling cannot be passed.
            lockEventLineup eventKey
            -- A full lineup is left unchanged. The check comes before the
            -- profile helper, so a buyer who cannot be listed does not get an
            -- artist profile created as a side effect.
            listed <- count [EventArtistEventId ==. eventKey]
            when (listed < maxListedEventArtists) $ do
                mArtist <- participantArtistFor now order partyIdValue
                case mArtist of
                    Nothing -> pure ()
                    Just artistKey -> do
                        linked <- get (EventArtistKey eventKey artistKey)
                        when (isNothing linked) $
                            void (insertUnique (EventArtist eventKey artistKey (Just "participant")))
        _ -> pure ()

-- The tickets inserted just before this already hold the event's KEY SHARE
-- lock through their foreign key. FOR NO KEY UPDATE is compatible with that
-- lock, so two musician orders cannot deadlock upgrading it, while it still
-- excludes other lineup locks and the editor's update of the event row.
lockEventLineup :: SocialEventId -> SqlPersistT IO ()
lockEventLineup eventKey = do
    backend <- T.toCaseFold <$> getRDBMS
    when (backend == "postgresql") $
        void
            ( rawSql
                "SELECT id FROM social_event WHERE id = ? FOR NO KEY UPDATE"
                (keyToValues eventKey)
                :: SqlPersistT IO [Single PersistValue]
            )

participantArtistFor :: UTCTime -> EventTicketOrder -> Int64 -> SqlPersistT IO (Maybe ArtistProfileId)
participantArtistFor now order partyIdValue = do
    -- Lock the owner the way artist creation does, so two orders issued at
    -- once for the same buyer cannot each create a profile.
    backend <- T.toCaseFold <$> getRDBMS
    let lockClause = if backend == "postgresql" then " FOR UPDATE" else ""
    owners <-
        rawSql
            ("SELECT display_name FROM party WHERE id = ?" <> lockClause)
            [PersistInt64 partyIdValue]
    case owners of
        [] -> pure Nothing
        (Single displayName : _) -> do
            let ownerText = T.pack (show partyIdValue)
            existing <- selectFirst [ArtistProfilePartyId ==. Just ownerText] [Asc ArtistProfileId]
            case existing of
                Just (Entity key _) -> pure (Just key)
                Nothing ->
                    case participantArtistName [eventTicketOrderBuyerName order, Just displayName] of
                        Nothing -> pure Nothing
                        Just nameValue ->
                            Just
                                <$> insert
                                    ArtistProfile
                                        { artistProfilePartyId = Just ownerText
                                        , artistProfileName = nameValue
                                        , artistProfileBio = Nothing
                                        , artistProfileAvatarUrl = Nothing
                                        , artistProfileGenres = Nothing
                                        , artistProfileSocialLinks = Nothing
                                        , artistProfileCountryCode = Nothing
                                        , artistProfileCountryId = Nothing
                                        , artistProfileCreatedAt = now
                                        , artistProfileUpdatedAt = now
                                        }

-- | First usable name: the name given at checkout, then the account name.
-- Follows artist-name validation (non-empty, no control characters).
participantArtistName :: [Maybe T.Text] -> Maybe T.Text
participantArtistName = listToMaybe . mapMaybe (>>= usable)
  where
    usable raw
        | T.any isControl raw = Nothing
        | T.null (T.strip raw) = Nothing
        | otherwise = Just (T.strip raw)

parsePartyId :: T.Text -> Maybe Int64
parsePartyId raw =
    let trimmed = T.strip raw
     in if not (T.null trimmed) && T.all isDigit trimmed
            then mfilterPositive (readMaybe (T.unpack trimmed))
            else Nothing
  where
    mfilterPositive (Just n) | n > 0 = Just n
    mfilterPositive _ = Nothing
