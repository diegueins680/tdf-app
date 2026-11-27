{-# LANGUAGE OverloadedStrings #-}
module TDF.Server.EventAttendees (attendeesQuery, loadAttendees) where

import qualified Data.Aeson as Aeson
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.Persist (PersistValue(..), toPersistValue)
import Database.Persist.Sql (Single(..), SqlPersistT, rawSql)
import TDF.Models.SocialEventsModels (SocialEventId)

-- The roster is served only for the two counted statuses and a non-negative
-- cursor; any other filter has no query.
attendeesQuery :: SocialEventId -> T.Text -> T.Text -> Maybe Int64 -> Maybe (SqlPersistT IO Aeson.Value)
attendeesQuery eventKey viewerPartyId status after
    | status `elem` ["accepted", "maybe"] && maybe True (>= 0) after =
        Just (loadAttendees eventKey viewerPartyId status after)
    | otherwise = Nothing

loadAttendees :: SocialEventId -> T.Text -> T.Text -> Maybe Int64 -> SqlPersistT IO Aeson.Value
loadAttendees eventKey viewerPartyId status after = do
    let viewer = PersistText viewerPartyId
    rows <- (rawSql
        ( "WITH viewer_profiles AS ( "
       <> "SELECT id FROM directory_profile WHERE subject_party_id=?::bigint "
       <> "UNION SELECT canonical_profile_id FROM directory_profile WHERE subject_party_id=?::bigint "
       <> ") "
       <> "SELECT r.id,coalesce(nullif(profile.profile_name,''),profile.party_name),directory_safe_image_url(profile.avatar_url) "
       <> "FROM event_rsvp r JOIN party p ON p.id::text=r.party_id AND NOT p.is_org "
       <> "CROSS JOIN LATERAL social_v2_profiles(?::bigint,ARRAY[p.id]) profile "
       <> "WHERE r.event_id=? AND r.status=? AND r.show_on_profile AND r.visibility_decided_at IS NOT NULL "
       <> "AND r.id>? AND EXISTS (SELECT 1 FROM user_credential u WHERE u.party_id=p.id AND u.active) "
       <> "AND NOT EXISTS (SELECT 1 FROM directory_profile target CROSS JOIN directory_profile_block b "
       <> "WHERE target.subject_party_id=p.id AND ( "
       <> "(b.blocker_profile_id IN (target.id,target.canonical_profile_id) AND b.blocked_profile_id IN (SELECT id FROM viewer_profiles)) "
       <> "OR (b.blocked_profile_id IN (target.id,target.canonical_profile_id) AND b.blocker_profile_id IN (SELECT id FROM viewer_profiles)))) "
       <> "ORDER BY r.id LIMIT 21 " )
        [viewer, viewer, viewer, toPersistValue eventKey, PersistText status, PersistInt64 (fromMaybe 0 after)]
        :: SqlPersistT IO [(Single Int64, Single T.Text, Single (Maybe T.Text))])
    let visible = take 20 rows
        items = [Aeson.object ["id" Aeson..= show rid, "name" Aeson..= name, "avatarUrl" Aeson..= avatar]
                | (Single rid, Single name, Single avatar) <- visible]
        next = case reverse visible of
            (Single rid, _, _):_ | length rows > 20 -> Just (show rid)
            _ -> Nothing
    pure $ Aeson.object ["items" Aeson..= items, "nextCursor" Aeson..= next]
