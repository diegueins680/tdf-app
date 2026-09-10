{-# LANGUAGE OverloadedStrings #-}

-- | Database boundary for private consent state and its append-only audit log.
module TDF.DB.ReputationConsent
  ( listReputationConsents
  , persistReputationConsent
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, rawExecute, rawSql)
import TDF.DTO.ReputationConsent (ReputationConsentDTO(..))

listReputationConsents :: Int64 -> SqlPersistT IO [ReputationConsentDTO]
listReputationConsents partyId = do
  rows <- rawSql
    "SELECT kind,coalesce(state.granted,false),coalesce(state.version,0),state.updated_at \
    \FROM unnest(ARRAY['pilot_participation','public_visibility','public_rankings','rating_reminders']::text[]) kind \
    \LEFT JOIN reputation_consent_state state ON state.party_id=? AND state.consent_kind=kind ORDER BY kind"
    [PersistInt64 partyId]
    :: SqlPersistT IO [(Single Text, Single Bool, Single Int, Single (Maybe UTCTime))]
  pure
    [ ReputationConsentDTO kind isGranted version changedAt
    | (Single kind, Single isGranted, Single version, Single changedAt) <- rows
    ]

-- | A repeated grant is deliberately a no-op only when the most recent grant
-- accepted the same disclosure version and locale. A new disclosure is a new
-- affirmative event even though the current boolean state remains true.
persistReputationConsent
  :: Int64 -> Text -> Bool -> Maybe Text -> Maybe Text -> SqlPersistT IO ()
persistReputationConsent partyId kind isGranted copyVersion locale = do
  rows <- rawSql
    "INSERT INTO reputation_consent_state(party_id,consent_kind,granted,version,updated_at) \
    \VALUES (?,?,?,1,now()) \
    \ON CONFLICT(party_id,consent_kind) DO UPDATE SET \
    \granted=EXCLUDED.granted,version=reputation_consent_state.version+1,updated_at=now() \
    \WHERE reputation_consent_state.granted IS DISTINCT FROM EXCLUDED.granted OR \
    \(EXCLUDED.granted AND ( \
    \(SELECT event.consent_copy_version FROM reputation_consent_event event \
    \ WHERE event.party_id=reputation_consent_state.party_id \
    \ AND event.consent_kind=reputation_consent_state.consent_kind AND event.granted \
    \ ORDER BY event.version DESC LIMIT 1) IS DISTINCT FROM ? OR \
    \(SELECT event.consent_locale FROM reputation_consent_event event \
    \ WHERE event.party_id=reputation_consent_state.party_id \
    \ AND event.consent_kind=reputation_consent_state.consent_kind AND event.granted \
    \ ORDER BY event.version DESC LIMIT 1) IS DISTINCT FROM ?)) RETURNING version"
    [ PersistInt64 partyId, PersistText kind, PersistBool isGranted
    , maybe PersistNull PersistText copyVersion, maybe PersistNull PersistText locale
    ] :: SqlPersistT IO [Single Int]
  case rows of
    [Single version] -> rawExecute
      "INSERT INTO reputation_consent_event(party_id,consent_kind,granted,version,source,consent_copy_version,consent_locale) VALUES (?,?,?,?, 'self_service',?,?)"
      [ PersistInt64 partyId, PersistText kind, PersistBool isGranted, PersistInt64 (fromIntegral version)
      , maybe PersistNull PersistText copyVersion, maybe PersistNull PersistText locale
      ]
    _ -> pure ()
