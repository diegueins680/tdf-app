{-# LANGUAGE OverloadedStrings #-}
module TDF.MusicPublicationSpec (spec) where

import Control.Exception (finally)
import Control.Monad (forM_, void)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (PersistValue(..))
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool)
import System.Environment (lookupEnv)
import Test.Hspec

-- Runs music_publish_due and music_withdraw_due against the fully migrated
-- synthetic database. Every release is built through the real submission
-- rules (no trigger bypass), so publication exercises the same checks as
-- production. Each step runs in its own transaction: the scheduler compares
-- release times with the transaction clock.
spec :: Spec
spec = describe "music-publication-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_MUSIC_PUBLICATION_DATABASE_URL")
  routingOverrides <- runIO (traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"])
  case configured of
    Nothing -> it "requires the isolated fully migrated integration runner" $
      pendingWith "Run scripts/test-music-publication.sh"
    Just _ | any (maybe False (not . null)) routingOverrides ->
      it "refuses inherited libpq routing overrides" $
        expectationFailure "Unset PGHOSTADDR, PGSERVICE and PGSERVICEFILE for the isolated runner"
    Just connection | not (safeConnection connection) ->
      it "refuses a non-disposable database" $ expectationFailure "Use the isolated integration runner"
    Just connection -> beforeAll (do
        pool <- runNoLoggingT (createPostgresqlPool (BS.pack connection) 2)
        runSqlPool installFixture pool
        pure pool) $
      afterAll (\pool -> runSqlPool dropFixture pool `finally` destroyAllResources pool) $ do

        it "publishes each due candidate on its own: a failing one is suspended with its error, the others publish" $ \pool -> do
          (_, failing) <- publishable pool "isolation-failing"
          (_, other) <- publishable pool "isolation-other"
          schedulePast pool failing
          schedulePast pool other
          withPublicationFailure pool failing "P0001" $ do
            publishDue pool `shouldReturn` 1
            versionState pool failing `shouldReturn` "suspended"
            versionState pool other `shouldReturn` "published"
            failures <- runSqlPool (rawSql
              "SELECT data->>'sqlstate' FROM music_release_audit_event WHERE release_version_id=?::uuid AND event_type='publication_failed' AND prior_state='scheduled' AND next_state='suspended'"
              [PersistText failing]) pool
            failures `shouldBe` [Single ("P0001" :: Text)]
            publishDue pool `shouldReturn` 0

        it "leaves the live version and the release untouched when its correction fails to publish" $ \pool -> do
          (release, v1) <- publishable pool "sibling"
          schedulePast pool v1
          publishDue pool `shouldReturn` 1
          v2 <- correction pool release v1
          schedulePast pool v2
          withPublicationFailure pool v2 "P0001" $ do
            publishDue pool `shouldReturn` 0
            versionState pool v1 `shouldReturn` "published"
            versionState pool v2 `shouldReturn` "suspended"
            releaseRow pool release `shouldReturn` (Just v1, False)

        it "keeps a candidate scheduled after a transient failure and publishes it on the next run" $ \pool -> do
          (release, v1) <- publishable pool "transient"
          schedulePast pool v1
          withPublicationFailure pool v1 "40P01" $ do
            publishDue pool `shouldReturn` 0
            versionState pool v1 `shouldReturn` "scheduled"
            failedPublications pool v1 `shouldReturn` 0
          publishDue pool `shouldReturn` 1
          versionState pool v1 `shouldReturn` "published"
          releaseRow pool release `shouldReturn` (Just v1, False)

        it "retires every live version of the release when a later correction publishes" $ \pool -> do
          (release, v1) <- publishable pool "retire"
          schedulePast pool v1
          publishDue pool `shouldReturn` 1
          v2 <- correction pool release v1
          v3 <- correction pool release v1
          schedulePast pool v2
          schedulePast pool v3
          publishDue pool `shouldReturn` 2
          versionState pool v1 `shouldReturn` "withdrawn"
          versionState pool v2 `shouldReturn` "withdrawn"
          versionState pool v3 `shouldReturn` "published"
          releaseRow pool release `shouldReturn` (Just v3, False)
          publishedCount pool release `shouldReturn` 1

        it "publishes a correction of a withdrawn release and reopens it" $ \pool -> do
          (release, v1) <- publishable pool "reopen"
          schedulePast pool v1
          publishDue pool `shouldReturn` 1
          takedownPast pool v1
          withdrawDue pool `shouldReturn` 1
          releaseRow pool release `shouldReturn` (Nothing, True)
          v2 <- correction pool release v1
          schedulePast pool v2
          publishDue pool `shouldReturn` 1
          versionState pool v2 `shouldReturn` "published"
          releaseRow pool release `shouldReturn` (Just v2, False)
          publicRows pool release `shouldReturn` 1

        it "clears the release pointer when the live version is withdrawn through the API path" $ \pool -> do
          (release, v1) <- publishable pool "pointer"
          schedulePast pool v1
          publishDue pool `shouldReturn` 1
          releaseRow pool release `shouldReturn` (Just v1, False)
          -- transitionReleaseVersion applies exactly these two updates.
          transition pool v1 "suspended"
          transition pool v1 "withdrawn"
          releaseRow pool release `shouldReturn` (Nothing, True)
          publicRows pool release `shouldReturn` 0

        it "withdraws each due takedown on its own and records a failure once" $ \pool -> do
          (_, failing) <- publishable pool "takedown-failing"
          (_, other) <- publishable pool "takedown-other"
          schedulePast pool failing
          schedulePast pool other
          publishDue pool `shouldReturn` 2
          takedownPast pool failing
          takedownPast pool other
          withWithdrawalFailure pool failing $ do
            withdrawDue pool `shouldReturn` 1
            withdrawDue pool `shouldReturn` 0
            versionState pool failing `shouldReturn` "takedown_scheduled"
            versionState pool other `shouldReturn` "withdrawn"
            failedEvents pool failing `shouldReturn` 1
          withdrawDue pool `shouldReturn` 1
          versionState pool failing `shouldReturn` "withdrawn"

        it "advances a one-row withdrawal batch past a permanently failing version" $ \pool -> do
          (_, failing) <- publishable pool "takedown-poison-first"
          (_, other) <- publishable pool "takedown-behind-poison"
          schedulePast pool failing
          schedulePast pool other
          publishDue pool `shouldReturn` 2
          takedownPast pool failing
          takedownPast pool other
          let withdrawOne = count pool "SELECT count(*) FROM music_withdraw_due(1)" []
          withWithdrawalFailure pool failing $ do
            withdrawOne `shouldReturn` 0
            withdrawOne `shouldReturn` 1
            versionState pool failing `shouldReturn` "takedown_scheduled"
            versionState pool other `shouldReturn` "withdrawn"
            failedEvents pool failing `shouldReturn` 1
          withdrawOne `shouldReturn` 1
          versionState pool failing `shouldReturn` "withdrawn"

safeConnection :: String -> Bool
safeConnection value = not (any (`elem` value) ['?', '#'])
  && "_test" `isSuffixOf` value && any (`isPrefixOf` value)
  ["postgresql://127.0.0.1/", "postgresql://localhost/",
   "postgresql://postgres:postgres@postgres:5432/"]

-- The fixture builds a complete release graph and moves it to 'approved'
-- through the state trigger, and makes approved corrections the same way.
installFixture :: SqlPersistT IO ()
installFixture = do
  rawExecute "INSERT INTO party(display_name,is_org,created_at) SELECT 'Music publication spec', FALSE, NOW() WHERE NOT EXISTS (SELECT 1 FROM party WHERE display_name='Music publication spec')" []
  rawExecute (T.unlines
    [ "CREATE OR REPLACE FUNCTION music_spec_publishable(slug TEXT) RETURNS TABLE(release_id UUID, version_id UUID) LANGUAGE plpgsql AS $$"
    , "DECLARE actor BIGINT; rid UUID; vid UUID; rec UUID; mp UUID; mr UUID; cr UUID; master UUID; cover UUID;"
    , "BEGIN"
    , "  SELECT id INTO actor FROM party WHERE display_name='Music publication spec' ORDER BY id LIMIT 1;"
    , "  INSERT INTO music_release(artist_party_id,canonical_slug,release_kind,created_by) VALUES (actor,'music-spec-'||slug||'-'||gen_random_uuid(),'single',actor) RETURNING id INTO rid;"
    , "  INSERT INTO music_release_version(release_id,version_number,title,display_artist,explicit_content,recording_copyright_text,work_copyright_text,primary_genre_id,created_by)"
    , "    VALUES (rid,1,'Spec '||slug,'Spec Artist','not_explicit','℗ 2026 Spec','© 2026 Spec','ea0ee25a-326a-4174-9682-72d063caf6f9',actor) RETURNING id INTO vid;"
    , "  INSERT INTO music_recording(canonical_title,duration_ms,explicit_content,created_by) VALUES ('Spec '||slug,180000,'not_explicit',actor) RETURNING id INTO rec;"
    , "  INSERT INTO music_party(tdf_party_id,display_name,created_by) VALUES (actor,'Spec Artist',actor) ON CONFLICT DO NOTHING;"
    , "  SELECT id INTO mp FROM music_party WHERE tdf_party_id=actor;"
    , "  INSERT INTO music_release_track(release_version_id,recording_id,track_number,display_artist) VALUES (vid,rec,1,'Spec Artist');"
    , "  INSERT INTO music_credit(release_version_id,recording_id,music_party_id,credit_role) VALUES (vid,rec,mp,'main_artist'),(vid,rec,mp,'composer');"
    , "  INSERT INTO music_terms_acceptance(release_version_id,terms_kind,terms_version,accepted_by,evidence) VALUES (vid,'publication_authority','publication-v1',actor,'{\"source\":\"music-publication-spec\"}');"
    , "  INSERT INTO music_rights_declaration(release_version_id,recording_id,rights_scope,authority_basis,territories,starts_on,declared_by) VALUES (vid,rec,'master','owned',ARRAY['Worldwide'],'2026-09-11',actor) RETURNING id INTO mr;"
    , "  INSERT INTO music_rights_declaration(release_version_id,recording_id,rights_scope,authority_basis,territories,starts_on,declared_by) VALUES (vid,rec,'composition','licensed',ARRAY['Worldwide'],'2026-09-11',actor) RETURNING id INTO cr;"
    , "  INSERT INTO music_rights_split(declaration_id,rights_holder_id,basis_points,territories,starts_on) VALUES (mr,mp,10000,ARRAY['Worldwide'],'2026-09-11'),(cr,mp,10000,ARRAY['Worldwide'],'2026-09-11');"
    , "  INSERT INTO music_asset(release_version_id,recording_id,asset_role,storage_provider,storage_class,bucket_name,object_key,original_filename,media_type,byte_size,sha256,processing_state,immutable,created_by,ready_at)"
    , "    VALUES (vid,rec,'master_audio','local_private','standard','music-private','masters/'||vid||'/master.wav','master.wav','audio/wav',123456,encode(sha256((vid||'m')::bytea),'hex'),'ready',TRUE,actor,NOW()) RETURNING id INTO master;"
    , "  INSERT INTO music_asset(release_version_id,recording_id,parent_asset_id,asset_role,storage_provider,storage_class,bucket_name,object_key,media_type,byte_size,sha256,processing_state,immutable,created_by,ready_at)"
    , "    VALUES (vid,rec,master,'stream_audio','local_private','standard','music-private','derivatives/'||vid||'/stream.m4a','audio/mp4',65432,encode(sha256((vid||'s')::bytea),'hex'),'ready',TRUE,actor,NOW());"
    , "  INSERT INTO music_asset(release_version_id,asset_role,storage_provider,storage_class,bucket_name,object_key,original_filename,media_type,byte_size,sha256,processing_state,immutable,created_by,ready_at)"
    , "    VALUES (vid,'cover_original','local_private','standard','music-private','art/'||vid||'/cover.png','cover.png','image/png',23456,encode(sha256((vid||'c')::bytea),'hex'),'ready',TRUE,actor,NOW()) RETURNING id INTO cover;"
    , "  INSERT INTO music_asset(release_version_id,parent_asset_id,asset_role,storage_provider,storage_class,bucket_name,object_key,media_type,byte_size,sha256,processing_state,immutable,created_by,ready_at)"
    , "    VALUES (vid,cover,'cover_display','local_private','standard','music-private','art/'||vid||'/cover.webp','image/webp',12345,encode(sha256((vid||'d')::bytea),'hex'),'ready',TRUE,actor,NOW());"
    , "  INSERT INTO music_availability_rule(release_version_id,territories,listening_policy,download_policy,purchasable) VALUES (vid,ARRAY['Worldwide'],'full','none',FALSE);"
    , "  INSERT INTO music_release_version_party(release_version_id,music_party_id,party_details,details_source)"
    , "    VALUES (vid,mp,jsonb_build_object('displayName','Spec Artist','partyKind','person','identifiers','[]'::jsonb),'user_provided');"
    , "  PERFORM music_refresh_validation_flags(vid);"
    , "  UPDATE music_release_version SET immutable_snapshot=jsonb_build_object('version',vid), snapshot_sha256=encode(sha256(vid::text::bytea),'hex') WHERE id=vid;"
    , "  UPDATE music_release_version SET state='ready_for_review' WHERE id=vid;"
    , "  UPDATE music_release_version SET state='in_review' WHERE id=vid;"
    , "  UPDATE music_release_version SET state='approved',approved_by=actor,approved_at=NOW() WHERE id=vid;"
    , "  RETURN QUERY SELECT rid, vid;"
    , "END $$;"
    ]) []
  rawExecute (T.unlines
    [ "CREATE OR REPLACE FUNCTION music_spec_correction(rid UUID, source UUID) RETURNS UUID LANGUAGE plpgsql AS $$"
    , "DECLARE actor BIGINT; vid UUID;"
    , "BEGIN"
    , "  SELECT id INTO actor FROM party WHERE display_name='Music publication spec' ORDER BY id LIMIT 1;"
    , "  vid := music_create_release_correction(rid, source, actor);"
    , "  INSERT INTO music_terms_acceptance(release_version_id,terms_kind,terms_version,accepted_by,evidence) VALUES (vid,'publication_authority','publication-v1',actor,'{\"source\":\"music-publication-spec\"}');"
    , "  PERFORM music_refresh_validation_flags(vid);"
    , "  UPDATE music_release_version SET immutable_snapshot=jsonb_build_object('version',vid), snapshot_sha256=encode(sha256(vid::text::bytea),'hex') WHERE id=vid;"
    , "  UPDATE music_release_version SET state='ready_for_review' WHERE id=vid;"
    , "  UPDATE music_release_version SET state='in_review' WHERE id=vid;"
    , "  UPDATE music_release_version SET state='approved',approved_by=actor,approved_at=NOW() WHERE id=vid;"
    , "  RETURN vid;"
    , "END $$;"
    ]) []
  -- Scheduled through the state trigger for a moment that passes before the
  -- next transaction starts.
  rawExecute (T.unlines
    [ "CREATE OR REPLACE FUNCTION music_spec_schedule_past(vid UUID) RETURNS void LANGUAGE plpgsql AS $$"
    , "DECLARE actor BIGINT;"
    , "BEGIN"
    , "  SELECT id INTO actor FROM party WHERE display_name='Music publication spec' ORDER BY id LIMIT 1;"
    , "  UPDATE music_release_version SET state='scheduled',scheduled_by=actor,release_at_utc=clock_timestamp()+interval '50 milliseconds',release_timezone='UTC' WHERE id=vid;"
    , "  PERFORM pg_sleep(0.06);"
    , "END $$;"
    ]) []
  rawExecute "CREATE OR REPLACE FUNCTION music_spec_fail() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'synthetic music publication failure' USING ERRCODE=TG_ARGV[0]; END $$" []

dropFixture :: SqlPersistT IO ()
dropFixture = forM_
  [ "DROP FUNCTION IF EXISTS music_spec_publishable(TEXT)"
  , "DROP FUNCTION IF EXISTS music_spec_correction(UUID, UUID)"
  , "DROP FUNCTION IF EXISTS music_spec_schedule_past(UUID)"
  , "DROP TRIGGER IF EXISTS music_spec_fail ON music_release_version"
  , "DROP FUNCTION IF EXISTS music_spec_fail()"
  ] $ \statement -> rawExecute statement []

publishable :: ConnectionPool -> Text -> IO (Text, Text)
publishable pool slug = do
  rows <- runSqlPool (rawSql "SELECT release_id::text, version_id::text FROM music_spec_publishable(?)" [PersistText slug]) pool
  case rows of
    [(Single release, Single version)] -> pure (release, version)
    _ -> fail "fixture did not return one release"

correction :: ConnectionPool -> Text -> Text -> IO Text
correction pool release source = do
  rows <- runSqlPool (rawSql "SELECT music_spec_correction(?::uuid, ?::uuid)::text" [PersistText release, PersistText source]) pool
  case rows of
    [Single version] -> pure version
    _ -> fail "correction fixture did not return one version"

schedulePast :: ConnectionPool -> Text -> IO ()
schedulePast pool version = void (runSqlPool (rawSql "SELECT music_spec_schedule_past(?::uuid)::text" [PersistText version] :: SqlPersistT IO [Single (Maybe Text)]) pool)

takedownPast :: ConnectionPool -> Text -> IO ()
takedownPast pool version = runSqlPool (rawExecute
  "UPDATE music_release_version SET state='takedown_scheduled', takedown_at_utc=NOW()-interval '1 minute', takedown_timezone='UTC' WHERE id=?::uuid"
  [PersistText version]) pool

transition :: ConnectionPool -> Text -> Text -> IO ()
transition pool version target = runSqlPool (rawExecute
  "UPDATE music_release_version SET state=?,updated_at=NOW() WHERE id=?::uuid" [PersistText target, PersistText version]) pool

publishDue :: ConnectionPool -> IO Int64
publishDue pool = count pool "SELECT count(*) FROM music_publish_due(100)" []

withdrawDue :: ConnectionPool -> IO Int64
withdrawDue pool = count pool "SELECT count(*) FROM music_withdraw_due(100)" []

publishedCount :: ConnectionPool -> Text -> IO Int64
publishedCount pool release = count pool "SELECT count(*) FROM music_release_version WHERE release_id=?::uuid AND state='published'" [PersistText release]

publicRows :: ConnectionPool -> Text -> IO Int64
publicRows pool release = count pool "SELECT count(*) FROM music_public_release WHERE id=?::uuid" [PersistText release]

failedPublications :: ConnectionPool -> Text -> IO Int64
failedPublications pool version = count pool "SELECT count(*) FROM music_release_audit_event WHERE release_version_id=?::uuid AND event_type='publication_failed'" [PersistText version]

failedEvents :: ConnectionPool -> Text -> IO Int64
failedEvents pool version = count pool "SELECT count(*) FROM music_release_audit_event WHERE release_version_id=?::uuid AND event_type='withdrawal_failed'" [PersistText version]

count :: ConnectionPool -> Text -> [PersistValue] -> IO Int64
count pool sql params = do
  rows <- runSqlPool (rawSql sql params) pool
  case rows of
    [Single n] -> pure n
    _ -> fail ("count query returned no row: " <> T.unpack sql)

versionState :: ConnectionPool -> Text -> IO Text
versionState pool version = do
  rows <- runSqlPool (rawSql "SELECT state FROM music_release_version WHERE id=?::uuid" [PersistText version]) pool
  case rows of
    [Single state] -> pure state
    _ -> fail "version not found"

-- (published_version_id, withdrawn_at IS NOT NULL)
releaseRow :: ConnectionPool -> Text -> IO (Maybe Text, Bool)
releaseRow pool release = do
  rows <- runSqlPool (rawSql "SELECT published_version_id::text, withdrawn_at IS NOT NULL FROM music_release WHERE id=?::uuid" [PersistText release]) pool
  case rows of
    [(Single pointer, Single withdrawn)] -> pure (pointer, withdrawn)
    _ -> fail "release not found"

-- A trigger that makes publication of exactly this version raise the given
-- SQLSTATE (P0001 for a lasting failure, 40P01 for a synthetic deadlock).
withPublicationFailure :: ConnectionPool -> Text -> Text -> IO a -> IO a
withPublicationFailure pool version sqlstate action = do
  runSqlPool (rawExecute
    ("CREATE TRIGGER music_spec_fail BEFORE UPDATE OF state ON music_release_version FOR EACH ROW WHEN (NEW.state='published' AND NEW.id='" <> version <> "'::uuid) EXECUTE FUNCTION music_spec_fail('" <> sqlstate <> "')") []) pool
  action `finally` runSqlPool (rawExecute "DROP TRIGGER IF EXISTS music_spec_fail ON music_release_version" []) pool

withWithdrawalFailure :: ConnectionPool -> Text -> IO a -> IO a
withWithdrawalFailure pool version action = do
  runSqlPool (rawExecute
    ("CREATE TRIGGER music_spec_fail BEFORE UPDATE OF state ON music_release_version FOR EACH ROW WHEN (NEW.state='withdrawn' AND NEW.id='" <> version <> "'::uuid) EXECUTE FUNCTION music_spec_fail('P0001')") []) pool
  action `finally` runSqlPool (rawExecute "DROP TRIGGER IF EXISTS music_spec_fail ON music_release_version" []) pool
