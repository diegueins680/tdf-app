-- Rollback for the additive music release platform schema.
-- Refuses to destroy user/catalog/evidence data.
BEGIN;

DO $$
DECLARE
  material_rows BIGINT;
BEGIN
  SELECT
      (SELECT count(*) FROM music_release)
    + (SELECT count(*) FROM music_party)
    + (SELECT count(*) FROM music_upload_session)
    + (SELECT count(*) FROM music_purchase_order)
    + (SELECT count(*) FROM music_playback_event)
    + (SELECT count(*) FROM music_playlist)
    + (SELECT count(*) FROM music_favorite)
    + (SELECT count(*) FROM music_legacy_sanitation_item)
    + (SELECT count(*) FROM music_infringement_report)
    + (SELECT count(*) FROM music_ddex_export)
    + (SELECT count(*) FROM music_ddex_party_registry)
  INTO material_rows;
  IF material_rows > 0 THEN
    RAISE EXCEPTION 'refusing music release rollback: % material rows exist', material_rows;
  END IF;
END;
$$;

DO $restore_music_notification_contract$
DECLARE
  constraint_comment TEXT;
  original_check TEXT;
BEGIN
  IF pg_catalog.to_regclass('public.notification') IS NULL THEN
    RETURN;
  END IF;

  SELECT pg_catalog.obj_description(constraint_row.oid, 'pg_constraint')
  INTO constraint_comment
  FROM pg_catalog.pg_constraint constraint_row
  WHERE constraint_row.conrelid='public.notification'::pg_catalog.regclass
    AND constraint_row.conname='notification_notif_type_check'
    AND constraint_row.contype='c';

  IF constraint_comment LIKE 'tdf_music_original:%' THEN
    IF EXISTS (
      SELECT 1 FROM public.notification WHERE notif_type LIKE 'music_release_%'
    ) THEN
      RAISE EXCEPTION 'refusing notification rollback: music release notifications still exist';
    END IF;
    original_check := substring(
      constraint_comment FROM length('tdf_music_original:') + 1
    );
    ALTER TABLE public.notification DROP CONSTRAINT notification_notif_type_check;
    EXECUTE pg_catalog.format(
      'ALTER TABLE public.notification ADD CONSTRAINT notification_notif_type_check CHECK (%s) NOT VALID',
      original_check
    );
    ALTER TABLE public.notification VALIDATE CONSTRAINT notification_notif_type_check;
  END IF;
END
$restore_music_notification_contract$;

DROP VIEW IF EXISTS music_legacy_release_sanitation_queue;
DROP FUNCTION IF EXISTS music_public_asset_accessible(UUID,TEXT);
DROP VIEW IF EXISTS music_public_release;

DROP FUNCTION IF EXISTS music_scan_legacy_release_sanitation(BIGINT,INTEGER);

DROP TRIGGER IF EXISTS trg_music_sync_verified_checkout ON commerce_checkout_session;
DROP TRIGGER IF EXISTS trg_music_checkout_require_verified_payment ON commerce_checkout_session;
DROP FUNCTION IF EXISTS music_sync_verified_checkout();
DROP FUNCTION IF EXISTS music_checkout_require_verified_payment();

DROP TABLE IF EXISTS music_ddex_export;
DROP TABLE IF EXISTS music_ddex_party_registry;
DROP TABLE IF EXISTS music_daily_metric;
DROP TABLE IF EXISTS music_playback_event;
DROP TABLE IF EXISTS music_playback_history;
DROP TABLE IF EXISTS music_playlist_item;
DROP TABLE IF EXISTS music_playlist;
DROP TABLE IF EXISTS music_favorite;
DROP TABLE IF EXISTS music_download_event;
DROP TABLE IF EXISTS music_entitlement;
DROP TABLE IF EXISTS music_purchase_order;
DROP TABLE IF EXISTS music_infringement_report;
DROP TABLE IF EXISTS music_legacy_sanitation_item;
DROP TABLE IF EXISTS music_release_audit_event;
DROP TABLE IF EXISTS music_editorial_comment;
DROP TABLE IF EXISTS music_terms_acceptance;
DROP TABLE IF EXISTS music_availability_rule;
DROP TABLE IF EXISTS music_processing_job;
DROP TABLE IF EXISTS music_upload_part;
DROP TABLE IF EXISTS music_upload_session;
DROP TABLE IF EXISTS music_rights_split;
DROP TABLE IF EXISTS music_rights_declaration;
DROP TABLE IF EXISTS music_asset;
DROP TABLE IF EXISTS music_identifier;
DROP TABLE IF EXISTS music_credit;
DROP TABLE IF EXISTS music_release_track;
DROP TABLE IF EXISTS music_recording;
DROP TABLE IF EXISTS music_party_identifier;
DROP TABLE IF EXISTS music_party;
ALTER TABLE music_release DROP CONSTRAINT IF EXISTS music_release_published_version_fk;
DROP TABLE IF EXISTS music_release_version;
DROP TABLE IF EXISTS artist_release_team_member;
DROP TABLE IF EXISTS music_release;

DROP FUNCTION IF EXISTS music_publish_due(INTEGER);
DROP FUNCTION IF EXISTS music_withdraw_due(INTEGER);
DROP FUNCTION IF EXISTS music_rebuild_daily_metrics(DATE);
DROP FUNCTION IF EXISTS music_protect_recording_content();
DROP FUNCTION IF EXISTS music_protect_version_content();
DROP FUNCTION IF EXISTS music_release_version_is_locked(UUID);
DROP FUNCTION IF EXISTS music_record_playback_event(UUID,UUID,INTEGER,BIGINT,TEXT,UUID,UUID,TEXT,BIGINT,BIGINT,TEXT,TEXT,TIMESTAMPTZ,JSONB);
DROP FUNCTION IF EXISTS music_record_state_event();
DROP FUNCTION IF EXISTS music_notify_infringement_report();
DROP FUNCTION IF EXISTS music_protect_asset();
DROP FUNCTION IF EXISTS music_protect_append_only();
DROP FUNCTION IF EXISTS music_validate_split_total();
DROP FUNCTION IF EXISTS music_validate_infringement_status();
DROP FUNCTION IF EXISTS music_validate_version_state();
DROP FUNCTION IF EXISTS music_refresh_validation_flags(UUID);
DROP FUNCTION IF EXISTS music_check_ddex_export(UUID);
DROP FUNCTION IF EXISTS music_create_release_correction(UUID,UUID,BIGINT);
DROP FUNCTION IF EXISTS music_check_submission(UUID);
DROP FUNCTION IF EXISTS music_valid_transition(TEXT, TEXT);
DROP FUNCTION IF EXISTS music_can(BIGINT, BIGINT, TEXT);
DROP FUNCTION IF EXISTS music_artist_is_verified(BIGINT);

DELETE FROM revenue_feature_flag WHERE flag_key LIKE 'music_releases.%';

COMMIT;
