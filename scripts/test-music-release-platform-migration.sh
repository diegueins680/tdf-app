#!/bin/sh
set -eu

TDF_MUSIC_CONTAINER="tdf-music-release-migration-test-$$"
TDF_MUSIC_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TDF_MUSIC_DB="tdf_music_release_test"

cleanup() {
  docker rm -f "$TDF_MUSIC_CONTAINER" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$TDF_MUSIC_CONTAINER" \
  -e POSTGRES_PASSWORD=music-release-test \
  -e POSTGRES_DB="$TDF_MUSIC_DB" \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$TDF_MUSIC_CONTAINER" pg_isready -U postgres -d "$TDF_MUSIC_DB" >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "Music release migration test database did not become ready" >&2
    exit 1
  fi
  sleep 1
done
# PostgreSQL can report ready while its one-time initialization entrypoint is
# still cycling the temporary server. Wait for the final server, as the older
# migration harnesses in this repository do.
sleep 5

psql_exec() {
  docker exec -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_MUSIC_CONTAINER" \
    psql -q -v ON_ERROR_STOP=1 -U postgres -d "$TDF_MUSIC_DB" "$@"
}

apply_file() {
  docker exec -i -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_MUSIC_CONTAINER" \
    psql -q -v ON_ERROR_STOP=1 -U postgres -d "$TDF_MUSIC_DB" < "$1" >/dev/null
}

apply_dependencies() {
  apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/init_schema.sql"
  apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-07-12_notification_table.sql"
  apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-08-05_artist_enrichment.sql"
  apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-08-13_unified_checkout_core.sql"
  apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-04_access_request_notification_types.sql"
}

apply_dependencies
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-11_music_release_platform.sql"

table_count=$(psql_exec -Atc "SELECT count(*) FROM information_schema.tables WHERE table_schema='public' AND table_name LIKE 'music_%';")
test "$table_count" = "33"

artist_id=$(psql_exec -Atc "INSERT INTO party(display_name) VALUES ('Verified Migration Artist') RETURNING id;")
member_id=$(psql_exec -Atc "INSERT INTO party(display_name) VALUES ('Authorized Team Member') RETURNING id;")
outsider_id=$(psql_exec -Atc "INSERT INTO party(display_name) VALUES ('Unauthorized User') RETURNING id;")
legacy_release_id=$(psql_exec -Atc "INSERT INTO artist_release(artist_party_id,title,release_date) VALUES ($artist_id,'Incomplete legacy release','2024-01-02') RETURNING id;")
test "$(psql_exec -Atc "SELECT scanned_count || ':' || next_cursor FROM music_scan_legacy_release_sanitation(0,100);")" = "1:$legacy_release_id"
test "$(psql_exec -Atc "SELECT status || ':' || scan_attempts || ':' || ('missing_master'=ANY(issues))::text || ':' || ('missing_release_kind'=ANY(issues))::text FROM music_legacy_sanitation_item WHERE legacy_release_id=$legacy_release_id;")" = "pending:1:true:true"
test "$(psql_exec -Atc "SELECT scanned_count || ':' || next_cursor FROM music_scan_legacy_release_sanitation(0,100);")" = "1:$legacy_release_id"
test "$(psql_exec -Atc "SELECT scan_attempts FROM music_legacy_sanitation_item WHERE legacy_release_id=$legacy_release_id;")" = "2"
psql_exec -c "
  INSERT INTO artist_profile(artist_party_id,slug,created_at) VALUES ($artist_id,'verified-migration-artist',NOW());
  INSERT INTO artist_profile_enrichment(artist_party_id,last_verified_at,review_status,created_at,updated_at)
    VALUES ($artist_id,NOW(),'verified',NOW(),NOW());
  INSERT INTO artist_release_team_member(artist_party_id,member_party_id,role_code,permissions,granted_by)
    VALUES ($artist_id,$member_id,'editor',ARRAY['release.create','release.edit','release.upload','release.submit'],$artist_id);
" >/dev/null

test "$(psql_exec -Atc "SELECT music_can($artist_id,$artist_id,'release.create');")" = "t"
test "$(psql_exec -Atc "SELECT music_can($member_id,$artist_id,'release.submit');")" = "t"
test "$(psql_exec -Atc "SELECT music_can($member_id,$artist_id,'release.schedule');")" = "f"
test "$(psql_exec -Atc "SELECT music_can($outsider_id,$artist_id,'release.create');")" = "f"

release_id=$(psql_exec -Atc "INSERT INTO music_release(artist_party_id,canonical_slug,release_kind,created_by) VALUES ($artist_id,'migration-single','single',$member_id) RETURNING id;")
version_id=$(psql_exec -Atc "INSERT INTO music_release_version(release_id,version_number,title,display_artist,explicit_content,recording_copyright_text,work_copyright_text,created_by) VALUES ('$release_id',1,'Migration Single','Verified Migration Artist','not_explicit','℗ 2026 Migration Artist','© 2026 Migration Artist',$member_id) RETURNING id;")
psql_exec -c "UPDATE music_release_version SET primary_genre_id='ea0ee25a-326a-4174-9682-72d063caf6f9' WHERE id='$version_id';" >/dev/null
recording_id=$(psql_exec -Atc "INSERT INTO music_recording(canonical_title,duration_ms,explicit_content,created_by) VALUES ('Migration Single',180000,'not_explicit',$member_id) RETURNING id;")
credit_party_id=$(psql_exec -Atc "INSERT INTO music_party(tdf_party_id,display_name,created_by) VALUES ($artist_id,'Verified Migration Artist',$member_id) RETURNING id;")

if psql_exec -c "UPDATE music_release_version SET state='ready_for_review' WHERE id='$version_id';" >/dev/null 2>&1; then
  echo "Incomplete release bypassed submission gates" >&2
  exit 1
fi

psql_exec -c "
  INSERT INTO music_release_track(release_version_id,recording_id,track_number,display_artist)
    VALUES ('$version_id','$recording_id',1,'Verified Migration Artist');
  INSERT INTO music_credit(release_version_id,recording_id,music_party_id,credit_role)
    VALUES ('$version_id','$recording_id','$credit_party_id','main_artist');
  INSERT INTO music_credit(release_version_id,recording_id,music_party_id,credit_role)
    VALUES ('$version_id','$recording_id','$credit_party_id','composer');
  INSERT INTO music_terms_acceptance(release_version_id,terms_kind,terms_version,accepted_by,evidence)
    VALUES ('$version_id','publication_authority','publication-v1',$member_id,'{\"source\":\"migration-test\"}');
" >/dev/null

master_rights_id=$(psql_exec -Atc "INSERT INTO music_rights_declaration(release_version_id,recording_id,rights_scope,authority_basis,territories,starts_on,declared_by) VALUES ('$version_id','$recording_id','master','owned',ARRAY['Worldwide'],'2026-09-11',$member_id) RETURNING id;")
composition_rights_id=$(psql_exec -Atc "INSERT INTO music_rights_declaration(release_version_id,recording_id,rights_scope,authority_basis,territories,starts_on,declared_by) VALUES ('$version_id','$recording_id','composition','licensed',ARRAY['Worldwide'],'2026-09-11',$member_id) RETURNING id;")

psql_exec -c "
  INSERT INTO music_rights_split(declaration_id,rights_holder_id,basis_points,territories,starts_on)
    VALUES ('$master_rights_id','$credit_party_id',10000,ARRAY['Worldwide'],'2026-09-11');
  INSERT INTO music_rights_split(declaration_id,rights_holder_id,basis_points,territories,starts_on)
    VALUES ('$composition_rights_id','$credit_party_id',10000,ARRAY['Worldwide'],'2026-09-11');
" >/dev/null

if psql_exec -c "BEGIN; UPDATE music_rights_split SET basis_points=9999 WHERE declaration_id='$master_rights_id'; COMMIT;" >/dev/null 2>&1; then
  echo "Invalid rights split total was accepted" >&2
  exit 1
fi

master_asset_id=$(psql_exec -Atc "INSERT INTO music_asset(release_version_id,recording_id,asset_role,storage_provider,storage_class,bucket_name,object_key,original_filename,media_type,byte_size,sha256,processing_state,immutable,created_by,ready_at) VALUES ('$version_id','$recording_id','master_audio','local_private','standard','music-private','masters/aa/00000000-0000-0000-0000-000000000001/master.wav','master.wav','audio/wav',123456,'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa','ready',TRUE,$member_id,NOW()) RETURNING id;")
stream_asset_id=$(psql_exec -Atc "INSERT INTO music_asset(release_version_id,recording_id,parent_asset_id,asset_role,storage_provider,storage_class,bucket_name,object_key,media_type,byte_size,sha256,processing_state,immutable,technical_metadata,created_by,ready_at) VALUES ('$version_id','$recording_id','$master_asset_id','stream_audio','local_private','standard','music-private','derivatives/bb/00000000-0000-0000-0000-000000000002/stream.m4a','audio/mp4',65432,'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb','ready',TRUE,'{\"codec\":\"aac\",\"bitrate_kbps\":192,\"loudness_lufs\":-14.0}',$member_id,NOW()) RETURNING id;")
cover_original_asset_id=$(psql_exec -Atc "INSERT INTO music_asset(release_version_id,asset_role,storage_provider,storage_class,bucket_name,object_key,original_filename,media_type,byte_size,sha256,processing_state,immutable,created_by,ready_at) VALUES ('$version_id','cover_original','local_private','standard','music-private','art/11/00000000-0000-0000-0000-000000000003/cover.png','cover.png','image/png',23456,'1111111111111111111111111111111111111111111111111111111111111111','ready',TRUE,$member_id,NOW()) RETURNING id;")
cover_display_asset_id=$(psql_exec -Atc "INSERT INTO music_asset(release_version_id,parent_asset_id,asset_role,storage_provider,storage_class,bucket_name,object_key,media_type,byte_size,sha256,processing_state,immutable,created_by,ready_at) VALUES ('$version_id','$cover_original_asset_id','cover_display','local_private','standard','music-private','art/22/00000000-0000-0000-0000-000000000004/cover.webp','image/webp',12345,'2222222222222222222222222222222222222222222222222222222222222222','ready',TRUE,$member_id,NOW()) RETURNING id;")

psql_exec -c "
  INSERT INTO music_availability_rule(release_version_id,territories,listening_policy,download_policy,purchasable,price_minor,currency,downloadable_asset_id)
    VALUES ('$version_id',ARRAY['EC'],'full','purchase',TRUE,250,'USD','$master_asset_id');
  INSERT INTO music_identifier(recording_id,identifier_type,identifier_value,provenance,verification_status)
    VALUES ('$recording_id','isrc','USRC17607839','provided','syntax_valid');
  SELECT * FROM music_refresh_validation_flags('$version_id');
  UPDATE music_release_version SET
    immutable_snapshot='{\"title\":\"Migration Single\",\"version\":1}',
    snapshot_sha256='cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc'
  WHERE id='$version_id';
  UPDATE music_release_version SET state='ready_for_review' WHERE id='$version_id';
  UPDATE music_release_version SET state='in_review' WHERE id='$version_id';
  UPDATE music_release_version SET state='approved',approved_by=$outsider_id,approved_at=NOW() WHERE id='$version_id';
" >/dev/null
test "$(psql_exec -Atc "SELECT metadata_valid AND assets_valid AND rights_valid AND access_valid FROM music_release_version WHERE id='$version_id';")" = "t"
test "$(psql_exec -Atc "SELECT count(*) FROM notification WHERE recipient_party_id=$artist_id AND notif_type='music_release_ready_for_review' AND target_type='music_release' AND target_id=$artist_id;")" = "1"
test "$(psql_exec -Atc "SELECT count(*) FROM notification WHERE recipient_party_id=$member_id AND notif_type='music_release_approved' AND target_type='music_release' AND target_id=$artist_id;")" = "1"

if psql_exec -c "UPDATE music_release_version SET state='published',published_at=NOW() WHERE id='$version_id';" >/dev/null 2>&1; then
  echo "Approved release skipped the scheduling state" >&2
  exit 1
fi

psql_exec -c "UPDATE music_release_version SET state='scheduled',scheduled_by=$outsider_id,release_at_utc=NOW() + interval '1 second',release_timezone='America/Guayaquil',embargo_until_utc=NOW() + interval '1 second' WHERE id='$version_id';" >/dev/null
sleep 2
test "$(psql_exec -Atc "SELECT count(*) FROM music_publish_due(10);")" = "1"
test "$(psql_exec -Atc "SELECT count(*) FROM music_publish_due(10);")" = "0"
test "$(psql_exec -Atc "SELECT count(*) FROM music_public_release WHERE id='$release_id';")" = "1"
test "$(psql_exec -Atc "SELECT music_public_asset_accessible('$cover_display_asset_id','EC');")" = "t"
test "$(psql_exec -Atc "SELECT music_public_asset_accessible('$stream_asset_id','EC');")" = "t"
test "$(psql_exec -Atc "SELECT music_public_asset_accessible('$cover_display_asset_id','US');")" = "f"
test "$(psql_exec -Atc "SELECT music_public_asset_accessible('$stream_asset_id','US');")" = "f"
test "$(psql_exec -Atc "SELECT music_public_asset_accessible('$cover_display_asset_id',NULL);")" = "f"
test "$(psql_exec -Atc "SELECT music_public_asset_accessible('$master_asset_id','EC');")" = "f"

# Apply to an already-published legacy graph: only evidenced links are backfilled.
pending_party_id=$(psql_exec -Atc "INSERT INTO music_party(display_name,created_by) VALUES ('Uncredited external collaborator',$artist_id) RETURNING id;")
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-15_music_version_parties.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-15_music_version_parties.sql"
test "$(psql_exec -Atc "SELECT count(*) FROM music_release_version_party WHERE release_version_id='$version_id';")" = "1"
test "$(psql_exec -Atc "SELECT count(*) FROM music_release_version_party WHERE music_party_id='$pending_party_id';")" = "0"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-15_music_version_parties_rollback.sql"
test "$(psql_exec -Atc "SELECT to_regclass('music_release_version_party') IS NULL;")" = "t"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-15_music_version_parties.sql"
for mutation in \
  "INSERT INTO music_release_version_party VALUES ('$version_id','$pending_party_id')" \
  "UPDATE music_release_version_party SET music_party_id='$pending_party_id' WHERE release_version_id='$version_id'" \
  "DELETE FROM music_release_version_party WHERE release_version_id='$version_id'"
do
  if psql_exec -c "$mutation" >/dev/null 2>&1; then
    echo "Published party membership accepted mutation" >&2
    exit 1
  fi
done

if psql_exec -c "UPDATE music_release_version SET title='Destructive edit' WHERE id='$version_id';" >/dev/null 2>&1; then
  echo "Published release accepted a destructive edit" >&2
  exit 1
fi
if psql_exec -c "UPDATE music_asset SET sha256='dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd' WHERE id='$master_asset_id';" >/dev/null 2>&1; then
  echo "Immutable master accepted a checksum mutation" >&2
  exit 1
fi
if psql_exec -c "UPDATE music_credit SET credit_role='featured_artist' WHERE release_version_id='$version_id';" >/dev/null 2>&1; then
  echo "Approved release accepted a destructive credit mutation" >&2
  exit 1
fi

playlist_id=$(psql_exec -Atc "INSERT INTO music_playlist(owner_party_id,name) VALUES ($outsider_id,'Migration playlist') RETURNING id;")
playlist_item_a=$(psql_exec -Atc "INSERT INTO music_playlist_item(playlist_id,recording_id,position,added_by) VALUES ('$playlist_id','$recording_id',0,$outsider_id) RETURNING id;")
playlist_item_b=$(psql_exec -Atc "INSERT INTO music_playlist_item(playlist_id,recording_id,position,added_by) VALUES ('$playlist_id','$recording_id',1,$outsider_id) RETURNING id;")
psql_exec -c "BEGIN; UPDATE music_playlist_item SET position=position-1 WHERE playlist_id='$playlist_id' AND position>0 AND position<=1; UPDATE music_playlist_item SET position=1 WHERE id='$playlist_item_a'; COMMIT;" >/dev/null
test "$(psql_exec -Atc "SELECT string_agg(id::text || ':' || position,',' ORDER BY position) FROM music_playlist_item WHERE playlist_id='$playlist_id';")" = "$playlist_item_b:0,$playlist_item_a:1"
psql_exec -c "DELETE FROM music_playlist WHERE id='$playlist_id';" >/dev/null
test "$(psql_exec -Atc "SELECT count(*) FROM music_playlist_item WHERE playlist_id='$playlist_id';")" = "0"

availability_rule_id=$(psql_exec -Atc "SELECT id FROM music_availability_rule WHERE release_version_id='$version_id';")
purchase_order_id="40000000-0000-4000-8000-000000000001"
checkout_id="40000000-0000-4000-8000-000000000002"
payment_attempt_id="40000000-0000-4000-8000-000000000003"
psql_exec -c "
  INSERT INTO music_purchase_order(id,buyer_party_id,release_version_id,availability_rule_id,state,gross_minor,net_minor,currency,idempotency_key)
    VALUES ('$purchase_order_id',$outsider_id,'$version_id','$availability_rule_id','awaiting_payment',250,250,'USD','music-purchase-1');
  INSERT INTO commerce_checkout_session(id,domain_type,domain_order_id,status,environment,currency,subtotal_minor,total_minor,customer_email,lookup_token_hash,idempotency_key,expires_at)
    VALUES ('$checkout_id','music_download','$purchase_order_id','awaiting_payment','sandbox','USD',250,250,'buyer@example.test','music-purchase-lookup-1','music-purchase-1',NOW()+interval '1 hour');
  UPDATE music_purchase_order SET checkout_id='$checkout_id' WHERE id='$purchase_order_id';
" >/dev/null
if psql_exec -c "UPDATE commerce_checkout_session SET status='paid',paid_minor=250,paid_at=NOW() WHERE id='$checkout_id';" >/dev/null 2>&1; then
  echo "Music checkout became paid without verified provider evidence" >&2
  exit 1
fi
psql_exec -c "
  INSERT INTO commerce_payment_attempt(id,checkout_id,provider,environment,operation,status,amount_minor,currency,merchant_account_ref,idempotency_key)
    VALUES ('$payment_attempt_id','$checkout_id','datafast','sandbox','capture','succeeded',250,'USD','synthetic-merchant','music-payment-capture-1');
  INSERT INTO commerce_provider_binding(payment_attempt_id,provider,environment,merchant_account_ref,resource_type,provider_resource_id,provider_resource_path,merchant_reference,amount_minor,currency)
    VALUES ('$payment_attempt_id','datafast','sandbox','synthetic-merchant','payment','synthetic-payment-1','/v1/checkouts/synthetic-checkout/payment','$purchase_order_id',250,'USD');
  UPDATE commerce_checkout_session SET status='paid',paid_minor=250,paid_at=NOW() WHERE id='$checkout_id';
" >/dev/null
test "$(psql_exec -Atc "SELECT state FROM music_purchase_order WHERE id='$purchase_order_id';")" = "paid"
test "$(psql_exec -Atc "SELECT count(*) FROM music_entitlement WHERE purchase_order_id='$purchase_order_id' AND status='active';")" = "1"
entitlement_id=$(psql_exec -Atc "SELECT id FROM music_entitlement WHERE purchase_order_id='$purchase_order_id';")
psql_exec -c "INSERT INTO music_download_event(entitlement_id,request_id,ip_hash,user_agent_hash,completed_at,byte_count) VALUES ('$entitlement_id','40000000-0000-4000-8000-000000000004','ip-hash','agent-hash',NOW(),65432);" >/dev/null
psql_exec -c "UPDATE commerce_checkout_session SET status='paid',updated_at=NOW() WHERE id='$checkout_id';" >/dev/null
test "$(psql_exec -Atc "SELECT count(*) FROM music_entitlement WHERE purchase_order_id='$purchase_order_id';")" = "1"
psql_exec -c "UPDATE commerce_checkout_session SET status='refunded',refunded_minor=250,updated_at=NOW() WHERE id='$checkout_id';" >/dev/null
test "$(psql_exec -Atc "SELECT state FROM music_purchase_order WHERE id='$purchase_order_id';")" = "refunded"
test "$(psql_exec -Atc "SELECT count(*) FROM music_entitlement WHERE purchase_order_id='$purchase_order_id' AND status='refunded' AND revoked_at IS NOT NULL;")" = "1"

upload_id=$(psql_exec -Atc "INSERT INTO music_upload_session(release_version_id,recording_id,asset_role,provider,bucket_name,quarantine_object_key,original_filename,expected_media_type,expected_size,expected_sha256,idempotency_key,part_size_bytes,expires_at,created_by) VALUES ('$version_id','$recording_id','master_audio','s3_compatible','music-quarantine','uploads/ee/00000000-0000-0000-0000-000000000003','master.wav','audio/wav',10485760,'eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee','resume-1',5242880,NOW()+interval '1 hour',$member_id) RETURNING id;")
psql_exec -c "INSERT INTO music_upload_part(upload_session_id,part_number,byte_size,etag,sha256) VALUES ('$upload_id',1,5242880,'etag-1','ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff') ON CONFLICT (upload_session_id,part_number) DO UPDATE SET etag=EXCLUDED.etag,sha256=EXCLUDED.sha256; INSERT INTO music_upload_part(upload_session_id,part_number,byte_size,etag,sha256) VALUES ('$upload_id',1,5242880,'etag-1','ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff') ON CONFLICT (upload_session_id,part_number) DO UPDATE SET etag=EXCLUDED.etag,sha256=EXCLUDED.sha256;" >/dev/null
test "$(psql_exec -Atc "SELECT count(*) FROM music_upload_part WHERE upload_session_id='$upload_id';")" = "1"

event_id="10000000-0000-0000-0000-000000000001"
eligible_event_id="10000000-0000-0000-0000-000000000003"
collision_event_id="10000000-0000-0000-0000-000000000004"
session_id="10000000-0000-0000-0000-000000000002"
test "$(psql_exec -Atc "SELECT music_record_playback_event('$event_id','$session_id',1,$outsider_id,NULL,'$version_id','$recording_id','progress',20000,20000,'high','EC',NOW(),'{\"source\":\"migration-test\"}');")" = "inserted"
test "$(psql_exec -Atc "SELECT music_record_playback_event('$event_id','$session_id',1,$outsider_id,NULL,'$version_id','$recording_id','progress',20000,20000,'high','EC',NOW(),'{\"source\":\"retry\"}');")" = "duplicate"
test "$(psql_exec -Atc "SELECT music_record_playback_event('$eligible_event_id','$session_id',2,$outsider_id,NULL,'$version_id','$recording_id','progress',30000,10000,'high','EC',NOW(),'{\"source\":\"migration-test\"}');")" = "inserted"
test "$(psql_exec -Atc "SELECT count(*) FROM music_playback_event WHERE event_id='$event_id';")" = "1"
test "$(psql_exec -Atc "SELECT count(*) FROM music_playback_event WHERE session_id='$session_id' AND eligible_play;")" = "1"
if psql_exec -c "SELECT music_record_playback_event('$collision_event_id','$session_id',2,$outsider_id,NULL,'$version_id','$recording_id','pause',30000,0,'high','EC',NOW(),'{}');" >/dev/null 2>&1; then
  echo "Playback session sequence collision was accepted" >&2
  exit 1
fi
test "$(psql_exec -Atc "SELECT music_rebuild_daily_metrics(CURRENT_DATE);")" = "2"
test "$(psql_exec -Atc "SELECT eligible_plays || ':' || purchases || ':' || downloads FROM music_daily_metric WHERE metric_date=CURRENT_DATE AND release_version_id='$version_id' AND recording_id='$recording_id' AND territory_code='EC';")" = "1:0:0"
test "$(psql_exec -Atc "SELECT purchases || ':' || downloads FROM music_daily_metric WHERE metric_date=CURRENT_DATE AND release_version_id='$version_id' AND recording_id='$recording_id' AND territory_code='ZZ';")" = "1:1"
test "$(psql_exec -Atc "SELECT music_rebuild_daily_metrics(CURRENT_DATE);")" = "2"
test "$(psql_exec -Atc "SELECT sum(eligible_plays) || ':' || sum(purchases) || ':' || sum(downloads) FROM music_daily_metric WHERE metric_date=CURRENT_DATE AND release_version_id='$version_id';")" = "1:1:1"

sender_registry_id=$(psql_exec -Atc "INSERT INTO music_ddex_party_registry(party_name,dpid,party_role,verification_authority,verification_evidence,verified_by,verified_at) VALUES ('TDF test sender','PADPIDA000TDF001','sender','Synthetic migration fixture','{\"fixture\":true}',$outsider_id,NOW()) RETURNING id;")
recipient_registry_id=$(psql_exec -Atc "INSERT INTO music_ddex_party_registry(party_name,dpid,party_role,verification_authority,verification_evidence,verified_by,verified_at) VALUES ('DSP test recipient','PADPIDA000DSP001','recipient','Synthetic migration fixture','{\"fixture\":true}',$outsider_id,NOW()) RETURNING id;")
if psql_exec -c "INSERT INTO music_ddex_export(release_version_id,operation,standard,ern_version,release_profile,release_profile_version,business_profile_version,avs_version,structural_dictionary_version,choreography,choreography_version,sender_registry_id,recipient_registry_id,sender_dpid,recipient_dpid,message_id,canonical_snapshot_sha256,idempotency_key,generated_by) VALUES ('$version_id','new_release','ERN','4.3.2','Audio','2.3.1','ern/432','011','DD-ERN-432','Cloud Storage','1.8.1','$sender_registry_id','$recipient_registry_id','PADPIDA000TDF001','PADPIDA000DSP001','MSG-INVALID-BP','cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc','invalid-business-profile',$outsider_id);" >/dev/null 2>&1; then
  echo "ERN 4 export incorrectly accepted a Business Profile version" >&2
  exit 1
fi

replacement_id=$(psql_exec -Atc "INSERT INTO music_release_version(release_id,version_number,title,display_artist,label_name,primary_genre_id,explicit_content,recording_copyright_text,work_copyright_text,immutable_snapshot,snapshot_sha256,correction_of_version_id,replaces_version_id,created_by) VALUES ('$release_id',2,'Migration Single (corrected)','Verified Migration Artist','Migration Records','ea0ee25a-326a-4174-9682-72d063caf6f9','not_explicit','℗ 2026 Migration Artist','© 2026 Migration Artist','{\"title\":\"Migration Single (corrected)\",\"version\":2}','dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd','$version_id','$version_id',$member_id) RETURNING id;")
test "$(psql_exec -Atc "SELECT count(*) FROM music_release_version_party WHERE release_version_id='$replacement_id';")" = "1"
psql_exec -c "INSERT INTO music_release_version_party VALUES ('$replacement_id','$pending_party_id');" >/dev/null
if apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-15_music_version_parties_rollback.sql" 2>/dev/null; then
  echo "Rollback lost an uncredited collaborator" >&2
  exit 1
fi
replacement_master_rights_id=$(psql_exec -Atc "INSERT INTO music_rights_declaration(release_version_id,recording_id,rights_scope,authority_basis,territories,starts_on,declared_by) VALUES ('$replacement_id','$recording_id','master','owned',ARRAY['Worldwide'],'2026-09-11',$member_id) RETURNING id;")
replacement_composition_rights_id=$(psql_exec -Atc "INSERT INTO music_rights_declaration(release_version_id,recording_id,rights_scope,authority_basis,territories,starts_on,declared_by) VALUES ('$replacement_id','$recording_id','composition','licensed',ARRAY['Worldwide'],'2026-09-11',$member_id) RETURNING id;")
replacement_master_asset_id=$(psql_exec -Atc "INSERT INTO music_asset(release_version_id,recording_id,asset_role,storage_provider,storage_class,bucket_name,object_key,original_filename,media_type,byte_size,sha256,processing_state,immutable,created_by,ready_at) VALUES ('$replacement_id','$recording_id','master_audio','local_private','standard','music-private','masters/33/00000000-0000-0000-0000-000000000005/master.wav','master.wav','audio/wav',123456,'3333333333333333333333333333333333333333333333333333333333333333','ready',TRUE,$member_id,NOW()) RETURNING id;")
replacement_stream_asset_id=$(psql_exec -Atc "INSERT INTO music_asset(release_version_id,recording_id,parent_asset_id,asset_role,storage_provider,storage_class,bucket_name,object_key,media_type,byte_size,sha256,processing_state,immutable,created_by,ready_at) VALUES ('$replacement_id','$recording_id','$replacement_master_asset_id','stream_audio','local_private','standard','music-private','derivatives/44/00000000-0000-0000-0000-000000000006/stream.m4a','audio/mp4',65432,'4444444444444444444444444444444444444444444444444444444444444444','ready',TRUE,$member_id,NOW()) RETURNING id;")
replacement_cover_original_asset_id=$(psql_exec -Atc "INSERT INTO music_asset(release_version_id,asset_role,storage_provider,storage_class,bucket_name,object_key,original_filename,media_type,byte_size,sha256,processing_state,immutable,created_by,ready_at) VALUES ('$replacement_id','cover_original','local_private','standard','music-private','art/55/00000000-0000-0000-0000-000000000007/cover.png','cover.png','image/png',23456,'5555555555555555555555555555555555555555555555555555555555555555','ready',TRUE,$member_id,NOW()) RETURNING id;")
replacement_cover_display_asset_id=$(psql_exec -Atc "INSERT INTO music_asset(release_version_id,parent_asset_id,asset_role,storage_provider,storage_class,bucket_name,object_key,media_type,byte_size,sha256,processing_state,immutable,created_by,ready_at) VALUES ('$replacement_id','$replacement_cover_original_asset_id','cover_display','local_private','standard','music-private','art/66/00000000-0000-0000-0000-000000000008/cover.webp','image/webp',12345,'6666666666666666666666666666666666666666666666666666666666666666','ready',TRUE,$member_id,NOW()) RETURNING id;")
ddex_cover_asset_id=$(psql_exec -Atc "INSERT INTO music_asset(release_version_id,parent_asset_id,asset_role,storage_provider,storage_class,bucket_name,object_key,media_type,byte_size,sha256,processing_state,immutable,created_by,ready_at) VALUES ('$replacement_id','$replacement_cover_original_asset_id','cover_display','local_private','standard','music-private','art/77/00000000-0000-0000-0000-000000000009/cover.jpg','image/jpeg',12345,'7777777777777777777777777777777777777777777777777777777777777777','ready',TRUE,$member_id,NOW()) RETURNING id;")
psql_exec -c "
  INSERT INTO music_release_track(release_version_id,recording_id,track_number,display_artist)
    VALUES ('$replacement_id','$recording_id',1,'Verified Migration Artist');
  INSERT INTO music_credit(release_version_id,recording_id,music_party_id,credit_role)
    VALUES ('$replacement_id','$recording_id','$credit_party_id','main_artist');
  INSERT INTO music_credit(release_version_id,recording_id,music_party_id,credit_role)
    VALUES ('$replacement_id','$recording_id','$credit_party_id','composer');
  INSERT INTO music_terms_acceptance(release_version_id,terms_kind,terms_version,accepted_by,evidence)
    VALUES ('$replacement_id','publication_authority','publication-v1',$member_id,'{\"source\":\"migration-test\"}');
  INSERT INTO music_rights_split(declaration_id,rights_holder_id,basis_points,territories,starts_on)
    VALUES ('$replacement_master_rights_id','$credit_party_id',10000,ARRAY['Worldwide'],'2026-09-11');
  INSERT INTO music_rights_split(declaration_id,rights_holder_id,basis_points,territories,starts_on)
    VALUES ('$replacement_composition_rights_id','$credit_party_id',10000,ARRAY['Worldwide'],'2026-09-11');
  INSERT INTO music_availability_rule(release_version_id,territories,listening_policy,download_policy,purchasable)
    VALUES ('$replacement_id',ARRAY['Worldwide'],'full','none',FALSE);
  INSERT INTO music_identifier(release_version_id,identifier_type,identifier_value,provenance,verification_status)
    VALUES ('$replacement_id','upc','036000291452','provided','syntax_valid');
  SELECT * FROM music_refresh_validation_flags('$replacement_id');
  UPDATE music_release_version SET state='ready_for_review' WHERE id='$replacement_id';
  UPDATE music_release_version SET state='in_review' WHERE id='$replacement_id';
  UPDATE music_release_version SET state='approved',approved_by=$outsider_id,approved_at=NOW() WHERE id='$replacement_id';
  UPDATE music_release_version SET state='scheduled',scheduled_by=$outsider_id,release_at_utc=NOW() + interval '1 second',release_timezone='America/Guayaquil',embargo_until_utc=NOW() + interval '1 second' WHERE id='$replacement_id';
" >/dev/null
sleep 2
test "$(psql_exec -Atc "SELECT count(*) FROM music_publish_due(10);")" = "1"
test "$(psql_exec -Atc "SELECT state FROM music_release_version WHERE id='$version_id';")" = "withdrawn"
test "$(psql_exec -Atc "SELECT state FROM music_release_version WHERE id='$replacement_id';")" = "published"
test "$(psql_exec -Atc "SELECT published_version_id='$replacement_id' FROM music_release WHERE id='$release_id';")" = "t"
test "$(psql_exec -Atc "SELECT count(*) FROM music_public_release WHERE id='$release_id' AND release_version_id='$replacement_id';")" = "1"
test "$(psql_exec -Atc "SELECT count(*) FROM music_check_ddex_export('$replacement_id');")" = "0"

infringement_id=$(psql_exec -Atc "INSERT INTO music_infringement_report(release_id,reporter_party_id,reason_code,description,idempotency_key) VALUES ('$release_id',$outsider_id,'copyright','Synthetic licensed test report','report-migration-1') RETURNING id;")
test "$(psql_exec -Atc "SELECT count(*) FROM notification WHERE recipient_party_id=$artist_id AND notif_type='music_release_infringement_reported' AND target_type='music_release' AND target_id=$artist_id;")" = "1"
psql_exec -c "UPDATE music_infringement_report SET status='triage',assigned_to=$outsider_id,resolution_notes='Triage started' WHERE id='$infringement_id'; UPDATE music_infringement_report SET status='investigating',resolution_notes='Evidence checked' WHERE id='$infringement_id'; UPDATE music_infringement_report SET status='actioned',resolution_notes='Synthetic suspension decision' WHERE id='$infringement_id'; UPDATE music_release_version SET state='suspended' WHERE id='$replacement_id';" >/dev/null
test "$(psql_exec -Atc "SELECT status || ':' || (resolved_at IS NOT NULL) FROM music_infringement_report WHERE id='$infringement_id';")" = "actioned:true"
test "$(psql_exec -Atc "SELECT count(*) FROM music_public_release WHERE id='$release_id';")" = "0"
if psql_exec -c "UPDATE music_infringement_report SET status='triage' WHERE id='$infringement_id';" >/dev/null 2>&1; then
  echo "Resolved infringement report accepted an invalid backward transition" >&2
  exit 1
fi

psql_exec -c "UPDATE music_release_version SET state='takedown_scheduled',takedown_at_utc=NOW() + interval '1 second',takedown_timezone='America/Guayaquil' WHERE id='$replacement_id';" >/dev/null
sleep 2
test "$(psql_exec -Atc "SELECT count(*) FROM music_withdraw_due(10);")" = "1"
test "$(psql_exec -Atc "SELECT count(*) FROM music_withdraw_due(10);")" = "0"
test "$(psql_exec -Atc "SELECT state FROM music_release_version WHERE id='$replacement_id';")" = "withdrawn"
test "$(psql_exec -Atc "SELECT published_version_id IS NULL AND withdrawn_at IS NOT NULL FROM music_release WHERE id='$release_id';")" = "t"
test "$(psql_exec -Atc "SELECT count(*) FROM music_public_release WHERE id='$release_id';")" = "0"

correction_id=$(psql_exec -Atc "SELECT music_create_release_correction('$release_id','$replacement_id',$member_id);")
test "$(psql_exec -Atc "SELECT version_number || ':' || state || ':' || (correction_of_version_id='$replacement_id') || ':' || (replaces_version_id='$replacement_id') FROM music_release_version WHERE id='$correction_id';")" = "3:draft:true:true"
test "$(psql_exec -Atc "SELECT count(*) FROM music_release_track old_track JOIN music_release_track new_track ON new_track.release_version_id='$correction_id' AND old_track.release_version_id='$replacement_id' WHERE old_track.recording_id<>new_track.recording_id;")" = "1"
test "$(psql_exec -Atc "SELECT count(*) FROM music_identifier identifier JOIN music_release_track track ON track.recording_id=identifier.recording_id WHERE track.release_version_id='$correction_id' AND identifier.identifier_type='isrc' AND identifier.identifier_value='USRC17607839';")" = "1"
test "$(psql_exec -Atc "SELECT count(*) FROM music_asset source JOIN music_asset cloned ON cloned.release_version_id='$correction_id' AND cloned.object_key=source.object_key AND cloned.sha256=source.sha256 WHERE source.release_version_id='$replacement_id';")" = "5"
test "$(psql_exec -Atc "SELECT count(*) FROM music_asset child JOIN music_asset parent ON parent.id=child.parent_asset_id AND parent.release_version_id='$correction_id' WHERE child.release_version_id='$correction_id' AND child.parent_asset_id IS NOT NULL;")" = "3"
test "$(psql_exec -Atc "SELECT count(*) FROM music_terms_acceptance WHERE release_version_id='$correction_id';")" = "0"
test "$(psql_exec -Atc "SELECT count(*) FROM music_release_version_party WHERE release_version_id='$correction_id';")" = "2"
psql_exec -c "DELETE FROM music_release_version_party WHERE release_version_id='$correction_id' AND music_party_id='$pending_party_id';" >/dev/null
test "$(psql_exec -Atc "SELECT count(*) FROM music_release_version_party WHERE release_version_id='$replacement_id' AND music_party_id='$pending_party_id';")" = "1"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-15_music_version_parties.sql"
test "$(psql_exec -Atc "SELECT count(*) FROM music_release_version_party WHERE release_version_id='$correction_id';")" = "1"

# Versioned details: a legacy observation is explicit, never historical proof.
prior_snapshot=$(psql_exec -Atc "SELECT md5(immutable_snapshot::text) FROM music_release_version WHERE id='$replacement_id';")
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-15_music_party_details.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-15_music_party_details.sql"
test "$(psql_exec -Atc "SELECT details_source FROM music_release_version_party WHERE release_version_id='$replacement_id' AND music_party_id='$credit_party_id';")" = "legacy_observed"
test "$(psql_exec -Atc "SELECT count(*) FROM music_check_submission('$correction_id') WHERE error_code='party_details_confirmation_required';")" = "1"
test "$(psql_exec -Atc "SELECT count(*) FROM music_check_ddex_export('$replacement_id') WHERE error_code='party_snapshot_missing';")" = "1"
test "$(psql_exec -Atc "SELECT md5(immutable_snapshot::text) FROM music_release_version WHERE id='$replacement_id';")" = "$prior_snapshot"
# Derived legacy observations can be rolled back before any new evidence.
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-15_music_party_details_rollback.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-15_music_party_details.sql"
psql_exec -c "UPDATE music_release_version_party SET party_details=jsonb_set(party_details,'{displayName}','\"Correction-only name\"'),details_source='user_provided' WHERE release_version_id='$correction_id';" >/dev/null
test "$(psql_exec -Atc "SELECT count(*) FROM music_check_submission('$correction_id') WHERE error_code='party_details_confirmation_required';")" = "0"
test "$(psql_exec -Atc "SELECT party_details->>'displayName' FROM music_release_version_party WHERE release_version_id='$replacement_id' AND music_party_id='$credit_party_id';")" = "Verified Migration Artist"
test "$(psql_exec -Atc "SELECT display_name FROM music_party WHERE id='$credit_party_id';")" = "Verified Migration Artist"
if psql_exec -c "UPDATE music_release_version_party SET party_details=jsonb_set(party_details,'{displayName}','\"Forbidden\"') WHERE release_version_id='$replacement_id';" >/dev/null 2>&1; then
  echo "Approved party details accepted mutation" >&2; exit 1
fi
if apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-15_music_party_details_rollback.sql" 2>/dev/null; then
  echo "Details rollback destroyed versioned evidence" >&2; exit 1
fi
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-15_music_party_details.sql"
test "$(psql_exec -Atc "SELECT party_details->>'displayName' FROM music_release_version_party WHERE release_version_id='$correction_id';")" = "Correction-only name"
psql_exec -c "UPDATE music_party SET display_name='Directory renamed independently' WHERE id='$credit_party_id';" >/dev/null
test "$(psql_exec -Atc "SELECT party->>'displayName' FROM jsonb_array_elements(music_version_parties('$replacement_id')) party WHERE party->>'id'='$credit_party_id';")" = "Verified Migration Artist"
test "$(psql_exec -Atc "SELECT party->>'displayName' FROM jsonb_array_elements(music_version_parties('$correction_id')) party WHERE party->>'id'='$credit_party_id';")" = "Correction-only name"
for invalid_details in "NULL" "'{}'::jsonb" "'[]'::jsonb"; do
  if psql_exec -c "UPDATE music_release_version_party SET party_details=$invalid_details WHERE release_version_id='$correction_id';" >/dev/null 2>&1; then
    echo "Invalid provided party snapshot was accepted" >&2; exit 1
  fi
done
# Synthetic evidence only: preserving an unchanged claim never verifies a new value.
test "$(psql_exec -Atc "SELECT music_merge_party_identifiers('[{\"identifier_type\":\"proprietary\",\"identifier_value\":\"synthetic:one\",\"verification_status\":\"authority_verified\",\"verification_authority\":\"Synthetic fixture authority\"}]','[{\"identifier_type\":\"proprietary\",\"identifier_value\":\"synthetic:one\",\"verification_status\":\"unvalidated\"}]')->0->>'verification_status';")" = "authority_verified"
test "$(psql_exec -Atc "SELECT music_merge_party_identifiers('[{\"identifier_type\":\"proprietary\",\"identifier_value\":\"synthetic:one\",\"verification_status\":\"authority_verified\"}]','[{\"identifier_type\":\"proprietary\",\"identifier_value\":\"synthetic:two\",\"verification_status\":\"unvalidated\"}]')->0->>'verification_status';")" = "unvalidated"
test "$(psql_exec -Atc "SELECT music_merge_party_identifiers('[{\"identifier_type\":\"proprietary\",\"identifier_value\":\"synthetic:one\"}]','[]')='[]'::jsonb;")" = "t"
details_copy_id=$(psql_exec -Atc "SELECT music_create_release_correction('$release_id','$replacement_id',$member_id);")
test "$(psql_exec -Atc "SELECT count(*) FROM music_release_version_party copied JOIN music_release_version_party source ON source.release_version_id='$replacement_id' AND source.music_party_id=copied.music_party_id AND source.party_details=copied.party_details AND source.details_source=copied.details_source WHERE copied.release_version_id='$details_copy_id';")" = "2"

check_correction_graph() {
  docker exec -i -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_MUSIC_CONTAINER" \
    psql -q -v ON_ERROR_STOP=1 -v "source_id=$replacement_id" -v "actor_id=$member_id" \
    -v "expect_fixed=$1" -U postgres -d "$TDF_MUSIC_DB" \
    < "$TDF_MUSIC_ROOT/tdf-hq/test/sql/music_correction_asset_graph.sql" >/dev/null
}
# Prove the regression fails with the original function, then test repeated
# application and a populated rollback/reapplication without changing old rows.
correction_function_before=$(psql_exec -Atc "SELECT md5(pg_get_functiondef('music_create_release_correction(uuid,uuid,bigint)'::regprocedure));")
check_correction_graph false
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_correction_asset_graph.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_correction_asset_graph.sql"
check_correction_graph true
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_correction_asset_graph_rollback.sql"
test "$(psql_exec -Atc "SELECT md5(pg_get_functiondef('music_create_release_correction(uuid,uuid,bigint)'::regprocedure));")" = "$correction_function_before"
check_correction_graph false
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_correction_asset_graph.sql"
check_correction_graph true
test "$(psql_exec -Atc "SELECT md5(immutable_snapshot::text) FROM music_release_version WHERE id='$replacement_id';")" = "$prior_snapshot"

check_correction_concurrency() {
  node "$TDF_MUSIC_ROOT/scripts/test-music-correction-concurrency.mjs" \
    "$TDF_MUSIC_CONTAINER" "$TDF_MUSIC_DB" "$release_id" "$version_id" \
    "$replacement_id" "$member_id" "$1"
}
concurrent_function_before=$(psql_exec -Atc "SELECT md5(pg_get_functiondef('music_create_release_correction(uuid,uuid,bigint)'::regprocedure));")
check_correction_concurrency legacy
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_correction_concurrency.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_correction_concurrency.sql"
check_correction_concurrency fixed
check_correction_graph true
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_correction_concurrency_rollback.sql"
test "$(psql_exec -Atc "SELECT md5(pg_get_functiondef('music_create_release_correction(uuid,uuid,bigint)'::regprocedure));")" = "$concurrent_function_before"
check_correction_concurrency legacy
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_correction_concurrency.sql"
check_correction_concurrency fixed

resource_gate_before=$(psql_exec -Atc "SELECT md5(pg_get_functiondef('music_check_submission(uuid)'::regprocedure));")
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_resource_graph_validation.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_resource_graph_validation.sql"
docker exec -i -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_MUSIC_CONTAINER" \
  psql -q -v ON_ERROR_STOP=1 -v "source_id=$replacement_id" -v "actor_id=$member_id" \
  -U postgres -d "$TDF_MUSIC_DB" \
  < "$TDF_MUSIC_ROOT/tdf-hq/test/sql/music_resource_graph_validation.sql" >/dev/null
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_resource_graph_validation_rollback.sql"
test "$(psql_exec -Atc "SELECT md5(pg_get_functiondef('music_check_submission(uuid)'::regprocedure));")" = "$resource_gate_before"
test "$(psql_exec -Atc "SELECT to_regclass('music_resource_graph_sanitation_queue') IS NULL;")" = "t"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_resource_graph_validation.sql"
test "$(psql_exec -Atc "SELECT md5(immutable_snapshot::text) FROM music_release_version WHERE id='$replacement_id';")" = "$prior_snapshot"

apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_ddex_operations.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_ddex_operations.sql"
docker exec -i "$TDF_MUSIC_CONTAINER" psql -q -v ON_ERROR_STOP=1 \
  -v "source_id=$replacement_id" -v "actor_id=$member_id" -U postgres -d "$TDF_MUSIC_DB" \
  < "$TDF_MUSIC_ROOT/tdf-hq/test/sql/music_ddex_operations.sql" >/dev/null
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_ddex_operations_rollback.sql"
test "$(psql_exec -Atc "SELECT to_regprocedure('music_check_ddex_operation(uuid,uuid,uuid,text)') IS NULL;")" = "t"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_ddex_operations.sql"
docker exec -i "$TDF_MUSIC_CONTAINER" psql -q -v ON_ERROR_STOP=1 \
  -v "source_id=$replacement_id" -v "actor_id=$member_id" -U postgres -d "$TDF_MUSIC_DB" \
  < "$TDF_MUSIC_ROOT/tdf-hq/test/sql/music_ddex_operations.sql" >/dev/null

playback_before=$(psql_exec -Atc "SELECT md5(pg_get_functiondef('music_record_playback_event(uuid,uuid,integer,bigint,text,uuid,uuid,text,bigint,bigint,text,text,timestamp with time zone,jsonb)'::regprocedure));")
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_playback_identity.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_playback_identity.sql"
docker exec -i "$TDF_MUSIC_CONTAINER" psql -q -v ON_ERROR_STOP=1 \
  -v "version_id=$version_id" -v "recording_id=$recording_id" \
  -v "actor_id=$member_id" -v "other_actor_id=$outsider_id" -U postgres -d "$TDF_MUSIC_DB" \
  < "$TDF_MUSIC_ROOT/tdf-hq/test/sql/music_playback_identity.sql" >/dev/null
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_playback_identity_rollback.sql"
test "$(psql_exec -Atc "SELECT md5(pg_get_functiondef('music_record_playback_event(uuid,uuid,integer,bigint,text,uuid,uuid,text,bigint,bigint,text,text,timestamp with time zone,jsonb)'::regprocedure));")" = "$playback_before"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_playback_identity.sql"

if apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-11_music_release_platform_rollback.sql" 2>/dev/null; then
  echo "Rollback removed a populated music release platform" >&2
  exit 1
fi

docker exec "$TDF_MUSIC_CONTAINER" psql -q -v ON_ERROR_STOP=1 -U postgres -d postgres \
  -c "DROP DATABASE $TDF_MUSIC_DB WITH (FORCE);" \
  -c "CREATE DATABASE $TDF_MUSIC_DB;" >/dev/null
apply_dependencies
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-11_music_release_platform.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_playback_identity.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_playback_identity_rollback.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-15_music_version_parties.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-15_music_party_details.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_correction_asset_graph.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_correction_concurrency.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_resource_graph_validation.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_ddex_operations.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_ddex_operations_rollback.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_resource_graph_validation_rollback.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_correction_concurrency_rollback.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-16_music_correction_asset_graph_rollback.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-15_music_party_details_rollback.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-15_music_version_parties_rollback.sql"
apply_file "$TDF_MUSIC_ROOT/tdf-hq/sql/2026-09-11_music_release_platform_rollback.sql"
test "$(psql_exec -Atc "SELECT to_regclass('public.music_release') IS NULL;")" = "t"

echo "Music release migration passed permissions, gates, immutable approved metadata and masters, versioned correction cloning, gapless playlist reorder/delete, infringement lifecycle/suspension visibility, verified-payment entitlement/refund idempotency, resumable-part idempotency, publish/replacement/takedown idempotency, embargo/public visibility, analytics deduplication and rebuilds, DDEX compatibility, and guarded rollback checks."
