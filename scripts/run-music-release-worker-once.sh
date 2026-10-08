#!/usr/bin/env bash
set -euo pipefail
# Isolate long-running child commands into process groups so TERM/lease loss
# can stop their subprocesses too (FFmpeg, renderer, curl), not only the shell.
set -m
# Keep optional timing separate from the error evidence file and stdout/SQL.
# Only fixed stage names and numeric durations/statuses are logged, never args.
exec 4>&2
timing() {
  [ "${MUSIC_WORKER_DIAGNOSTICS:-false}" = true ] || return 0
  printf '{"event":"music_worker_timing","stage":"%s","phase":"%s","elapsedSeconds":%s,"status":%s}\n' "$1" "$2" "$3" "$4" >&4
}
active_child=''
run_child() {
  local started=$SECONDS result=0 stage
  case "${1##*/}" in
    perl) stage=storage_upload ;;
    curl) stage=storage_request ;;
    process-music-release-audio.sh) stage=audio_pipeline ;;
    process-music-release-artwork.sh) stage=artwork_pipeline ;;
    *) stage=external ;;
  esac
  timing "$stage" start 0 0
  "$@" &
  active_child=$!
  wait "$active_child" || result=$?
  timing "$stage" finish "$((SECONDS-started))" "$result"
  [ "$result" -eq 0 ] || return "$result"
  active_child=''
}

for variable in DATABASE_URL MUSIC_S3_ENDPOINT MUSIC_S3_ACCESS_KEY_ID MUSIC_S3_SECRET_ACCESS_KEY MUSIC_S3_REGION MUSIC_S3_MASTER_BUCKET MUSIC_S3_DERIVATIVE_BUCKET MUSIC_S3_DDEX_BUCKET; do
  eval "value=\${$variable:-}"
  if [ -z "$value" ]; then echo "$variable is required" >&2; exit 2; fi
done
for command_name in psql curl jq shasum ffmpeg ffprobe xmllint zip unzip perl; do
  command -v "$command_name" >/dev/null 2>&1 || { echo "Required command is unavailable: $command_name" >&2; exit 2; }
done

worker_id=${MUSIC_WORKER_ID:-"music-worker-$$"}
repository_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
audio_pipeline=${MUSIC_AUDIO_PIPELINE:-"$repository_root/scripts/process-music-release-audio.sh"}
artwork_pipeline=${MUSIC_ARTWORK_PIPELINE:-"$repository_root/scripts/process-music-release-artwork.sh"}
ddex_package_builder=${MUSIC_DDEX_PACKAGE_BUILDER:-"$repository_root/scripts/build-ddex-ern432-package.sh"}
ddex_renderer=${MUSIC_DDEX_RENDER_BIN:-tdf-ddex-render}
endpoint=${MUSIC_S3_ENDPOINT%/}
lease_seconds=${MUSIC_WORKER_LEASE_SECONDS:-900}
heartbeat_seconds=${MUSIC_WORKER_HEARTBEAT_SECONDS:-30}
transfer_timeout_seconds=${MUSIC_WORKER_TRANSFER_TIMEOUT_SECONDS:-3600}
for interval in "$lease_seconds" "$heartbeat_seconds" "$transfer_timeout_seconds"; do
  case "$interval" in ''|*[!0-9]*|0*) echo "Worker lease intervals must be positive decimal integers" >&2; exit 2 ;; esac
  [ "${#interval}" -le 5 ] || exit 2
done
[ "$lease_seconds" -ge 3 ] && [ "$lease_seconds" -le 86400 ] &&
  [ "$transfer_timeout_seconds" -le 86400 ] &&
  [ "$((heartbeat_seconds * 3))" -le "$lease_seconds" ] || {
    echo "Lease must be 3..86400 seconds and at least three heartbeat intervals" >&2; exit 2;
  }
lease_active=false

# The check and the caller's writes share one transaction/row lock. A worker
# cannot renew an expired lease, even if nobody has reclaimed it yet. Use the
# attempt as a fencing token because worker names may be reused across restarts.
lease_guard_sql='SET LOCAL lock_timeout = '\''5s'\'';
SET LOCAL statement_timeout = '\''30s'\'';
CREATE OR REPLACE FUNCTION pg_temp.music_worker_guard(job_uuid uuid, owner_name text, job_attempt integer, ttl integer, final_status text)
RETURNS void LANGUAGE plpgsql AS $$
BEGIN
  -- Serialize writes for sibling jobs before locking their individual rows.
  -- NO KEY UPDATE remains compatible with foreign-key KEY SHARE checks.
  PERFORM version.id FROM music_release_version version
    JOIN music_processing_job job ON job.release_version_id=version.id
    WHERE job.id=job_uuid FOR NO KEY UPDATE OF version;
  PERFORM id FROM music_processing_job WHERE id=job_uuid FOR UPDATE;
  IF final_status IN ('\''retry'\'','\''dead_letter'\'') THEN
    PERFORM id FROM music_processing_job WHERE id=job_uuid AND attempt_count=job_attempt
      AND status=final_status AND locked_by IS NULL;
    IF NOT FOUND THEN RAISE EXCEPTION '\''Music worker completion ownership lost'\''; END IF;
    RETURN;
  END IF;
  UPDATE music_processing_job SET locked_at=clock_timestamp(),updated_at=clock_timestamp()
    WHERE id=job_uuid AND status='\''running'\'' AND locked_by=owner_name
      AND attempt_count=job_attempt AND locked_at>clock_timestamp()-make_interval(secs=>ttl);
  IF NOT FOUND THEN RAISE EXCEPTION '\''Music worker lease lost or expired'\''; END IF;
END $$;
SELECT pg_temp.music_worker_guard(:'\''lease_job'\''::uuid, :'\''lease_owner'\'', :'\''lease_attempt'\''::integer, :'\''lease_ttl'\''::integer, :'\''lease_final_status'\'')
\g /dev/null
'

psql_db() {
  # -c sends SQL directly to PostgreSQL without psql variable interpolation.
  # All callers supply SQL last; keep values in -v and read SQL with -f instead.
  local sql="${!#}"
  local sql_flag="${@: -2:1}"
  [ "$sql_flag" = -c ] || { echo "psql_db requires trailing -c SQL" >&2; return 2; }
  local -a options=("$DATABASE_URL" -XAtq -v ON_ERROR_STOP=1 "${@:1:$#-2}")
  if [ "$lease_active" = true ]; then
    [ ! -f "$working_dir/lease-lost" ] || { echo "Music worker lease lost" >&2; return 1; }
    options+=( -v lease_job="$job_id" -v lease_owner="$worker_id" -v lease_attempt="$attempt" -v lease_ttl="$lease_seconds" -v lease_final_status="${lease_final_status:-}" )
    sql="$lease_guard_sql$sql"
  fi
  local started=$SECONDS result=0
  timing sql start 0 0
  printf '%s\n' "$sql" | psql "${options[@]}" --single-transaction -f - || result=$?
  timing sql finish "$((SECONDS-started))" "$result"
  return "$result"
}

s3_get() {
  local bucket=$1 key=$2 destination=$3
  psql_db -c 'SELECT 1;' >/dev/null
  set -- --fail --silent --show-error --retry 3 --aws-sigv4 "aws:amz:${MUSIC_S3_REGION}:s3" --user "${MUSIC_S3_ACCESS_KEY_ID}:${MUSIC_S3_SECRET_ACCESS_KEY}"
  if [ -n "${MUSIC_S3_SESSION_TOKEN:-}" ]; then set -- "$@" -H "x-amz-security-token: ${MUSIC_S3_SESSION_TOKEN}"; fi
  run_child curl "$@" --connect-timeout 10 --max-time "$transfer_timeout_seconds" -o "$destination" "$endpoint/$bucket/$key"
}

s3_put() {
  local source_file=$1 bucket=$2 key=$3 media_type=$4
  psql_db -c 'SELECT 1;' >/dev/null
  run_child perl "$repository_root/scripts/music-s3-upload.pl" "$source_file" "$bucket" "$key" "$media_type"
}

s3_delete() {
  local bucket=$1 key=$2
  psql_db -c 'SELECT 1;' >/dev/null || return 1
  set -- --fail --silent --show-error --retry 2 --aws-sigv4 "aws:amz:${MUSIC_S3_REGION}:s3" --user "${MUSIC_S3_ACCESS_KEY_ID}:${MUSIC_S3_SECRET_ACCESS_KEY}"
  if [ -n "${MUSIC_S3_SESSION_TOKEN:-}" ]; then set -- "$@" -H "x-amz-security-token: ${MUSIC_S3_SESSION_TOKEN}"; fi
  run_child curl "$@" --connect-timeout 10 --max-time "$transfer_timeout_seconds" -X DELETE "$endpoint/$bucket/$key"
}

# Publication and withdrawal are transactionally idempotent and run even when
# the media queue is empty.
psql_db -c "SELECT count(*) FROM music_publish_due(100); SELECT count(*) FROM music_withdraw_due(100);" >/dev/null
psql_db -c 'SELECT music_queue_preview_jobs(100);' >/dev/null

job_json=$(psql_db -v worker_id="$worker_id" -v lease_seconds="$lease_seconds" -c "WITH candidate AS (SELECT id FROM music_processing_job WHERE ((status IN ('queued','retry') AND run_after<=NOW()) OR (status='running' AND locked_at<NOW()-make_interval(secs=>:'lease_seconds'::integer))) ORDER BY run_after,created_at,id FOR UPDATE SKIP LOCKED LIMIT 1), claimed AS (UPDATE music_processing_job job SET status='running',attempt_count=attempt_count+1,locked_at=NOW(),locked_by=:'worker_id',updated_at=NOW(),error_code=NULL,error_summary=NULL FROM candidate WHERE job.id=candidate.id RETURNING job.*) SELECT jsonb_build_object('id',job.id,'kind',job.job_kind,'versionId',job.release_version_id,'sourceAssetId',job.source_asset_id,'attempt',job.attempt_count,'maxAttempts',job.max_attempts,'output',job.output,'assetRole',asset.asset_role,'recordingId',asset.recording_id,'bucket',asset.bucket_name,'objectKey',asset.object_key,'mediaType',asset.media_type,'sha256',asset.sha256,'createdBy',asset.created_by) FROM claimed job LEFT JOIN music_asset asset ON asset.id=job.source_asset_id;")

if [ -z "$job_json" ]; then
  echo "No due music-release processing job"
  exit 0
fi

# Parse once before starting renewal, without eval or whitespace-delimited
# fields. PostgreSQL text cannot contain NUL; empty values/quotes remain data.
{
  IFS= read -r -d '' job_id
  IFS= read -r -d '' job_kind
  IFS= read -r -d '' version_id
  IFS= read -r -d '' source_asset_id
  IFS= read -r -d '' recording_id
  IFS= read -r -d '' source_bucket
  IFS= read -r -d '' source_key
  IFS= read -r -d '' source_media_type
  IFS= read -r -d '' expected_sha256
  IFS= read -r -d '' created_by
  IFS= read -r -d '' attempt
  IFS= read -r -d '' max_attempts
} < <(printf '%s' "$job_json" | jq -j '[.id,.kind,.versionId,
  (.sourceAssetId // ""),(.recordingId // ""),(.bucket // ""),(.objectKey // ""),
  (.mediaType // "application/octet-stream"),(.sha256 // ""),(.createdBy // ""),
  .attempt,.maxAttempts] | .[] | tostring + "\u0000"')

working_dir=$(mktemp -d "${TMPDIR:-/tmp}/tdf-music-worker.XXXXXX")
lease_active=true
cleanup() { rm -rf "$working_dir"; }

version_completion_sql="SELECT id FROM music_release_version WHERE id=:'version_id'::uuid FOR NO KEY UPDATE; SELECT * FROM music_refresh_validation_flags(:'version_id'::uuid); UPDATE music_release_version version SET state=CASE WHEN EXISTS(SELECT 1 FROM music_processing_job job WHERE job.release_version_id=version.id AND job.status IN ('failed','dead_letter')) OR EXISTS(SELECT 1 FROM music_check_submission(version.id)) THEN 'validation_failed' ELSE 'ready_for_review' END,updated_at=NOW() WHERE version.id=:'version_id'::uuid AND version.state='processing' AND NOT EXISTS(SELECT 1 FROM music_processing_job job WHERE job.release_version_id=version.id AND job.status IN ('queued','running','retry'));"

mark_succeeded() {
  output_json=$1
  stop_heartbeat
  psql_db -v job_id="$job_id" -v version_id="$version_id" -v output_json="$output_json" -c "UPDATE music_processing_job SET status='succeeded',locked_at=NULL,locked_by=NULL,error_code=NULL,error_summary=NULL,output=output || :'output_json'::jsonb,updated_at=NOW() WHERE id=:'job_id'::uuid AND status='running'; $version_completion_sql" >/dev/null
}

mark_failed() {
  error_code=$1 error_summary=$2
  if [ "$attempt" -ge "$max_attempts" ]; then next_status=dead_letter; else next_status=retry; fi
  delay_seconds=$((attempt * attempt * 30))
  export_failure_sql=''
  export_id=''
  if [ "$job_kind" = generate_ddex ]; then
    export_id=$(printf '%s' "$job_json" | jq -r '.output.export_id // empty')
    if [ -n "$export_id" ]; then
      export_failure_sql="UPDATE music_ddex_export SET status=CASE WHEN :'next_status'='dead_letter' THEN 'failed' ELSE 'validation_failed' END,validation_report=COALESCE(NULLIF(:'validation_report','')::jsonb,jsonb_build_object('valid',FALSE,'code',:'error_code','message',:'error_summary','checkedAt',NOW())) WHERE id=:'export_id'::uuid AND release_version_id=:'version_id'::uuid AND status<>'valid';"
    fi
  fi
  psql_db -v job_id="$job_id" -v version_id="$version_id" -v export_id="$export_id" -v next_status="$next_status" -v error_code="$error_code" -v validation_report="${ddex_validation_report:-}" -v error_summary="$(printf '%.1000s' "$error_summary")" -v delay_seconds="$delay_seconds" -c "UPDATE music_processing_job SET status=:'next_status',locked_at=NULL,locked_by=NULL,error_code=:'error_code',error_summary=:'error_summary',run_after=NOW()+(:'delay_seconds' || ' seconds')::interval,updated_at=NOW() WHERE id=:'job_id'::uuid; $export_failure_sql" >/dev/null || return 1
  # A broken validation refresh must not roll back recording the failed attempt.
  # The second transaction still fences on the same attempt and terminal state.
  lease_final_status=$next_status
  psql_db -v version_id="$version_id" -c "$version_completion_sql" >/dev/null
}

audio_derivative_metadata() {
  # Read the same manifest once, instead of starting three jq processes per file.
  jq -c --argjson bitrate "$2" '{pipeline:"audio-v2",preview:.preview,bitrate_kbps:$bitrate,loudness:.normalization.measurement,normalized:true,masterModified:false}' "$1"
}

process_audio() {
  [ -n "$source_asset_id" ] && [ -n "$recording_id" ] && [ -n "$source_bucket" ] || return 1
  source_file="$working_dir/master"
  output_dir="$working_dir/audio-v2"
  psql_db -v asset_id="$source_asset_id" -c "UPDATE music_asset SET processing_state='inspecting' WHERE id=:'asset_id'::uuid AND processing_state IN ('uploaded','failed','inspecting');" >/dev/null
  s3_get "$source_bucket" "$source_key" "$source_file"
  actual_sha256=$(shasum -a 256 "$source_file" | awk '{print $1}')
  [ "$actual_sha256" = "$expected_sha256" ] || { echo "Final master checksum does not match upload evidence" >&2; return 1; }
  preview_request=$(psql_db -v version_id="$version_id" -v recording_id="$recording_id" -c "SELECT jsonb_build_object('startMs',preview_start_ms,'durationMs',preview_duration_ms) FROM music_release_track WHERE release_version_id=:'version_id'::uuid AND recording_id=:'recording_id'::uuid;")
  preview_request=${preview_request:-'{"startMs":null,"durationMs":null}'}
  run_child "$audio_pipeline" "$source_file" "$output_dir" \
    "$(printf '%s' "$preview_request" | jq -c '.startMs')" "$(printf '%s' "$preview_request" | jq -c '.durationMs')"
  manifest=$(jq -c '.' "$output_dir/manifest.json")
  duration_ms=$(jq -r '(.source.durationSeconds * 1000) | round' "$output_dir/manifest.json")
  master_key="masters/$version_id/$source_asset_id/original"
  s3_put "$source_file" "$MUSIC_S3_MASTER_BUCKET" "$master_key" "$source_media_type"
  psql_db -v asset_id="$source_asset_id" -v recording_id="$recording_id" -v manifest="$manifest" -v duration_ms="$duration_ms" -v expected_sha256="$expected_sha256" -v master_bucket="$MUSIC_S3_MASTER_BUCKET" -v master_key="$master_key" -c "UPDATE music_asset SET processing_state='valid',storage_class='standard',bucket_name=:'master_bucket',object_key=:'master_key',technical_metadata=:'manifest'::jsonb,immutable=TRUE WHERE id=:'asset_id'::uuid AND sha256=:'expected_sha256' AND (NOT immutable OR (bucket_name=:'master_bucket' AND object_key=:'master_key')); UPDATE music_recording SET duration_ms=:'duration_ms'::bigint WHERE id=:'recording_id'::uuid;" >/dev/null
  if [ "$source_bucket/$source_key" != "$MUSIC_S3_MASTER_BUCKET/$master_key" ]; then
    s3_delete "$source_bucket" "$source_key" || echo "Warning: verified quarantine source could not be deleted" >&2
  fi
  for file_name in stream-low.m4a stream-medium.m4a stream-high.m4a stream-lossless.flac preview.m4a; do
    case "$file_name" in
      stream-low.m4a) role=stream_audio; media_type=audio/mp4; bitrate=96 ;;
      stream-medium.m4a) role=stream_audio; media_type=audio/mp4; bitrate=160 ;;
      stream-high.m4a) role=stream_audio; media_type=audio/mp4; bitrate=256 ;;
      stream-lossless.flac) role=stream_audio; media_type=audio/flac; bitrate=0 ;;
      preview.m4a) role=preview_audio; media_type=audio/mp4; bitrate=96 ;;
    esac
    derivative_file="$output_dir/$file_name"
    derivative_sha=$(shasum -a 256 "$derivative_file" | awk '{print $1}')
    derivative_bytes=$(wc -c < "$derivative_file" | tr -d ' ')
    object_key="derivatives/$version_id/$source_asset_id/audio-v2/$derivative_sha/$file_name"
    s3_put "$derivative_file" "$MUSIC_S3_DERIVATIVE_BUCKET" "$object_key" "$media_type"
    metadata=$(audio_derivative_metadata "$output_dir/manifest.json" "$bitrate")
    psql_db -v version_id="$version_id" -v recording_id="$recording_id" -v parent_id="$source_asset_id" -v role="$role" -v bucket="$MUSIC_S3_DERIVATIVE_BUCKET" -v object_key="$object_key" -v media_type="$media_type" -v bytes="$derivative_bytes" -v sha256="$derivative_sha" -v metadata="$metadata" -v created_by="$created_by" -c "INSERT INTO music_asset(release_version_id,recording_id,parent_asset_id,asset_role,storage_provider,storage_class,bucket_name,object_key,media_type,byte_size,sha256,processing_state,technical_metadata,provenance,immutable,created_by,ready_at) VALUES(:'version_id'::uuid,:'recording_id'::uuid,:'parent_id'::uuid,:'role','s3_compatible','standard',:'bucket',:'object_key',:'media_type',:'bytes'::bigint,:'sha256','ready',:'metadata'::jsonb,jsonb_build_object('pipeline','audio-v2','sourceAssetId',:'parent_id'::uuid),TRUE,:'created_by'::bigint,NOW()) ON CONFLICT(release_version_id,asset_role,sha256) DO NOTHING;" >/dev/null
  done
  mark_succeeded "$(jq -nc --arg sourceSha256 "$expected_sha256" '{pipeline:"audio-v2",sourceSha256:$sourceSha256}')"
}

process_preview() {
  [ "$source_media_type" = audio/flac ] && [ -n "$recording_id" ] || return 1
  preview_spec=$(printf '%s' "$job_json" | jq -ce '.output.preview | select(type=="object")')
  source_file="$working_dir/normalized.flac"
  derivative_file="$working_dir/preview.m4a"
  s3_get "$source_bucket" "$source_key" "$source_file"
  [ "$(shasum -a 256 "$source_file" | awk '{print $1}')" = "$expected_sha256" ] || {
    echo 'Normalized source checksum does not match its immutable asset' >&2; return 1;
  }
  preview_start=$(printf '%s' "$preview_spec" | jq -r 'if .selection=="auto" then "null" else .startMs end')
  preview_duration=$(printf '%s' "$preview_spec" | jq -r 'if .selection=="auto" then "null" else .durationMs end')
  run_child sh "$repository_root/scripts/process-music-release-preview.sh" "$source_file" "$derivative_file" \
    "$preview_start" "$preview_duration"
  derivative_sha=$(shasum -a 256 "$derivative_file" | awk '{print $1}')
  derivative_bytes=$(wc -c < "$derivative_file" | tr -d ' ')
  object_key="derivatives/$version_id/$source_asset_id/preview-v2/$derivative_sha/preview.m4a"
  s3_put "$derivative_file" "$MUSIC_S3_DERIVATIVE_BUCKET" "$object_key" audio/mp4
  metadata=$(jq -nc --argjson preview "$preview_spec" '{pipeline:"preview-v2",preview:$preview,bitrate_kbps:96,normalized:true,masterModified:false}')
  psql_db -v version_id="$version_id" -v recording_id="$recording_id" -v parent_id="$source_asset_id" \
    -v bucket="$MUSIC_S3_DERIVATIVE_BUCKET" -v object_key="$object_key" -v bytes="$derivative_bytes" \
    -v sha256="$derivative_sha" -v metadata="$metadata" -v created_by="$created_by" -c "INSERT INTO music_asset(release_version_id,recording_id,parent_asset_id,asset_role,storage_provider,storage_class,bucket_name,object_key,media_type,byte_size,sha256,processing_state,technical_metadata,provenance,immutable,created_by,ready_at) VALUES(:'version_id'::uuid,:'recording_id'::uuid,:'parent_id'::uuid,'preview_audio','s3_compatible','standard',:'bucket',:'object_key','audio/mp4',:'bytes'::bigint,:'sha256','ready',:'metadata'::jsonb,jsonb_build_object('pipeline','preview-v2','sourceAssetId',:'parent_id'::uuid),TRUE,:'created_by'::bigint,NOW()) ON CONFLICT(release_version_id,asset_role,sha256) DO NOTHING;" >/dev/null
  mark_succeeded "$(jq -nc --argjson preview "$preview_spec" '{pipeline:"preview-v2",preview:$preview}')"
}

process_artwork() {
  [ -n "$source_asset_id" ] && [ -n "$source_bucket" ] || return 1
  source_file="$working_dir/cover"
  output_dir="$working_dir/artwork-v1"
  psql_db -v asset_id="$source_asset_id" -c "UPDATE music_asset SET processing_state='inspecting' WHERE id=:'asset_id'::uuid AND processing_state IN ('uploaded','failed','inspecting');" >/dev/null
  s3_get "$source_bucket" "$source_key" "$source_file"
  actual_sha256=$(shasum -a 256 "$source_file" | awk '{print $1}')
  [ "$actual_sha256" = "$expected_sha256" ] || { echo "Final cover checksum does not match upload evidence" >&2; return 1; }
  run_child "$artwork_pipeline" "$source_file" "$output_dir"
  manifest=$(jq -c '.' "$output_dir/manifest.json")
  master_key="artwork-originals/$version_id/$source_asset_id/original"
  s3_put "$source_file" "$MUSIC_S3_MASTER_BUCKET" "$master_key" "$source_media_type"
  psql_db -v asset_id="$source_asset_id" -v manifest="$manifest" -v expected_sha256="$expected_sha256" -v master_bucket="$MUSIC_S3_MASTER_BUCKET" -v master_key="$master_key" -c "UPDATE music_asset SET processing_state='valid',storage_class='standard',bucket_name=:'master_bucket',object_key=:'master_key',technical_metadata=:'manifest'::jsonb,immutable=TRUE WHERE id=:'asset_id'::uuid AND sha256=:'expected_sha256' AND (NOT immutable OR (bucket_name=:'master_bucket' AND object_key=:'master_key'));" >/dev/null
  if [ "$source_bucket/$source_key" != "$MUSIC_S3_MASTER_BUCKET/$master_key" ]; then
    s3_delete "$source_bucket" "$source_key" || echo "Warning: verified quarantine source could not be deleted" >&2
  fi
  for file_name in cover-display.jpg cover-1200.jpg thumbnail-600.jpg; do
    case "$file_name" in cover-display.jpg|cover-1200.jpg) role=cover_display ;; *) role=thumbnail ;; esac
    derivative_file="$output_dir/$file_name"
    derivative_sha=$(shasum -a 256 "$derivative_file" | awk '{print $1}')
    derivative_bytes=$(wc -c < "$derivative_file" | tr -d ' ')
    object_key="artwork/$version_id/$source_asset_id/artwork-v1/$derivative_sha/$file_name"
    s3_put "$derivative_file" "$MUSIC_S3_DERIVATIVE_BUCKET" "$object_key" image/jpeg
    size=$(printf '%s' "$file_name" | sed -n 's/[^0-9]*\([0-9][0-9]*\).*/\1/p'); [ -n "$size" ] || size=3000
    metadata=$(jq -nc --arg pipeline artwork-v1 --argjson width "$size" --argjson height "$size" '{pipeline:$pipeline,width:$width,height:$height}')
    psql_db -v version_id="$version_id" -v parent_id="$source_asset_id" -v role="$role" -v bucket="$MUSIC_S3_DERIVATIVE_BUCKET" -v object_key="$object_key" -v bytes="$derivative_bytes" -v sha256="$derivative_sha" -v metadata="$metadata" -v created_by="$created_by" -c "INSERT INTO music_asset(release_version_id,parent_asset_id,asset_role,storage_provider,storage_class,bucket_name,object_key,media_type,byte_size,sha256,processing_state,technical_metadata,provenance,immutable,created_by,ready_at) VALUES(:'version_id'::uuid,:'parent_id'::uuid,:'role','s3_compatible','standard',:'bucket',:'object_key','image/jpeg',:'bytes'::bigint,:'sha256','ready',:'metadata'::jsonb,jsonb_build_object('pipeline','artwork-v1','sourceAssetId',:'parent_id'::uuid),TRUE,:'created_by'::bigint,NOW()) ON CONFLICT(release_version_id,asset_role,sha256) DO NOTHING;" >/dev/null
  done
  mark_succeeded "$(jq -nc --arg sourceSha256 "$expected_sha256" '{pipeline:"artwork-v1",sourceSha256:$sourceSha256}')"
}

# Check current rules at claim AND commit time. The gate and export mutation
# share a transaction, version/lease fence and registry locks. Never keep a
# database transaction open while rendering or transferring bytes.
ddex_gate() {
  local mutation=$1 report
  shift
  report=$(psql_db "$@" -v export_id="$export_id" -v version_id="$version_id" -c "
    SELECT registry.id FROM music_ddex_party_registry registry
      JOIN music_ddex_export export ON registry.id IN (export.sender_registry_id,export.recipient_registry_id)
      WHERE export.id=:'export_id'::uuid AND export.release_version_id=:'version_id'::uuid
      ORDER BY registry.id FOR SHARE OF registry
    \g /dev/null
    WITH issues AS MATERIALIZED (
      SELECT issue.* FROM music_ddex_export export CROSS JOIN LATERAL
        music_check_ddex_operation(export.release_version_id,export.sender_registry_id,export.recipient_registry_id,export.operation) issue
        WHERE export.id=:'export_id'::uuid AND export.release_version_id=:'version_id'::uuid
      UNION ALL
      SELECT 'version.snapshotSha256','snapshot_mismatch','El snapshot aprobado no coincide con la exportación solicitada.'
        FROM music_ddex_export export JOIN music_release_version version ON version.id=export.release_version_id
        WHERE export.id=:'export_id'::uuid AND (version.immutable_snapshot IS NULL
          OR version.snapshot_sha256 IS DISTINCT FROM export.canonical_snapshot_sha256)
      UNION ALL
      SELECT 'senderRegistryId','sender_registry_unavailable','Revisa la identidad, autorización y vigencia del remitente DDEX.'
        FROM music_ddex_export export JOIN music_ddex_party_registry registry ON registry.id=export.sender_registry_id
        WHERE export.id=:'export_id'::uuid AND (NOT registry.active OR registry.party_role NOT IN ('sender','both') OR registry.dpid<>export.sender_dpid)
      UNION ALL
      SELECT 'recipientRegistryId','recipient_registry_unavailable','Revisa la identidad, autorización y vigencia del destinatario DDEX.'
        FROM music_ddex_export export JOIN music_ddex_party_registry registry ON registry.id=export.recipient_registry_id
        WHERE export.id=:'export_id'::uuid AND (NOT registry.active OR registry.party_role NOT IN ('recipient','both') OR registry.dpid<>export.recipient_dpid)
    ), changed AS ($mutation AND NOT EXISTS (SELECT 1 FROM issues) RETURNING id), errors AS (
      SELECT * FROM issues
      UNION ALL SELECT 'export.status','export_state_changed','La exportación cambió de estado; revisa el trabajo antes de reintentarlo.'
        WHERE NOT EXISTS (SELECT 1 FROM issues) AND NOT EXISTS (SELECT 1 FROM changed)
    ) SELECT jsonb_build_object('valid',NOT EXISTS(SELECT 1 FROM errors),
      'code','ddex_preconditions_failed','message','La exportación requiere corregir los campos indicados.',
      'checkedAt',NOW(),'errors',COALESCE((SELECT jsonb_agg(jsonb_build_object(
        'fieldPath',field_path,'code',error_code,'message',message) ORDER BY field_path,error_code) FROM errors),'[]'::jsonb));")
  if ! printf '%s' "$report" | jq -e '.valid == true' >/dev/null; then
    ddex_validation_report=$report
    failure_code=ddex_preconditions_failed
    echo 'DDEX preconditions failed; see the export field-level validation report' >&2
    return 1
  fi
  ddex_validation_report=''
}

process_ddex() {
  export_id=$(printf '%s' "$job_json" | jq -r '.output.export_id // empty')
  [ -n "$export_id" ] || return 1
  # A crash can occur after the export commits but before the job closes. Never
  # regenerate or change the references of an already validated export on retry.
  export_state=$(psql_db -v export_id="$export_id" -v version_id="$version_id" -c "SELECT jsonb_build_object('status',status,'packageSha256',package_sha256) FROM music_ddex_export WHERE id=:'export_id'::uuid AND release_version_id=:'version_id'::uuid;")
  [ -n "$export_state" ] || { echo "DDEX export does not belong to this release version" >&2; return 1; }
  existing_package_sha=$(printf '%s' "$export_state" | jq -r 'if .status == "valid" then .packageSha256 else empty end')
  if [ -n "$existing_package_sha" ]; then
    mark_succeeded "$(jq -nc --arg exportId "$export_id" --arg packageSha256 "$existing_package_sha" '{exportId:$exportId,packageSha256:$packageSha256,validated:true,recoveredExistingExport:true}')"
    return
  fi
  ddex_gate "UPDATE music_ddex_export SET status='generating',validation_report=jsonb_build_object('status','generating','checkedAt',NOW()) WHERE id=:'export_id'::uuid AND release_version_id=:'version_id'::uuid AND status IN ('queued','validation_failed','generating')"
  schema_dir=${MUSIC_DDEX_SCHEMA_DIR:-}
  [ -f "$schema_dir/release-notification.xsd" ] || { echo "Pinned official ERN 4.3.2 XSD is unavailable" >&2; return 1; }
  xml_file="$working_dir/release.xml"
  resource_manifest="$working_dir/resources.tsv"
  resource_root="$working_dir/resources"
  package_file="$working_dir/package.zip"
  mkdir -p "$resource_root"
  run_child "$ddex_renderer" "$export_id" "$xml_file" "$resource_manifest"
  while IFS="$(printf '\t')" read -r bucket key sha256 relative_path; do
    [ -n "$bucket" ] || continue
    case "$relative_path" in resources/*) destination="$resource_root/${relative_path#resources/}" ;; *) echo "Unsafe DDEX resource path" >&2; return 1 ;; esac
    mkdir -p "$(dirname "$destination")"
    s3_get "$bucket" "$key" "$destination"
    actual_sha=$(shasum -a 256 "$destination" | awk '{print $1}')
    [ "$actual_sha" = "$sha256" ] || { echo "DDEX source checksum mismatch: $relative_path" >&2; return 1; }
  done < "$resource_manifest"
  snapshot_sha=$(psql_db -v export_id="$export_id" -c "SELECT canonical_snapshot_sha256 FROM music_ddex_export WHERE id=:'export_id'::uuid;")
  generated_by=$(psql_db -v export_id="$export_id" -c "SELECT generated_by::text FROM music_ddex_export WHERE id=:'export_id'::uuid;")
  run_child "$ddex_package_builder" "$schema_dir" "$xml_file" "$resource_root" "$package_file" "$generated_by" "$snapshot_sha" >/dev/null
  unzip -p "$package_file" manifest.json > "$working_dir/manifest.json"
  for artifact in xml manifest package; do
    case "$artifact" in
      xml) file="$xml_file"; role=ddex_xml; media_type=application/xml; name=release.xml ;;
      manifest) file="$working_dir/manifest.json"; role=ddex_manifest; media_type=application/json; name=manifest.json ;;
      package) file="$package_file"; role=ddex_package; media_type=application/zip; name=package.zip ;;
    esac
    sha256=$(shasum -a 256 "$file" | awk '{print $1}')
    bytes=$(wc -c < "$file" | tr -d ' ')
    key="ddex/$version_id/$export_id/$sha256/$name"
    s3_put "$file" "$MUSIC_S3_DDEX_BUCKET" "$key" "$media_type"
    asset_id=$(psql_db -v export_id="$export_id" -v version_id="$version_id" -v role="$role" -v bucket="$MUSIC_S3_DDEX_BUCKET" -v key="$key" -v media_type="$media_type" -v bytes="$bytes" -v sha256="$sha256" -v created_by="$generated_by" -c "WITH inserted AS (INSERT INTO music_asset(release_version_id,asset_role,storage_provider,storage_class,bucket_name,object_key,original_filename,media_type,byte_size,sha256,processing_state,technical_metadata,provenance,immutable,created_by,ready_at) VALUES(:'version_id'::uuid,:'role','s3_compatible','standard',:'bucket',:'key',:'role',:'media_type',:'bytes'::bigint,:'sha256','ready',jsonb_build_object('ernVersion','4.3.2','releaseProfile','Audio','releaseProfileVersion','2.3.1','avsVersion','011'),jsonb_build_object('exportId',:'export_id'::uuid),TRUE,:'created_by'::bigint,NOW()) ON CONFLICT(release_version_id,asset_role,sha256) DO NOTHING RETURNING id) SELECT id FROM inserted UNION ALL SELECT id FROM music_asset WHERE release_version_id=:'version_id'::uuid AND asset_role=:'role' AND sha256=:'sha256' LIMIT 1;")
    case "$artifact" in xml) xml_asset_id=$asset_id ;; manifest) manifest_asset_id=$asset_id ;; package) package_asset_id=$asset_id; package_sha=$sha256 ;; esac
  done
  report=$(jq -nc --arg xsd release-notification.xsd --arg packageSha256 "$package_sha" '{valid:true,xsd:$xsd,adapterVersion:"tdf-ern432-audio-v5",profileRules:"TDF ERN432 Audio adapter v5: free on-demand only; stable track-release IDs",packageSha256:$packageSha256,validatedLocally:true,packageFormat:"tdf-offline-review-bundle",recipientAcceptance:"not-verified",deliveryPerformed:false}')
  ddex_gate "UPDATE music_ddex_export SET status='valid',xml_asset_id=:'xml_asset_id'::uuid,manifest_asset_id=:'manifest_asset_id'::uuid,package_asset_id=:'package_asset_id'::uuid,package_sha256=:'package_sha',validation_report=:'report'::jsonb,generated_at=NOW() WHERE id=:'export_id'::uuid AND release_version_id=:'version_id'::uuid AND status='generating' AND canonical_snapshot_sha256=:'snapshot_sha'" \
    -v xml_asset_id="$xml_asset_id" -v manifest_asset_id="$manifest_asset_id" -v package_asset_id="$package_asset_id" -v package_sha="$package_sha" -v report="$report" -v snapshot_sha="$snapshot_sha"
  mark_succeeded "$(jq -nc --arg exportId "$export_id" --arg packageSha256 "$package_sha" '{exportId:$exportId,packageSha256:$packageSha256,validated:true}')"
}

error_log="$working_dir/error.log"
heartbeat_pid=''
stop_heartbeat() {
  if [ -n "$heartbeat_pid" ]; then
    kill -TERM -- "-$heartbeat_pid" 2>/dev/null || true
    wait "$heartbeat_pid" 2>/dev/null || true
    heartbeat_pid=''
  fi
}
heartbeat() {
  # Descendants share this heartbeat's group, so shutdown also stops sleep/psql.
  # No nested signal trap/wait (which is unsafe on macOS Bash 3.2).
  set +m
  trap - EXIT TERM INT USR1
  while kill -0 "$$" 2>/dev/null; do
    sleep "$heartbeat_seconds"
    kill -0 "$$" 2>/dev/null || return
    if ! psql_db -c 'SELECT 1;' >/dev/null; then
      touch "$working_dir/lease-lost"
      kill -USR1 "$$" 2>/dev/null || true
      return 1
    fi
  done
}
# Invoke tasks directly: placing a function inside `if` disables errexit in
# its whole body. EXIT records the failure after the first failed command.
finish_worker() {
  # EXIT may run while a child/SQL helper's local scope is still active. Keep
  # its status separate from the helpers' dynamically scoped result variables.
  local worker_exit_status=$?
  trap - EXIT
  trap '' INT TERM USR1
  stop_heartbeat
  if [ -n "$active_child" ]; then
    kill -TERM -- "-$active_child" 2>/dev/null || true
    wait "$active_child" 2>/dev/null || true
  fi
  exec 2>&3 3>&-
  if [ "$worker_exit_status" -ne 0 ]; then
    summary=$(tail -20 "$error_log" | tr '\n' ' ')
    mark_failed "${failure_code:-${job_kind}_failed}" "${summary:-processing command failed}" || true
  fi
  cat "$error_log" >&2
  cleanup
  exit "$worker_exit_status"
}
exec 3>&2 2>"$error_log"
trap finish_worker EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
trap 'echo "Music worker lease lost; stopping active command" >&2; exit 75' USR1
heartbeat &
heartbeat_pid=$!
case "$job_kind" in
  inspect_audio|create_preview|inspect_artwork|validate_release|generate_ddex|publish_release|withdraw_release) ;;
  *)
    echo "Unsupported music processing job kind: $job_kind" >&2
    exit 1
    ;;
esac

case "$job_kind" in
  inspect_audio) process_audio ;;
  create_preview) process_preview ;;
  inspect_artwork) process_artwork ;;
  validate_release) mark_succeeded '{}' ;;
  generate_ddex) process_ddex ;;
  publish_release|withdraw_release) mark_succeeded '{}' ;;
esac
echo "Music processing job $job_id ($job_kind) succeeded"
