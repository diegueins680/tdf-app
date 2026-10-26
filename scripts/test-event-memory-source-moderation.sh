#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
. "$repo_root/scripts/lib/postgres-test-database.sh"
tdf_test_db_init tdf_event_memory_source_moderation_test
batch=$(mktemp)
definition=$(mktemp)
cleanup_source() { rm -f "$batch" "$definition"; tdf_test_db_cleanup; }
trap cleanup_source EXIT INT TERM
tdf_test_psql -f "$repo_root/scripts/__tests__/fixtures/production-schema-20260814.sql" >/dev/null
tdf_test_psql -f "$repo_root/scripts/__tests__/fixtures/catalog-production-source-fixture.sql" >/dev/null
# Exercise the historical evidence-free rollback before installing its guarded
# authorization successor. The final canonical batch is exercised afterwards.
node --input-type=module - "$repo_root" > "$batch" <<'JS'
import fs from 'node:fs/promises';
import path from 'node:path';
import { createHash } from 'node:crypto';
import { pathToFileURL } from 'node:url';
const root=process.argv[2];
const { buildMigrationBatchSql, expandMigrationIncludes }=await import(pathToFileURL(path.join(root,'scripts/lib/production-release.mjs')));
const manifest=JSON.parse(await fs.readFile(path.join(root,'scripts/production-migrations.json'),'utf8'));
const repair=manifest.migrations.findIndex(entry=>entry.id==='2026-10-25_event_memory_source_authorization');
const prefix=repair<0?manifest.migrations:manifest.migrations.slice(0,repair);
const migrations=await Promise.all(prefix.map(async entry=>{
 const content=await expandMigrationIncludes({...entry,content:await fs.readFile(path.join(root,entry.path),'utf8')},p=>fs.readFile(path.join(root,p),'utf8'));
 return {...entry,content,checksum:createHash('sha256').update(content).digest('hex')};
}));
process.stdout.write(buildMigrationBatchSql(migrations,{sourceCommit:'0'.repeat(40)})+'\n');
JS
tdf_test_psql -f "$batch" >/dev/null
tdf_test_psql -f "$repo_root/tdf-hq/sql/2026-10-25_event_memory_source_moderation.sql" >/dev/null
tdf_test_psql -f "$repo_root/tdf-hq/sql/2026-10-25_event_memory_source_moderation.sql" >/dev/null
tdf_test_psql -f "$repo_root/tdf-hq/test/integration/event_memory_source_moderation.sql" >/dev/null
tdf_test_psql -f "$repo_root/tdf-hq/sql/2026-10-25_event_memory_source_moderation_rollback.sql" >/dev/null
tdf_test_psql -qAtc "SELECT bool_and(pg_get_functiondef(signature::regprocedure)=definition) FROM event_memory_moderation_authority" | grep -qx t
tdf_test_psql -f "$repo_root/tdf-hq/sql/2026-10-25_event_memory_source_moderation.sql" >/dev/null
tdf_test_psql -f "$repo_root/tdf-hq/test/integration/event_memory_source_moderation.sql" >/dev/null
tdf_test_psql -f "$repo_root/tdf-hq/sql/2026-10-25_event_memory_source_authorization.sql" >/dev/null
tdf_test_psql -f "$repo_root/tdf-hq/sql/2026-10-25_event_memory_source_authorization.sql" >/dev/null
tdf_test_psql -f "$repo_root/tdf-hq/test/integration/event_memory_source_null_organizer.sql" >/dev/null
tdf_test_psql -qAtc "SELECT pg_get_functiondef('public.event_memory_source_context(uuid,bigint)'::regprocedure)" > "$definition"
tdf_test_psql >/dev/null <<'SQL'
DO $$ DECLARE original text; changed text; BEGIN
 original:=pg_get_functiondef('public.event_memory_source_context(uuid,bigint)'::regprocedure);
 changed:=replace(original,E'\nDECLARE m event_moment',E'\n-- Synthetic unreviewed successor\nDECLARE m event_moment');
 ASSERT changed<>original;
 EXECUTE changed;
END $$;
SQL
if tdf_test_psql -f "$repo_root/tdf-hq/sql/2026-10-25_event_memory_source_authorization.sql" >/dev/null 2>&1; then
 echo 'Unknown successor was unexpectedly overwritten by the authorization repair' >&2; exit 1
fi
tdf_test_psql -f "$definition" >/dev/null
tdf_test_psql -f "$repo_root/tdf-hq/sql/2026-10-25_event_memory_source_authorization.sql" >/dev/null
tdf_test_psql -f "$repo_root/tdf-hq/test/integration/event_memory_source_moderation.sql" >/dev/null
if tdf_test_psql -f "$repo_root/tdf-hq/sql/2026-10-25_event_memory_source_moderation.sql" >/dev/null 2>&1; then
 echo 'Unsafe predecessor unexpectedly replaced the authorization repair' >&2; exit 1
fi
if tdf_test_psql -f "$repo_root/tdf-hq/sql/2026-10-25_event_memory_source_moderation_rollback.sql" >/dev/null 2>&1; then
 echo 'Unsafe predecessor rollback unexpectedly removed the authorization repair' >&2; exit 1
fi
tdf_test_psql -f "$repo_root/tdf-hq/test/integration/event_memory_source_null_organizer.sql" >/dev/null
node "$repo_root/scripts/render-production-migration-batch.mjs" > "$batch"
tdf_test_psql -f "$batch" >/dev/null
tdf_test_psql -f "$batch" >/dev/null
tdf_test_psql -f "$repo_root/tdf-hq/test/integration/event_memory_source_null_organizer.sql" >/dev/null
printf '%s\n' 'Source moderation: full-schema, replay, historical rollback, NULL-organizer denial and guarded successor reapply passed'
