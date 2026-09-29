#!/usr/bin/env bash
set -euo pipefail
interaction_repo=$(cd "$(dirname "$0")/../.." && pwd)
interaction_db=${1:?Pass the name of a new isolated tdf_interaction_ database}
case "$interaction_db" in tdf_interaction_*) ;; *) echo 'Refusing non-test database name' >&2; exit 1;; esac
createdb "$interaction_db"
psql -X -v ON_ERROR_STOP=1 -d "$interaction_db" -f "$interaction_repo/scripts/__tests__/fixtures/production-schema-20260814.sql" >/dev/null
psql -X -v ON_ERROR_STOP=1 -d "$interaction_db" -f "$interaction_repo/scripts/__tests__/fixtures/catalog-production-source-fixture.sql" >/dev/null
node "$interaction_repo/scripts/render-production-migration-batch.mjs" | psql -X -v ON_ERROR_STOP=1 -d "$interaction_db" >/dev/null
for interaction_sql in \
  2026-09-14_social_v2_foundation \
  2026-09-14_social_v2_read_models \
  2026-09-15_social_v2_dm_write_boundary \
  2026-09-15_social_v2_chat_api \
  2026-09-15_social_v2_relationship_reads \
  2026-09-15_social_v2_profile_reads \
  2026-09-16_social_v2_legacy_writes \
  2026-09-16_social_v2_fan_effects \
  2026-09-28_universal_interactions \
  2026-09-28_interaction_policy \
  2026-09-28_interaction_discussions \
  2026-09-28_interaction_locks \
  2026-09-28_interaction_commands \
  2026-09-28_interaction_blocks \
  2026-09-28_interaction_summary \
  2026-09-28_interaction_notifications \
  2026-09-28_interaction_navigation \
  2026-09-28_interaction_read_optimization \
  2026-09-28_interaction_legacy \
  2026-09-28_interaction_compatibility \
  2026-09-28_interaction_account_controls \
  2026-09-28_interaction_integrity \
  2026-09-29_interaction_review_repairs \
  2026-09-29_interaction_publication_authority \
  2026-09-29_interaction_reaction_withdrawal \
  2026-09-29_interaction_moderation_block_boundary \
  2026-09-29_interaction_moderation_access \
  2026-09-29_interaction_legacy_reply_aliases \
  2026-09-29_interaction_moderation_delivery \
  2026-09-29_interaction_event_destinations \
  2026-09-29_interaction_mention_privacy \
  2026-09-29_interaction_inactive_allowlists \
  2026-09-29_interaction_legacy_moment_choices \
  2026-09-29_interaction_catalog_authority; do
  psql -X -v ON_ERROR_STOP=1 -d "$interaction_db" -f "$interaction_repo/tdf-hq/sql/$interaction_sql.sql" >/dev/null
done
printf 'Prepared isolated database %s\n' "$interaction_db"
