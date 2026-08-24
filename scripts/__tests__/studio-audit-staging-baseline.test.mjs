import assert from 'node:assert/strict';
import test from 'node:test';

import {
  buildStudioAuditStagingBaselineSql,
  buildStudioAuditStagingRuntimeSql,
  loadStudioAuditStagingBaselineEntries,
} from '../baseline-studio-audit-staging-migrations.mjs';

const sourceCommit = '1'.repeat(40);

test('staging baseline covers every migration before the studio audit and leaves it pending', async () => {
  const entries = await loadStudioAuditStagingBaselineEntries();
  const sql = buildStudioAuditStagingBaselineSql(entries, sourceCommit);

  assert.equal(entries.length, 61);
  assert.match(sql, /BEGIN CANONICAL STAGING RUNTIME MIGRATION 2026-08-02_ddex_catalog_core/u);
  assert.match(sql, /CREATE TABLE IF NOT EXISTS catalog_release/u);
  assert.match(sql, /BEGIN CANONICAL STAGING RUNTIME MIGRATION 2026-08-13_unified_checkout_core/u);
  assert.match(sql, /CREATE TABLE IF NOT EXISTS commerce_checkout_session/u);
  assert.match(sql, /BEGIN CANONICAL STAGING RUNTIME MIGRATION 2026-08-18_domo_quote_checkout_runtime/u);
  assert.match(sql, /END CANONICAL STAGING RUNTIME MIGRATION REPLAY/u);
  assert.ok(sql.indexOf('runtime migration replay outside') < sql.indexOf('CREATE TABLE IF NOT EXISTS commerce_checkout_session'));
  assert.equal(sql.match(/^BEGIN;\s*$/gmu)?.length, 2);
  assert.match(sql, /current_database\(\) <> 'tdf_studio_audit_staging'/u);
  assert.match(sql, /NOT LIKE '%@persona\.test'/u);
  assert.match(sql, /The studio-audit migration must remain pending/u);
  assert.doesNotMatch(sql, /\('2026-08-21_studio_internship_audit', '[0-9a-f]{64}',/u);
  assert.doesNotMatch(sql, /\('2026-08-24_ticket_qr_constraint_compatibility', '[0-9a-f]{64}',/u);
  assert.match(sql, /ON CONFLICT \(migration_id\) DO NOTHING/u);
  assert.match(sql, /RENAME CONSTRAINT unique_ticket_q_r_code TO unique_ticket_qr_code/u);
  assert.match(sql, /uq_event_ticket_order_stripe_payment_intent[\s\S]*stripe_payment_intent_id IS NOT NULL/u);
  assert.match(sql, /runtime_type_compatibility/u);
  assert.match(sql, /external_event_discovery_run', 'artists_created', 'integer'/u);
  assert.match(sql, /social_discovery_review', 'status', 'text'/u);
  assert.match(sql, /artist_media_asset', 'source_height', 'integer'/u);
  assert.match(sql, /feature_navigation_preferences', 'pin_order', 'integer'/u);
  assert.match(sql, /ddex_document', 'file_name', 'text'/u);
  assert.match(sql, /DDEX synthetic bootstrap identifiers exceed the canonical integer range/u);
  assert.match(sql, /ALTER COLUMN uploaded_by TYPE integer/u);
  assert.match(sql, /ddex_message_header_document_id_fkey[\s\S]*ON DELETE CASCADE/u);
  assert.match(sql, /event_city_subscription_city_id_fkey[\s\S]*ON DELETE CASCADE/u);
  assert.match(sql, /idx_external_event_ref_city[\s\S]*lower\(city\)/u);
  assert.match(sql, /idx_external_event_ref_event_id[\s\S]*event_id/u);
  assert.match(sql, /CREATE UNIQUE INDEX unique_external_event_discovery_slot[\s\S]*scheduled_for IS NOT NULL/u);
  assert.match(sql, /uq_artist_enrichment_active_full_run[\s\S]*scope = 'full'/u);
  assert.match(sql, /idx_artist_media_asset_hash/u);
  assert.match(sql, /fk_artist_inventory_social_artist[\s\S]*ON DELETE SET NULL/u);
  assert.match(sql, /social_event_time_order/u);
  assert.match(sql, /Refusing synthetic cutover markers when non-dry-run history exists/u);
  assert.match(sql, /ddex-operational-cutover-2026-08-12/u);
  assert.doesNotMatch(sql, /\n\+\s+\('[^']+-cutover-/u);
  assert.match(sql, /"productionData":false/u);
  assert.doesNotMatch(sql, /^\+\s*\('/mu);
  assert.match(sql, new RegExp(`\\b${sourceCommit}\\b`, 'u'));
});

test('runtime-only staging replay is independently guarded and excludes migration ledger writes', async () => {
  const entries = await loadStudioAuditStagingBaselineEntries();
  const sql = buildStudioAuditStagingRuntimeSql(entries);

  assert.match(sql, /current_database\(\) <> 'tdf_studio_audit_staging'/u);
  assert.match(sql, /NOT LIKE '%@persona\.test'/u);
  assert.match(sql, /BEGIN CANONICAL STAGING RUNTIME MIGRATION 2026-08-02_ddex_catalog_core/u);
  assert.match(sql, /DDEX synthetic bootstrap identifiers exceed the canonical integer range/u);
  assert.match(sql, /CREATE TABLE IF NOT EXISTS commerce_checkout_session/u);
  assert.equal(sql.match(/^BEGIN;\s*$/gmu)?.length, 1);
  assert.equal(sql.match(/^COMMIT;\s*$/gmu)?.length, 1);
  assert.doesNotMatch(sql, /INSERT INTO public\.tdf_schema_migration/u);
  assert.doesNotMatch(sql, /2026-08-21_studio_internship_audit/u);
});

test('staging baseline rejects malformed checksums and source revisions', () => {
  assert.throws(
    () => buildStudioAuditStagingBaselineSql([{ id: 'safe-id', checksum: 'x' }], sourceCommit),
    /checksum/u,
  );
  assert.throws(
    () => buildStudioAuditStagingBaselineSql([{ id: 'safe-id', checksum: 'a'.repeat(64) }], 'main'),
    /commit/u,
  );
});
