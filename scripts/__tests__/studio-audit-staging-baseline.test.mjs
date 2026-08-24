import assert from 'node:assert/strict';
import test from 'node:test';

import {
  buildStudioAuditStagingBaselineSql,
  loadStudioAuditStagingBaselineEntries,
} from '../baseline-studio-audit-staging-migrations.mjs';

const sourceCommit = '1'.repeat(40);

test('staging baseline covers every migration before the studio audit and leaves it pending', async () => {
  const entries = await loadStudioAuditStagingBaselineEntries();
  const sql = buildStudioAuditStagingBaselineSql(entries, sourceCommit);

  assert.equal(entries.length, 61);
  assert.match(sql, /current_database\(\) <> 'tdf_studio_audit_staging'/u);
  assert.match(sql, /NOT LIKE '%@persona\.test'/u);
  assert.match(sql, /The studio-audit migration must remain pending/u);
  assert.doesNotMatch(sql, /\('2026-08-21_studio_internship_audit', '[0-9a-f]{64}',/u);
  assert.doesNotMatch(sql, /\('2026-08-24_ticket_qr_constraint_compatibility', '[0-9a-f]{64}',/u);
  assert.match(sql, /ON CONFLICT \(migration_id\) DO NOTHING/u);
  assert.match(sql, new RegExp(`\\b${sourceCommit}\\b`, 'u'));
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
