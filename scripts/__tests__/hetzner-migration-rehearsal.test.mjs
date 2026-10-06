import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import test from 'node:test';
import { migrationRehearsalBundle } from '../lib/hetzner-migration-rehearsal.mjs';
import { buildMigrationBatchSql } from '../lib/production-release.mjs';
const sha = value => createHash('sha256').update(value).digest('hex');
test('migration rehearsal uses the canonical immutable batch and its schema verifier', () => {
  const contract = { sourceRevision: 'a'.repeat(40), manifestSha256: 'b'.repeat(64), migrations: [
    { id: 'synthetic', path: 'tdf-hq/sql/synthetic.sql', introducedBy: 'c'.repeat(40),
      content: 'SELECT 1;', checksum: sha('SELECT 1;'), compatibleAppliedChecksums: [] },
  ] };
  const result = migrationRehearsalBundle(contract);
  assert.equal(result.sql, buildMigrationBatchSql(contract.migrations, { sourceCommit: contract.sourceRevision }));
  assert.equal(result.sqlSha256, sha(result.sql));
  assert.equal(result.sourceRevision, contract.sourceRevision);
  assert.equal(result.manifestSha256, contract.manifestSha256);
  assert.deepEqual(result.migrations, [{ id: 'synthetic', checksum: sha('SELECT 1;'), compatibleAppliedChecksums: [] }]);
  assert.match(result.sql, /Checksum mismatch for migration synthetic/);
  assert.match(result.sql, /tdf-production-schema-migrations/);
});
