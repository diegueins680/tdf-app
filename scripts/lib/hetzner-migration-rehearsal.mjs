import { buildMigrationBatchSql } from './production-release.mjs';
import { sha256 } from './migration-contract.mjs';

// Input is the immutable, ancestry-checked contract. This bundle authorizes no
// deployment; the remote helper can execute it only on its admitted isolate.
export function migrationRehearsalBundle(contract) {
  const sql = buildMigrationBatchSql(contract.migrations, { sourceCommit: contract.sourceRevision });
  return { schemaVersion: 1, sourceRevision: contract.sourceRevision,
    manifestSha256: contract.manifestSha256, sqlSha256: sha256(sql), sql,
    migrations: contract.migrations.map(({ id, checksum, compatibleAppliedChecksums }) =>
      ({ id, checksum, compatibleAppliedChecksums })) };
}
