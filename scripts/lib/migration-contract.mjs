import { createHash } from 'node:crypto';
import { expandMigrationIncludes, normalizeFullSha, requireMigrationIntroductionAncestor,
  validateMigrationRelativePath, validateSafeName } from './production-release.mjs';

export const sha256 = value => createHash('sha256').update(value).digest('hex');
const checksum = value => {
  if (typeof value !== 'string' || !/^[a-f0-9]{64}$/.test(value)) throw new Error('Invalid migration checksum');
  return value;
};

// Readers must select immutable blobs from the supplied revision, including every
// SQL include. The same expanded bytes are used by the existing migration runner.
export async function loadMigrationContract(revision, readBlob, isAncestor) {
  const sourceRevision = normalizeFullSha(revision);
  const manifestBytes = await readBlob(sourceRevision, 'scripts/production-migrations.json');
  const manifest = JSON.parse(manifestBytes);
  if (manifest.schemaVersion !== 1 || !Array.isArray(manifest.migrations) || !manifest.migrations.length) {
    throw new Error('Unsupported or empty production migration manifest');
  }
  const ids = new Set(), paths = new Set(), migrations = [];
  for (const entry of manifest.migrations) {
    const id = validateSafeName(entry.id, 'Migration id');
    const path = validateMigrationRelativePath(entry.path);
    if (ids.has(id) || paths.has(path)) throw new Error('Duplicate migration id or path');
    ids.add(id); paths.add(path);
    const introducedBy = normalizeFullSha(entry.introducedBy);
    requireMigrationIntroductionAncestor({ id, introducedBy }, sourceRevision,
      await isAncestor(introducedBy, sourceRevision));
    if (entry.compatibleAppliedChecksums !== undefined && !Array.isArray(entry.compatibleAppliedChecksums)) {
      throw new Error('Compatible migration checksums must be an array');
    }
    const compatibleAppliedChecksums = (entry.compatibleAppliedChecksums ?? []).map(checksum);
    const content = await expandMigrationIncludes({ path, content: await readBlob(sourceRevision, path) },
      included => readBlob(sourceRevision, included));
    migrations.push({ id, path, introducedBy, content, checksum: sha256(content), compatibleAppliedChecksums });
  }
  return { sourceRevision, manifestSha256: sha256(manifestBytes), migrations };
}

export function reconcileMigrationLedger(contract, ledger) {
  if (!Array.isArray(ledger)) throw new Error('Missing applied migration ledger');
  const byId = new Map(contract.migrations.map(entry => [entry.id, entry]));
  const observed = new Map();
  for (const row of ledger) {
    if (observed.has(row.migration_id)) throw new Error('Duplicate applied migration');
    const entry = byId.get(row.migration_id);
    if (!entry) throw new Error('Applied migration is absent from the candidate manifest');
    const appliedChecksum = checksum(row.checksum);
    normalizeFullSha(row.source_commit);
    if (appliedChecksum !== entry.checksum && !entry.compatibleAppliedChecksums.includes(appliedChecksum)) {
      throw new Error('Applied migration history differs from the reviewed candidate');
    }
    observed.set(row.migration_id, row);
  }
  // Ledger query ordering is not migration execution ordering. Preserve manifest
  // order for pending work; do not silently omit a hole in the applied ledger.
  return contract.migrations.map(({ content: _content, ...entry }) => ({ ...entry,
    state: observed.has(entry.id) ? 'applied' : 'pending',
    appliedChecksum: observed.get(entry.id)?.checksum ?? null,
    appliedSourceRevision: observed.get(entry.id)?.source_commit ?? null,
  }));
}
