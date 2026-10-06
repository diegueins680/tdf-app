import { normalizeFullSha } from './production-release.mjs';
import { reconcileMigrationLedger } from './migration-contract.mjs';

const requireFact = (condition, message) => { if (!condition) throw new Error(message); };
const immutableImage = /^(?:[a-z0-9][a-z0-9._/-]*)@sha256:[a-f0-9]{64}$/;

// This reconciles observations, not approval, backup health, image provenance or
// deployment eligibility. A future executor must independently re-observe facts
// under its release lock and satisfy every remaining gate.
export function prepareHetznerRelease({ contract, receipt, provenance, now = Date.now() }) {
  const start = Date.parse(receipt.startedAt), observed = Date.parse(receipt.observedAt);
  requireFact(Number.isFinite(now) && Number.isFinite(start) && Number.isFinite(observed)
    && start <= observed && observed <= now && now - start <= 15 * 60_000, 'Stale or invalid runtime observation');
  normalizeFullSha(receipt.sourceRevision);
  requireFact(receipt.sourceWorktreeDirty === false, 'Runtime inspection used dirty sources');
  for (const key of ['inspectorSha256', 'launcherSha256']) {
    requireFact(typeof provenance[key] === 'string' && /^[a-f0-9]{64}$/.test(provenance[key])
      && receipt[key] === provenance[key], 'Runtime collector provenance differs');
  }
  const snapshot = receipt.snapshot;
  requireFact(snapshot?.schemaVersion === 1 && snapshot.project === 'tdf-production'
    && snapshot.directory === '/opt/tdf/production', 'Wrong production target');
  const database = snapshot.database;
  requireFact(database?.database === 'tdf_hq' && database.role === 'tdf_catalog_inventory'
    && database.readOnly === true && database.localConnection === true, 'Unverified database observation');
  requireFact(snapshot.publicBackend?.name === 'tdf-hq', 'Wrong public backend identity');
  normalizeFullSha(snapshot.publicBackend.commit);
  for (const service of ['api', 'db', 'edge']) {
    const container = snapshot.containers?.[service];
    requireFact(container?.running === true && /^[a-f0-9]{64}$/.test(container.containerId)
      && immutableImage.test(container.image) && /^sha256:[a-f0-9]{64}$/.test(container.imageId),
    'Unverified production container');
  }
  requireFact(snapshot.containers.db.volume === 'tdf_production_postgres_data', 'Wrong production database volume');
  const observedFlags = snapshot.containers.api.booleanConfiguration;
  requireFact(observedFlags && Object.values(observedFlags).every(value => ['true', 'false'].includes(value)),
    'Invalid observed flag state');
  requireFact(observedFlags.RUN_MIGRATIONS === 'false' && observedFlags.RESET_DB === 'false'
    && observedFlags.SEED_DB === 'false', 'Unsafe automatic schema or destructive runtime flag');
  const migrations = reconcileMigrationLedger(contract, database.migrations);
  return {
    schemaVersion: 1, status: 'prepared-not-authorized', executionAllowed: false,
    sourceRevision: contract.sourceRevision, manifestSha256: contract.manifestSha256,
    target: { host: '178.105.93.101', project: snapshot.project, directory: snapshot.directory,
      database: database.database, volume: snapshot.containers.db.volume },
    observedAt: receipt.observedAt, observedBackend: snapshot.publicBackend,
    observedContainers: Object.fromEntries(Object.entries(snapshot.containers).map(([service, value]) =>
      [service, { containerId: value.containerId, image: value.image, imageId: value.imageId }])),
    migrations, counts: { applied: migrations.filter(row => row.state === 'applied').length,
      pending: migrations.filter(row => row.state === 'pending').length },
    observedFlags, missingFlags: snapshot.containers.api.missingBooleanConfiguration,
    observedPrivateUploads: snapshot.containers.api.privateUploads ?? null,
    observedDatabaseControls: { revenueFlags: database.revenueFlags ?? null,
      merchReputationFlags: database.merchReputationFlags ?? null,
      missingMerchReputationFlags: database.missingMerchReputationFlags ?? null,
      eventOperationFlags: database.eventOperationFlags ?? null,
      interactionRuntime: database.interactionRuntime ?? null,
      interactionEntityKinds: database.interactionEntityKinds ?? null,
      providerAccounts: database.providerAccounts ?? null, socialRuntime: database.socialRuntime ?? null },
    requiredCorsConfiguration: { ALLOW_ALL_ORIGINS: 'false', CORS_DISABLE_DEFAULTS: 'true',
      ALLOWED_ORIGINS: 'https://www.tdfrecords.net,https://tdfrecords.net' },
    requiredOutboundConfiguration: { SOCIAL_AUTO_REPLY_ENABLED: 'false', COURSE_PAYMENT_REMINDER_ENABLED: 'false' },
    remainingGates: [
      'Current exact-head review, all quality gates, normal merge and post-merge CI',
      'Registry image digest and embedded version bound to merged source; compatible reviewed recovery image',
      'Exclusive release lock; recheck runtime identity, ledger, effective Compose configuration and provider/feature flags',
      'Drain all old writers, including background workers; do not treat a stop request as proof of drainage',
      'Keep automatic social replies and course payment reminders disabled pending durable claims/reconciliation; candidate-only flags do not qualify legacy-image recovery',
      'Preserve and verify legacy /app/uploads under the writer fence; provision the private host bind with the image user ownership before replacing the API container',
      'Consistent database, public assets and private uploads backup; actual isolated restore and schema/content verification',
      'Apply reviewed pending migrations and verify full schema; preserve operator provider/feature choices',
      'Restricted canary with external effects disabled, enforcing CORS configuration and exact source version',
      'Route traffic, verify backend/frontend/mobile compatibility, live flags, ledger and safe smoke tests',
      'After writes resume, recover forward without restoring an old backup over accepted data',
    ],
    limitations: 'Source and observational correspondence only. Local receipts are not signed attestations. '
      + 'No remote command, SQL write, image verification, restore, approval or deployment was performed.',
  };
}
