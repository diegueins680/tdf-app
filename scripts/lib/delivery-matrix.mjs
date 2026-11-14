// Evidence inventory only: consistent receipts are not deployment authorization.
export const DELIVERY_CHECKS = Object.freeze([
  { id: 'api-contract', priority: 1, level: 'contract-test', mobile: true,
    criterion: 'Regenerate both clients; compare committed bytes and exercise the candidate server contract.',
    entrypoint: 'npm run generate:api && node scripts/check-generated-api.mjs' },
  { id: 'backend-runtime', priority: 1, level: 'runtime-observation',
    criterion: 'Observed backend revision, immutable image, migration manifest and non-secret flag digest match the candidate.' },
  { id: 'web-acceptance', priority: 1, level: 'browser-test',
    criterion: 'Built web artifact against the candidate backend: sign-in, upload, playback and checkout journeys.' },
  ...['ios', 'android'].map(platform => ({ id: `${platform}-device`, priority: 1, level: 'device-test', mobile: true,
    criterion: 'Signed binary identity and physical-device journeys against the candidate backend; a simulator is not a physical device.' })),
  ...['macos', 'windows'].map(platform => ({ id: `${platform}-installed`, priority: 1, level: 'installed-app-test',
    criterion: 'Signed installed desktop artifact: login, media, uploads, deep links and update recovery.' })),
  { id: 'payment-concurrency', priority: 2, level: 'database-test',
    criterion: 'Capture versus expiry, duplicate/reordered callbacks, refund limits and stale worker leases; negative controls on the complete migrated schema.',
    entrypoint: 'sh scripts/test-provider-retry-runtime.sh; sh scripts/test-ledger-posting.sh' },
  { id: 'booking-concurrency', priority: 2, level: 'http-test',
    criterion: 'Same-key replay, changed-payload conflict, overlapping resource refusal and no orphan rows.',
    entrypoint: 'sh scripts/test-public-booking-http-concurrency.sh' },
  { id: 'capacity-concurrency', priority: 2, level: 'database-test',
    criterion: 'Tickets, courses and merchandise: concurrent final units, expiry/release and no negative remaining capacity.' },
  { id: 'authorization-isolation', priority: 3, level: 'http-test', mobile: true,
    criterion: 'Cross-account and cross-event denial, revoked access and late responses after session changes.' },
  { id: 'coordinated-restore', priority: 3, level: 'recovery-rehearsal',
    criterion: 'Isolated recovery of one fenced database/assets/private-uploads/secrets set, content hashes and private access checks.' },
  { id: 'formal-assurance', priority: 3, level: 'formal-report',
    criterion: 'Retain requirement/refinement status, proof scope and release decision without upgrading model proofs to whole-system correctness.',
    entrypoint: 'python scripts/verification-report.py --output NEW_EXTERNAL_DIRECTORY' },
]);

const sha = value => typeof value === 'string' && /^[a-f0-9]{40}$/.test(value);
const digest = value => typeof value === 'string' && /^[a-f0-9]{64}$/.test(value);
const nonempty = value => typeof value === 'string' && value.trim().length > 0;

export function assessReceipt(check, receipt, identity, now = Date.now()) {
  if (!receipt) return { status: 'missing', reasons: ['No receipt supplied.'] };
  const reasons = [];
  const add = (condition, message) => { if (!condition) reasons.push(message); };
  add(receipt.schemaVersion === 1 && receipt.checkId === check.id, 'Receipt schema or check identity differs.');
  add(receipt.level === check.level, 'Evidence level cannot substitute for the requested verification.');
  add(receipt.result === 'passed', 'Execution is not recorded as passed.');
  add(sha(receipt.rootRevision) && receipt.rootRevision === identity.rootRevision, 'Root revision differs.');
  add(digest(receipt.sourceDigest) && receipt.sourceDigest === identity.sourceDigest, 'Source fingerprint differs.');
  if (check.mobile) {
    add(sha(receipt.mobileRevision) && receipt.mobileRevision === identity.mobilePin, 'Mobile receipt differs from the parent gitlink.');
  }
  const when = Date.parse(receipt.completedAt);
  add(nonempty(receipt.completedAt) && /(?:Z|[+-]\d\d:\d\d)$/.test(receipt.completedAt)
    && Number.isFinite(when) && when <= now, 'Completion timestamp is missing, invalid or in the future.');
  add(nonempty(receipt.provenance), 'Execution provenance is missing.');
  add(nonempty(receipt.scope), 'Verification scope is missing.');
  add(Array.isArray(receipt.limitations) && receipt.limitations.every(nonempty), 'Explicit limitations array is required.');
  const counts = receipt.counts;
  add(counts && Number.isInteger(counts.passed) && counts.passed > 0
    && counts.failed === 0 && counts.skipped === 0, 'Nonempty execution without failed/skipped cases is required.');
  add(Array.isArray(receipt.artifacts) && receipt.artifacts.length > 0
    && receipt.artifacts.every(a => nonempty(a?.reference) && digest(a?.sha256)), 'Artifact references and SHA-256 digests are required.');
  // Prevent build-only, simulated, or database-only evidence from satisfying acceptance.
  if (check.level === 'device-test') add(receipt.physicalDevice === true, 'Physical-device execution is required.');
  if (['device-test', 'installed-app-test'].includes(check.level)) {
    add(digest(receipt.binarySha256) && receipt.signatureVerified === true, 'Exact binary and signature verification are required.');
  }
  if (['database-test', 'http-test'].includes(check.level)) {
    add(receipt.completeSchema === true && digest(receipt.migrationManifestSha256), 'Complete migrated schema identity is required.');
  }
  if (check.id.includes('concurrency')) {
    add(receipt.concurrentScheduleObserved === true && receipt.negativeControlsPassed === true,
      'Observed concurrency schedules and negative controls are required.');
  }
  if (check.id === 'backend-runtime') {
    add(Number.isFinite(when) && now - when <= 24 * 60 * 60 * 1000, 'Runtime observation is older than the 24-hour review window.');
    add(receipt.observedRevision === identity.rootRevision && digest(receipt.imageSha256)
      && digest(receipt.migrationManifestSha256) && digest(receipt.runtimeConfigSha256),
    'Backend identity, image, migrations and non-secret runtime configuration must be recorded.');
  }
  if (check.id === 'coordinated-restore') {
    add(receipt.isolated === true && nonempty(receipt.recoverySetId)
      && ['database', 'publicAssets', 'privateUploads', 'secrets'].every(k => receipt.restored?.[k] === true)
      && receipt.contentHashesVerified === true && receipt.privateAccessVerified === true,
    'A coordinated isolated restore with content and access checks is required.');
  }
  if (check.id === 'formal-assurance') {
    add(['BLOCKED', 'READY'].includes(receipt.releaseDecision), 'Formal release decision is missing.');
    add(receipt.releaseDecision === 'READY', 'Formal assurance retains a BLOCKED release decision.');
  }
  return { status: reasons.length ? 'inconsistent' : 'receipt-consistent', reasons };
}

export function buildDeliveryMatrix(identity, receipts = {}, { now = Date.now() } = {}) {
  const blockers = [];
  if (!sha(identity.rootRevision) || !digest(identity.sourceDigest)) blockers.push('Root source identity is unavailable.');
  if (!sha(identity.mobilePin)) blockers.push('Committed Mobile gitlink is unavailable.');
  if (identity.rootDirty !== false) blockers.push('Root candidate is dirty or its cleanliness is unknown.');
  if (identity.mobileCheckoutRevision !== identity.mobilePin || identity.mobileDirty !== false) {
    blockers.push('An exact clean Mobile checkout has not been observed.');
  }
  const rows = DELIVERY_CHECKS.map(check => ({ ...check, ...assessReceipt(check, receipts[check.id], identity, now) }));
  for (const row of rows) if (row.status !== 'receipt-consistent') blockers.push(`${row.id}: ${row.status}`);
  return { schemaVersion: 1, generatedAt: new Date(now).toISOString(), identity, rows, blockers,
    status: blockers.length ? 'INCOMPLETE' : 'RECEIPTS_CONSISTENT', releaseAuthorized: false,
    limitations: [
      'Receipt claims are attributed input, not independently authenticated execution or cryptographic attestation.',
      'Artifact digests are recorded, not downloaded or independently recomputed by this inventory.',
      'Fingerprints cover selected material roots; they are not a full compiler dependency graph.',
      'Completeness does not establish formal correctness, store publication, production acceptance or deployment authority.',
    ] };
}
