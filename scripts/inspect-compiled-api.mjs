import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { sourceManifest } from './lib/verification-evidence.mjs';
import { compiledApiSurface, compareApiSurface, compiledApiDeclarationSnapshot, verifyCompiledApiDeclarationSnapshot, verifyApiAvailability } from './lib/compiled-api-surface.mjs';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const args = process.argv.slice(2);
if (args.length !== 4 || args[0] !== '--binary' || args[2] !== '--output') {
  throw new Error('Usage: node scripts/inspect-compiled-api.mjs --binary PATH --output NEW_DIRECTORY');
}
const binary = path.resolve(args[1]), output = path.resolve(args[3]);
mkdirSync(output, { mode: 0o700 });
const sha256 = value => createHash('sha256').update(value).digest('hex');
const git = (...flags) => execFileSync('git', flags, { cwd: root, encoding: 'utf8' }).trim();
const before = sourceManifest(root);
const revision = git('rev-parse', 'HEAD'), tree = git('rev-parse', 'HEAD^{tree}');
const sourceWorktreeDirty = Boolean(git('status', '--porcelain'));
const bytes = readFileSync(binary), binarySha256 = sha256(bytes);
// A deliberately unusable database and no inherited credentials: description
// must terminate without loading runtime configuration, connecting or serving.
const raw = execFileSync(binary, ['--describe-api'], {
  cwd: output, env: { PATH: process.env.PATH, LANG: 'C.UTF-8', DATABASE_URL: 'invalid://contract-must-not-connect', APP_PORT: 'invalid' },
  encoding: 'utf8', timeout: 60_000, maxBuffer: 32 * 1024 * 1024,
});
writeFileSync(path.join(output, 'compiled-types.json'), raw, { flag: 'wx', mode: 0o600 });
const description = JSON.parse(raw), surface = compiledApiSurface(description);
const traceBytes = readFileSync(path.join(root, 'formal/system/traceability.json'));
const trace = JSON.parse(traceBytes), comparison = compareApiSurface(surface, trace.apiOperations);
// Capture every proof input before the final freshness check. Admission below
// uses these exact bytes, never a later read of a potentially changed baseline.
const snapshotPath = 'formal/system/compiled-api-surface.json';
const snapshotBytes = readFileSync(path.join(root, snapshotPath));
const snapshotSha256 = sha256(snapshotBytes);
const availabilityPath = 'formal/system/api-availability.json';
const availabilityBytes = readFileSync(path.join(root, availabilityPath));
const availabilitySha256 = sha256(availabilityBytes);
if (git('rev-parse', 'HEAD') !== revision || sha256(readFileSync(binary)) !== binarySha256
  || snapshotSha256 !== before.files[snapshotPath]
  || availabilitySha256 !== before.files[availabilityPath]
  || sha256(traceBytes) !== before.files['formal/system/traceability.json']
  || sourceManifest(root).digest !== before.digest) {
  throw new Error('Compiled description provenance changed during inspection');
}
const report = { schemaVersion: 1, sourceRevision: revision, sourceTree: tree, sourceWorktreeDirty,
  binarySha256, sourceDigest: before.digest, snapshotSha256, availabilitySha256, descriptionSha256: sha256(raw), traceabilitySha256: sha256(traceBytes),
  observedAt: new Date().toISOString(), ...surface, comparison,
  status: 'observed-not-conformance-approved',
  limitations: 'Compiler syntax from the supplied executable, not attestation that its binary was built from this checkout. CI binds the build separately. Type names are not JSON schema. Discovery gaps are unresolved obligations.',
};
writeFileSync(path.join(output, 'surface.json'), JSON.stringify(report, null, 2) + '\n', { flag: 'wx', mode: 0o600 });
console.log(JSON.stringify({ revision, binarySha256, operations: surface.operations.length, rawMounts: surface.rawMounts.length,
  undocumented: comparison.undocumented.length, documentedWithoutTypedRoute: comparison.documentedWithoutTypedRoute.length,
  competing: comparison.competingCompiledRoutes.length, statusDifferences: comparison.successStatusDifferences.length,
  status: report.status }));

// Preserve the candidate and full discrepancy report even when drift fails CI.
// Updating this discovery snapshot is a reviewed source change, never an
// automatic declaration that undocumented/unmounted routes are acceptable.
writeFileSync(path.join(output, 'contract-candidate.json'),
  JSON.stringify(compiledApiDeclarationSnapshot(surface), null, 2) + '\n', { flag: 'wx', mode: 0o600 });
if (comparison.competingCompiledRoutes.length) {
  throw new Error('Competing compiled route declarations: disambiguate routing before admitting the snapshot');
}
const availability = verifyApiAvailability(surface, trace.apiOperations, JSON.parse(availabilityBytes));
writeFileSync(path.join(output, 'availability.json'), JSON.stringify(availability, null, 2) + '\n', { flag: 'wx', mode: 0o600 });
verifyCompiledApiDeclarationSnapshot(surface,
  JSON.parse(snapshotBytes));
console.log('Compiled API declaration snapshot matches; documented conformance gaps remain open.');
