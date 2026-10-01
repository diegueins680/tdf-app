#!/usr/bin/env node
// Prepare an isolated restore rehearsal using the same source/artifact policy
// as the production release lane. This command never contacts a database.
import fs from 'node:fs/promises';
import path from 'node:path';
import { createHash } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { resolveReleaseContext, verifyImageExists } from './production-release.mjs';


const [sha, recoverySha, directory] = process.argv.slice(2);
if (!sha || !recoverySha || !directory || process.argv.length !== 5) {
  throw new Error('Usage: node scripts/prepare-portable-restore.mjs <full-sha> <recovery-sha> <new-output-directory>');
}
const context = await resolveReleaseContext({
  mode: 'plan', sha, recoverySha, app: 'tdf-hq', dbApp: 'tdf-hq-db', database: 'tdf_hq',
});
// Load the schema builders from the reviewed revision, never the caller's HEAD.
// This module imports only Node built-ins and is loaded without writing source files.
const builderSource = execFileSync('git', ['show', context.sha + ':scripts/lib/production-release.mjs'], { encoding: 'utf8' });
const { buildMigrationBatchSql, buildSchemaPreflightSql, buildSchemaVerificationSql } =
  await import('data:text/javascript;base64,' + Buffer.from(builderSource).toString('base64'));
const image = await verifyImageExists(context.image, context.sha);
const recovery = await verifyImageExists(`diegueins680/tdf-hq:${context.recoverySha}`, context.recoverySha);
const files = {
  'preflight.sql': buildSchemaPreflightSql(),
  'migrations.sql': buildMigrationBatchSql(context.migrations, { sourceCommit: context.sha }),
  'verify.sql': buildSchemaVerificationSql(),
  'security-emergency-preflight.sql': context.securityEmergencyPreflightSql,
};
const report = {
  purpose: 'isolated-restore-rehearsal',
  productionCutoverAuthorizedByThisArtifact: false,
  sha: context.sha,
  image,
  recovery: { sha: context.recoverySha, ...recovery },
  migrations: context.migrations.map(({ id, path: migrationPath, checksum }) => ({ id, path: migrationPath, checksum })),
  files: Object.fromEntries(Object.entries(files).map(([name, content]) => [name, createHash('sha256').update(content).digest('hex')])),
};
// An exclusive directory avoids mixing artifacts from different releases.
await fs.mkdir(directory, { mode: 0o700 });
for (const [name, content] of Object.entries(files)) {
  await fs.writeFile(path.join(directory, name), content, { mode: 0o600, flag: 'wx' });
}
await fs.writeFile(path.join(directory, 'release.json'), `${JSON.stringify(report, null, 2)}\n`, { mode: 0o600, flag: 'wx' });
console.log(JSON.stringify({ sha: context.sha, image: image.resolvedImage, migrations: context.migrations.length, directory }));
