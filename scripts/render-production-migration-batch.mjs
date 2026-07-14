#!/usr/bin/env node
import { createHash } from 'node:crypto';
import fs from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

import { buildMigrationBatchSql, normalizeFullSha } from './lib/production-release.mjs';

const rootDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const manifest = JSON.parse(
  await fs.readFile(path.join(rootDir, 'scripts', 'production-migrations.json'), 'utf8'),
);
const migrations = await Promise.all(manifest.migrations.map(async (entry) => {
  const content = await fs.readFile(path.join(rootDir, entry.path), 'utf8');
  return {
    ...entry,
    content,
    checksum: createHash('sha256').update(content).digest('hex'),
  };
}));
const sourceCommit = normalizeFullSha(process.env.SOURCE_COMMIT ?? '0'.repeat(40));

process.stdout.write(`${buildMigrationBatchSql(migrations, { sourceCommit })}\n`);
