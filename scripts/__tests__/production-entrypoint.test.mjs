import assert from 'node:assert/strict';
import { mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import test from 'node:test';
import { fileURLToPath } from 'node:url';

const repoRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..', '..');
const entrypoint = path.join(repoRoot, 'tdf-hq', 'production-entrypoint.sh');

function fixture() {
  const directory = mkdtempSync(path.join(tmpdir(), 'tdf-production-entrypoint-'));
  const binDirectory = path.join(directory, 'bin');
  const packagedAssets = path.join(directory, 'packaged-assets');
  const servedAssets = path.join(directory, 'served-assets');
  const migrationSql = path.join(directory, 'migrations.sql');
  const psqlLog = path.join(directory, 'psql.log');
  const serverLog = path.join(directory, 'server.log');
  mkdirSync(binDirectory);
  mkdirSync(path.join(packagedAssets, 'directory', 'profiles'), { recursive: true });
  writeFileSync(
    path.join(packagedAssets, 'directory', 'profiles', 'artist.webp'),
    'packaged artist photo',
  );
  writeFileSync(migrationSql, 'SELECT 1;\n');
  writeFileSync(
    path.join(binDirectory, 'psql'),
    `#!/bin/sh\nprintf '%s\\n' "$*" > "${psqlLog}"\n`,
    { mode: 0o755 },
  );
  writeFileSync(
    path.join(binDirectory, 'server'),
    `#!/bin/sh\nprintf 'RUN_MIGRATIONS=%s\\nAPP_PORT=%s\\n' "$RUN_MIGRATIONS" "$APP_PORT" > "${serverLog}"\n`,
    { mode: 0o755 },
  );
  return {
    directory,
    migrationSql,
    packagedAssets,
    psqlLog,
    servedAssets,
    serverBin: path.join(binDirectory, 'server'),
    serverLog,
    path: `${binDirectory}:${process.env.PATH}`,
  };
}

function run(current, overrides = {}) {
  return spawnSync(entrypoint, [], {
    encoding: 'utf8',
    env: {
      ...process.env,
      PATH: current.path,
      DATABASE_URL: 'postgresql://test:test@localhost:5432/test',
      TDF_PRODUCTION_MIGRATIONS_SQL: current.migrationSql,
      TDF_SERVER_BIN: current.serverBin,
      AUTO_APPLY_PRODUCTION_MIGRATIONS: 'true',
      RUN_MIGRATIONS: 'false',
      PORT: '18881',
      ...overrides,
    },
  });
}

test('production entrypoint applies reviewed SQL then disables Persistent migrations', (context) => {
  const current = fixture();
  context.after(() => rmSync(current.directory, { recursive: true, force: true }));

  const result = run(current);

  assert.equal(result.status, 0, result.stderr);
  assert.match(result.stdout, /schema verification passed/i);
  assert.match(readFileSync(current.psqlLog, 'utf8'), /-X -v ON_ERROR_STOP=1 -f/);
  assert.equal(readFileSync(current.serverLog, 'utf8'), 'RUN_MIGRATIONS=false\nAPP_PORT=18881\n');
});

test('production entrypoint copies packaged assets into the served asset volume', (context) => {
  const current = fixture();
  context.after(() => rmSync(current.directory, { recursive: true, force: true }));

  const result = run(current, {
    HQ_ASSETS_DIR: current.servedAssets,
    TDF_PACKAGED_ASSETS_DIR: current.packagedAssets,
  });

  assert.equal(result.status, 0, result.stderr);
  assert.match(result.stdout, /packaged assets synchronized/i);
  assert.equal(
    readFileSync(path.join(current.servedAssets, 'directory', 'profiles', 'artist.webp'), 'utf8'),
    'packaged artist photo',
  );
});

test('production entrypoint skips asset self-copy for equivalent paths', (context) => {
  const current = fixture();
  context.after(() => rmSync(current.directory, { recursive: true, force: true }));

  const result = run(current, {
    HQ_ASSETS_DIR: `${current.packagedAssets}/`,
    TDF_PACKAGED_ASSETS_DIR: current.packagedAssets,
  });

  assert.equal(result.status, 0, result.stderr);
  assert.doesNotMatch(result.stdout, /packaged assets synchronized/i);
  assert.equal(
    readFileSync(path.join(current.packagedAssets, 'directory', 'profiles', 'artist.webp'), 'utf8'),
    'packaged artist photo',
  );
});

test('production entrypoint rejects inferred and reviewed migrations together', (context) => {
  const current = fixture();
  context.after(() => rmSync(current.directory, { recursive: true, force: true }));

  const result = run(current, { RUN_MIGRATIONS: 'true' });

  assert.equal(result.status, 64);
  assert.match(result.stderr, /refusing to combine/i);
});

test('production entrypoint rejects invalid flags and a missing bundle', (context) => {
  const current = fixture();
  context.after(() => rmSync(current.directory, { recursive: true, force: true }));

  const invalid = run(current, { AUTO_APPLY_PRODUCTION_MIGRATIONS: 'yes' });
  assert.equal(invalid.status, 64);
  assert.match(invalid.stderr, /must be true or false/i);

  const missing = run(current, { TDF_PRODUCTION_MIGRATIONS_SQL: path.join(current.directory, 'missing.sql') });
  assert.equal(missing.status, 66);
  assert.match(missing.stderr, /missing or unreadable/i);
});
