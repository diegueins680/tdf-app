import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, writeFileSync, readFileSync, existsSync, chmodSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { spawnSync } from 'node:child_process';

const root = new URL('../../', import.meta.url).pathname;
const fixturePassword = 'synthetic-local-database-password';

function inspectDatabaseClient({ ci, native, password }) {
  const directory = mkdtempSync(path.join(tmpdir(), 'tdf-memory-password-control-'));
  const capture = path.join(directory, 'client.json');
  const client = path.join(directory, 'createdb');
  // Stop at the first database command: no actual database, server, media,
  // network request or application can be started by this control.
  writeFileSync(client, `#!${process.execPath}\nrequire('node:fs').writeFileSync(${JSON.stringify(capture)}, JSON.stringify({ present: Object.hasOwn(process.env, 'PGPASSWORD'), matches: process.env.PGPASSWORD === ${JSON.stringify(fixturePassword)}, args: process.argv.slice(2) })); process.stderr.write('Synthetic control stops before database allocation\\n'); process.exit(73);\n`);
  chmodSync(client, 0o700);
  const environment = { ...process.env, PATH: `${directory}${path.delimiter}${process.env.PATH}`, GITHUB_ACTIONS: ci ? 'true' : '', TDF_TEST_NATIVE_POSTGRES: native ? '1' : '', TDF_TEST_NATIVE_POSTGRES_PORT: '55486', TDF_TEST_NATIVE_POSTGRES_USER: 'postgres', PGPASSWORD: 'ambient-password-must-not-be-inherited' };
  for (const key of ['PGHOSTADDR', 'PGSERVICE', 'PGSERVICEFILE', 'TDF_TEST_POSTGRES_PASSWORD']) delete environment[key];
  if (password !== undefined) environment.TDF_TEST_POSTGRES_PASSWORD = password;
  try {
    const result = spawnSync('python3', ['scripts/test-event-memories-http.py', '--binary', process.execPath, '--output', path.join(directory, 'output')], { cwd: root, env: environment, encoding: 'utf8', timeout: 30_000 });
    assert.equal(result.signal, null, result.stderr);
    assert.equal(result.status, 1, result.stderr);
    return { stderr: result.stderr, client: existsSync(capture) ? JSON.parse(readFileSync(capture, 'utf8')) : null };
  } finally { rmSync(directory, { recursive: true, force: true }); }
}

test('local Docker/native fixture forwards its explicitly supplied password', () => {
  const { client, stderr } = inspectDatabaseClient({ ci: false, native: true, password: fixturePassword });
  assert.match(stderr, /Synthetic control stops before database allocation/);
  assert.equal(client.present, true);
  assert.equal(client.matches, true);
  assert.equal(client.args[client.args.indexOf('-h') + 1], '127.0.0.1');
});

test('passwordless native execution does not inherit ambient database credentials', () => {
  const { client } = inspectDatabaseClient({ ci: false, native: true });
  assert.equal(client.present, false);
  assert.equal(client.matches, false);
});

test('GitHub service database retains its explicit fixture password and service host', () => {
  const { client } = inspectDatabaseClient({ ci: true, native: false, password: fixturePassword });
  assert.equal(client.matches, true);
  assert.equal(client.args[client.args.indexOf('-h') + 1], 'postgres');
});

test('GitHub native loopback database retains its explicit fixture password', () => {
  const { client } = inspectDatabaseClient({ ci: true, native: true, password: fixturePassword });
  assert.equal(client.matches, true);
  assert.equal(client.args[client.args.indexOf('-h') + 1], '127.0.0.1');
});

test('an explicitly empty native fixture password fails before database allocation', () => {
  const { client, stderr } = inspectDatabaseClient({ ci: false, native: true, password: '' });
  assert.equal(client, null);
  assert.match(stderr, /explicit nonempty fixture database password is required/);
});
