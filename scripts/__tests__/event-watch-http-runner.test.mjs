import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { spawnSync } from 'node:child_process';

// These are CLI safety protocol controls, never PostgreSQL/HTTP pass evidence.
function fixture(run) {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'tdf-event-watch-runner-'));
  try {
    const commands = path.join(dir, 'commands');
    fs.mkdirSync(commands);
    const calls = path.join(dir, 'calls');
    const environment = path.join(dir, 'environment');
    for (const command of ['createdb', 'dropdb', 'psql']) {
      fs.writeFileSync(path.join(commands, command),
        `#!/bin/sh\nprintf '%s\\n' '${command}' >> '${calls}'\nenv > '${environment}'\nexit 7\n`,
        { mode: 0o700 });
    }
    const output = path.join(dir, 'output');
    const base = { ...process.env, PATH: commands + path.delimiter + process.env.PATH,
      TDF_TEST_POSTGRES_PASSWORD: '' };
    for (const name of ['PGHOSTADDR', 'PGSERVICE', 'PGSERVICEFILE']) delete base[name];
    const invoke = (url, extra = {}) => spawnSync('python3', [
      'scripts/test-event-watch-http.py', '--binary', '/usr/bin/true',
      '--database-url', url, '--output', output,
    ], { env: { ...base, ...extra }, encoding: 'utf8', timeout: 15000 });
    run({ invoke, output, calls, environment });
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

test('service HTTP runner rejects remote/query routes before creating files or databases', () => {
  fixture(({ invoke, output, calls }) => {
    for (const url of ['postgresql://fixture@remote.example/fixture_test',
      'postgresql://fixture@127.0.0.1/fixture_test?host=remote.example']) {
      assert.notEqual(invoke(url).status, 0);
      assert.equal(fs.existsSync(output), false);
      assert.equal(fs.existsSync(calls), false);
    }
  });
});

test('service HTTP runner rejects inherited service routing before database creation', () => {
  fixture(({ invoke, output, calls }) => {
    for (const name of ['PGHOSTADDR', 'PGSERVICE', 'PGSERVICEFILE']) {
      assert.notEqual(invoke('postgresql://fixture@127.0.0.1/fixture_test',
        { [name]: 'synthetic-routing-override' }).status, 0);
      assert.equal(fs.existsSync(output), false);
      assert.equal(fs.existsSync(calls), false);
    }
  });
});

test('failed database creation never claims ownership or attempts deletion', () => {
  fixture(({ invoke, output, calls }) => {
    const result = invoke('postgresql://fixture@127.0.0.1/fixture_test');
    assert.notEqual(result.status, 0);
    assert.equal(fs.readFileSync(calls, 'utf8'), 'createdb\n');
    const report = JSON.parse(fs.readFileSync(path.join(output, 'result.json'), 'utf8'));
    assert.equal(report.result, 'FAIL');
    assert.equal(report.ownedDatabaseDropped, false);
    assert.equal(report.ownedServerStopped, true);
    assert.match(report.primaryFailure.message, /Owned fixture command failed/);
  });
});

test('child database commands strip libpq options/password files and unrelated provider secrets', () => {
  fixture(({ invoke, calls, environment }) => {
    const result = invoke('postgresql://fixture@127.0.0.1/fixture_test', {
      PGHOST: 'synthetic-foreign-host', PGDATABASE: 'foreign_database',
      PGOPTIONS: '-c search_path=synthetic', PGPASSWORD: 'synthetic-inherited-password',
      PGPASSFILE: '/synthetic/foreign/passwords', SMTP_PASSWORD: 'synthetic-provider-secret',
      TDF_TEST_POSTGRES_PASSWORD: '',
    });
    assert.notEqual(result.status, 0);
    assert.equal(fs.readFileSync(calls, 'utf8'), 'createdb\n');
    const lines = fs.readFileSync(environment, 'utf8').split('\n');
    assert.ok(lines.includes('PGPASSFILE=/dev/null'));
    assert.ok(lines.includes('PGCONNECT_TIMEOUT=5'));
    for (const name of ['PGHOST', 'PGDATABASE', 'PGOPTIONS', 'PGPASSWORD', 'SMTP_PASSWORD']) {
      assert.equal(lines.some(line => line.startsWith(name + '=')), false, name);
    }
  });
});
