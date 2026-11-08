import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, writeFile, readFile, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { spawnSync } from 'node:child_process';

async function fixture(body) {
  const folder = await mkdtemp(join(tmpdir(), 'tdf-video-pg-runner-'));
  const trace = join(folder, 'commands.jsonl');
  try {
    for (const command of ['createdb', 'dropdb', 'stack', 'bash']) {
      await writeFile(join(folder, command), `#!${process.execPath}
const fs=require('node:fs');
const command=${JSON.stringify(command)},args=process.argv.slice(2);
fs.appendFileSync(process.env.VIDEO_TRACE,JSON.stringify({command,args,identity:process.env.TDF_IDENTITY_TEST_DATABASE_URL,manager:process.env.TDF_VIDEO_CAMPAIGN_MANAGER_TEST_DB,user:process.env.PGUSER,password:process.env.PGPASSWORD,host:process.env.PGHOST,port:process.env.PGPORT})+'\\n');
if(command==='createdb')process.exit(Number(process.env.VIDEO_CREATE_EXIT||0));
if(command==='stack'){console.log(process.env.VIDEO_SUMMARY);process.exit(Number(process.env.VIDEO_STACK_EXIT||0));}
if(command==='bash')process.exit(Number(process.env.VIDEO_IDENTITY_EXIT||0));
`, { mode: 0o700 });
    }
    const clean = { ...process.env, PATH: `${folder}:${process.env.PATH}`, CI: 'false', PGHOST: '127.0.0.1', PGPORT: '5432',
      PGUSER: "synthetic 'role host=remote.example", PGPASSWORD: "synthetic 'password dbname=production",
      TDF_STACK_FLAGS: '', TDF_IDENTITY_TEST_DATABASE_URL: 'postgresql://remote/production',
      TDF_VIDEO_CAMPAIGN_MANAGER_TEST_DB: 'postgresql://remote/production',
      VIDEO_TRACE: trace, VIDEO_SUMMARY: '1 example, 0 failures', VIDEO_CREATE_EXIT: '0', VIDEO_STACK_EXIT: '0', VIDEO_IDENTITY_EXIT: '0' };
    for (const key of ['PGHOSTADDR', 'PGSERVICE', 'PGSERVICEFILE']) delete clean[key];
    const run = async (overrides = {}) => {
      await writeFile(trace, '');
      const result = spawnSync('/bin/bash', ['scripts/test-video-campaign-postgres.sh'], { encoding: 'utf8', env: { ...clean, ...overrides }, timeout: 20000 });
      assert.equal(result.error, undefined, 'Only the bounded synthetic command stubs may run');
      const text = (await readFile(trace, 'utf8')).trim();
      return { result, calls: text ? text.split('\n').map(row => JSON.parse(row)) : [] };
    };
    await body(run, clean);
  } finally { await rm(folder, { recursive: true, force: true }); }
}

test('unsafe libpq routes are rejected before any database, SQL, canonical driver or Stack command', () => fixture(async run => {
  for (const overrides of [
    { PGHOST: 'remote.example' }, { PGHOST: 'localhost host=remote.example' }, { PGHOST: 'postgres' },
    { PGHOST: 'synthetic@localhost' }, { PGHOST: 'synthetic:password@localhost' }, { PGHOST: 'localhost:5432' },
    { PGHOSTADDR: '192.0.2.1' }, { PGSERVICE: 'production' }, { PGSERVICEFILE: '/untrusted/service' },
    { PGPORT: '0' }, { PGPORT: '65536' }, { PGPORT: '5432?host=remote.example' },
    { PGPORT: '5432@127.0.0.1' }, { PGPORT: '1e3' }, { PGPORT: '+5432' }, { PGPORT: ' 5432' }, { PGPORT: '05432' },
  ]) {
    const { result, calls } = await run(overrides);
    assert.notEqual(result.status, 0); assert.deepEqual(calls, []);
  }
}));

test('failed create never drops or reuses a pre-existing database', () => fixture(async run => {
  const { result, calls } = await run({ VIDEO_CREATE_EXIT: '73' });
  assert.equal(result.status, 73);
  assert.deepEqual(calls.map(call => call.command), ['createdb']);
}));

test('owned runner preserves the owner canonical identity driver before the exact manager group', () => fixture(async (run, clean) => {
  const owned = new Set();
  for (const overrides of [{}, { PGHOST: 'localhost' }, { PGHOST: 'postgres', CI: 'true' }, { PGPORT: '6432' }]) {
    const { result, calls } = await run(overrides);
    assert.equal(result.status, 0, result.stderr);
    const database = calls[0].args[0];
    assert.match(database, /^tdf_video_campaign_[0-9]+_test$/);
    assert.ok(!owned.has(database)); owned.add(database);
    assert.deepEqual(calls[0].args, [database]);
    const drop = calls.at(-1); assert.equal(drop.command, 'dropdb'); assert.deepEqual(drop.args, ['--if-exists', database]);
    const driver = calls.find(call => call.command === 'bash'); assert.deepEqual(driver.args, ['scripts/test-identity-reconciliation.sh']);
    const stack = calls.find(call => call.command === 'stack');
    assert.ok(stack.args.includes('--test-arguments=--match "video campaign current directory grants" --fail-on=pending'));
    assert.equal(stack.manager, stack.identity);
    const url = new URL(stack.identity); assert.equal(url.hostname, overrides.PGHOST || clean.PGHOST);
    assert.equal(url.port, overrides.PGPORT || clean.PGPORT);
    assert.equal(url.pathname, `/${database}`); assert.equal(url.username, ''); assert.equal(url.password, ''); assert.equal(url.search, '');
    for (const call of calls) {
      assert.equal(call.user, clean.PGUSER); assert.equal(call.password, clean.PGPASSWORD);
      assert.equal(call.host, url.hostname); assert.equal(call.port, url.port);
    }
    assert.deepEqual(calls.map(call => call.command), ['createdb', 'bash', 'stack', 'dropdb']);
  }
}));

test('a failed owner canonical identity suite stops before manager execution and cleans only its owned database', () => fixture(async run => {
  const { result, calls } = await run({ VIDEO_IDENTITY_EXIT: '47' });
  assert.equal(result.status, 47);
  assert.deepEqual(calls.map(call => call.command), ['createdb', 'bash', 'dropdb']);
  assert.deepEqual(calls.at(-1).args, ['--if-exists', calls[0].args[0]]);
}));

test('coloured positive summaries preserve raw bytes while zero, pending and failed execution cannot pass', () => fixture(async run => {
  for (const [summary, status, pass] of [
    ['\u001b[32m1 example, 0 failures\u001b[0m', '0', true],
    ['\u001b[1;32m12 examples, 0 failures\u001b[0m', '0', true],
    ['0 examples, 0 failures', '0', false], ['no examples', '0', false],
    ['1 example, 0 failures, 1 pending', '0', false], ['1 example, 1 failure', '0', false],
    ['1 example, 0 failures', '1', false],
  ]) {
    const { result, calls } = await run({ VIDEO_SUMMARY: summary, VIDEO_STACK_EXIT: status });
    assert.equal(result.status === 0, pass, result.stderr);
    assert.ok(result.stdout.endsWith(`${summary}\n`), 'Raw Stack summary remains intact');
    assert.equal(calls.at(-1).command, 'dropdb', 'Owned cleanup runs after all completed creations');
  }
}));
