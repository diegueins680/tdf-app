import assert from 'node:assert/strict';
import { mkdtempSync, writeFileSync, readFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import test from 'node:test';

const script = path.resolve('tdf-hq/scripts/create_artists_from_list.sh');
function fixture(fn) {
  const dir = mkdtempSync(path.join(tmpdir(), 'tdf-artist-import-test-'));
  try {
    const log = path.join(dir, 'requests.jsonl');
    const types = path.join(dir, 'types.json');
    writeFileSync(types, JSON.stringify(Array.from({ length: 31 }, (_, i) => i % 2 === 0)));
    writeFileSync(path.join(dir, 'curl'), `#!/usr/bin/env python3
import json, os, sys
args = sys.argv[1:]
headers = [args[i+1] for i, v in enumerate(args[:-1]) if v == '-H']
key = next(h.split(': ', 1)[1] for h in headers if h.startswith('Idempotency-Key:'))
body = json.loads(args[args.index('--data-raw')+1])
assert args[args.index('-X')+1] == 'POST'
assert any(a.endswith('/parties') for a in args)
assert '--fail-with-body' in args
with open(os.environ['IMPORT_TEST_LOG'], 'a') as f:
    f.write(json.dumps({'key': key, 'body': body})+'\\n')
if os.environ.get('IMPORT_TEST_FAIL') == '1':
    sys.exit(22)
print(json.dumps({'partyId': 900+int(key.rsplit('-',1)[1])}))
`, { mode: 0o700 });
    const env = { ...process.env, PATH: `${dir}:${process.env.PATH}`, ADMIN_TOKEN: 'synthetic-only', ARTIST_ENTITY_TYPES_FILE: types, IMPORT_TEST_LOG: log, BASE_URL: 'https://invalid.test' };
    const run = (extra = {}) => spawnSync('bash', [script], { env: { ...env, ...extra }, encoding: 'utf8' });
    fn({ run, log, types });
  } finally { rmSync(dir, { recursive: true, force: true }); }
}

test('import retries preserve source keys, explicit entity types, and avoid profile upserts', () => fixture(({ run, log }) => {
  for (let i = 0; i < 2; i++) { const result = run(); assert.equal(result.status, 0, result.stderr); }
  const requests = readFileSync(log, 'utf8').trim().split('\n').map(JSON.parse);
  assert.equal(requests.length, 62);
  assert.deepEqual(requests.slice(0, 31), requests.slice(31));
  assert.equal(new Set(requests.map(r => r.key)).size, 31);
  requests.slice(0,31).forEach((r, i) => assert.equal(r.body.cIsOrg, i % 2 === 0));
}));

test('an HTTP error stops the batch before another record', () => fixture(({ run, log }) => {
  assert.notEqual(run({ IMPORT_TEST_FAIL: '1' }).status, 0);
  assert.equal(readFileSync(log, 'utf8').trim().split('\n').length, 1);
}));

test('missing reviewed entity types reject the import before network access', () => fixture(({ run }) => {
  assert.notEqual(run({ ARTIST_ENTITY_TYPES_FILE: '' }).status, 0);
}));
