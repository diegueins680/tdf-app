import assert from 'node:assert/strict';
import { test } from 'node:test';
import { mkdtempSync, writeFileSync, existsSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

test('actual merchandise fixture runner validates routing before its first SQL command', () => {
  const directory = mkdtempSync(path.join(tmpdir(), 'tdf-merch-routing-'));
  const marker = path.join(directory, 'sql-was-called');
  writeFileSync(path.join(directory, 'psql'), '#!/bin/sh\n: > "$SQL_MARKER"\nexit 97\n', { mode: 0o700 });
  const script = fileURLToPath(new URL('../test-artist-merch-runtime.sh', import.meta.url));
  const invoke = (url, extra = {}) => spawnSync('sh', [script], {
    encoding: 'utf8', env: { PATH: `${directory}:${process.env.PATH}`, SQL_MARKER: marker,
      TDF_MERCH_RUNTIME_DATABASE_URL: url, ...extra },
  });
  try {
    for (const [url, extra] of [
      ['postgresql://example.invalid/merch_runtime_test', {}],
      ['postgresql://localhost/merch_runtime_test?host=example.invalid', {}],
      ['postgresql://localhost/merch_runtime_test', { PGHOSTADDR: '192.0.2.1' }],
      ['postgresql://localhost/merch_runtime_test', { PGSERVICE: 'remote' }],
      ['postgresql://localhost/merch_runtime_test', { PGSERVICEFILE: '/synthetic/service' }],
    ]) {
      const r = invoke(url, extra);
      assert.notEqual(r.status, 0);
      assert.equal(existsSync(marker), false, 'invalid routing must never reach psql');
    }
    const allowed = invoke('postgresql://127.0.0.1/merch_runtime_test');
    assert.equal(allowed.status, 97, allowed.stderr);
    assert.equal(existsSync(marker), true, 'valid loopback input reaches the deliberately stopped SQL stub');
  } finally { rmSync(directory, { recursive: true, force: true }); }
});
