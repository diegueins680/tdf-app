import assert from 'node:assert/strict';
import { test } from 'node:test';
import { mkdtempSync, writeFileSync, existsSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

for (const [label, scriptName, variable, database] of [
  ['ticket admission', 'test-ticket-admission.sh', 'TICKET_ADMISSION_TEST_DSN', 'tdf_ticket_admission_test'],
  ['ticket manual review', 'test-ticket-manual-review.sh', 'TICKET_MANUAL_REVIEW_TEST_DSN', 'tdf_ticket_manual_review_test'],
  ['merchandise', 'test-artist-merch-runtime.sh', 'TDF_MERCH_RUNTIME_DATABASE_URL', 'merch_runtime_test'],
  ['provider retry', 'test-provider-retry-runtime.sh', 'TDF_PROVIDER_RETRY_DATABASE_URL', 'tdf_provider_retry_test'],
]) test(`actual ${label} fixture runner validates routing before its first SQL command`, () => {
  const directory = mkdtempSync(path.join(tmpdir(), 'tdf-merch-routing-'));
  const marker = path.join(directory, 'sql-was-called');
  writeFileSync(path.join(directory, 'psql'), '#!/bin/sh\n: > "$SQL_MARKER"\nexit 97\n', { mode: 0o700 });
  // Stop connection retry loops immediately after the SQL sentinel was reached.
  writeFileSync(path.join(directory, 'sleep'), '#!/bin/sh\nexit 97\n', { mode: 0o700 });
  const script = fileURLToPath(new URL(`../${scriptName}`, import.meta.url));
  const invoke = (url, extra = {}) => spawnSync('bash', [script], {
    encoding: 'utf8', env: { PATH: `${directory}:${process.env.PATH}`, SQL_MARKER: marker,
      [variable]: url, ...extra },
  });
  try {
    for (const [url, extra] of [
      [`postgresql://example.invalid/${database}`, {}],
      [`postgresql://localhost/${database}?host=example.invalid`, {}],
      [`postgresql://localhost/${database}`, { PGHOSTADDR: '192.0.2.1' }],
      [`postgresql://localhost/${database}`, { PGSERVICE: 'remote' }],
      [`postgresql://localhost/${database}`, { PGSERVICEFILE: '/synthetic/service' }],
    ]) {
      const r = invoke(url, extra);
      assert.notEqual(r.status, 0);
      assert.equal(existsSync(marker), false, 'invalid routing must never reach psql');
    }
    if (label === 'ticket admission' || label === 'ticket manual review') {
      for (const url of [`postgresql://localhost:5433/${database}`, `postgresql://user@localhost/${database}`]) {
        assert.notEqual(invoke(url).status, 0);
        assert.equal(existsSync(marker), false, 'unsupported direct-harness URL must fail before fixture setup');
      }
    }
    const allowed = invoke(`postgresql://127.0.0.1/${database}`);
    assert.equal(allowed.status, 97, allowed.stderr);
    assert.equal(existsSync(marker), true, 'valid loopback input reaches the deliberately stopped SQL stub');
  } finally { rmSync(directory, { recursive: true, force: true }); }
});
