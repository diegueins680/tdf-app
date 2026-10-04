import test from 'node:test';
import assert from 'node:assert/strict';
import { disposablePostgresUrl } from '../lib/disposable-postgres-url.mjs';

test('admits named local and explicitly opted-in CI fixtures', () => {
  for (const value of ['postgresql://127.0.0.1/local_test', 'postgres://localhost:5432/local_test']) {
    assert.ok(disposablePostgresUrl(value));
  }
  assert.ok(disposablePostgresUrl('postgresql://postgres:postgres@postgres:5432/tdf_hq_automatic_migration_test', { ci: true }));
});
for (const value of [
  'postgresql://127.0.0.1/local_test?host=remote.example',
  'postgresql://127.0.0.1/local_test?dbname=production',
  'postgresql://127.0.0.1/local_test?service=production',
  'postgresql://localhost/local_test#ignored',
  'postgresql://remote.example/local_test',
  'postgresql://localhost/production',
  'postgresql://localhost/path/local_test',
  'postgresql://localhost/%70roduction_test',
  'https://localhost/local_test',
  'host=localhost dbname=production',
]) test(`rejects unsafe fixture connection form: ${value}`, () => {
  assert.throws(() => disposablePostgresUrl(value, { ci: true }));
});
test('CI service name is not a local default', () => {
  assert.throws(() => disposablePostgresUrl('postgresql://postgres/local_test'));
});
