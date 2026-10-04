import assert from 'node:assert/strict';

// libpq connection-string query parameters can override the URI host/database.
// Test runners must validate the full connection, before invoking any SQL.
export function disposablePostgresUrl(value, { ci = false } = {}) {
  assert.equal(typeof value, 'string', 'A disposable database URL is required');
  assert.ok(!value.includes('?') && !value.includes('#'), 'Database URL overrides are forbidden');
  const url = new URL(value);
  assert.ok(['postgres:', 'postgresql:'].includes(url.protocol), 'PostgreSQL URL required');
  assert.ok(['127.0.0.1', 'localhost'].includes(url.hostname)
    || (ci && url.hostname === 'postgres'), 'Only isolated local/CI databases are allowed');
  assert.match(url.pathname, /^\/[a-zA-Z0-9_-]+_test$/, 'Disposable database name required');
  return url;
}
