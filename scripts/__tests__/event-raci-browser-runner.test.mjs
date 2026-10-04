import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { spawnSync } from 'node:child_process';
const root = new URL('../../', import.meta.url);
const read = path => readFileSync(new URL(path, root), 'utf8');

test('browser launch and config fail closed outside the disposable runner', () => {
  const env = { ...process.env, EVENT_RACI_DISPOSABLE_BROWSER_TEST: '', EVENT_RACI_UI_ORIGIN: '' };
  for (const file of ['scripts/run-event-raci-browser.mjs', 'playwright.event-raci-browser.config.mjs']) {
    const result = spawnSync(process.execPath, [new URL(file, root).pathname], { env, encoding: 'utf8', timeout: 20_000 });
    assert.equal(result.status, 1, result.stderr);
    assert.match(result.stderr, /disposable|do not reuse/);
  }
});

test('database is owned, loopback-only, freshly migrated, and cleaned by exact container ID', () => {
  const runner = read('scripts/test-event-raci-browser.sh');
  assert.match(runner, /docker run --rm -d -p 127\.0\.0\.1::5432/);
  assert.match(runner, /until docker exec "\$test_container_id" psql -h 127\.0\.0\.1/);
  assert.match(runner, /unix:\/\/\/\*/);
  assert.match(runner, /export DOCKER_HOST="\$test_docker_endpoint"/);
  assert.match(runner, /unset DOCKER_CONTEXT/);
  assert.match(runner, /docker rm -f "\$test_container_id"/);
  assert.match(runner, /trap cleanup EXIT/);
  assert.match(runner, /test_exit_code=\$\?/);
  assert.match(runner, /Failed to remove owned test container/);
  assert.doesNotMatch(runner, /docker rm[^\n]*\|\| true/);
  assert.match(runner, /EVENT_OPERATIONS_TEST_DSN="host=127\.0\.0\.1/);
  assert.match(runner, /stack path --dist-dir/);
  assert.match(runner, /test -f "\$test_autogen\/Paths_tdf_hq.hs"/);
  assert.doesNotMatch(runner, /\$\{(?:DATABASE_URL|EVENT_OPERATIONS_TEST_DSN)|source .*env|dropdb/);
  const prerequisite = runner.indexOf('sql/2026-09-15_event_raci_editor_context.sql');
  assert(prerequisite > runner.indexOf('sql/2026-09-15_event_raci_reassignment.sql'));
  assert(runner.indexOf('test/integration/event_raci_browser_fixture.sql') > prerequisite);
});

test('remote Docker endpoints fail before build or container creation', () => {
  for (const endpoint of ['ssh://unreachable.invalid', 'tcp://unreachable.invalid:2376', 'tcp://127.0.0.1:2375']) {
    const result = spawnSync('/bin/sh', [new URL('scripts/test-event-raci-browser.sh', root).pathname], {
      env: { ...process.env, DOCKER_CONTEXT: '', DOCKER_HOST: endpoint }, encoding: 'utf8', timeout: 10_000,
    });
    assert.equal(result.status, 1, result.stderr);
    assert.match(result.stderr, /local Unix-socket Docker endpoint is required/);
    assert.doesNotMatch(result.stdout, /Compiling|PostgreSQL/);
  }
});

test('real handlers are composed without production startup or invented sessions', () => {
  const main = read('tdf-hq/test/EventRaciBrowserMain.hs');
  for (const name of ['Auth.sessionServer', 'authContext env', 'eventOperationsServer', 'httpTestConfig',
    'Warp.setHost "127.0.0.1"', 'exitWith result']) assert(main.includes(name));
  assert.doesNotMatch(main, /loadConfig|mkApp|runMigrations|SessionResponse\s*\{/);
  const vite = read('scripts/run-event-raci-browser.mjs');
  assert.match(vite, /envFile: false/);
  assert.match(vite, /envPrefix: \[\]/);
  assert.match(vite, /server\.close\(\)/);
  assert.match(vite, /maxRedirects|proxy:/);
  assert.match(read('playwright.event-raci-browser.config.mjs'), /testDir: '\.\/e2e\/integration'/);
});

test('faults discard actual commits, never manufacture success receipts', () => {
  const spec = read('e2e/integration/event-raci-browser.spec.mjs');
  assert.match(spec, /route\.fetch\(\{ maxRetries: 0, maxRedirects: 0 \}\)/);
  assert.match(spec, /route\.fulfill\(\{ response \}\)/);
  assert(spec.includes('page.route(new URL(`${target.path}/raci/reassign`, baseURL).href'));
  assert.doesNotMatch(spec, /route\.fulfill\(\{\s*(json|body|status)/);
  for (const expected of ['replayed: true', 'toBe(409)', 'toBe(401)', 'toBe(403)', 'toBe(404)',
    'expect(attempts[1]).toEqual(attempts[0])', 'event_operation_audit_event',
    'url.origin !== origin', 'expect(persisted(target)).toEqual(committed)']) assert(spec.includes(expected));
});

test('dedicated CI runs owned database and desktop/phone browsers without relaxing existing gates', () => {
  const workflow = read('.github/workflows/event-raci-browser.yml');
  for (const command of ['stack build tdf-hq:exe:tdf-hq-exe', 'playwright install --with-deps chromium',
    'sh scripts/test-event-raci-browser.sh', 'node --test scripts/__tests__/event-raci-browser-runner.test.mjs']) {
    assert(workflow.includes(command));
  }
  assert.match(workflow, /contents: read/);
  assert.doesNotMatch(workflow, /continue-on-error|pull_request_target/);
});
