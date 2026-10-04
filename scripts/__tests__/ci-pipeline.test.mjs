import assert from 'node:assert/strict';
import { readFile, readdir } from 'node:fs/promises';
import path from 'node:path';
import test from 'node:test';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');

test('PostgreSQL runner uses the CI service and deletes only its newly created test database', () => {
  const script = `
    . "$1"
    createdb() { test "$PGPASSWORD" = "$TDF_TEST_POSTGRES_PASSWORD"; echo "createdb $*"; return "$CREATE_RESULT"; }
    psql() { echo "psql $*"; }
    dropdb() { echo "dropdb $*"; }
    docker() { echo unexpected-docker; exit 97; }
    tdf_test_db_init tdf_owned_test
    tdf_test_db_cleanup
  `;
  const invoke = (createResult) => spawnSync('sh', ['-eu', '-c', script, 'runner', path.join(root, 'scripts/lib/postgres-test-database.sh')], {
    encoding: 'utf8',
    env: { PATH: process.env.PATH, GITHUB_ACTIONS: 'true', TDF_TEST_POSTGRES_HOST: 'postgres', TDF_TEST_POSTGRES_PASSWORD: 'synthetic-test-only', CREATE_RESULT: createResult },
  });
  const owned = invoke('0');
  assert.equal(owned.status, 0, owned.stderr);
  assert.match(owned.stdout, /createdb -h postgres -U postgres tdf_owned_test/);
  assert.equal((owned.stdout.match(/dropdb/g) ?? []).length, 1);
  assert.doesNotMatch(owned.stdout, /unexpected-docker/);
  const existing = invoke('17');
  assert.equal(existing.status, 17);
  assert.doesNotMatch(existing.stdout, /dropdb|unexpected-docker/);
});

async function source(relativePath) {
  return readFile(path.join(root, relativePath), 'utf8');
}

test('UI quality keeps the lazy-validation regression and production artifact gate', async () => {
  const quality = await source('scripts/quality-ui.sh');
  assert.match(quality, /node --test "\$ROOT\/scripts\/__tests__\/ui-validation-bundle\.test\.mjs"/);
  assert.match(quality, /run_npm run build --workspace=tdf-hq-ui/);
  const ui = JSON.parse(await source('tdf-hq-ui/package.json'));
  assert.match(ui.scripts.build, /node scripts\/check-initial-bundle\.mjs/);
});

test('backend CI retains event operations HTTP and runner-safety checks', async () => {
  const workflow = await source('.github/workflows/ci.yml');
  const backendJob = workflow.split('  backend-quality:')[1].split('\n  quality:')[0];
  assert.match(backendJob, /run: sh scripts\/test-event-operations-http-ci\.sh/);
  assert.match(backendJob, /run: node --test scripts\/__tests__\/event-operations-http-runner\.test\.mjs/);
  assert.doesNotMatch(backendJob, /continue-on-error: true/);
});

test('CI splits component checks and preserves Stack build caches', async () => {
  const workflow = await source('.github/workflows/ci.yml');
  // General config tests must not inherit the runtime fixture's libpq password.
  assert.doesNotMatch(workflow, /^      PGPASSWORD:/m);
  assert.match(workflow, /^      TDF_TEST_POSTGRES_PASSWORD: postgres$/m);
  for (const job of ['repo-quality:', 'ui-quality:', 'mobile-quality:', 'backend-quality:', 'quality:']) {
    assert.match(workflow, new RegExp(`^  ${job}`, 'm'));
  }
  assert.match(workflow, /uses: actions\/cache@v6/);
  assert.match(workflow, /tdf-hq\/\.stack-work/);
  assert.match(workflow, /stack-work-v1-ghc-9\.10\.3/);
  assert.doesNotMatch(workflow, /stack-work[^\n]*github\.sha/);
  assert.match(workflow, /BACKEND_BINARY_OUT:/);
  assert.match(workflow, /uses: actions\/upload-artifact@v7/);
  assert.match(workflow, /concurrency:[\s\S]*?cancel-in-progress: true/);
});

test('safe-install CI permits missing scripts without masking script failures', async () => {
  const workflow = await source('.github/workflows/ci-safe-install.yml');
  assert.match(workflow, /run: npm run -s --if-present build/);
  assert.match(workflow, /run: npm run -s --if-present test/);
  assert.doesNotMatch(workflow, /\|\||no (?:build|test) script; skipping/);
});

test('configured Datadog checks fail when tests or results are missing', async () => {
  const workflow = await source('.github/workflows/datadog-synthetics.yml');
  assert.match(workflow, /test_search_query: 'tag:e2e-tests'/);
  assert.match(workflow, /datadog_site: datadoghq\.com/);
  assert.match(workflow, /fail_on_critical_errors: true/);
  assert.match(workflow, /fail_on_missing_tests: true/);
  assert.match(workflow, /permissions:\n  contents: read/);
  assert.doesNotMatch(workflow, /runs-on: ubuntu-latest\n    env:/);
  assert.match(workflow, /api_key: \$\{\{ secrets\.DD_API_KEY \}\}/);
  assert.match(workflow, /app_key: \$\{\{ secrets\.DD_APP_KEY \}\}/);
  assert.match(workflow, /steps\.datadog-config\.outputs\.configured == 'true'/);
});

test('migration CI runs the owning merch checkout expiry assertions without waiving failures', async () => {
  const workflow = await source('.github/workflows/ci.yml');
  const migrationJob = workflow.split('  migration-tests:')[1].split('\n  production-migrations:')[0];
  assert.match(migrationJob, /run: \.\/scripts\/test-artist-merch-storefronts-migration\.sh/);
  assert.doesNotMatch(migrationJob, /continue-on-error: true/);
  const runner = await source('scripts/test-artist-merch-storefronts-migration.sh');
  assert.match(runner, /apply_file "\$TDF_MERCH_DATABASE" "\$TDF_MERCH_ROOT\/tdf-hq\/test\/integration\/merch_checkout_expiry_assertions\.sql"/);
});

test('persona browser journeys are artifacted and gate aggregate quality', async () => {
  const workflow = await source('.github/workflows/ci.yml');
  const playwrightConfig = await source('playwright.config.mjs');
  assert.match(workflow, /^  persona-web-e2e:/m);
  assert.match(workflow, /npx playwright install --with-deps chromium firefox webkit/);
  assert.match(workflow, /run: npm run test:e2e:web/);
  assert.match(workflow, /path: artifacts\/persona-playwright/);
  assert.match(workflow, /retention-days: 14/);
  assert.match(workflow, /quality:[\s\S]*?needs:[\s\S]*?- persona-web-e2e/);
  assert.match(workflow, /PERSONA_WEB_E2E_RESULT: \$\{\{ needs\.persona-web-e2e\.result \}\}/);
  assert.match(playwrightConfig, /npm run build:e2e --workspace=tdf-hq-ui && npm run preview:e2e --workspace=tdf-hq-ui/);
  assert.doesNotMatch(playwrightConfig, /npm run dev --workspace=tdf-hq-ui/);
});

test('backend quality compiles, tests and exports the binary in one Stack pass', async () => {
  const script = await source('scripts/quality-backend.sh');
  assert.match(script, /build_args=\(--no-terminal test tdf-hq\)/);
  assert.equal((script.match(/stack "\$\{build_args\[@\]\}"/g) ?? []).length, 1);
  assert.doesNotMatch(script, /stack --no-terminal test/);
});

test('backend quality requires real invitation and dependency PostgreSQL regressions', async () => {
  const quality = await source('scripts/quality-backend.sh');
  for (const runner of ['test-invitation-update-concurrency.sh', 'test-event-relations-runtime.sh']) {
    assert.ok(quality.includes(`scripts/${runner}`));
    const script = await source(`scripts/${runner}`);
    assert.match(script, /--fail-on=empty/);
    assert.match(script, /test -x "\$test_binary"/);
    assert.doesNotMatch(script, /stack test/);
  }
});

test('backend quality exercises public booking concurrency with its tested binary', async () => {
  const workflow = await source('.github/workflows/ci.yml');
  const integration = await source('scripts/test-public-booking-http-concurrency.sh');
  assert.match(
    workflow,
    /Exercise public booking HTTP idempotency and resource conflicts[\s\S]*TDF_PUBLIC_BOOKING_HTTP_DATABASE_URL: postgresql:\/\/postgres:postgres@postgres:5432\/tdf_hq_automatic_migration_test[\s\S]*TDF_PUBLIC_BOOKING_HTTP_SERVER_BIN: \$\{\{ env\.BACKEND_BINARY_OUT \}\}[\s\S]*run: bash scripts\/test-public-booking-http-concurrency\.sh/,
  );
  assert.match(integration, /env -i/);
  assert.match(integration, /Refusing to run the HTTP write test outside loopback or the CI postgres service/);
  assert.match(integration, /Refusing to write to database without a _test suffix/);
});

test('change detection includes deleted paths', async () => {
  const classifier = await source('scripts/ci-change-scope.mjs');
  assert.match(classifier, /--diff-filter=ACMRD/);
});

test('backend image packages the tested artifact instead of recompiling Haskell', async () => {
  const workflow = await source('.github/workflows/build.yml');
  assert.match(workflow, /uses: actions\/download-artifact@v8/);
  assert.match(workflow, /file: \.\/tdf-hq\/Dockerfile\.runtime/);
  assert.match(workflow, /cache-from: type=gha,scope=tdf-hq-backend/);
  assert.match(workflow, /group: build-image-/);
  assert.doesNotMatch(workflow, /npm --prefix tdf-mobile ci/);
  assert.doesNotMatch(workflow, /^\s+- 'package(?:-lock)?\.json'$/m);

  const runtimeDockerfile = await source('tdf-hq/Dockerfile.runtime');
  assert.match(runtimeDockerfile, /COPY tdf-hq\/\.release\/tdf-hq-exe/);
  assert.match(runtimeDockerfile, /production-migrations\.sql/);
  assert.match(runtimeDockerfile, /production-entrypoint\.sh/);
  assert.match(runtimeDockerfile, /ENV AUTO_APPLY_PRODUCTION_MIGRATIONS=true/);
  assert.match(runtimeDockerfile, /postgresql-client/);
  assert.doesNotMatch(runtimeDockerfile, /stack (?:--[^\n]+ )?build/);
});

test('image workflow grants the reusable native artifact job its required read permissions', async () => {
  const caller = await source('.github/workflows/build.yml');
  const callee = await source('.github/workflows/ci.yml');
  const callerJob = caller.match(/^  required-tests:\n([\s\S]*?)(?=^  \S)/m)?.[1];
  const nativeJob = callee.match(/^  native-android-e2e:\n([\s\S]*?)(?=^  \S)/m)?.[1];
  assert.ok(callerJob && nativeJob);
  for (const permission of ['contents', 'actions']) {
    const requiredRead = new RegExp(`^      ${permission}: read$`, 'm');
    assert.match(nativeJob, requiredRead);
    // GitHub validates called-job permissions even when that job is skipped.
    assert.match(callerJob, requiredRead);
  }
  assert.doesNotMatch(callerJob, /: write\b|write-all/);
});

test('automatic migration integration matches the persisted production locale', async () => {
  const integration = await source('scripts/test-automatic-migrations-production-schema.sh');
  assert.equal(integration.match(/DEFAULT_LOCALE=es/g)?.length, 1);
  assert.match(integration, /AUTO_APPLY_PRODUCTION_MIGRATIONS=true/);
  assert.match(integration, /production-entrypoint\.sh/);
});

test('backend image receives deterministic, non-empty release metadata', async () => {
  const workflow = await source('.github/workflows/build.yml');
  assert.match(workflow, /id: image-metadata/);
  assert.match(workflow, /git show -s --format=%ct "\$GITHUB_SHA"/);
  assert.match(workflow, /new Date\(Number\(process\.argv\[1\]\) \* 1000\)\.toISOString\(\)/);
  assert.match(workflow, /BUILD_TIME=\$\{\{ steps\.image-metadata\.outputs\.build_time \}\}/);
  assert.doesNotMatch(workflow, /github\.event\.head_commit\.timestamp|github\.run_started_at/);

  const runtimeDockerfile = await source('tdf-hq/Dockerfile.runtime');
  assert.match(runtimeDockerfile, /ARG SOURCE_COMMIT\nARG BUILD_TIME\n/);
  assert.match(runtimeDockerfile, /grep -Eq '\^\[0-9a-f\]\{40\}\$'/);
  assert.match(runtimeDockerfile, /grep -Eq '\^\[0-9\]\{4\}.*T.*Z\$'/);
  assert.doesNotMatch(runtimeDockerfile, /ARG SOURCE_COMMIT=(?:dev|unknown)|ARG BUILD_TIME=(?:dev|unknown)/);
});

test('source Dockerfile introduces changing release metadata after compilation', async () => {
  const dockerfile = await source('tdf-hq/Dockerfile');
  const builder = dockerfile.slice(0, dockerfile.indexOf('FROM debian:bookworm-slim'));
  assert.ok(builder.indexOf('build --copy-bins') < builder.indexOf('ARG SOURCE_COMMIT=dev'));
  assert.ok(builder.indexOf('build --copy-bins') < builder.indexOf('ARG BUILD_TIME=unknown'));
});

test('affected active workflow actions use Node 24 majors', async () => {
  const workflowDirectory = path.join(root, '.github', 'workflows');
  const workflowFiles = (await readdir(workflowDirectory))
    .filter((name) => /\.ya?ml$/u.test(name));
  const workflows = (await Promise.all(
    workflowFiles.map((name) => source(path.join('.github', 'workflows', name))),
  )).join('\n');
  const requiredPins = new Map([
    ['actions/checkout', 'v7'],
    ['actions/setup-node', 'v7'],
    ['actions/cache', 'v6'],
    ['actions/upload-artifact', 'v7'],
    ['actions/download-artifact', 'v8'],
    ['docker/login-action', 'v4'],
    ['docker/setup-buildx-action', 'v4'],
    ['docker/build-push-action', 'v7'],
  ]);

  for (const [action, expectedRef] of requiredPins) {
    const escapedAction = action.replace(/[.*+?^${}()|[\]\\]/gu, '\\$&');
    const references = [...workflows.matchAll(
      new RegExp(`uses:\\s*${escapedAction}@(\\S+)`, 'gu'),
    )].map((match) => match[1]);
    assert.ok(references.length > 0, `${action} must remain covered by the runtime pin guard`);
    assert.deepEqual(
      [...new Set(references)],
      [expectedRef],
      `${action} must use its Node 24 major`,
    );
  }
});

test('native artifact admission rejects application drift while allowing only Maestro fixture changes', async () => {
  const workflow = await source('.github/workflows/ci.yml');
  const match = workflow.match(/python3 - <<'PYVERIFY'\n([\s\S]*?)\n          PYVERIFY/);
  assert.ok(match, 'Native artifact admission must run before download');
  const admission = match[1].split('\n').map(line => line.slice(10)).join('\n');
  const harness = `
import json, os, sys
from unittest.mock import patch
expected='a'*40
run={'head_sha':'b'*40,'conclusion':sys.argv[3],'path':sys.argv[4]}
changed=json.loads(sys.argv[2])
def output(command, **kwargs):
    return expected if 'rev-parse' in command else '\\n'.join(changed)
os.environ['RUNNER_TEMP']='/synthetic'
with patch('pathlib.Path.read_text',return_value=json.dumps(run)), patch('subprocess.check_output',side_effect=output), patch('subprocess.run'):
    exec(compile(sys.argv[1], '<actual-workflow-admission>', 'exec'))
`;
  const invoke = (paths, conclusion = 'success', workflowPath = '.github/workflows/interaction-android.yml') => spawnSync('python3', ['-c', harness, admission, JSON.stringify(paths), conclusion, workflowPath], { encoding: 'utf8' });
  assert.equal(invoke(['e2e/interactions/create.yaml']).status, 0);
  for (const paths of [['src/features/interactions/DiscussionScreen.tsx'], ['android/app/build.gradle'], ['package-lock.json'], ['.github/workflows/interaction-android.yml'], ['scripts/android-release.py'], ['e2e/interactions/entry.js'], ['e2e/interactions/create.yaml', 'app.config.ts']]) {
    assert.notEqual(invoke(paths).status, 0, `Must rebuild after ${paths.join(', ')}`);
  }
  assert.notEqual(invoke(['e2e/interactions/create.yaml'], 'failure').status, 0);
  assert.notEqual(invoke(['e2e/interactions/create.yaml'], 'success', '.github/workflows/untrusted.yml').status, 0);
});
