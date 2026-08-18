import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import path from 'node:path';
import test from 'node:test';
import { fileURLToPath } from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');

async function source(relativePath) {
  return readFile(path.join(root, relativePath), 'utf8');
}

test('CI splits component checks and preserves Stack build caches', async () => {
  const workflow = await source('.github/workflows/ci.yml');
  for (const job of ['repo-quality:', 'ui-quality:', 'mobile-quality:', 'backend-quality:', 'quality:']) {
    assert.match(workflow, new RegExp(`^  ${job}`, 'm'));
  }
  assert.match(workflow, /uses: actions\/cache@v4/);
  assert.match(workflow, /tdf-hq\/\.stack-work/);
  assert.match(workflow, /stack-work-v1-ghc-9\.10\.3/);
  assert.doesNotMatch(workflow, /stack-work[^\n]*github\.sha/);
  assert.match(workflow, /BACKEND_BINARY_OUT:/);
  assert.match(workflow, /uses: actions\/upload-artifact@v4/);
  assert.match(workflow, /concurrency:[\s\S]*?cancel-in-progress: true/);
});

test('backend quality compiles, tests and exports the binary in one Stack pass', async () => {
  const script = await source('scripts/quality-backend.sh');
  assert.match(script, /build_args=\(--no-terminal test tdf-hq\)/);
  assert.equal((script.match(/stack "\$\{build_args\[@\]\}"/g) ?? []).length, 1);
  assert.doesNotMatch(script, /stack --no-terminal test/);
});

test('change detection includes deleted paths', async () => {
  const classifier = await source('scripts/ci-change-scope.mjs');
  assert.match(classifier, /--diff-filter=ACMRD/);
});

test('backend image packages the tested artifact instead of recompiling Haskell', async () => {
  const workflow = await source('.github/workflows/build.yml');
  assert.match(workflow, /uses: actions\/download-artifact@v4/);
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
