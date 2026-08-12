import { execFileSync } from 'node:child_process';
import { appendFileSync } from 'node:fs';
import { pathToFileURL } from 'node:url';

const ZERO_SHA = /^0+$/;

function normalizedPath(value) {
  return String(value ?? '').trim().replaceAll('\\', '/').replace(/^\.\//, '');
}

function isPathOrChild(file, root) {
  return file === root || file.startsWith(`${root}/`);
}

function isRootDependencyFile(file) {
  return file === 'package.json' || file === 'package-lock.json' || file === '.npmrc';
}

function affectsContracts(file) {
  return file === 'tdf-hq/docs/openapi/api.yaml'
    || file === 'tdf-hq-ui/src/api/generated/types.ts'
    || file === 'tdf-mobile/src/api/generated/types.ts'
    || file === 'tdf-hq-ui/package.json'
    || file === 'tdf-mobile/package.json'
    || isRootDependencyFile(file);
}

function affectsMigrations(file) {
  return isPathOrChild(file, 'tdf-hq/sql')
    || file === 'scripts/production-migrations.json'
    || file === 'scripts/production-release.mjs'
    || file === 'scripts/lib/production-release.mjs'
    || file.startsWith('scripts/render-production-')
    || isPathOrChild(file, 'scripts/__tests__/fixtures')
    || /^tdf-hq\/src\/TDF\/(Models(?:Extra)?|Models\/[^/]+)\.hs$/.test(file);
}

function isPipelineDefinition(file) {
  return file === '.github/workflows/ci.yml'
    || file === '.github/workflows/build.yml'
    || file === 'scripts/ci-change-scope.mjs'
    || file === 'scripts/quality-check.sh'
    || file.startsWith('scripts/quality-')
    || file === 'scripts/__tests__/ci-change-scope.test.mjs'
    || file === 'scripts/__tests__/ci-pipeline.test.mjs';
}

export function allChangeScopes() {
  return {
    repo: true,
    ui: true,
    mobile: true,
    backend: true,
    contracts: true,
    migrations: true,
  };
}

export function classifyChangedFiles(files, options = {}) {
  const changed = [...new Set(files.map(normalizedPath).filter(Boolean))];
  const pipelineChanged = changed.some(isPipelineDefinition);
  const rootDependenciesChanged = changed.some(isRootDependencyFile);

  return {
    repo: changed.length > 0 || options.forceBackend === true,
    ui: pipelineChanged
      || rootDependenciesChanged
      || changed.some((file) => isPathOrChild(file, 'tdf-hq-ui')),
    mobile: pipelineChanged
      || rootDependenciesChanged
      || changed.some((file) => isPathOrChild(file, 'tdf-mobile')),
    backend: options.forceBackend === true
      || pipelineChanged
      || changed.some((file) => isPathOrChild(file, 'tdf-hq') || file === 'fly.toml' || file === '.dockerignore'),
    contracts: pipelineChanged || changed.some(affectsContracts),
    migrations: pipelineChanged || changed.some(affectsMigrations),
  };
}

function parseArgs(argv) {
  const options = { forceBackend: false };
  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    if (arg === '--all') options.all = true;
    else if (arg === '--force-backend') options.forceBackend = true;
    else if (arg === '--base') options.base = argv[++index];
    else if (arg === '--head') options.head = argv[++index];
    else if (arg === '--github-output') options.githubOutput = argv[++index];
    else throw new Error(`Unknown argument: ${arg}`);
  }
  return options;
}

function changedFilesBetween(base, head) {
  if (!base || !head || ZERO_SHA.test(base) || ZERO_SHA.test(head)) return null;
  try {
    const output = execFileSync('git', ['diff', '--name-only', '--diff-filter=ACMRD', `${base}...${head}`], {
      encoding: 'utf8',
      stdio: ['ignore', 'pipe', 'pipe'],
    });
    return output.split('\n').map(normalizedPath).filter(Boolean);
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    process.stderr.write(`Could not classify the Git diff safely; running all CI scopes. ${message}\n`);
    return null;
  }
}

export function resolveChangeScopes(options) {
  if (options.all) return { scopes: allChangeScopes(), files: [], fallback: true };
  const files = changedFilesBetween(options.base, options.head);
  if (files === null || files.length === 0) {
    return { scopes: allChangeScopes(), files: files ?? [], fallback: true };
  }
  return {
    scopes: classifyChangedFiles(files, { forceBackend: options.forceBackend }),
    files,
    fallback: false,
  };
}

function writeGitHubOutputs(outputPath, scopes) {
  const body = Object.entries(scopes)
    .map(([name, enabled]) => `${name}=${enabled ? 'true' : 'false'}`)
    .join('\n');
  appendFileSync(outputPath, `${body}\n`, 'utf8');
}

function main() {
  const options = parseArgs(process.argv.slice(2));
  const result = resolveChangeScopes(options);
  if (options.githubOutput) writeGitHubOutputs(options.githubOutput, result.scopes);
  process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  main();
}
