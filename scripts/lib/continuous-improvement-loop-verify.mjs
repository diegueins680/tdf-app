// Chooses which build-and-test gates must pass before the continuous improvement loop may
// commit an iteration. The repository-wide gate always runs; the UI and backend gates run
// when their trees (or the dependency manifests they build from) changed.

const UI_DEPENDENCY_FILES = new Set(['package.json', 'package-lock.json']);
const MOBILE_SUBMODULE = 'tdf-mobile';

export const VERIFICATION_GATES = {
  repo: {
    label: 'repository-wide invariants',
    command: ['bash', 'scripts/quality-repo.sh'],
    needsNodeModules: true,
  },
  ui: {
    label: 'web UI lint, typecheck, tests and build',
    command: ['bash', 'scripts/quality-ui.sh'],
    needsNodeModules: true,
  },
  backend: {
    label: 'Haskell backend build and tests',
    command: ['bash', 'scripts/quality-backend.sh'],
    env: { REQUIRE_STACK: '1' },
    minFreeMegabytes: 6144,
  },
};

function normalizePath(filePath) {
  return String(filePath ?? '').trim().replace(/\\/g, '/').replace(/^\.\//, '');
}

export function selectVerificationGates(changedPaths = []) {
  const paths = [...new Set(changedPaths.map(normalizePath).filter(Boolean))];
  if (paths.length === 0) {
    return { ok: true, gates: [], paths, reason: 'no changed files' };
  }

  const mobilePaths = paths.filter((filePath) => filePath === MOBILE_SUBMODULE || filePath.startsWith(`${MOBILE_SUBMODULE}/`));
  if (mobilePaths.length > 0) {
    return {
      ok: false,
      gates: [],
      paths,
      reason: `changes under the ${MOBILE_SUBMODULE} submodule cannot be verified or published by the loop: ${mobilePaths.join(', ')}`,
    };
  }

  const gates = ['repo'];
  if (paths.some((filePath) => filePath.startsWith('tdf-hq-ui/') || UI_DEPENDENCY_FILES.has(filePath))) {
    gates.push('ui');
  }
  if (paths.some((filePath) => filePath.startsWith('tdf-hq/'))) {
    gates.push('backend');
  }
  return {
    ok: true,
    gates,
    paths,
    reason: '',
    // Installed dependencies no longer match the manifest once the lockfile changes.
    reinstallDependencies: paths.some((filePath) => filePath.endsWith('package-lock.json')),
  };
}

export function parsePorcelainPaths(porcelainZ) {
  // `git status --porcelain -z`: entries are NUL separated; a rename/copy entry is followed
  // by its original path as a separate entry.
  const entries = String(porcelainZ ?? '').split('\0');
  const paths = [];
  for (let index = 0; index < entries.length; index += 1) {
    const entry = entries[index];
    if (entry.length < 4) continue;
    const status = entry.slice(0, 2);
    paths.push(entry.slice(3));
    if (status.includes('R') || status.includes('C')) {
      const original = entries[index + 1];
      if (original) paths.push(original);
      index += 1;
    }
  }
  return [...new Set(paths.map(normalizePath).filter(Boolean))];
}
