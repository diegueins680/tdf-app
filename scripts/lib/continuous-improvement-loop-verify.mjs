// Chooses which build-and-test gates must pass before the continuous improvement loop may
// commit an iteration. The specification and repository-wide gates always run; the UI and
// backend gates run when their trees (or the dependency manifests they build from) changed.

import { createHash } from 'node:crypto';
import fs from 'node:fs';
import path from 'node:path';

const UI_DEPENDENCY_FILES = new Set(['package.json', 'package-lock.json']);
const MOBILE_SUBMODULE = 'tdf-mobile';

export const TRACEABILITY_FILE = 'formal/system/traceability.json';

// node_modules is ignored, so it survives the loop resetting the worktree to a new base or
// discarding a proposal that changed the lockfile. The marker records which lockfile the
// installed tree came from; any difference means the dependencies must be reinstalled.
export const INSTALLED_LOCKFILE_MARKER = path.join('node_modules', '.continuous-loop-lockfile.sha256');

export function lockfileFingerprint(repoRoot) {
  return createHash('sha256').update(fs.readFileSync(path.join(repoRoot, 'package-lock.json'))).digest('hex');
}

export function dependenciesNeedInstall(repoRoot) {
  if (!fs.existsSync(path.join(repoRoot, 'node_modules'))) return true;
  try {
    return fs.readFileSync(path.join(repoRoot, INSTALLED_LOCKFILE_MARKER), 'utf8').trim() !== lockfileFingerprint(repoRoot);
  } catch {
    return true; // no marker: the installed tree's lockfile is unknown
  }
}

export function recordInstalledLockfile(repoRoot) {
  fs.writeFileSync(path.join(repoRoot, INSTALLED_LOCKFILE_MARKER), `${lockfileFingerprint(repoRoot)}\n`, 'utf8');
}

export const VERIFICATION_GATES = {
  specification: {
    label: 'specification indexes, requirement mapping and catalog list review',
    // Runs as several steps in the entry script; see runSpecificationGate there.
    needsNodeModules: true,
  },
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

  const gates = ['specification', 'repo'];
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

// Paths of the traceability index's source files that no requirement covers.
export function unmappedSurfacePaths(traceability) {
  const surfaces = Array.isArray(traceability?.surfaces) ? traceability.surfaces : [];
  return surfaces
    .filter((surface) => !Array.isArray(surface?.requirements) || surface.requirements.length === 0)
    .map((surface) => normalizePath(surface?.path))
    .filter(Boolean)
    .sort();
}

// Changed files that the (freshly regenerated) traceability index lists without a requirement.
// CI refuses such a change, so the loop must not publish it.
export function unmappedChangedSurfaces(traceability, changedPaths = []) {
  const unmapped = new Set(unmappedSurfacePaths(traceability));
  return [...new Set(changedPaths.map(normalizePath).filter(Boolean))].filter((filePath) => unmapped.has(filePath)).sort();
}
