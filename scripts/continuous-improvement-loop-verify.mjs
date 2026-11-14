#!/usr/bin/env node
// Build-and-test gate for the continuous improvement loop. Run from the worktree that
// holds the uncommitted iteration; exits non-zero unless every selected gate passes.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import process from 'node:process';
import { execFileSync, spawnSync } from 'node:child_process';
import { parseArgs } from 'node:util';
import {
  TRACEABILITY_FILE,
  VERIFICATION_GATES,
  parsePorcelainPaths,
  selectVerificationGates,
  unmappedChangedSurfaces,
} from './lib/continuous-improvement-loop-verify.mjs';

function changedPaths(repoRoot) {
  const output = execFileSync('git', ['status', '--porcelain', '-z', '--untracked-files=all'], {
    cwd: repoRoot,
    encoding: 'utf8',
    maxBuffer: 64 * 1024 * 1024,
  });
  return parsePorcelainPaths(output);
}

function freeMegabytes(directory) {
  const stats = fs.statfsSync(directory);
  return Math.floor((Number(stats.bavail) * Number(stats.bsize)) / (1024 * 1024));
}

function run(command, args, repoRoot, extraEnv = {}) {
  console.log(`$ ${[command, ...args].join(' ')}`);
  const result = spawnSync(command, args, {
    cwd: repoRoot,
    env: { ...process.env, ...extraEnv },
    stdio: 'inherit',
  });
  return result.status ?? 1;
}

// Mirrors the hosted specification and catalog checks, which the repository gate does not
// run: regenerate the two generated indexes (they are committed with the change), refuse a
// change to a source file no requirement covers, and require every static list to be reviewed.
function runSpecificationGate(repoRoot) {
  const python = process.env.CONTINUOUS_LOOP_PYTHON || 'python3';
  const env = { PYTHONDONTWRITEBYTECODE: '1' };
  if (run(python, ['-c', 'import yaml'], repoRoot, env) !== 0) {
    console.error(
      `${python} cannot import PyYAML, which the specification indexes need. Install it (see formal/system/requirements-verification.txt) or point CONTINUOUS_LOOP_PYTHON at an interpreter that has it.`,
    );
    return 1;
  }
  for (const script of ['scripts/specification-inventory.py', 'scripts/specification-conformance.py']) {
    const status = run(python, [script], repoRoot, env);
    if (status !== 0) return status;
  }

  const traceability = JSON.parse(fs.readFileSync(path.join(repoRoot, TRACEABILITY_FILE), 'utf8'));
  const unmapped = unmappedChangedSurfaces(traceability, changedPaths(repoRoot));
  if (unmapped.length > 0) {
    console.error(
      `Changed source file(s) that no requirement in formal/system/requirements.json covers: ${unmapped.join(', ')}`,
    );
    return 1;
  }

  const auditOutput = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'loop-catalog-audit-')), 'audit.json');
  try {
    return run(
      'node',
      [
        'scripts/catalog-list-audit.mjs',
        '--decisions',
        'docs/catalog-persistence/catalog-list-decisions.json',
        '--fail-on-unreviewed',
        '--output',
        auditOutput,
      ],
      repoRoot,
    );
  } finally {
    fs.rmSync(path.dirname(auditOutput), { recursive: true, force: true });
  }
}

function main() {
  const { values } = parseArgs({
    options: {
      'repo-root': { type: 'string' },
      plan: { type: 'boolean' },
    },
  });
  const repoRoot = path.resolve(values['repo-root'] ?? process.cwd());
  const selection = selectVerificationGates(changedPaths(repoRoot));

  if (!selection.ok) {
    console.error(`Verification refused: ${selection.reason}`);
    process.exit(1);
  }
  if (selection.gates.length === 0) {
    console.log('Nothing to verify: no changed files.');
    return;
  }

  console.log(`Verifying ${selection.paths.length} changed file(s) with gate(s): ${selection.gates.join(', ')}`);
  if (values.plan) {
    return;
  }

  let dependenciesInstalled = false;
  for (const gateName of selection.gates) {
    const gate = VERIFICATION_GATES[gateName];
    console.log(`\n== Gate: ${gate.label} ==`);

    if (gate.minFreeMegabytes) {
      const free = freeMegabytes(repoRoot);
      if (free < gate.minFreeMegabytes) {
        console.error(
          `Gate "${gateName}" needs at least ${gate.minFreeMegabytes} MB of free disk space but only ${free} MB is available. Refusing to treat the change as verified.`,
        );
        process.exit(1);
      }
    }

    const mustInstall =
      !fs.existsSync(path.join(repoRoot, 'node_modules')) || (selection.reinstallDependencies && !dependenciesInstalled);
    if (gate.needsNodeModules && mustInstall) {
      dependenciesInstalled = true;
      const installStatus = run('npm', ['ci', '--no-audit', '--no-fund'], repoRoot);
      if (installStatus !== 0) {
        console.error('Dependency installation failed; the change is not verified.');
        process.exit(1);
      }
    }

    const env = { ...(gate.env ?? {}) };
    if (gateName === 'backend' && !process.env.STACK_ROOT) {
      // Reuse the machine's Stack toolchain instead of downloading a compiler per worktree.
      const sharedStackRoot = path.join(os.homedir(), '.stack');
      if (fs.existsSync(sharedStackRoot)) {
        env.STACK_ROOT = sharedStackRoot;
      }
    }

    let status;
    if (gateName === 'specification') {
      status = runSpecificationGate(repoRoot);
    } else {
      const [command, ...args] = gate.command;
      status = run(command, args, repoRoot, env);
    }
    if (status !== 0) {
      console.error(`Gate "${gateName}" failed with exit code ${status}.`);
      process.exit(1);
    }
  }

  console.log('\nAll verification gates passed.');
}

main();
