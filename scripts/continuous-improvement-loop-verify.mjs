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
  VERIFICATION_GATES,
  parsePorcelainPaths,
  selectVerificationGates,
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

    const [command, ...args] = gate.command;
    const status = run(command, args, repoRoot, env);
    if (status !== 0) {
      console.error(`Gate "${gateName}" failed with exit code ${status}.`);
      process.exit(1);
    }
  }

  console.log('\nAll verification gates passed.');
}

main();
