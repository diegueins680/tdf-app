#!/usr/bin/env node

import process from 'node:process';
import { parseArgs } from 'node:util';
import { checkpointDirtyWorktree } from './lib/continuous-improvement-loop-dirty-worktree.mjs';

const { values } = parseArgs({
  options: {
    'repo-root': { type: 'string' },
    branch: { type: 'string' },
  },
  allowPositionals: false,
});

const repoRoot = values['repo-root'] || process.cwd();
const result = await checkpointDirtyWorktree({
  repoRoot,
  currentBranch: values.branch || undefined,
});

process.stdout.write(`${JSON.stringify(result)}\n`);
if (!result.recovered && !result.clean) {
  process.exitCode = 1;
}
