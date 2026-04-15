#!/usr/bin/env node
import fs from 'node:fs/promises';
import path from 'node:path';
import process from 'node:process';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { fileURLToPath } from 'node:url';

const execFileAsync = promisify(execFile);
const repoRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');

function formatDate(date) {
  const year = date.getFullYear();
  const month = `${date.getMonth() + 1}`.padStart(2, '0');
  const day = `${date.getDate()}`.padStart(2, '0');
  return `${year}-${month}-${day}`;
}

function isTruthyFlag(value) {
  return ['1', 'true', 'yes', 'on'].includes(String(value ?? '').trim().toLowerCase());
}

async function exists(relativePath) {
  try {
    await fs.access(path.join(repoRoot, relativePath));
    return true;
  } catch {
    return false;
  }
}

async function run(command, args, options = {}) {
  try {
    const result = await execFileAsync(command, args, {
      cwd: repoRoot,
      maxBuffer: 1024 * 1024,
      ...options,
    });
    return {
      ok: true,
      stdout: (result.stdout ?? '').trim(),
      stderr: (result.stderr ?? '').trim(),
    };
  } catch (error) {
    return {
      ok: false,
      stdout: (error.stdout ?? '').trim(),
      stderr: (error.stderr ?? '').trim(),
      error: error.message,
    };
  }
}

function add(results, level, title, detail) {
  results.push({ level, title, detail });
}

function printResults(results) {
  const icon = {
    ok: 'OK',
    warn: 'WARN',
    error: 'ERROR',
  };

  for (const result of results) {
    console.log(`[${icon[result.level]}] ${result.title}`);
    if (result.detail) {
      console.log(`       ${result.detail}`);
    }
  }
}

async function main() {
  const results = [];
  const today = new Date();
  const yesterday = new Date(today.getTime() - 24 * 60 * 60 * 1000);
  const todayNote = `memory/${formatDate(today)}.md`;
  const yesterdayNote = `memory/${formatDate(yesterday)}.md`;

  add(results, (await exists('AGENTS.md')) ? 'ok' : 'error', 'Workspace instructions', 'AGENTS.md');
  add(results, (await exists('SOUL.md')) ? 'ok' : 'warn', 'Agent identity file', 'SOUL.md');
  add(results, (await exists('USER.md')) ? 'ok' : 'warn', 'User context file', 'USER.md');
  add(results, (await exists(todayNote)) ? 'ok' : 'warn', 'Today memory note', todayNote);
  add(results, (await exists(yesterdayNote)) ? 'ok' : 'warn', 'Yesterday memory note', yesterdayNote);
  add(results, (await exists('MEMORY.md')) ? 'ok' : 'warn', 'Long-term memory file', 'MEMORY.md');
  add(results, (await exists('AI_WORKFLOW.md')) ? 'ok' : 'warn', 'AI workflow guide', 'AI_WORKFLOW.md');

  for (const command of ['git', 'node', 'codex', 'gh']) {
    const check = await run(process.env.SHELL || '/bin/zsh', ['-lc', `command -v ${command}`]);
    add(
      results,
      check.ok && check.stdout ? 'ok' : command === 'codex' ? 'error' : 'warn',
      `CLI available: ${command}`,
      check.stdout || check.stderr || 'not found in PATH',
    );
  }

  const branch = await run('git', ['rev-parse', '--abbrev-ref', 'HEAD']);
  add(
    results,
    branch.ok ? 'ok' : 'error',
    'Current git branch',
    branch.ok ? branch.stdout : branch.stderr || branch.error,
  );

  const status = await run('git', ['status', '--short']);
  if (!status.ok) {
    add(results, 'error', 'Git worktree status', status.stderr || status.error);
  } else if (status.stdout) {
    add(results, 'warn', 'Git worktree status', 'worktree is dirty');
  } else {
    add(results, 'ok', 'Git worktree status', 'clean');
  }

  const backendTests = await exists('tdf-hq/test/Spec.hs');
  add(results, backendTests ? 'ok' : 'warn', 'Backend test suite', backendTests ? 'tdf-hq/test present' : 'missing tdf-hq/test');

  const mobilePackageRaw = await fs.readFile(path.join(repoRoot, 'tdf-mobile/package.json'), 'utf8').catch(() => null);
  if (mobilePackageRaw) {
    const mobilePackage = JSON.parse(mobilePackageRaw);
    add(
      results,
      mobilePackage.scripts?.test ? 'ok' : 'warn',
      'Mobile test script',
      mobilePackage.scripts?.test ?? 'missing npm test script in tdf-mobile/package.json',
    );
  } else {
    add(results, 'warn', 'Mobile workspace', 'tdf-mobile/package.json not found');
  }

  const sanitizedGhStatus = await run(process.env.SHELL || '/bin/zsh', [
    '-lc',
    'GH_TOKEN= GITHUB_TOKEN= GITHUB_PAT= gh auth status',
  ]);
  const ghAuth = sanitizedGhStatus.ok ? { ok: true, detail: 'authenticated' } : sanitizedGhStatus;
  add(
    results,
    ghAuth.ok ? 'ok' : 'warn',
    'GitHub auth for CI polling',
    ghAuth.ok
      ? ghAuth.detail
      : `${ghAuth.stderr || ghAuth.stdout || ghAuth.error}\n       Re-auth with \`gh auth login -h github.com\` or clear stale GH_TOKEN/GITHUB_TOKEN/GITHUB_PAT values.`,
  );

  const loopConfigPath = path.join(repoRoot, 'scripts/continuous-improvement-loop.codex.json');
  try {
    const loopConfig = JSON.parse(await fs.readFile(loopConfigPath, 'utf8'));
    const resolvedPushBranch = String(loopConfig.pushBranch || branch.stdout || '').trim();
    const allowPushToMain =
      Boolean(loopConfig.allowPushToMain) || isTruthyFlag(process.env.CONTINUOUS_LOOP_ALLOW_PUSH_TO_MAIN);

    if (resolvedPushBranch === 'main' && !loopConfig.dryRun && !allowPushToMain) {
      add(
        results,
        'warn',
        'Continuous loop target branch',
        'resolved push target is main; set pushBranch to a feature branch or use allowPushToMain / CONTINUOUS_LOOP_ALLOW_PUSH_TO_MAIN=1 intentionally',
      );
    } else {
      add(
        results,
        'ok',
        'Continuous loop target branch',
        resolvedPushBranch || 'inherits current branch',
      );
    }

    if (loopConfig.pollGitHub && !ghAuth.ok) {
      add(results, 'warn', 'Continuous loop GitHub polling', 'pollGitHub=true but gh auth is unavailable');
    } else {
      add(
        results,
        'ok',
        'Continuous loop GitHub polling',
        `pollGitHub=${String(Boolean(loopConfig.pollGitHub))}`,
      );
    }
  } catch (error) {
    add(results, 'warn', 'Continuous loop config', error.message);
  }

  printResults(results);

  const summary = results.reduce(
    (acc, result) => {
      acc[result.level] += 1;
      return acc;
    },
    { ok: 0, warn: 0, error: 0 },
  );

  console.log('');
  console.log(`Summary: ${summary.ok} ok, ${summary.warn} warnings, ${summary.error} errors`);

  if (summary.error > 0) {
    process.exit(1);
  }
}

main().catch((error) => {
  console.error(error.message);
  process.exit(1);
});
