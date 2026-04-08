import { execFileSync } from 'node:child_process';
import path from 'node:path';

function sanitizeGitRefFragment(raw, fallback = 'unknown') {
  const text = String(raw ?? '')
    .trim()
    .toLowerCase()
    .replace(/[^a-z0-9._/-]+/g, '-')
    .replace(/\/+/g, '/')
    .replace(/^-+|-+$/g, '')
    .replace(/^\/+|\/+$/g, '');
  return text || fallback;
}

function runGit(repoRoot, args, { capture = true, trimOutput = true } = {}) {
  try {
    const output = execFileSync('git', args, {
      cwd: repoRoot,
      encoding: 'utf8',
      stdio: capture ? ['ignore', 'pipe', 'pipe'] : 'inherit',
    });
    return capture ? (trimOutput ? output.trim() : output) : '';
  } catch (error) {
    const stdout = error?.stdout ? String(error.stdout) : '';
    const stderr = error?.stderr ? String(error.stderr) : '';
    throw new Error(`git ${args.join(' ')} failed.\n${stdout}${stderr}`.trim());
  }
}

export function parseGitStatusPaths(rawStatus) {
  const lines = String(rawStatus ?? '')
    .split(/\r?\n/)
    .map((line) => line.trimEnd())
    .filter(Boolean);
  const paths = [];
  for (const line of lines) {
    const match = line.match(/^[ A-Z?!]{1,2}\s+(.*)$/);
    const entry = match?.[1]?.trim() ?? '';
    if (!entry) continue;
    const nextPath = entry.includes(' -> ') ? entry.split(' -> ').pop() : entry;
    if (!nextPath) continue;
    paths.push(nextPath.replace(/^"(.*)"$/, '$1'));
  }
  return Array.from(new Set(paths));
}

export function buildDirtyCheckpointBranchName({ currentBranch, timestamp }) {
  const base = sanitizeGitRefFragment(currentBranch || 'main', 'main');
  const stamp = sanitizeGitRefFragment(String(timestamp ?? '').replace(/[:.]/g, '-'), 'now').replace(/\//g, '-');
  return `continuous-improvement-loop/dirty/${base}/dirty-worktree-${stamp}`;
}

export function analyzeDirtyWorktree(paths, currentBranch) {
  const topScopes = Array.from(
    new Set(
      paths
        .map((item) => String(item).split('/')[0] || String(item))
        .filter(Boolean),
    ),
  ).slice(0, 4);
  const scopeSummary = topScopes.length > 0 ? topScopes.join(', ') : 'misc';
  return {
    commitMessage: 'chore(loop): checkpoint dirty worktree',
    summary: `Checkpoint dirty worktree from ${currentBranch || 'main'} covering ${scopeSummary}.`,
  };
}

function isNestedGitRepo(repoRoot, relPath) {
  const candidate = path.resolve(repoRoot, relPath);
  try {
    const nestedRoot = runGit(candidate, ['rev-parse', '--show-toplevel']);
    return path.resolve(nestedRoot) === candidate;
  } catch {
    return false;
  }
}

async function checkpointDirtySubmodules({ repoRoot, dirtyPaths, visited }) {
  const recoveries = [];
  for (const relPath of dirtyPaths) {
    if (!isNestedGitRepo(repoRoot, relPath)) continue;
    const nestedRoot = path.resolve(repoRoot, relPath);
    const nestedResult = await checkpointDirtyWorktree({ repoRoot: nestedRoot, visited });
    recoveries.push({ path: relPath, result: nestedResult });
    if (!nestedResult.recovered && !nestedResult.clean) {
      return {
        ok: false,
        recoveries,
        reason: `nested dirty worktree at ${relPath}: ${nestedResult.reason}`,
      };
    }
  }
  return { ok: true, recoveries };
}

export async function checkpointDirtyWorktree({ repoRoot, currentBranch, visited = new Set() } = {}) {
  const normalizedRoot = path.resolve(repoRoot);
  if (visited.has(normalizedRoot)) {
    return {
      recovered: false,
      clean: false,
      paths: [],
      reason: `dirty worktree checkpoint recursion detected at ${normalizedRoot}`,
      summary: 'dirty worktree checkpoint failed',
    };
  }
  visited.add(normalizedRoot);

  let dirtyStatus = runGit(normalizedRoot, ['status', '--porcelain'], { trimOutput: false });
  let dirtyPaths = parseGitStatusPaths(dirtyStatus);
  if (dirtyPaths.length === 0) {
    return {
      recovered: false,
      clean: true,
      paths: [],
      reason: '',
      summary: 'worktree already clean',
    };
  }

  const nestedRecovery = await checkpointDirtySubmodules({
    repoRoot: normalizedRoot,
    dirtyPaths,
    visited,
  });
  if (!nestedRecovery.ok) {
    return {
      recovered: false,
      clean: false,
      paths: dirtyPaths,
      reason: nestedRecovery.reason,
      summary: 'dirty worktree checkpoint failed',
    };
  }

  dirtyStatus = runGit(normalizedRoot, ['status', '--porcelain'], { trimOutput: false });
  dirtyPaths = parseGitStatusPaths(dirtyStatus);
  if (dirtyPaths.length === 0) {
    return {
      recovered: true,
      clean: false,
      paths: [],
      reason: nestedRecovery.recoveries.map((item) => item.result.summary).filter(Boolean).join('; '),
      summary:
        nestedRecovery.recoveries.length > 0
          ? `checkpointed nested dirty worktrees: ${nestedRecovery.recoveries.map((item) => item.path).join(', ')}`
          : 'checkpointed nested dirty worktrees',
    };
  }

  const loopBranch = currentBranch || runGit(normalizedRoot, ['branch', '--show-current']) || 'main';
  const checkpointBranch = buildDirtyCheckpointBranchName({
    currentBranch: loopBranch,
    timestamp: new Date().toISOString(),
  });
  const analysis = analyzeDirtyWorktree(dirtyPaths, loopBranch);
  const commitBody = [
    analysis.summary,
    '',
    'Dirty worktree analysis:',
    `- source branch: ${loopBranch}`,
    `- paths: ${dirtyPaths.join(', ')}`,
  ].join('\n');

  try {
    runGit(normalizedRoot, ['config', 'user.name', 'continuous-improvement-loop[bot]'], { capture: false });
    runGit(normalizedRoot, ['config', 'user.email', 'continuous-improvement-loop[bot]@users.noreply.github.com'], {
      capture: false,
    });
    runGit(normalizedRoot, ['checkout', '-b', checkpointBranch], { capture: false });
    runGit(normalizedRoot, ['add', '-A'], { capture: false });
    runGit(normalizedRoot, ['commit', '-m', analysis.commitMessage, '-m', commitBody], { capture: false });
    runGit(normalizedRoot, ['push', '-u', 'origin', `${checkpointBranch}:refs/heads/${checkpointBranch}`], {
      capture: false,
    });
    const commit = runGit(normalizedRoot, ['rev-parse', 'HEAD']);
    runGit(normalizedRoot, ['checkout', loopBranch], { capture: false });
    return {
      recovered: true,
      clean: false,
      branch: checkpointBranch,
      commit,
      paths: dirtyPaths,
      commitMessage: analysis.commitMessage,
      reason: analysis.summary,
      summary: `checkpointed dirty worktree to ${checkpointBranch} (${commit})`,
    };
  } catch (error) {
    try {
      const branchAfterError = runGit(normalizedRoot, ['branch', '--show-current']);
      if (branchAfterError && branchAfterError !== loopBranch) {
        runGit(normalizedRoot, ['checkout', loopBranch], { capture: false });
      }
    } catch {}
    return {
      recovered: false,
      clean: false,
      paths: dirtyPaths,
      commitMessage: analysis.commitMessage,
      reason: error instanceof Error ? error.message : String(error),
      summary: 'dirty worktree checkpoint failed',
    };
  }
}
