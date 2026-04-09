import { execFileSync } from 'node:child_process';
import path from 'node:path';

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

function uniqueStrings(items) {
  return Array.from(new Set(items.map((item) => String(item).trim()).filter(Boolean)));
}

function listUnmergedConflictPaths(repoRoot) {
  return uniqueStrings(runGit(repoRoot, ['diff', '--name-only', '--diff-filter=U'], { trimOutput: false }).split(/\r?\n/));
}

function mergeRefOntoHead(repoRoot, branchRef, shortName) {
  const headBefore = runGit(repoRoot, ['rev-parse', 'HEAD']);

  try {
    runGit(repoRoot, ['merge', '--no-ff', '--no-edit', branchRef], { capture: false });
  } catch (error) {
    const conflicts = listUnmergedConflictPaths(repoRoot);
    if (conflicts.length === 0) {
      throw error;
    }

    // Conflict resolution stays on the live main lane and keeps the current HEAD content.
    runGit(repoRoot, ['restore', '--source=HEAD', '--staged', '--worktree', '--', ...conflicts], { capture: false });
    runGit(repoRoot, ['commit', '--no-edit'], { capture: false });

    return {
      outcome: 'conflict-resolved',
      ref: branchRef,
      shortName,
      headBefore,
      headAfter: runGit(repoRoot, ['rev-parse', 'HEAD']),
      conflicts,
    };
  }

  const headAfter = runGit(repoRoot, ['rev-parse', 'HEAD']);
  return {
    outcome: headAfter === headBefore ? 'noop' : 'merged',
    ref: branchRef,
    shortName,
    headBefore,
    headAfter,
    conflicts: [],
  };
}

function syncHeadToRemoteBranch(repoRoot, remoteName, branchName) {
  const remoteRef = `${remoteName}/${branchName}`;
  const headBefore = runGit(repoRoot, ['rev-parse', 'HEAD']);

  try {
    runGit(repoRoot, ['fetch', remoteName, branchName], { capture: false });
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    if (/\bcouldn't find remote ref\b|\bremote ref\b.*\bnot found\b/i.test(message)) {
      return {
        outcome: 'missing-remote',
        ref: remoteRef,
        shortName: branchName,
        headBefore,
        headAfter: headBefore,
        conflicts: [],
      };
    }
    throw new Error(`Failed to fetch ${remoteRef} before push.\n${message}`);
  }

  const remoteHead = runGit(repoRoot, ['rev-parse', remoteRef]);
  if (headBefore === remoteHead) {
    return {
      outcome: 'noop',
      ref: remoteRef,
      shortName: branchName,
      headBefore,
      headAfter: headBefore,
      conflicts: [],
    };
  }

  return mergeRefOntoHead(repoRoot, remoteRef, branchName);
}

function pushHeadToRemoteBranchWithRetry(repoRoot, remoteName, branchName) {
  try {
    runGit(repoRoot, ['push', remoteName, `HEAD:refs/heads/${branchName}`], { capture: false });
    return { pushed: true, retried: false, retrySync: null };
  } catch {
    runGit(repoRoot, ['fetch', remoteName, '--prune'], { capture: false });
    const retrySync = syncHeadToRemoteBranch(repoRoot, remoteName, branchName);
    runGit(repoRoot, ['push', remoteName, `HEAD:refs/heads/${branchName}`], { capture: false });
    return { pushed: true, retried: true, retrySync };
  }
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
    runGit(normalizedRoot, ['add', '-A'], { capture: false });
    runGit(normalizedRoot, ['commit', '-m', analysis.commitMessage, '-m', commitBody], { capture: false });
    const pushResult = pushHeadToRemoteBranchWithRetry(normalizedRoot, 'origin', loopBranch);
    const commit = runGit(normalizedRoot, ['rev-parse', 'HEAD']);
    return {
      recovered: true,
      clean: false,
      branch: loopBranch,
      commit,
      pushed: pushResult.pushed,
      retried: pushResult.retried,
      retrySync: pushResult.retrySync,
      paths: dirtyPaths,
      commitMessage: analysis.commitMessage,
      reason: analysis.summary,
      summary: pushResult.retried
        ? `committed dirty worktree directly to ${loopBranch} (${commit}) after syncing origin/${loopBranch}`
        : `committed dirty worktree directly to ${loopBranch} (${commit})`,
    };
  } catch (error) {
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
