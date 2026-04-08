import { execFileSync } from 'node:child_process';

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

export async function checkpointDirtyWorktree({ repoRoot, currentBranch } = {}) {
  const dirtyStatus = runGit(repoRoot, ['status', '--porcelain'], { trimOutput: false });
  const dirtyPaths = parseGitStatusPaths(dirtyStatus);
  if (dirtyPaths.length === 0) {
    return {
      recovered: false,
      clean: true,
      paths: [],
      reason: '',
      summary: 'worktree already clean',
    };
  }

  const loopBranch = currentBranch || runGit(repoRoot, ['branch', '--show-current']) || 'main';
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
    runGit(repoRoot, ['config', 'user.name', 'continuous-improvement-loop[bot]'], { capture: false });
    runGit(repoRoot, ['config', 'user.email', 'continuous-improvement-loop[bot]@users.noreply.github.com'], {
      capture: false,
    });
    runGit(repoRoot, ['checkout', '-b', checkpointBranch], { capture: false });
    runGit(repoRoot, ['add', '-A'], { capture: false });
    runGit(repoRoot, ['commit', '-m', analysis.commitMessage, '-m', commitBody], { capture: false });
    runGit(repoRoot, ['push', '-u', 'origin', `${checkpointBranch}:refs/heads/${checkpointBranch}`], {
      capture: false,
    });
    const commit = runGit(repoRoot, ['rev-parse', 'HEAD']);
    runGit(repoRoot, ['checkout', loopBranch], { capture: false });
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
      const branchAfterError = runGit(repoRoot, ['branch', '--show-current']);
      if (branchAfterError && branchAfterError !== loopBranch) {
        runGit(repoRoot, ['checkout', loopBranch], { capture: false });
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
