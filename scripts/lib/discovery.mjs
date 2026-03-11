import fs from 'node:fs/promises';
import path from 'node:path';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { collectUiFindings } from './ui-static-audit.mjs';

const execFileAsync = promisify(execFile);
const SCANNED_EXTENSIONS = new Set([
  '.ts',
  '.tsx',
  '.js',
  '.jsx',
  '.mjs',
  '.cjs',
  '.json',
  '.md',
  '.hs',
  '.yml',
  '.yaml',
  '.sh',
  '.sql',
  '.txt',
]);
const TODO_PATTERN = /\b(TODO|FIXME|HACK|XXX)\b[^\n]*/g;
const SKIPPED_SEGMENTS = [
  `${path.sep}node_modules${path.sep}`,
  `${path.sep}archives${path.sep}`,
  `${path.sep}dist${path.sep}`,
  `${path.sep}build${path.sep}`,
  `${path.sep}coverage${path.sep}`,
  `${path.sep}screencast${path.sep}meta-app-review${path.sep}output${path.sep}`,
];

async function listTrackedFiles(repoRoot) {
  const { stdout } = await execFileAsync('git', ['ls-files'], {
    cwd: repoRoot,
    maxBuffer: 10 * 1024 * 1024,
  });

  return stdout
    .split('\n')
    .map((value) => value.trim())
    .filter(Boolean)
    .map((relativePath) => path.join(repoRoot, relativePath));
}

function shouldScanFile(filePath) {
  if (SKIPPED_SEGMENTS.some((segment) => filePath.includes(segment))) {
    return false;
  }
  return SCANNED_EXTENSIONS.has(path.extname(filePath));
}

async function scanTodoMatches(repoRoot) {
  const files = await listTrackedFiles(repoRoot);
  const matches = [];

  for (const filePath of files) {
    if (!shouldScanFile(filePath)) continue;

    const stats = await fs.stat(filePath);
    if (stats.size > 300_000) continue;

    const source = await fs.readFile(filePath, 'utf8');
    for (const match of source.matchAll(TODO_PATTERN)) {
      const index = match.index ?? 0;
      matches.push({
        file: path.relative(repoRoot, filePath),
        line: source.slice(0, index).split('\n').length,
        text: match[0].trim(),
      });
      if (matches.length >= 50) return matches;
    }
  }

  return matches;
}

function buildUiIdea(repoRoot, finding) {
  const relativeFile = path.relative(repoRoot, finding.file);
  return {
    source: 'builtin-ui-audit',
    title: `Address ${finding.rule} in ${relativeFile}`,
    markdown: [
      '# Improvement Idea',
      '',
      `Source: builtin static UI audit`,
      `Target: ${relativeFile}:${finding.line}`,
      `Reason: ${finding.message}`,
      '',
      'Acceptance criteria:',
      '- The affected control has a clear accessible name or alternative text.',
      '- The touched screen reads more clearly for keyboard and screen-reader users.',
      '- `npm run audit:ui:static` no longer reports this location.',
      '',
      'Context snippet:',
      `- ${finding.snippet}`,
      '',
      'Implement the smallest high-signal fix that removes the ambiguity without changing unrelated flows.',
      '',
    ].join('\n'),
  };
}

function buildTodoIdea(match) {
  return {
    source: 'builtin-todo-scan',
    title: `Resolve TODO in ${match.file}`,
    markdown: [
      '# Improvement Idea',
      '',
      `Source: tracked TODO/FIXME scan`,
      `Target: ${match.file}:${match.line}`,
      `Reason: ${match.text}`,
      '',
      'Acceptance criteria:',
      '- Replace the placeholder behavior with explicit logic or documentation.',
      '- Keep the change scoped to the referenced TODO.',
      '- Add or update verification so the new behavior is defended.',
      '',
    ].join('\n'),
  };
}

function buildFallbackIdea() {
  return {
    source: 'builtin-fallback',
    title: 'Reduce one source of UI ambiguity',
    markdown: [
      '# Improvement Idea',
      '',
      'Source: fallback discovery',
      '',
      'Review the busiest admin-facing page you can validate locally and make one concrete improvement that:',
      '- reduces clutter or duplicated actions,',
      '- improves clarity for first-time users, and',
      '- can be defended with a targeted test or invariant.',
      '',
    ].join('\n'),
  };
}

export async function buildDefaultIdea(repoRoot) {
  const uiRoot = path.join(repoRoot, 'tdf-hq-ui', 'src');
  try {
    const uiFindings = await collectUiFindings(uiRoot);
    if (uiFindings.length > 0) {
      return buildUiIdea(repoRoot, uiFindings[0]);
    }
  } catch {
    // Ignore missing UI workspace and fall back to TODO scanning.
  }

  const todoMatches = await scanTodoMatches(repoRoot);
  if (todoMatches.length > 0) {
    return buildTodoIdea(todoMatches[0]);
  }

  return buildFallbackIdea();
}
