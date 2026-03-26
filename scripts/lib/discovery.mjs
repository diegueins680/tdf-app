import fs from 'node:fs/promises';
import path from 'node:path';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { collectUiFindings } from './ui-static-audit.mjs';

const execFileAsync = promisify(execFile);
const DISCOVERY_LANE_UI = 'ui';
const DISCOVERY_LANE_BACKEND = 'backend';
const DISCOVERY_PRIORITY_REAL = 0;
const DISCOVERY_PRIORITY_FALLBACK = 1;
const DISCOVERY_STATE_FILE = path.join('tmp', 'continuous-improvement-loop-discovery.json');
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
const TODO_PATTERN = /\b(TODO|FIXME|HACK|XXX)\b[^\r\n]*/;
const TEXT_TODO_EXTENSIONS = new Set(['.json', '.md', '.txt']);
const COMMENT_PATTERNS = [
  /(?:^|\s)\/\/\s*(?<text>.*)$/,
  /(?:^|\s)#\s*(?<text>.*)$/,
  /(?:^|\s)--\s*(?<text>.*)$/,
  /(?:^|\s)\/\*+\s*(?<text>.*)$/,
  /^\s*\*\s*(?<text>.*)$/,
  /(?:^|\s)<!--\s*(?<text>.*)$/,
];
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

function normalizeTodoText(text) {
  return text.replace(/\s*(?:\*\/|-->)\s*$/, '').trim();
}

async function pathExists(filePath) {
  try {
    await fs.access(filePath);
    return true;
  } catch (error) {
    if (error?.code === 'ENOENT') {
      return false;
    }
    throw error;
  }
}

function matchTodoText(line, extension) {
  if (TEXT_TODO_EXTENSIONS.has(extension)) {
    const todoMatch = line.match(TODO_PATTERN);
    return todoMatch ? normalizeTodoText(todoMatch[0]) : null;
  }

  for (const pattern of COMMENT_PATTERNS) {
    const commentMatch = line.match(pattern);
    if (!commentMatch) continue;

    const commentText = commentMatch.groups?.text ?? '';
    const todoMatch = commentText.match(TODO_PATTERN);
    if (todoMatch) {
      return normalizeTodoText(todoMatch[0]);
    }
  }

  return null;
}

async function scanTodoMatches(repoRoot) {
  const files = await listTrackedFiles(repoRoot);
  const matches = [];

  for (const filePath of files) {
    if (!shouldScanFile(filePath)) continue;

    const stats = await fs.stat(filePath);
    if (stats.size > 300_000) continue;

    const source = await fs.readFile(filePath, 'utf8');
    const extension = path.extname(filePath);
    const lines = source.split(/\r?\n/);

    for (const [index, line] of lines.entries()) {
      const text = matchTodoText(line, extension);
      if (!text) continue;

      matches.push({
        file: path.relative(repoRoot, filePath),
        line: index + 1,
        text,
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
    lane: DISCOVERY_LANE_UI,
    title: `Address ${finding.rule} in ${relativeFile}`,
    markdown: [
      '# Improvement Idea',
      '',
      `Source: builtin static UI audit`,
      `Lane: ${DISCOVERY_LANE_UI}`,
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

function buildTodoIdea(match, lane) {
  const scope = lane === DISCOVERY_LANE_BACKEND ? 'backend' : 'ui';
  return {
    source: 'builtin-todo-scan',
    lane,
    title: `Resolve TODO in ${match.file}`,
    markdown: [
      '# Improvement Idea',
      '',
      `Source: tracked ${scope} TODO/FIXME scan`,
      `Lane: ${lane}`,
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

function buildUiFallbackIdea() {
  return {
    source: 'builtin-ui-fallback',
    lane: DISCOVERY_LANE_UI,
    title: 'Reduce one source of UI ambiguity',
    markdown: [
      '# Improvement Idea',
      '',
      'Source: ui fallback discovery',
      `Lane: ${DISCOVERY_LANE_UI}`,
      '',
      'Review the busiest admin-facing page you can validate locally and make one concrete improvement that:',
      '- reduces clutter or duplicated actions,',
      '- improves clarity for first-time users, and',
      '- can be defended with a targeted test or invariant.',
      '',
    ].join('\n'),
  };
}

function buildBackendFallbackIdea() {
  return {
    source: 'builtin-backend-fallback',
    lane: DISCOVERY_LANE_BACKEND,
    title: 'Tighten one backend invariant',
    markdown: [
      '# Improvement Idea',
      '',
      'Source: backend fallback discovery',
      `Lane: ${DISCOVERY_LANE_BACKEND}`,
      '',
      'Review one production-facing backend path in `tdf-hq` and make one concrete improvement that:',
      '- tightens a validation, authorization, or data-shape invariant,',
      '- improves error handling or removes ambiguous behavior, and',
      '- can be defended with a targeted Haskell test, contract check, or explicit invariant.',
      '',
      'Keep the change scoped to the backend lane. Avoid UI-only edits for this iteration.',
      '',
    ].join('\n'),
  };
}

function normalizeLane(value) {
  return value === DISCOVERY_LANE_UI || value === DISCOVERY_LANE_BACKEND ? value : '';
}

function classifyTodoLane(filePath) {
  if (filePath.startsWith('tdf-hq-ui/') || filePath.startsWith('tdf-mobile/')) {
    return DISCOVERY_LANE_UI;
  }

  if (filePath.startsWith('tdf-hq/')) {
    return DISCOVERY_LANE_BACKEND;
  }

  return DISCOVERY_LANE_BACKEND;
}

function createCandidate(lane, priority, idea) {
  return {
    lane,
    priority,
    idea,
  };
}

function setCandidate(candidates, candidate) {
  const current = candidates[candidate.lane];
  if (!current || candidate.priority < current.priority) {
    candidates[candidate.lane] = candidate;
  }
}

async function collectDiscoveryCandidates(repoRoot) {
  const candidates = {
    [DISCOVERY_LANE_UI]: null,
    [DISCOVERY_LANE_BACKEND]: null,
  };

  const uiRoot = path.join(repoRoot, 'tdf-hq-ui', 'src');
  if (await pathExists(uiRoot)) {
    const uiFindings = await collectUiFindings(uiRoot);
    if (uiFindings.length > 0) {
      setCandidate(candidates, createCandidate(DISCOVERY_LANE_UI, DISCOVERY_PRIORITY_REAL, buildUiIdea(repoRoot, uiFindings[0])));
    }
  }

  const todoMatches = await scanTodoMatches(repoRoot);
  for (const match of todoMatches) {
    const lane = classifyTodoLane(match.file);
    setCandidate(candidates, createCandidate(lane, DISCOVERY_PRIORITY_REAL, buildTodoIdea(match, lane)));
    if (candidates[DISCOVERY_LANE_UI]?.priority === DISCOVERY_PRIORITY_REAL
      && candidates[DISCOVERY_LANE_BACKEND]?.priority === DISCOVERY_PRIORITY_REAL) {
      break;
    }
  }

  if (!candidates[DISCOVERY_LANE_UI]) {
    setCandidate(candidates, createCandidate(DISCOVERY_LANE_UI, DISCOVERY_PRIORITY_FALLBACK, buildUiFallbackIdea()));
  }

  const backendRoot = path.join(repoRoot, 'tdf-hq');
  if (!candidates[DISCOVERY_LANE_BACKEND] && await pathExists(backendRoot)) {
    setCandidate(
      candidates,
      createCandidate(DISCOVERY_LANE_BACKEND, DISCOVERY_PRIORITY_FALLBACK, buildBackendFallbackIdea()),
    );
  }

  return candidates;
}

export function chooseDiscoveryLane(candidates, options = {}) {
  const uiCandidate = candidates?.[DISCOVERY_LANE_UI] ?? null;
  const backendCandidate = candidates?.[DISCOVERY_LANE_BACKEND] ?? null;
  const lastLane = normalizeLane(options.lastLane);

  if (!uiCandidate && !backendCandidate) {
    return '';
  }

  if (!uiCandidate) {
    return DISCOVERY_LANE_BACKEND;
  }

  if (!backendCandidate) {
    return DISCOVERY_LANE_UI;
  }

  if (uiCandidate.priority < backendCandidate.priority) {
    return DISCOVERY_LANE_UI;
  }

  if (backendCandidate.priority < uiCandidate.priority) {
    return DISCOVERY_LANE_BACKEND;
  }

  if (lastLane === DISCOVERY_LANE_UI) {
    return DISCOVERY_LANE_BACKEND;
  }

  if (lastLane === DISCOVERY_LANE_BACKEND) {
    return DISCOVERY_LANE_UI;
  }

  return DISCOVERY_LANE_BACKEND;
}

function defaultDiscoveryState() {
  return {
    version: 1,
    lastLane: '',
    counts: {
      [DISCOVERY_LANE_UI]: 0,
      [DISCOVERY_LANE_BACKEND]: 0,
    },
  };
}

function normalizeCount(value) {
  return Number.isFinite(Number(value)) && Number(value) >= 0 ? Number(value) : 0;
}

function normalizeDiscoveryState(value) {
  const state = defaultDiscoveryState();
  if (!value || typeof value !== 'object') {
    return state;
  }

  state.lastLane = normalizeLane(value.lastLane);
  const counts = value.counts && typeof value.counts === 'object' ? value.counts : {};
  state.counts[DISCOVERY_LANE_UI] = normalizeCount(counts[DISCOVERY_LANE_UI]);
  state.counts[DISCOVERY_LANE_BACKEND] = normalizeCount(counts[DISCOVERY_LANE_BACKEND]);
  return state;
}

function resolveDiscoveryStatePath(repoRoot, customStatePath) {
  if (!customStatePath) {
    return path.join(repoRoot, DISCOVERY_STATE_FILE);
  }

  return path.isAbsolute(customStatePath) ? customStatePath : path.join(repoRoot, customStatePath);
}

async function readDiscoveryState(repoRoot, options = {}) {
  const statePath = resolveDiscoveryStatePath(repoRoot, options.statePath);

  try {
    const raw = await fs.readFile(statePath, 'utf8');
    return {
      statePath,
      state: normalizeDiscoveryState(JSON.parse(raw)),
    };
  } catch (error) {
    if (error?.code === 'ENOENT' || error instanceof SyntaxError) {
      return {
        statePath,
        state: defaultDiscoveryState(),
      };
    }
    throw error;
  }
}

async function writeDiscoveryState(statePath, state) {
  await fs.mkdir(path.dirname(statePath), { recursive: true });
  await fs.writeFile(statePath, `${JSON.stringify(normalizeDiscoveryState(state), null, 2)}\n`, 'utf8');
}

function recordDiscoverySelection(state, lane) {
  const nextState = normalizeDiscoveryState(state);
  const normalizedLane = normalizeLane(lane);
  nextState.lastLane = normalizedLane;
  if (normalizedLane) {
    nextState.counts[normalizedLane] += 1;
  }
  return nextState;
}

export function verifyDiscoveryPolicyModel() {
  const findings = [];
  const priorities = [null, DISCOVERY_PRIORITY_REAL, DISCOVERY_PRIORITY_FALLBACK];
  const lastLanes = ['', DISCOVERY_LANE_UI, DISCOVERY_LANE_BACKEND];
  let casesChecked = 0;

  for (const lastLane of lastLanes) {
    for (const uiPriority of priorities) {
      for (const backendPriority of priorities) {
        casesChecked += 1;
        const lane = chooseDiscoveryLane(
          {
            [DISCOVERY_LANE_UI]:
              uiPriority == null ? null : createCandidate(DISCOVERY_LANE_UI, uiPriority, { source: 'test-ui' }),
            [DISCOVERY_LANE_BACKEND]:
              backendPriority == null
                ? null
                : createCandidate(DISCOVERY_LANE_BACKEND, backendPriority, { source: 'test-backend' }),
          },
          { lastLane },
        );

        if (uiPriority == null && backendPriority == null && lane !== '') {
          findings.push('Discovery policy selected a lane even though no candidates were available.');
        }

        if (uiPriority != null && backendPriority == null && lane !== DISCOVERY_LANE_UI) {
          findings.push('Discovery policy failed to choose the UI lane when it was the only available candidate.');
        }

        if (uiPriority == null && backendPriority != null && lane !== DISCOVERY_LANE_BACKEND) {
          findings.push('Discovery policy failed to choose the backend lane when it was the only available candidate.');
        }

        if (uiPriority != null && backendPriority != null) {
          if (uiPriority < backendPriority && lane !== DISCOVERY_LANE_UI) {
            findings.push('Discovery policy failed to prefer a higher-priority UI candidate over a backend fallback.');
          }

          if (backendPriority < uiPriority && lane !== DISCOVERY_LANE_BACKEND) {
            findings.push('Discovery policy failed to prefer a higher-priority backend candidate over a UI fallback.');
          }

          if (uiPriority === backendPriority) {
            if (lastLane === DISCOVERY_LANE_UI && lane !== DISCOVERY_LANE_BACKEND) {
              findings.push('Discovery policy did not alternate to backend after a UI selection when priorities were equal.');
            }

            if (lastLane === DISCOVERY_LANE_BACKEND && lane !== DISCOVERY_LANE_UI) {
              findings.push('Discovery policy did not alternate to UI after a backend selection when priorities were equal.');
            }

            if (lastLane === '' && lane !== DISCOVERY_LANE_BACKEND) {
              findings.push('Discovery policy did not bias the first equal-priority tie toward the backend lane.');
            }
          }
        }
      }
    }
  }

  return {
    ok: findings.length === 0,
    findings,
    casesChecked,
  };
}

export async function buildDefaultIdea(repoRoot, options = {}) {
  const candidates = await collectDiscoveryCandidates(repoRoot);
  const lane = chooseDiscoveryLane(candidates, { lastLane: options.lastLane });
  const selectedCandidate = lane ? candidates[lane] : null;

  if (!selectedCandidate) {
    return buildUiFallbackIdea();
  }

  return {
    ...selectedCandidate.idea,
    lane: selectedCandidate.lane,
    priority: selectedCandidate.priority,
  };
}

export async function discoverImprovementIdea(repoRoot, options = {}) {
  const { statePath, state } = await readDiscoveryState(repoRoot, options);
  const idea = await buildDefaultIdea(repoRoot, { lastLane: state.lastLane });

  if (options.persistState === false) {
    return idea;
  }

  const nextState = recordDiscoverySelection(state, idea.lane);
  await writeDiscoveryState(statePath, nextState);
  return {
    ...idea,
    discoveryState: nextState,
    discoveryStatePath: statePath,
  };
}
