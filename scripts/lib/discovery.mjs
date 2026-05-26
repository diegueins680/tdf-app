import fs from 'node:fs/promises';
import path from 'node:path';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { collectUiFindings } from './ui-static-audit.mjs';
import { collectLogicalFindings } from './logical-correctness-audit.mjs';
import { collectFormalFindings } from './formal-methods-audit.mjs';
import { collectUxFindings } from './ux-quality-audit.mjs';

const execFileAsync = promisify(execFile);
const DISCOVERY_LANE_LOGICAL = 'logical';
const DISCOVERY_LANE_FORMAL = 'formal';
const DISCOVERY_LANE_UX = 'ux';
const DISCOVERY_LANE_UI = 'ui';
const DISCOVERY_LANE_BACKEND = 'backend';

const DISCOVERY_PRIORITY_CRITICAL = 0;
const DISCOVERY_PRIORITY_REAL = 1;
const DISCOVERY_PRIORITY_FALLBACK = 2;
const DISCOVERY_LANES = Object.freeze([
  DISCOVERY_LANE_LOGICAL,
  DISCOVERY_LANE_FORMAL,
  DISCOVERY_LANE_UX,
  DISCOVERY_LANE_UI,
  DISCOVERY_LANE_BACKEND,
]);
const DISCOVERY_TIE_BREAKS = Object.freeze({
  [DISCOVERY_PRIORITY_CRITICAL]: Object.freeze([
    DISCOVERY_LANE_LOGICAL,
    DISCOVERY_LANE_FORMAL,
    DISCOVERY_LANE_UX,
    DISCOVERY_LANE_UI,
    DISCOVERY_LANE_BACKEND,
  ]),
  [DISCOVERY_PRIORITY_REAL]: Object.freeze([
    DISCOVERY_LANE_LOGICAL,
    DISCOVERY_LANE_FORMAL,
    DISCOVERY_LANE_BACKEND,
    DISCOVERY_LANE_UI,
    DISCOVERY_LANE_UX,
  ]),
  [DISCOVERY_PRIORITY_FALLBACK]: Object.freeze([
    DISCOVERY_LANE_BACKEND,
    DISCOVERY_LANE_UI,
    DISCOVERY_LANE_UX,
    DISCOVERY_LANE_LOGICAL,
    DISCOVERY_LANE_FORMAL,
  ]),
});

const DISCOVERY_STATE_FILE = path.join('tmp', 'continuous-improvement-loop-discovery.json');
const SCANNED_EXTENSIONS = new Set([
  '.ts', '.tsx', '.js', '.jsx', '.mjs', '.cjs', '.json', '.md', '.hs', '.yml', '.yaml', '.sh', '.sql', '.txt',
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
      matches.push({ file: path.relative(repoRoot, filePath), line: index + 1, text });
      if (matches.length >= 50) return matches;
    }
  }
  return matches;
}

// ─── Idea Builders ───

function buildLogicalIdea(repoRoot, finding) {
  const relativeFile = path.relative(repoRoot, finding.file);
  return {
    source: 'builtin-logical-audit',
    lane: DISCOVERY_LANE_LOGICAL,
    title: `Fix ${finding.rule} in ${relativeFile}`,
    markdown: [
      '# Improvement Idea',
      '',
      `Source: builtin logical correctness audit`,
      `Lane: ${DISCOVERY_LANE_LOGICAL}`,
      `Target: ${relativeFile}:${finding.line}`,
      `Reason: ${finding.message}`,
      '',
      'Acceptance criteria:',
      '- The logical flaw is corrected with the smallest safe change.',
      '- No new warnings or type errors are introduced.',
      '- Add or update a test that would have caught the original flaw.',
      '',
      'Context snippet:',
      `- ${finding.snippet}`,
      '',
      'Implement the most defensible fix. Prefer explicit over clever.',
      '',
    ].join('\n'),
  };
}

function buildFormalIdea(repoRoot, finding) {
  const relativeFile = path.relative(repoRoot, finding.file);
  return {
    source: 'builtin-formal-audit',
    lane: DISCOVERY_LANE_FORMAL,
    title: `Strengthen ${finding.rule} in ${relativeFile}`,
    markdown: [
      '# Improvement Idea',
      '',
      `Source: builtin formal methods audit`,
      `Lane: ${DISCOVERY_LANE_FORMAL}`,
      `Target: ${relativeFile}:${finding.line}`,
      `Reason: ${finding.message}`,
      '',
      'Acceptance criteria:',
      '- Add an explicit invariant, precondition, or type contract.',
      '- The change should be verifiable by static analysis or tests.',
      '- Keep the change minimal and focused on the reported location.',
      '',
      'Context snippet:',
      `- ${finding.snippet}`,
      '',
      'Prefer total functions, explicit exports, and documented side-effects.',
      '',
    ].join('\n'),
  };
}

function buildUxIdea(repoRoot, finding) {
  const relativeFile = path.relative(repoRoot, finding.file);
  return {
    source: 'builtin-ux-audit',
    lane: DISCOVERY_LANE_UX,
    title: `Improve UX: ${finding.rule} in ${relativeFile}`,
    markdown: [
      '# Improvement Idea',
      '',
      `Source: builtin UX quality audit`,
      `Lane: ${DISCOVERY_LANE_UX}`,
      `Target: ${relativeFile}:${finding.line}`,
      `Reason: ${finding.message}`,
      '',
      'Acceptance criteria:',
      '- The UI becomes simpler, more minimal, or more intuitive.',
      '- First-time users can complete the task without confusion.',
      '- The change is defensible with a targeted test or visual snapshot.',
      '',
      'Context snippet:',
      `- ${finding.snippet}`,
      '',
      'Remove clutter, reduce click depth, and make affordances obvious.',
      '',
    ].join('\n'),
  };
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

function buildUxFallbackIdea() {
  return {
    source: 'builtin-ux-fallback',
    lane: DISCOVERY_LANE_UX,
    title: 'Simplify and engage one user-facing screen',
    markdown: [
      '# Improvement Idea',
      '',
      'Source: ux fallback discovery',
      `Lane: ${DISCOVERY_LANE_UX}`,
      '',
      'Review one user-facing screen and make one concrete improvement that:',
      '- reduces visual clutter or duplicated actions,',
      '- makes the next step obvious for first-time users,',
      '- adds or improves a loading, empty, or error state,',
      '- can be defended with a targeted test or visual snapshot.',
      '',
      'Keep the change scoped to one screen or component. Avoid backend-only edits.',
      '',
    ].join('\n'),
  };
}

function buildUiFallbackIdea() {
  return {
    source: 'builtin-ui-fallback',
    lane: DISCOVERY_LANE_UI,
    title: 'Tighten one UI accessibility affordance',
    markdown: [
      '# Improvement Idea',
      '',
      'Source: ui fallback discovery',
      `Lane: ${DISCOVERY_LANE_UI}`,
      '',
      'Review one user-facing UI surface and make one concrete improvement that:',
      '- improves accessible names, labels, focus behavior, or alternative text,',
      '- keeps the existing visual design intact, and',
      '- can be defended with a targeted test or static UI audit.',
      '',
      'Keep the change scoped to one component or screen. Avoid backend-only edits.',
      '',
    ].join('\n'),
  };
}

// ─── Candidate Logic ───

function normalizeLane(value) {
  const valid = [DISCOVERY_LANE_LOGICAL, DISCOVERY_LANE_FORMAL, DISCOVERY_LANE_UX, DISCOVERY_LANE_UI, DISCOVERY_LANE_BACKEND];
  return valid.includes(value) ? value : '';
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

function createCandidate(lane, priority, idea, importance = 0) {
  return { lane, priority, idea, importance };
}

function uiFindingImportance(finding) {
  if (finding.severity === 'critical') return 100;
  if (finding.severity === 'error') return 150;
  if (finding.severity === 'warning') return 20;
  return 5;
}

function uiFindingPriority(finding) {
  return finding.severity === 'critical' || finding.severity === 'error'
    ? DISCOVERY_PRIORITY_CRITICAL
    : DISCOVERY_PRIORITY_REAL;
}

function tieBreakOrderForPriority(priority) {
  return DISCOVERY_TIE_BREAKS[priority] ?? DISCOVERY_TIE_BREAKS[DISCOVERY_PRIORITY_REAL];
}

function setCandidate(candidates, candidate) {
  const current = candidates[candidate.lane];
  if (!current || candidate.priority < current.priority) {
    candidates[candidate.lane] = candidate;
    return;
  }
  if (candidate.priority === current.priority && candidate.importance > (current.importance ?? 0)) {
    candidates[candidate.lane] = candidate;
  }
}

async function collectDiscoveryCandidates(repoRoot, options = {}) {
  const candidates = {
    [DISCOVERY_LANE_LOGICAL]: null,
    [DISCOVERY_LANE_FORMAL]: null,
    [DISCOVERY_LANE_UX]: null,
    [DISCOVERY_LANE_UI]: null,
    [DISCOVERY_LANE_BACKEND]: null,
  };

  // 1. Logical correctness audit (highest priority, whole codebase)
  const logicalFindings = await collectLogicalFindings(repoRoot);
  const criticalLogical = logicalFindings.find((f) => f.severity === 'critical');
  const errorLogical = logicalFindings.find((f) => f.severity === 'error');
  const firstLogical = criticalLogical || errorLogical || logicalFindings[0];
  if (firstLogical) {
    const priority = criticalLogical ? DISCOVERY_PRIORITY_CRITICAL : DISCOVERY_PRIORITY_REAL;
    setCandidate(
      candidates,
      createCandidate(DISCOVERY_LANE_LOGICAL, priority, buildLogicalIdea(repoRoot, firstLogical), firstLogical.importance ?? 0),
    );
  }

  // 2. Formal methods audit (second highest)
  const formalFindings = await collectFormalFindings(repoRoot);
  const criticalFormal = formalFindings.find((f) => f.severity === 'critical');
  const errorFormal = formalFindings.find((f) => f.severity === 'error');
  const firstFormal = criticalFormal || errorFormal || formalFindings[0];
  if (firstFormal) {
    const priority = criticalFormal ? DISCOVERY_PRIORITY_CRITICAL : DISCOVERY_PRIORITY_REAL;
    setCandidate(
      candidates,
      createCandidate(DISCOVERY_LANE_FORMAL, priority, buildFormalIdea(repoRoot, firstFormal), firstFormal.importance ?? 0),
    );
  }

  // 3. UX quality audit (third)
  const uxRoot = path.join(repoRoot, 'tdf-hq-ui', 'src');
  if (await pathExists(uxRoot)) {
    const uxFindings = await collectUxFindings(uxRoot);
    const criticalUx = uxFindings.find((f) => f.severity === 'critical');
    const errorUx = uxFindings.find((f) => f.severity === 'error');
    const firstUx = criticalUx || errorUx || uxFindings[0];
    if (firstUx) {
      const priority = criticalUx ? DISCOVERY_PRIORITY_CRITICAL : DISCOVERY_PRIORITY_REAL;
      setCandidate(
        candidates,
        createCandidate(DISCOVERY_LANE_UX, priority, buildUxIdea(repoRoot, firstUx), firstUx.importance ?? 0),
      );
    }
  }

  // 4. Legacy UI accessibility audit (fourth)
  const uiRoot = path.join(repoRoot, 'tdf-hq-ui', 'src');
  if (await pathExists(uiRoot)) {
    const uiFindings = await collectUiFindings(uiRoot);
    if (uiFindings.length > 0) {
      setCandidate(
        candidates,
        createCandidate(DISCOVERY_LANE_UI, uiFindingPriority(uiFindings[0]), buildUiIdea(repoRoot, uiFindings[0]), uiFindingImportance(uiFindings[0])),
      );
    }
  }

  // 5. TODO scan
  const todoMatches = await scanTodoMatches(repoRoot);
  for (const match of todoMatches) {
    const lane = classifyTodoLane(match.file);
    setCandidate(candidates, createCandidate(lane, DISCOVERY_PRIORITY_REAL, buildTodoIdea(match, lane)));
    if (
      candidates[DISCOVERY_LANE_UI]?.priority === DISCOVERY_PRIORITY_REAL &&
      candidates[DISCOVERY_LANE_BACKEND]?.priority === DISCOVERY_PRIORITY_REAL
    ) {
      break;
    }
  }

  // 6. Fallbacks (lowest priority)
  if (!candidates[DISCOVERY_LANE_LOGICAL]) {
    // No logical fallback — this lane should only fire when there are real issues
  }
  if (!candidates[DISCOVERY_LANE_FORMAL]) {
    // No formal fallback — only real issues
  }
  if (!candidates[DISCOVERY_LANE_UX]) {
    setCandidate(candidates, createCandidate(DISCOVERY_LANE_UX, DISCOVERY_PRIORITY_FALLBACK, buildUxFallbackIdea()));
  }
  if (!candidates[DISCOVERY_LANE_UI]) {
    setCandidate(candidates, createCandidate(DISCOVERY_LANE_UI, DISCOVERY_PRIORITY_FALLBACK, buildUiFallbackIdea()));
  }

  const backendRoot = path.join(repoRoot, 'tdf-hq');
  if (!candidates[DISCOVERY_LANE_BACKEND] && (await pathExists(backendRoot))) {
    setCandidate(candidates, createCandidate(DISCOVERY_LANE_BACKEND, DISCOVERY_PRIORITY_FALLBACK, buildBackendFallbackIdea()));
  }

  return candidates;
}

// ─── Lane Selection ───

export function chooseDiscoveryLane(candidates, options = {}) {
  const lanes = DISCOVERY_LANES;
  const lastLane = normalizeLane(options.lastLane);

  // Contract: priority is primary, critical lane precedence is next, and
  // importance only ranks non-critical candidates inside the same priority.
  let bestPriority = Infinity;

  for (const lane of lanes) {
    const candidate = candidates?.[lane] ?? null;
    if (!candidate) continue;
    if (candidate.priority < bestPriority) {
      bestPriority = candidate.priority;
    }
  }

  if (bestPriority === Infinity) {
    return '';
  }

  const priorityLanes = tieBreakOrderForPriority(bestPriority).filter((lane) => {
    const candidate = candidates?.[lane] ?? null;
    return candidate && candidate.priority === bestPriority;
  });

  // Critical audit findings are safety gates. Rotation and importance cannot
  // displace logical > formal > UX > UI > backend precedence for this tier.
  if (bestPriority === DISCOVERY_PRIORITY_CRITICAL) {
    return priorityLanes[0] ?? '';
  }

  const bestImportance = priorityLanes.reduce((best, lane) => {
    const candidate = candidates?.[lane] ?? null;
    return Math.max(best, candidate?.importance ?? 0);
  }, -1);
  const tiedLanes = priorityLanes.filter((lane) => (candidates?.[lane]?.importance ?? 0) === bestImportance);

  if (tiedLanes.length > 1) {
    if (lastLane && tiedLanes.includes(lastLane)) {
      const nextIndex = (tiedLanes.indexOf(lastLane) + 1) % tiedLanes.length;
      return tiedLanes[nextIndex];
    }
    return tiedLanes[0];
  }

  return tiedLanes[0] ?? '';
}

// ─── State Management ───

function defaultDiscoveryState() {
  return {
    version: 2,
    lastLane: '',
    counts: {
      [DISCOVERY_LANE_LOGICAL]: 0,
      [DISCOVERY_LANE_FORMAL]: 0,
      [DISCOVERY_LANE_UX]: 0,
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
  for (const lane of Object.keys(state.counts)) {
    state.counts[lane] = normalizeCount(counts[lane]);
  }
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
    return { statePath, state: normalizeDiscoveryState(JSON.parse(raw)) };
  } catch (error) {
    if (error?.code === 'ENOENT' || error instanceof SyntaxError) {
      return { statePath, state: defaultDiscoveryState() };
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

// ─── Model Verification ───

export function verifyDiscoveryPolicyModel() {
  const findings = [];
  const priorities = [null, DISCOVERY_PRIORITY_CRITICAL, DISCOVERY_PRIORITY_REAL, DISCOVERY_PRIORITY_FALLBACK];
  const lanes = [DISCOVERY_LANE_LOGICAL, DISCOVERY_LANE_FORMAL, DISCOVERY_LANE_UX, DISCOVERY_LANE_UI, DISCOVERY_LANE_BACKEND];
  const lastLanes = ['', ...lanes];
  let casesChecked = 0;

  for (const lastLane of lastLanes) {
    for (const logicalPriority of priorities) {
      for (const formalPriority of priorities) {
        for (const uxPriority of priorities) {
          for (const uiPriority of priorities) {
            for (const backendPriority of priorities) {
              casesChecked += 1;
              const cands = {};
              const addCand = (lane, p) => {
                if (p != null) {
                  cands[lane] = { lane, priority: p, idea: { source: `test-${lane}` }, importance: 0 };
                }
              };
              addCand(DISCOVERY_LANE_LOGICAL, logicalPriority);
              addCand(DISCOVERY_LANE_FORMAL, formalPriority);
              addCand(DISCOVERY_LANE_UX, uxPriority);
              addCand(DISCOVERY_LANE_UI, uiPriority);
              addCand(DISCOVERY_LANE_BACKEND, backendPriority);

              const lane = chooseDiscoveryLane(cands, { lastLane });

              const allNull = logicalPriority == null && formalPriority == null && uxPriority == null && uiPriority == null && backendPriority == null;
              if (allNull && lane !== '') {
                findings.push('Discovery policy selected a lane even though no candidates were available.');
              }

              // Critical should always win over real
              if (logicalPriority === DISCOVERY_PRIORITY_CRITICAL && lane !== DISCOVERY_LANE_LOGICAL) {
                findings.push('Discovery policy failed to prefer critical logical issue.');
              }
              if (formalPriority === DISCOVERY_PRIORITY_CRITICAL && logicalPriority !== DISCOVERY_PRIORITY_CRITICAL && lane !== DISCOVERY_LANE_FORMAL) {
                findings.push('Discovery policy failed to prefer critical formal issue.');
              }
              if (uxPriority === DISCOVERY_PRIORITY_CRITICAL && logicalPriority !== DISCOVERY_PRIORITY_CRITICAL && formalPriority !== DISCOVERY_PRIORITY_CRITICAL && lane !== DISCOVERY_LANE_UX) {
                findings.push('Discovery policy failed to prefer critical UX issue.');
              }
            }
          }
        }
      }
    }
  }

  const criticalPrecedenceCases = [
    {
      expected: DISCOVERY_LANE_LOGICAL,
      candidates: {
        [DISCOVERY_LANE_LOGICAL]: { priority: DISCOVERY_PRIORITY_CRITICAL, importance: 1 },
        [DISCOVERY_LANE_FORMAL]: { priority: DISCOVERY_PRIORITY_CRITICAL, importance: 999 },
        [DISCOVERY_LANE_UX]: { priority: DISCOVERY_PRIORITY_CRITICAL, importance: 999 },
        [DISCOVERY_LANE_UI]: { priority: DISCOVERY_PRIORITY_CRITICAL, importance: 999 },
      },
    },
    {
      expected: DISCOVERY_LANE_FORMAL,
      candidates: {
        [DISCOVERY_LANE_FORMAL]: { priority: DISCOVERY_PRIORITY_CRITICAL, importance: 1 },
        [DISCOVERY_LANE_UX]: { priority: DISCOVERY_PRIORITY_CRITICAL, importance: 999 },
        [DISCOVERY_LANE_UI]: { priority: DISCOVERY_PRIORITY_CRITICAL, importance: 999 },
      },
    },
    {
      expected: DISCOVERY_LANE_UX,
      candidates: {
        [DISCOVERY_LANE_UX]: { priority: DISCOVERY_PRIORITY_CRITICAL, importance: 1 },
        [DISCOVERY_LANE_UI]: { priority: DISCOVERY_PRIORITY_CRITICAL, importance: 999 },
        [DISCOVERY_LANE_BACKEND]: { priority: DISCOVERY_PRIORITY_CRITICAL, importance: 999 },
      },
    },
  ];

  for (const lastLane of lastLanes) {
    for (const { expected, candidates: caseCandidates } of criticalPrecedenceCases) {
      casesChecked += 1;
      const cands = {};
      for (const [lane, candidate] of Object.entries(caseCandidates)) {
        cands[lane] = { lane, idea: { source: `test-${lane}` }, ...candidate };
      }
      const lane = chooseDiscoveryLane(cands, { lastLane });
      if (lane !== expected) {
        findings.push(`Discovery policy allowed importance or rotation to displace critical ${expected} issue.`);
      }
    }
  }

  return { ok: findings.length === 0, findings, casesChecked };
}

// ─── Public API ───

export async function buildDefaultIdea(repoRoot, options = {}) {
  const candidates = await collectDiscoveryCandidates(repoRoot, { counts: options.counts });
  const lane = chooseDiscoveryLane(candidates, { lastLane: options.lastLane, counts: options.counts });
  const selectedCandidate = lane ? candidates[lane] : null;

  if (!selectedCandidate) {
    return buildUxFallbackIdea();
  }

  return {
    ...selectedCandidate.idea,
    lane: selectedCandidate.lane,
    priority: selectedCandidate.priority,
  };
}

export async function discoverImprovementIdea(repoRoot, options = {}) {
  const { statePath, state } = await readDiscoveryState(repoRoot, options);
  const idea = await buildDefaultIdea(repoRoot, { lastLane: state.lastLane, counts: state.counts });

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
