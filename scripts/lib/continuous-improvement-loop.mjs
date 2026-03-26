import path from 'node:path';
import { verifyDiscoveryPolicyModel } from './discovery.mjs';

const SUCCESSFUL_CHECK_CONCLUSIONS = new Set(['success', 'neutral', 'skipped']);
const FAILED_CHECK_CONCLUSIONS = new Set([
  'failure',
  'timed_out',
  'cancelled',
  'action_required',
  'startup_failure',
  'stale',
]);

function hasOwn(object, key) {
  return Object.prototype.hasOwnProperty.call(object, key);
}

function normalizeWhitespace(value) {
  return String(value ?? '')
    .replace(/\s+/g, ' ')
    .trim();
}

function stripMarkdown(value) {
  return value.replace(/[`*_#[\]()]/g, ' ');
}

function splitCamelCase(value) {
  return value
    .replace(/([a-z0-9])([A-Z])/g, '$1 $2')
    .replace(/([A-Z]+)([A-Z][a-z])/g, '$1 $2');
}

function cleanCommitText(value) {
  return normalizeWhitespace(
    stripMarkdown(splitCamelCase(value))
      .replace(/\.[A-Za-z0-9]+/g, ' ')
      .replace(/[_/\\.-]+/g, ' ')
      .replace(/[^a-zA-Z0-9 ]+/g, ' '),
  ).toLowerCase();
}

function truncateSubject(value, maxLength = 72) {
  if (value.length <= maxLength) {
    return value;
  }

  const clipped = value.slice(0, maxLength - 1).trimEnd();
  const lastSpace = clipped.lastIndexOf(' ');
  if (lastSpace >= 24) {
    return `${clipped.slice(0, lastSpace)}…`;
  }
  return `${clipped}…`;
}

function isDocFile(filePath) {
  const base = path.basename(filePath).toLowerCase();
  return base === 'readme.md' || /\.(md|mdx|txt)$/i.test(filePath);
}

function isTestFile(filePath) {
  return /(^|\/)(__tests__|tests?)\//.test(filePath) || /\.(test|spec)\.[^.]+$/i.test(filePath);
}

function isLoopRelatedFile(filePath) {
  return (
    /continuous-improvement-loop|codex-loop-worker|discover-improvement-idea|verify-improvement-loop/.test(filePath) ||
    filePath === 'scripts/ui-static-audit.mjs' ||
    filePath === 'scripts/lib/continuous-improvement-loop.mjs' ||
    filePath === 'scripts/lib/discovery.mjs'
  );
}

function describeFileArea(filePath) {
  const baseName = path.basename(filePath, path.extname(filePath));

  if (/continuous-improvement-loop/.test(filePath)) return 'continuous improvement loop';
  if (baseName === 'codex-loop-worker') return 'codex loop worker';
  if (baseName === 'discover-improvement-idea') return 'improvement idea discovery';
  if (baseName === 'verify-improvement-loop') return 'improvement loop verification';
  if (baseName === 'README') return 'README';
  if (baseName === 'package') return 'workspace package metadata';
  if (baseName === 'index') {
    return cleanCommitText(path.basename(path.dirname(filePath)));
  }

  return cleanCommitText(baseName);
}

function unique(values) {
  return [...new Set(values.filter(Boolean))];
}

function escapeRegExp(value) {
  return value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function extractTargetPath(value) {
  const match = normalizeWhitespace(value).match(/([A-Za-z0-9_./-]+\.[A-Za-z0-9]+)(?::\d+(?::\d+)?)?/);
  return match ? match[1] : '';
}

function matchesTargetPath(stagedFiles, targetPath) {
  if (!targetPath) return false;
  return stagedFiles.some((filePath) => filePath === targetPath || path.basename(filePath) === path.basename(targetPath));
}

function summarizeIdeaTitle(title, targetPath) {
  let summary = normalizeWhitespace(title);
  if (!summary) return '';

  if (targetPath) {
    const area = describeFileArea(targetPath);
    const replacements = unique([targetPath, path.basename(targetPath)]);
    for (const candidate of replacements) {
      summary = summary.replace(new RegExp(escapeRegExp(candidate), 'g'), area);
    }
  }

  return cleanCommitText(summary);
}

function isGenericIdeaTitle(title) {
  const normalized = cleanCommitText(title);
  return normalized === '' || normalized === 'improvement idea' || normalized === 'improvement' || normalized === 'idea';
}

function summarizeIdeaReason(reason, targetPath) {
  const normalizedReason = normalizeWhitespace(reason);
  if (!normalizedReason) return '';

  const area = targetPath ? describeFileArea(targetPath) : '';
  const lowerReason = normalizedReason.toLowerCase();

  if (/iconbutton\b.*explicit accessible label/.test(lowerReason)) {
    return area ? `label icon buttons in ${area}` : 'label icon buttons';
  }

  if (/textfield\b.*visible or programmatic label/.test(lowerReason)) {
    return area ? `label form fields in ${area}` : 'label form fields';
  }

  if (/\b(img|image)\b.*\balt\b/.test(lowerReason)) {
    return area ? `add image alt text in ${area}` : 'add image alt text';
  }

  if (/build failed/.test(lowerReason)) {
    return area ? `stabilize build in ${area}` : 'stabilize build';
  }

  const summary = area ? `${normalizedReason} in ${area}` : normalizedReason;
  return cleanCommitText(summary);
}

function inferCommitType(context, stagedFiles) {
  if (stagedFiles.length > 0 && stagedFiles.every(isDocFile)) return 'docs';
  if (stagedFiles.length > 0 && stagedFiles.every(isTestFile)) return 'test';
  if (stagedFiles.length > 0 && stagedFiles.every((filePath) => filePath.startsWith('scripts/') || isDocFile(filePath) || filePath === 'package.json')) {
    return 'chore';
  }

  const hintText = normalizeWhitespace([context.idea_title, context.idea_reason].filter(Boolean).join(' ')).toLowerCase();
  if (/\b(add|implement|introduce|create|build)\b/.test(hintText)) return 'feat';
  if (/\b(address|resolve|repair|correct|stabilize|reduce|remove|improve|fix|missing|error|bug|todo|audit|verify|accessib|label)\b/.test(hintText)) {
    return 'fix';
  }

  if (
    stagedFiles.some(
      (filePath) =>
        filePath.startsWith('tdf-hq-ui/src/') ||
        filePath.startsWith('tdf-hq/src/') ||
        filePath.startsWith('tdf-mobile/src/') ||
        /\.(ts|tsx|js|jsx|hs|sql)$/i.test(filePath),
    )
  ) {
    return 'fix';
  }

  return 'chore';
}

function summarizeStagedArea(stagedFiles) {
  if (stagedFiles.length === 0) return 'project files';

  if (stagedFiles.every((filePath) => isLoopRelatedFile(filePath) || isDocFile(filePath) || filePath === 'package.json')) {
    return 'continuous improvement loop';
  }

  const focusFiles = stagedFiles.filter((filePath) => !isDocFile(filePath));
  const targetFiles = focusFiles.length > 0 ? focusFiles : stagedFiles;

  if (targetFiles.length === 1) {
    return describeFileArea(targetFiles[0]);
  }

  const areas = unique(targetFiles.map(describeFileArea));
  if (areas.length === 1) {
    return areas[0];
  }

  const topLevels = unique(targetFiles.map((filePath) => filePath.split('/')[0]));
  if (topLevels.length === 1) {
    if (topLevels[0] === 'scripts') return 'automation scripts';
    return cleanCommitText(topLevels[0]);
  }

  return 'project files';
}

function buildAreaSummary(commitType, stagedFiles) {
  const area = summarizeStagedArea(stagedFiles);
  if (commitType === 'docs') return `document ${area}`;
  if (commitType === 'test') return `cover ${area}`;
  if (commitType === 'feat') return `add ${area}`;
  if (commitType === 'fix') return `improve ${area}`;
  return `update ${area}`;
}

export function expandTemplate(template, context) {
  return template.replace(/\{([a-zA-Z0-9_]+)\}/g, (match, key) => {
    if (!hasOwn(context, key)) return match;
    return String(context[key]);
  });
}

export function parseIdeaMarkdown(markdown) {
  const lines = String(markdown ?? '').split(/\r?\n/);
  const metadata = {
    title: '',
    source: '',
    target: '',
    reason: '',
  };

  for (const rawLine of lines) {
    const line = rawLine.trim();
    if (!metadata.title && /^#\s+/.test(line)) {
      metadata.title = normalizeWhitespace(line.replace(/^#+\s*/, ''));
      continue;
    }

    const match = line.match(/^([A-Za-z][A-Za-z ]+):\s*(.+)$/);
    if (!match) continue;

    const key = match[1].trim().toLowerCase();
    const value = normalizeWhitespace(match[2]);
    if (key === 'source') metadata.source = value;
    if (key === 'target') metadata.target = value;
    if (key === 'reason') metadata.reason = value;
  }

  return metadata;
}

export function buildCommitContext(context, stagedFiles) {
  const primaryPath = stagedFiles[0] ?? '';
  const targetPath = extractTargetPath(context.idea_target || context.idea_title || '');
  const commitType = inferCommitType(context, stagedFiles);

  let commitSummary = '';
  if (
    context.idea_title &&
    !isGenericIdeaTitle(context.idea_title) &&
    (!targetPath || matchesTargetPath(stagedFiles, targetPath))
  ) {
    commitSummary = summarizeIdeaTitle(context.idea_title, targetPath);
  }

  const canUseTargetReason = !targetPath || matchesTargetPath(stagedFiles, targetPath);
  if (!commitSummary && context.idea_reason && canUseTargetReason) {
    commitSummary = summarizeIdeaReason(context.idea_reason, targetPath || primaryPath);
  }

  if (!commitSummary) {
    commitSummary = buildAreaSummary(commitType, stagedFiles);
  }

  const summaryBudget = Math.max(24, 72 - (`${commitType}: `.length));
  const clippedSummary = truncateSubject(commitSummary, summaryBudget);

  return {
    ...context,
    primary_path: primaryPath,
    files_changed: String(stagedFiles.length),
    staged_files: stagedFiles.join(', '),
    commit_type: commitType,
    commit_summary: clippedSummary,
    commit_message: `${commitType}: ${clippedSummary}`,
  };
}

export function parseGitHubRemote(remoteUrl) {
  if (!remoteUrl) return null;
  const trimmed = remoteUrl.trim();
  const patterns = [
    /^git@github\.com:([^/]+)\/(.+?)(?:\.git)?$/,
    /^ssh:\/\/git@github\.com\/([^/]+)\/(.+?)(?:\.git)?$/,
    /^https:\/\/github\.com\/([^/]+)\/(.+?)(?:\.git)?\/?$/,
  ];

  for (const pattern of patterns) {
    const match = trimmed.match(pattern);
    if (!match) continue;
    return {
      owner: match[1],
      repo: match[2].replace(/\.git$/, ''),
    };
  }

  return null;
}

export function buildImprovementLoopModel() {
  return {
    initial: 'discover',
    transitions: {
      discover: ['implement'],
      implement: ['uiAudit'],
      uiAudit: ['uiFix', 'formalAudit'],
      uiFix: ['formalAudit'],
      formalAudit: ['formalFix', 'commit'],
      formalFix: ['commit'],
      commit: ['push'],
      push: ['pollCi'],
      pollCi: ['discover', 'ciRepair'],
      ciRepair: ['commit'],
    },
  };
}

function nextStates(model, state, blockedSet) {
  if (blockedSet.has(state)) return [];
  return (model.transitions[state] ?? []).filter((candidate) => !blockedSet.has(candidate));
}

export function reachableStates(model, options = {}) {
  const start = options.start ?? model.initial;
  const blockedSet = new Set(options.blocked ?? []);
  if (blockedSet.has(start)) return new Set();

  const visited = new Set([start]);
  const queue = [start];

  while (queue.length > 0) {
    const state = queue.shift();
    for (const candidate of nextStates(model, state, blockedSet)) {
      if (visited.has(candidate)) continue;
      visited.add(candidate);
      queue.push(candidate);
    }
  }

  return visited;
}

export function isReachable(model, from, to, options = {}) {
  return reachableStates(model, { start: from, blocked: options.blocked }).has(to);
}

export function listDeadEnds(model, options = {}) {
  const blockedSet = new Set(options.blocked ?? []);
  return [...reachableStates(model, { blocked: [...blockedSet] })].filter(
    (state) => nextStates(model, state, blockedSet).length === 0,
  );
}

export function verifyImprovementLoopModel() {
  const model = buildImprovementLoopModel();
  const states = Object.keys(model.transitions);
  const referencedStates = new Set(Object.values(model.transitions).flat());
  const findings = [];

  for (const state of referencedStates) {
    if (!hasOwn(model.transitions, state)) {
      findings.push(`Transition graph references undefined state "${state}".`);
    }
  }

  const reachable = reachableStates(model);
  const unreachable = states.filter((state) => !reachable.has(state));
  if (unreachable.length > 0) {
    findings.push(`Unreachable states detected: ${unreachable.join(', ')}.`);
  }

  const deadEnds = listDeadEnds(model);
  if (deadEnds.length > 0) {
    findings.push(`Dead-end states detected: ${deadEnds.join(', ')}.`);
  }

  if (!isReachable(model, 'push', 'pollCi')) {
    findings.push('Push cannot reach CI polling.');
  }

  if (!isReachable(model, 'pollCi', 'discover')) {
    findings.push('A green CI result cannot return the loop to discovery.');
  }

  if (!isReachable(model, 'pollCi', 'pollCi', { blocked: ['discover'] })) {
    findings.push('A red CI result cannot re-enter polling through the repair cycle.');
  }

  if (isReachable(model, model.initial, 'push', { blocked: ['commit'] })) {
    findings.push('Push is reachable without committing first.');
  }

  if (isReachable(model, model.initial, 'pollCi', { blocked: ['push'] })) {
    findings.push('CI polling is reachable without a push.');
  }

  if (isReachable(model, model.initial, 'commit', { blocked: ['uiAudit'] })) {
    findings.push('Commit is reachable without a UI audit.');
  }

  if (isReachable(model, model.initial, 'commit', { blocked: ['formalAudit'] })) {
    findings.push('Commit is reachable without a formal verification step.');
  }

  const discoveryPolicy = verifyDiscoveryPolicyModel();
  if (!discoveryPolicy.ok) {
    findings.push(...discoveryPolicy.findings.map((finding) => `Discovery policy: ${finding}`));
  }

  return {
    ok: findings.length === 0,
    findings,
    initial: model.initial,
    states,
    reachableStates: [...reachable].sort(),
    discoveryPolicy,
  };
}

export function summarizeCheckRuns(checkRuns) {
  const summary = {
    pending: [],
    failed: [],
    successful: [],
  };

  for (const checkRun of checkRuns) {
    const record = {
      name: checkRun.name ?? '(unnamed check)',
      status: checkRun.status ?? 'unknown',
      conclusion: checkRun.conclusion ?? 'pending',
      detailsUrl: checkRun.details_url ?? checkRun.html_url ?? '',
    };

    if (record.status !== 'completed' || !checkRun.conclusion) {
      summary.pending.push(record);
      continue;
    }

    if (SUCCESSFUL_CHECK_CONCLUSIONS.has(checkRun.conclusion)) {
      summary.successful.push(record);
      continue;
    }

    if (FAILED_CHECK_CONCLUSIONS.has(checkRun.conclusion) || checkRun.conclusion) {
      summary.failed.push(record);
    }
  }

  return summary;
}

export function summarizeWorkflowRuns(workflowRuns) {
  const summary = {
    pending: [],
    failed: [],
    successful: [],
  };

  for (const workflowRun of workflowRuns) {
    const record = {
      id: workflowRun.id ?? 0,
      name: workflowRun.name ?? workflowRun.display_title ?? '(unnamed workflow)',
      status: workflowRun.status ?? 'unknown',
      conclusion: workflowRun.conclusion ?? 'pending',
      detailsUrl: workflowRun.html_url ?? '',
    };

    if (record.status !== 'completed' || !workflowRun.conclusion) {
      summary.pending.push(record);
      continue;
    }

    if (SUCCESSFUL_CHECK_CONCLUSIONS.has(workflowRun.conclusion)) {
      summary.successful.push(record);
      continue;
    }

    if (FAILED_CHECK_CONCLUSIONS.has(workflowRun.conclusion) || workflowRun.conclusion) {
      summary.failed.push(record);
    }
  }

  return summary;
}

export function htmlToText(value) {
  return normalizeWhitespace(
    String(value ?? '')
      .replace(/<[^>]+>/g, ' ')
      .replace(/&nbsp;/gi, ' ')
      .replace(/&amp;/gi, '&')
      .replace(/&lt;/gi, '<')
      .replace(/&gt;/gi, '>')
      .replace(/&quot;/gi, '"')
      .replace(/&#39;/gi, "'"),
  );
}
