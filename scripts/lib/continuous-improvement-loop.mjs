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

export function expandTemplate(template, context) {
  return template.replace(/\{([a-zA-Z0-9_]+)\}/g, (match, key) => {
    if (!hasOwn(context, key)) return match;
    return String(context[key]);
  });
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

  return {
    ok: findings.length === 0,
    findings,
    initial: model.initial,
    states,
    reachableStates: [...reachable].sort(),
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
