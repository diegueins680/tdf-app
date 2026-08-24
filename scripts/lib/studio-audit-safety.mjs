const LOCAL_DRAFT_HOSTS = new Set(['localhost', '127.0.0.1', '::1']);
const DEFAULT_REMOTE_DRAFT_HOSTS = new Set(['tdf-hq-studio-audit-staging.fly.dev']);

export function parseAdditionalDraftHosts(raw = '') {
  return raw
    .split(',')
    .map((value) => value.trim().toLowerCase())
    .filter(Boolean);
}

export function isAllowedDraftApiBase(rawApiBase, additionalHosts = []) {
  let url;
  try {
    url = new URL(rawApiBase);
  } catch {
    return false;
  }

  if (url.username || url.password) return false;
  const host = url.hostname.toLowerCase();
  const local = LOCAL_DRAFT_HOSTS.has(host);
  if (local && !['http:', 'https:'].includes(url.protocol)) return false;
  if (!local && url.protocol !== 'https:') return false;

  const allowedRemoteHosts = new Set([
    ...DEFAULT_REMOTE_DRAFT_HOSTS,
    ...additionalHosts.map((value) => value.trim().toLowerCase()).filter(Boolean),
  ]);
  return local || allowedRemoteHosts.has(host);
}

export function assertReusableAuditDraft({ task, plan, expectedPartyId, draft }) {
  const mismatches = [];
  if (task.itProposedAssignee !== expectedPartyId) mismatches.push('task proposed assignee');
  if (plan.iapProposedAssignee !== expectedPartyId) mismatches.push('plan proposed assignee');
  if (plan.iapProjectId !== task.itProjectId) mismatches.push('plan project');
  if (plan.iapTaskId !== task.itId) mismatches.push('plan task');
  if (plan.iapEnvironment !== draft.environment) mismatches.push('environment');
  if (plan.iapDurationDays !== draft.durationDaysFromActivation) mismatches.push('duration');
  if (plan.iapExpectedHoursMin !== draft.expectedEffortHours.minimum) mismatches.push('minimum effort');
  if (plan.iapExpectedHoursMax !== draft.expectedEffortHours.maximum) mismatches.push('maximum effort');
  if (plan.iapMidpointPercent !== draft.midpointPercent) mismatches.push('midpoint');
  if (plan.iapFinalReviewRequired !== draft.finalReviewAndDemonstrationRequired) {
    mismatches.push('final review requirement');
  }
  if (mismatches.length > 0) {
    throw new Error(`The matching draft differs from the approved configuration (${mismatches.join(', ')}); refusing to modify it.`);
  }
}
