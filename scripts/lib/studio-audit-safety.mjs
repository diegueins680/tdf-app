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
