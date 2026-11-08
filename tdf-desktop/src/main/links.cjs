'use strict';
// Deep links and navigation policy. Every URL that may open the shell or move
// its window is classified here before anything acts on it.
const { INTERNAL_HOST, INTERNAL_SCHEME, sanitizeAppPath } = require('./config.cjs');

const MAX_URL_LENGTH = 8192;

/**
 * Parses `tdf-desktop://open?path=/ruta` or `tdf-desktop://open/ruta?x=1`, and
 * absolute links on the hosted origin, into a safe in-app path. Returns null
 * for anything else (other hosts, schemes, traversal, protocol-relative paths).
 */
function parseDeepLink(rawUrl, appOrigin) {
  if (typeof rawUrl !== 'string' || rawUrl.length === 0 || rawUrl.length > MAX_URL_LENGTH) return null;
  let url;
  try { url = new URL(rawUrl.trim()); } catch { return null; }
  if (url.username || url.password) return null;
  if (url.protocol === `${INTERNAL_SCHEME}:`) {
    if (url.host !== 'open') return null;
    const explicit = url.searchParams.get('path');
    const candidate = explicit !== null ? explicit : `${url.pathname}${url.search}${url.hash}`;
    const path = sanitizeAppPath(candidate === '' ? '/' : candidate);
    return path ? { path } : null;
  }
  if ((url.protocol === 'https:' || url.protocol === 'http:') && url.origin === appOrigin) {
    const path = sanitizeAppPath(`${url.pathname}${url.search}${url.hash}`);
    return path ? { path } : null;
  }
  return null;
}

/** Picks the deep link from process arguments (Windows/Linux second-instance). */
function deepLinkFromArgv(argv, appOrigin) {
  for (const argument of Array.isArray(argv) ? argv : []) {
    if (typeof argument !== 'string' || !argument.startsWith(`${INTERNAL_SCHEME}://`)) continue;
    const parsed = parseDeepLink(argument, appOrigin);
    if (parsed) return parsed;
  }
  return null;
}

/** http(s) with no embedded credentials; anything else is refused. */
function isSafeExternalUrl(rawUrl) {
  if (typeof rawUrl !== 'string' || rawUrl.length === 0 || rawUrl.length > MAX_URL_LENGTH) return false;
  let url;
  try { url = new URL(rawUrl); } catch { return false; }
  return (url.protocol === 'https:' || url.protocol === 'http:') && !url.username && !url.password;
}

/**
 * app:      stays inside the hosted application origin.
 * internal: a bundled shell page (offline/about) on the privileged scheme.
 * external: an http(s) destination for the system browser.
 * blocked:  everything else (file:, javascript:, data:, foreign schemes, credentials).
 */
function classifyNavigation(rawUrl, appOrigin) {
  if (typeof rawUrl !== 'string' || rawUrl.length === 0 || rawUrl.length > MAX_URL_LENGTH) return 'blocked';
  let url;
  try { url = new URL(rawUrl); } catch { return 'blocked'; }
  if (url.username || url.password) return 'blocked';
  if (url.protocol === `${INTERNAL_SCHEME}:`) return url.host === INTERNAL_HOST ? 'internal' : 'blocked';
  if (url.protocol !== 'https:' && url.protocol !== 'http:') return 'blocked';
  if (url.origin === appOrigin) return 'app';
  return 'external';
}

module.exports = { MAX_URL_LENGTH, classifyNavigation, deepLinkFromArgv, isSafeExternalUrl, parseDeepLink };
