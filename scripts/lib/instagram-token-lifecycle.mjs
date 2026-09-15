import { createCipheriv, createDecipheriv, createHash, hkdfSync, randomBytes } from 'node:crypto';
import { mkdir, open, readFile, rename, unlink } from 'node:fs/promises';
import { dirname } from 'node:path';

const DAY = 86400;
const MAX_TTL = 60 * DAY;
const GRAPH = 'https://graph.instagram.com';
const CODE_ENDPOINT = 'https://api.instagram.com/oauth/access_token';
const BASIC = 'instagram_business_basic';

// Only locally authored messages reach logs. Provider bodies, URLs, tokens,
// authorization codes and low-level exception messages must never be printed.
export class LifecycleError extends Error {}
function requireValue(condition, message) {
  if (!condition) throw new LifecycleError(message);
}
const fingerprint = token => createHash('sha256').update(token).digest('hex');
const seconds = () => Math.floor(Date.now() / 1000);
const validId = value => typeof value === 'string' && /^\d+$/.test(value);
const validToken = value => typeof value === 'string' && value.length > 0 && value.length <= 32768 && !/\s/.test(value);

export function validateConfiguration({ appId, appSecret, context }) {
  requireValue(validId(appId), 'INSTAGRAM_APP_ID must be an explicit numeric Instagram app ID');
  requireValue(typeof appSecret === 'string' && appSecret.length >= 16, 'INSTAGRAM_APP_SECRET is required');
  requireValue(typeof context === 'string' && /^[\w.-]+\/[\w.-]+$/.test(context), 'A repository-scoped lifecycle context is required');
}

function encryptionParameters(config) {
  validateConfiguration(config);
  const aad = Buffer.from(`instagram-lifecycle:v1:${config.context}:${config.appId}`);
  const key = Buffer.from(hkdfSync('sha256', config.appSecret, 'tdf-instagram-lifecycle-v1', aad, 32));
  return { aad, key };
}

export function sealBundle(bundle, config) {
  const { aad, key } = encryptionParameters(config);
  const iv = randomBytes(12);
  const cipher = createCipheriv('aes-256-gcm', key, iv);
  cipher.setAAD(aad);
  const ciphertext = Buffer.concat([cipher.update(JSON.stringify(bundle), 'utf8'), cipher.final()]);
  return JSON.stringify({ version: 1, iv: iv.toString('base64'), tag: cipher.getAuthTag().toString('base64'), ciphertext: ciphertext.toString('base64') });
}

export function openBundle(envelope, config) {
  const { aad, key } = encryptionParameters(config);
  try {
    requireValue(typeof envelope === 'string' && envelope.length <= 131072, 'Invalid encrypted lifecycle state');
    const data = JSON.parse(envelope);
    requireValue(data.version === 1, 'Unsupported lifecycle envelope version');
    for (const field of ['iv', 'tag', 'ciphertext']) {
      requireValue(typeof data[field] === 'string' && /^[A-Za-z0-9+/]+={0,2}$/.test(data[field]), 'Invalid lifecycle envelope encoding');
    }
    const iv = Buffer.from(data.iv, 'base64');
    const tag = Buffer.from(data.tag, 'base64');
    requireValue(iv.length === 12 && tag.length === 16, 'Invalid lifecycle envelope');
    const decipher = createDecipheriv('aes-256-gcm', key, iv);
    decipher.setAAD(aad);
    decipher.setAuthTag(tag);
    return JSON.parse(Buffer.concat([decipher.update(Buffer.from(data.ciphertext, 'base64')), decipher.final()]).toString('utf8'));
  } catch {
    throw new LifecycleError('Lifecycle state failed authentication; do not adopt or repair it by hand');
  }
}

export async function loadBundle(path, config) {
  let encrypted;
  try { encrypted = await readFile(path, 'utf8'); } catch {
    throw new LifecycleError('Encrypted lifecycle state is missing or unreadable; an approved OAuth bootstrap is required');
  }
  return openBundle(encrypted, config);
}

export async function saveBundle(path, bundle, config) {
  const encrypted = sealBundle(bundle, config);
  const temporary = `${path}.${randomBytes(8).toString('hex')}.tmp`;
  let created = false;
  try {
    await mkdir(dirname(path), { recursive: true, mode: 0o700 });
    const handle = await open(temporary, 'wx', 0o600);
    created = true;
    try { await handle.writeFile(encrypted); await handle.sync(); } finally { await handle.close(); }
    await rename(temporary, path);
    created = false;
  } catch {
    throw new LifecycleError('Encrypted lifecycle state could not be persisted; no deployment was attempted');
  } finally {
    if (created) await unlink(temporary).catch(() => {});
  }
}

export function retryDelay(attempt, retryAfter, nowMs = Date.now(), random = Math.random) {
  const numeric = retryAfter == null || retryAfter === '' ? NaN : Number(retryAfter);
  const date = typeof retryAfter === 'string' ? Date.parse(retryAfter) : NaN;
  const advised = Number.isFinite(numeric) && numeric >= 0 ? numeric * 1000 : date - nowMs;
  // Do not retry sooner than Retry-After. An excessive server delay is an
  // explicit deferred failure, not a capped sleep followed by a premature call.
  if (Number.isFinite(advised) && advised > 30000) throw new LifecycleError('Meta requested a longer retry delay; retry this check later');
  if (Number.isFinite(advised) && advised >= 0) return advised;
  return Math.round(Math.min(1000 * (2 ** (attempt - 1)), 8000) * (0.75 + random() * 0.5));
}

export function parseMetaResponse(text) {
  requireValue(typeof text === 'string' && text.length <= 1048576, 'Meta response exceeds the supported size');
  return JSON.parse(text, (key, value, context) => {
    // Meta can serialize Instagram IDs as JSON integer literals larger than
    // MAX_SAFE_INTEGER. Node 22's source-aware reviver preserves their exact
    // digits; String(value) would silently bind evidence to a rounded ID.
    if ((key === 'user_id' || key === 'id') && typeof value === 'number') {
      requireValue(typeof context?.source === 'string' && /^\d+$/.test(context.source), 'Meta returned a non-canonical numeric account ID');
      return context.source;
    }
    return value;
  });
}

export async function requestMeta(url, init = {}, {
  fetchImpl = globalThis.fetch, sleep = ms => new Promise(resolve => setTimeout(resolve, ms)),
  random = Math.random, maxAttempts = 3,
} = {}) {
  const target = new URL(url);
  requireValue(target.origin === GRAPH || target.href === CODE_ENDPOINT, 'Unsupported Meta endpoint');
  requireValue(Number.isInteger(maxAttempts) && maxAttempts >= 1 && maxAttempts <= 3, 'Invalid retry bound');
  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    let response;
    let data;
    let networkFailure = false;
    try {
      response = await fetchImpl(target.toString(), { ...init, redirect: 'error', signal: AbortSignal.timeout(15000) });
      try { data = parseMetaResponse(await response.text()); } catch {
        if (response.status === 429 || response.status >= 500) data = {};
        else throw new LifecycleError('Meta returned malformed JSON');
      }
    } catch (err) {
      if (err instanceof LifecycleError) throw err;
      networkFailure = true;
    }
    if (!networkFailure && response.ok && data && typeof data === 'object' && !data.error && !data.error_type) return data;
    const code = Number.isInteger(data?.error?.code) ? data.error.code : undefined;
    const status = Number.isInteger(response?.status) ? response.status : undefined;
    const permanent = code === 102 || code === 190 || status === 401 || status === 403;
    const transient = !permanent && (networkFailure || status === 429 || status >= 500 || [1, 2, 4, 17, 341].includes(code) || data?.error?.is_transient === true
      || /system error|temporar|try again|service unavailable/i.test(data?.error?.message || data?.error_message || ''));
    if (!transient || attempt === maxAttempts) {
      throw new LifecycleError(networkFailure ? 'Meta network request failed or timed out' : `Meta request failed (HTTP ${status ?? 'unknown'}, API code ${code ?? 'unknown'})`);
    }
    await sleep(retryDelay(attempt, response?.headers?.get('retry-after'), Date.now(), random));
  }
}

function oneAccount(data) {
  const account = Array.isArray(data?.data) && data.data.length === 1 ? data.data[0] : data;
  requireValue(validId(account?.user_id), 'Instagram account validation returned no unambiguous user ID');
  return account;
}

export async function readAccount(token, options) {
  requireValue(validToken(token), 'A valid token value is required');
  const url = new URL(`${GRAPH}/v26.0/me`);
  url.searchParams.set('fields', 'user_id');
  url.searchParams.set('access_token', token);
  return oneAccount(await requestMeta(url, {}, options)).user_id;
}

function parsePermissions(value) {
  const list = typeof value === 'string' ? value.split(',').map(x => x.trim()) : value;
  requireValue(Array.isArray(list) && list.length > 0 && list.every(x => typeof x === 'string' && /^instagram_business_[a-z_]+$/.test(x)), 'Meta did not return valid granted permissions');
  requireValue(list.includes(BASIC), 'The Instagram basic permission was not granted');
  return [...new Set(list)].sort();
}

function dataDeadline(data, previous = null) {
  if (data.data_access_expires_at === undefined) return previous;
  requireValue(Number.isSafeInteger(data.data_access_expires_at) && data.data_access_expires_at > 0, 'Meta returned an invalid data-access deadline');
  return previous === null ? data.data_access_expires_at : Math.min(previous, data.data_access_expires_at);
}

function issuedBundle(data, { appId, userId, permissions, issuedAt, receivedAt, authorization, previousDeadline = null }) {
  requireValue(validToken(data.access_token) && data.token_type?.toLowerCase() === 'bearer', 'Meta returned an invalid lifecycle credential');
  requireValue(Number.isSafeInteger(data.expires_in) && data.expires_in > 0 && data.expires_in <= MAX_TTL, 'Meta returned no valid authoritative token lifetime');
  return {
    version: 1,
    token: data.access_token,
    tokenSha256: fingerprint(data.access_token),
    appId, userId, permissions, issuedAt, receivedAt,
    expiresIn: data.expires_in,
    expiresAt: issuedAt + data.expires_in,
    dataAccessExpiresAt: dataDeadline(data, previousDeadline),
    authorization,
  };
}

export function validateBundle(bundle, { appId, expectedUserId, now = seconds() }) {
  requireValue(bundle?.version === 1 && validToken(bundle.token), 'An authenticated lifecycle credential is required');
  requireValue(bundle.tokenSha256 === fingerprint(bundle.token), 'Token and lifecycle evidence do not match');
  requireValue(validId(appId) && bundle.appId === appId, 'Instagram access token belongs to a different application');
  requireValue(validId(bundle.userId) && (!expectedUserId || bundle.userId === expectedUserId), 'Instagram account does not match the approved account');
  requireValue(bundle.authorization?.method === 'instagram_authorization_code' && bundle.authorization.appId === appId && bundle.authorization.userId === bundle.userId, 'OAuth ownership provenance is missing or inconsistent');
  requireValue(validId(bundle.authorization.scopedUserId), 'OAuth app-scoped identity evidence is missing');
  requireValue(Number.isSafeInteger(bundle.authorization.authorizedAt) && bundle.authorization.authorizedAt > 0 && bundle.authorization.authorizedAt <= bundle.issuedAt, 'Invalid authorization timestamp');
  requireValue(Number.isSafeInteger(bundle.issuedAt) && bundle.issuedAt > 0 && bundle.issuedAt <= now, 'Invalid lifecycle issuance timestamp');
  requireValue(Number.isSafeInteger(bundle.receivedAt) && bundle.receivedAt >= bundle.issuedAt && bundle.receivedAt <= now, 'Invalid lifecycle response timestamp');
  requireValue(Number.isSafeInteger(bundle.expiresIn) && bundle.expiresIn > 0 && bundle.expiresIn <= MAX_TTL && bundle.expiresAt === bundle.issuedAt + bundle.expiresIn, 'Authoritative expiry evidence is missing or inconsistent');
  requireValue(bundle.expiresAt > now, 'Instagram access token is expired');
  requireValue(bundle.dataAccessExpiresAt === null || (Number.isSafeInteger(bundle.dataAccessExpiresAt) && bundle.dataAccessExpiresAt > now), 'Instagram data-access deadline is invalid or expired');
  parsePermissions(bundle.permissions);
  return { isValid: true, expiresAt: Math.min(bundle.expiresAt, bundle.dataAccessExpiresAt ?? Infinity), userId: bundle.userId };
}

export async function checkLifecycle(bundle, config, options = {}) {
  validateConfiguration(config);
  const status = validateBundle(bundle, { ...config, now: options.now ?? seconds() });
  const userId = await readAccount(bundle.token, options);
  requireValue(userId === bundle.userId, 'Live Instagram account does not match the lifecycle evidence');
  return status;
}

export async function bootstrapLifecycle({ code, redirectUri, ...config }, options = {}) {
  validateConfiguration(config);
  requireValue(validToken(code), 'A fresh INSTAGRAM_AUTHORIZATION_CODE is required; legacy tokens cannot establish ownership provenance');
  let redirect;
  try { redirect = new URL(redirectUri); } catch { throw new LifecycleError('INSTAGRAM_REDIRECT_URI must match the registered HTTPS callback'); }
  requireValue(redirect.protocol === 'https:' && !redirect.username && !redirect.password && !redirect.hash, 'INSTAGRAM_REDIRECT_URI must match the registered HTTPS callback');
  requireValue(validId(config.expectedUserId), 'INSTAGRAM_USER_ID is required to pin the approved account');
  const clock = options.clock ?? seconds;
  const authorizedAt = clock();
  const form = new FormData();
  for (const [key, value] of Object.entries({ client_id: config.appId, client_secret: config.appSecret, grant_type: 'authorization_code', redirect_uri: redirectUri, code })) form.set(key, value);
  // Single-use codes and credential mutations are never automatically retried.
  const short = oneAccount(await requestMeta(CODE_ENDPOINT, { method: 'POST', body: form }, { ...options, maxAttempts: 1 }));
  requireValue(validToken(short.access_token), 'Meta returned no short-lived credential');
  const permissions = parsePermissions(short.permissions);
  // The grant's user_id is app-scoped; /me.user_id is the professional
  // account ID. Bind the namespaces through the provider's same-token profile,
  // never by accepting an arbitrary different ID or changing the configured pin.
  const profileUrl = new URL(`${GRAPH}/v26.0/me`);
  profileUrl.searchParams.set('fields', 'id,user_id');
  profileUrl.searchParams.set('access_token', short.access_token);
  const profile = oneAccount(await requestMeta(profileUrl, {}, options));
  requireValue(validId(profile.id) && profile.id === short.user_id, 'OAuth grant does not match the live app-scoped identity');
  requireValue(profile.user_id === config.expectedUserId, 'OAuth authorization belongs to a different Instagram professional account');
  const url = new URL(`${GRAPH}/access_token`);
  url.searchParams.set('grant_type', 'ig_exchange_token');
  url.searchParams.set('client_secret', config.appSecret);
  url.searchParams.set('access_token', short.access_token);
  const issuedAt = clock();
  const data = await requestMeta(url, {}, { ...options, maxAttempts: 1 });
  const bundle = issuedBundle(data, { appId: config.appId, userId: profile.user_id, permissions, issuedAt, receivedAt: clock(),
    authorization: { method: 'instagram_authorization_code', appId: config.appId, userId: profile.user_id, scopedUserId: short.user_id, authorizedAt },
    previousDeadline: dataDeadline(short),
  });
  await checkLifecycle(bundle, config, { ...options, now: clock() });
  return bundle;
}

export async function refreshLifecycle(bundle, config, options = {}) {
  const clock = options.clock ?? seconds;
  await checkLifecycle(bundle, config, { ...options, now: clock() });
  // Expiry uses request start (conservative lower bound). Minimum refresh age
  // uses response receipt (upper bound), never time spent waiting on Meta.
  requireValue(clock() - bundle.receivedAt >= DAY, 'A long-lived token must be at least 24 hours old before refresh');
  const url = new URL(`${GRAPH}/refresh_access_token`);
  url.searchParams.set('grant_type', 'ig_refresh_token');
  url.searchParams.set('access_token', bundle.token);
  const issuedAt = clock();
  const data = await requestMeta(url, {}, { ...options, maxAttempts: 1 });
  const next = issuedBundle(data, { ...bundle, issuedAt, receivedAt: clock(), previousDeadline: bundle.dataAccessExpiresAt });
  await checkLifecycle(next, config, { ...options, now: clock() });
  return next;
}
