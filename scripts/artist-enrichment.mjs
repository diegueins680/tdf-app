#!/usr/bin/env node
/**
 * Resumable artist research and media pipeline.
 *
 * Secrets are read only from environment variables and are never logged:
 * ADMIN_TOKEN, SPOTIFY_CLIENT_ID, SPOTIFY_CLIENT_SECRET, YOUTUBE_API_KEY,
 * MUSICBRAINZ_USER_AGENT, DRIVE_CLIENT_ID, DRIVE_CLIENT_SECRET,
 * DRIVE_REFRESH_TOKEN, DRIVE_UPLOAD_FOLDER_ID (or UPLOAD_FOLDER_ID).
 */
import crypto from 'node:crypto';
import { execFile } from 'node:child_process';
import { mkdir, mkdtemp, readFile, rm, stat, writeFile } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import { promisify } from 'node:util';
import { fileURLToPath } from 'node:url';

const execFileAsync = promisify(execFile);
const DEFAULT_API_BASE = 'https://tdf-hq.fly.dev';
const DRIVE_API = 'https://www.googleapis.com/drive/v3';
const DRIVE_UPLOAD_API = 'https://www.googleapis.com/upload/drive/v3';
const SECRET_KEY_PATTERN = /(token|secret|password|authorization|api[-_]?key)/i;
const nowIso = () => new Date().toISOString();
let musicBrainzQueue = Promise.resolve();
let musicBrainzLastRequestAt = 0;
let discogsQueue = Promise.resolve();
let discogsLastRequestAt = 0;
let spotifyQueue = Promise.resolve();
let spotifyLastRequestAt = 0;
let youtubeQueue = Promise.resolve();
let youtubeLastRequestAt = 0;

export const normalizeName = (value) => String(value ?? '')
  .normalize('NFKD')
  .replace(/[\u0300-\u036f]/g, '')
  .toLocaleLowerCase('en-US')
  .replace(/[^a-z0-9]+/g, ' ')
  .trim()
  .replace(/\s+/g, ' ');

export const meaningfulSignals = (signals) => [...new Set(signals.filter(Boolean))];
export const artistNameAliasCandidate = (left, right) => {
  const forms = (value) => {
    const tokens = normalizeName(value).split(' ').filter((token) =>
      token && !['duplicate', 'duplicado', 'test', 'delete', 'me'].includes(token));
    const variants = [tokens.join('')];
    if (tokens[0]?.length === 1) variants.push(tokens.slice(1).join(''));
    return new Set(variants.filter(Boolean));
  };
  const rightForms = forms(right);
  return [...forms(left)].some((value) => rightForms.has(value));
};
export const automaticMatchAllowed = (signals, homonymCount = 1) =>
  homonymCount <= 1 && meaningfulSignals(signals).length >= 2;
export const retryDelayMs = (attempt, baseMs = 500) =>
  Math.min(30_000, baseMs * (2 ** Math.max(0, attempt)));

export function selectRunBatch(items, batchSize, runDate, rotate = false) {
  if (!rotate || items.length <= batchSize) return items.slice(0, batchSize);
  const dayNumber = Math.floor(Date.parse(`${runDate}T00:00:00Z`) / 86_400_000);
  if (!Number.isSafeInteger(dayNumber)) throw new Error('run date must use YYYY-MM-DD');
  const start = (dayNumber * batchSize) % items.length;
  return Array.from({ length: batchSize }, (_, index) => items[(start + index) % items.length]);
}

export function prepareCheckpointForAttempt(checkpoint) {
  const previousErrors = [
    ...(Array.isArray(checkpoint.previousErrors) ? checkpoint.previousErrors : []),
    ...(Array.isArray(checkpoint.errors) ? checkpoint.errors : []),
  ];
  return {
    completedArtists: Array.isArray(checkpoint.completedArtists) ? checkpoint.completedArtists : [],
    completedInventory: Array.isArray(checkpoint.completedInventory) ? checkpoint.completedInventory : [],
    previousErrors,
    errors: [],
  };
}

export function detectImageMime(bytes) {
  if (bytes.length >= 12 && bytes.subarray(0, 4).toString('hex') === '52494646'
    && bytes.subarray(8, 12).toString() === 'WEBP') return 'image/webp';
  if (bytes.length >= 8 && bytes.subarray(0, 8).toString('hex') === '89504e470d0a1a0a') return 'image/png';
  if (bytes.length >= 3 && bytes.subarray(0, 3).toString('hex') === 'ffd8ff') return 'image/jpeg';
  if (bytes.length >= 12 && bytes.subarray(4, 8).toString() === 'ftyp'
    && ['avif', 'avis'].includes(bytes.subarray(8, 12).toString())) return 'image/avif';
  return null;
}

function redact(value) {
  if (Array.isArray(value)) return value.map(redact);
  if (value && typeof value === 'object') {
    return Object.fromEntries(Object.entries(value).map(([key, child]) => [
      key,
      SECRET_KEY_PATTERN.test(key) ? '[REDACTED]' : redact(child),
    ]));
  }
  return value;
}

function log(level, event, details = {}) {
  process.stdout.write(`${JSON.stringify(redact({ timestamp: nowIso(), level, event, ...details }))}\n`);
}

export function parseArgs(argv) {
  const options = {
    mode: 'dry_run',
    scope: 'full',
    artistId: null,
    batchSize: 25,
    concurrency: 3,
    autoPublish: false,
    imageSourceUrl: null,
    imageRights: null,
    imageAttribution: null,
    focalPoint: 'center',
    resume: true,
    rotateBatches: false,
    checkpoint: path.resolve('.tmp/artist-enrichment/checkpoint.json'),
    report: path.resolve('.tmp/artist-enrichment/report.json'),
  };
  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    const next = () => argv[++index];
    if (arg === '--mode') options.mode = next()?.replace('-', '_');
    else if (arg === '--scope') options.scope = next();
    else if (arg === '--artist') options.artistId = Number(next());
    else if (arg === '--batch-size') options.batchSize = Number(next());
    else if (arg === '--concurrency') options.concurrency = Number(next());
    else if (arg === '--checkpoint') options.checkpoint = path.resolve(next());
    else if (arg === '--report') options.report = path.resolve(next());
    else if (arg === '--auto-publish') options.autoPublish = true;
    else if (arg === '--image-source-url') options.imageSourceUrl = next();
    else if (arg === '--image-rights') options.imageRights = next();
    else if (arg === '--image-attribution') options.imageAttribution = next();
    else if (arg === '--focal-point') options.focalPoint = next();
    else if (arg === '--no-resume') options.resume = false;
    else if (arg === '--rotate-batches') options.rotateBatches = true;
    else if (arg === '--help') options.help = true;
    else throw new Error(`Unknown argument: ${arg}`);
  }
  if (!['dry_run', 'production'].includes(options.mode)) throw new Error('--mode must be dry-run or production');
  if (!['audit', 'research', 'media', 'full'].includes(options.scope)) throw new Error('--scope must be audit, research, media, or full');
  if (options.artistId != null && (!Number.isSafeInteger(options.artistId) || options.artistId <= 0)) {
    throw new Error('--artist must be a positive integer');
  }
  if (!Number.isSafeInteger(options.batchSize) || options.batchSize < 1 || options.batchSize > 500) {
    throw new Error('--batch-size must be between 1 and 500');
  }
  if (!Number.isSafeInteger(options.concurrency) || options.concurrency < 1 || options.concurrency > 8) {
    throw new Error('--concurrency must be between 1 and 8');
  }
  if (options.autoPublish && options.mode !== 'production') throw new Error('--auto-publish requires --mode production');
  if (options.imageSourceUrl && options.artistId == null) throw new Error('--image-source-url requires --artist');
  if (options.imageSourceUrl && !['authorized', 'licensed'].includes(options.imageRights)) {
    throw new Error('--image-source-url requires --image-rights authorized|licensed');
  }
  if (options.imageSourceUrl && !String(options.imageAttribution ?? '').trim()) {
    throw new Error('--image-source-url requires --image-attribution');
  }
  if (options.imageRights && !options.imageSourceUrl) throw new Error('--image-rights requires --image-source-url');
  return options;
}

function help() {
  process.stdout.write(`Usage: node scripts/artist-enrichment.mjs [options]\n\n`
    + `  --mode dry-run|production  Never publish in dry-run mode (default)\n`
    + `  --scope audit|research|media|full\n`
    + `  --artist ID                 Limit to one artist\n`
    + `  --batch-size N              Process 1-500 artists (default 25)\n`
    + `  --concurrency N             External requests in parallel (default 3)\n`
    + `  --auto-publish              Approve only matches with >=2 signals\n`
    + `  --image-source-url URL       Ingest an explicitly authorized image for --artist\n`
    + `  --image-rights STATUS        authorized or licensed (required for image ingestion)\n`
    + `  --image-attribution TEXT     Rights/source attribution retained with the asset\n`
    + `  --focal-point VALUE          Crop/focal metadata (default center)\n`
    + `  --checkpoint PATH           Resumable checkpoint file\n`
    + `  --report PATH               Structured report output\n`
    + `  --no-resume                 Ignore an existing checkpoint\n`
    + `  --rotate-batches            Rotate bounded full-platform batches by UTC date\n`);
}

export async function retryFetch(url, options = {}, policy = {}) {
  const attempts = policy.attempts ?? 4;
  const timeoutMs = policy.timeoutMs ?? 20_000;
  for (let attempt = 0; attempt < attempts; attempt += 1) {
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), timeoutMs);
    try {
      const response = await fetch(url, { ...options, signal: controller.signal });
      if (response.ok || (response.status < 500 && response.status !== 429)) return response;
      const retryAfter = Number(response.headers.get('retry-after'));
      if (attempt === attempts - 1) return response;
      await new Promise((resolve) => setTimeout(resolve,
        Number.isFinite(retryAfter) && retryAfter > 0 ? retryAfter * 1000 : retryDelayMs(attempt)));
    } catch (error) {
      if (attempt === attempts - 1) throw error;
      await new Promise((resolve) => setTimeout(resolve, retryDelayMs(attempt)));
    } finally {
      clearTimeout(timeout);
    }
  }
  throw new Error('retry policy exhausted');
}

export function apiRequestTimeoutMs(rawValue = process.env.TDF_API_TIMEOUT_MS) {
  if (rawValue == null || String(rawValue).trim() === '') return 180_000;
  const parsed = Number(rawValue);
  if (!Number.isSafeInteger(parsed) || parsed < 1_000 || parsed > 900_000) {
    throw new Error('TDF_API_TIMEOUT_MS must be an integer between 1000 and 900000');
  }
  return parsed;
}

function createApiClient(baseUrl, adminToken) {
  const normalizedBase = baseUrl.replace(/\/+$/, '');
  const timeoutMs = apiRequestTimeoutMs();
  return async function api(method, route, body) {
    const response = await retryFetch(`${normalizedBase}${route}`, {
      method,
      headers: {
        Authorization: `Bearer ${adminToken}`,
        Accept: 'application/json',
        ...(body == null ? {} : { 'Content-Type': 'application/json' }),
      },
      ...(body == null ? {} : { body: JSON.stringify(body) }),
    }, { attempts: 4, timeoutMs });
    const text = await response.text();
    if (!response.ok) throw new Error(`TDF API ${method} ${route} failed (${response.status}): ${text.slice(0, 500)}`);
    return text ? JSON.parse(text) : null;
  };
}

async function readCheckpoint(filePath, enabled) {
  if (!enabled) return { completedArtists: [], completedInventory: [], previousErrors: [], errors: [] };
  try {
    const parsed = JSON.parse(await readFile(filePath, 'utf8'));
    return {
      completedArtists: Array.isArray(parsed.completedArtists) ? parsed.completedArtists : [],
      completedInventory: Array.isArray(parsed.completedInventory) ? parsed.completedInventory : [],
      previousErrors: Array.isArray(parsed.previousErrors) ? parsed.previousErrors : [],
      errors: Array.isArray(parsed.errors) ? parsed.errors : [],
    };
  } catch (error) {
    if (error?.code === 'ENOENT') return { completedArtists: [], completedInventory: [], previousErrors: [], errors: [] };
    throw error;
  }
}

async function writeJson(filePath, value) {
  await mkdir(path.dirname(filePath), { recursive: true });
  await writeFile(filePath, `${JSON.stringify(redact(value), null, 2)}\n`, { mode: 0o600 });
}

async function musicBrainzFetch(url, options) {
  const queued = musicBrainzQueue.then(async () => {
    const waitMs = Math.max(0, 1100 - (Date.now() - musicBrainzLastRequestAt));
    if (waitMs > 0) await new Promise((resolve) => setTimeout(resolve, waitMs));
    const response = await retryFetch(url, options);
    musicBrainzLastRequestAt = Date.now();
    return response;
  });
  musicBrainzQueue = queued.catch(() => undefined);
  return queued;
}

async function discogsFetch(url, options) {
  const queued = discogsQueue.then(async () => {
    const intervalMs = process.env.DISCOGS_TOKEN ? 1100 : 2500;
    const waitMs = Math.max(0, intervalMs - (Date.now() - discogsLastRequestAt));
    if (waitMs > 0) await new Promise((resolve) => setTimeout(resolve, waitMs));
    const response = await retryFetch(url, options);
    discogsLastRequestAt = Date.now();
    return response;
  });
  discogsQueue = queued.catch(() => undefined);
  return queued;
}

async function queuedProviderFetch(provider, url, options) {
  const isSpotify = provider === 'spotify';
  const queue = isSpotify ? spotifyQueue : youtubeQueue;
  const queued = queue.then(async () => {
    const lastAt = isSpotify ? spotifyLastRequestAt : youtubeLastRequestAt;
    const waitMs = Math.max(0, 200 - (Date.now() - lastAt));
    if (waitMs > 0) await new Promise((resolve) => setTimeout(resolve, waitMs));
    const response = await retryFetch(url, options);
    if (isSpotify) spotifyLastRequestAt = Date.now();
    else youtubeLastRequestAt = Date.now();
    return response;
  });
  if (isSpotify) spotifyQueue = queued.catch(() => undefined);
  else youtubeQueue = queued.catch(() => undefined);
  return queued;
}

async function mapConcurrent(items, concurrency, worker) {
  const results = new Array(items.length);
  let cursor = 0;
  const runners = Array.from({ length: Math.min(concurrency, items.length) }, async () => {
    while (cursor < items.length) {
      const index = cursor;
      cursor += 1;
      results[index] = await worker(items[index], index);
    }
  });
  await Promise.all(runners);
  return results;
}

async function spotifyAccessToken() {
  const clientId = process.env.SPOTIFY_CLIENT_ID;
  const clientSecret = process.env.SPOTIFY_CLIENT_SECRET;
  if (!clientId || !clientSecret) return null;
  const response = await retryFetch('https://accounts.spotify.com/api/token', {
    method: 'POST',
    headers: {
      Authorization: `Basic ${Buffer.from(`${clientId}:${clientSecret}`).toString('base64')}`,
      'Content-Type': 'application/x-www-form-urlencoded',
    },
    body: 'grant_type=client_credentials',
  });
  if (!response.ok) throw new Error(`Spotify token request failed (${response.status})`);
  return (await response.json()).access_token;
}

async function researchSpotify(name, token) {
  if (!token) return null;
  const response = await queuedProviderFetch('spotify', `https://api.spotify.com/v1/search?${new URLSearchParams({ q: `artist:${name}`, type: 'artist', limit: '5' })}`, {
    headers: { Authorization: `Bearer ${token}` },
  });
  if (!response.ok) throw new Error(`Spotify artist search failed (${response.status})`);
  const candidates = (await response.json()).artists?.items ?? [];
  const exactMatches = candidates.filter((candidate) => normalizeName(candidate.name) === normalizeName(name));
  return exactMatches.length > 0
    ? { candidate: exactMatches[0], exactMatchCount: exactMatches.length }
    : null;
}

async function spotifyAlbums(artistId, token) {
  if (!artistId || !token) return [];
  const response = await queuedProviderFetch('spotify', `https://api.spotify.com/v1/artists/${encodeURIComponent(artistId)}/albums?include_groups=album,single&limit=50`, {
    headers: { Authorization: `Bearer ${token}` },
  });
  if (!response.ok) return [];
  return (await response.json()).items ?? [];
}

async function researchMusicBrainz(name) {
  const userAgent = process.env.MUSICBRAINZ_USER_AGENT || 'TDFArtistEnrichment/1.0 (https://tdfrecords.com)';
  const search = await musicBrainzFetch(`https://musicbrainz.org/ws/2/artist?${new URLSearchParams({ query: `artist:"${name.replaceAll('"', '')}"`, fmt: 'json', limit: '5' })}`, {
    headers: { 'User-Agent': userAgent, Accept: 'application/json' },
  });
  if (!search.ok) throw new Error(`MusicBrainz search failed (${search.status})`);
  const candidates = (await search.json()).artists ?? [];
  const exactMatches = candidates.filter((item) => normalizeName(item.name) === normalizeName(name));
  const candidate = exactMatches[0] ?? null;
  if (!candidate) return null;
  const details = await musicBrainzFetch(`https://musicbrainz.org/ws/2/artist/${candidate.id}?inc=url-rels+release-groups+genres&fmt=json`, {
    headers: { 'User-Agent': userAgent, Accept: 'application/json' },
  });
  return {
    candidate: details.ok ? await details.json() : candidate,
    exactMatchCount: exactMatches.length,
  };
}

async function researchYouTube(name) {
  const apiKey = process.env.YOUTUBE_API_KEY;
  if (!apiKey) return null;
  const response = await queuedProviderFetch('youtube', `https://www.googleapis.com/youtube/v3/search?${new URLSearchParams({
    part: 'snippet', q: name, type: 'channel', maxResults: '5', key: apiKey,
  })}`);
  if (!response.ok) throw new Error(`YouTube search failed (${response.status})`);
  const candidates = (await response.json()).items ?? [];
  const exactMatches = candidates.filter((item) =>
    normalizeName(item.snippet?.channelTitle) === normalizeName(name));
  return exactMatches.length > 0
    ? { candidate: exactMatches[0], exactMatchCount: exactMatches.length }
    : null;
}

async function researchDiscogs(name) {
  const headers = {
    'User-Agent': process.env.MUSICBRAINZ_USER_AGENT || 'TDFArtistEnrichment/1.0 (https://tdfrecords.com)',
    ...(process.env.DISCOGS_TOKEN
      ? { Authorization: `Discogs token=${process.env.DISCOGS_TOKEN}` }
      : {}),
  };
  const response = await discogsFetch(`https://api.discogs.com/database/search?${new URLSearchParams({
    q: name,
    type: 'artist',
    per_page: '5',
  })}`, { headers });
  if (!response.ok) return null;
  const results = (await response.json()).results ?? [];
  const exactMatches = results.filter((item) => normalizeName(item.title) === normalizeName(name));
  const selected = exactMatches[0];
  if (!selected?.resource_url) return null;
  const details = await discogsFetch(selected.resource_url, { headers });
  return {
    candidate: details.ok ? await details.json() : selected,
    exactMatchCount: exactMatches.length,
  };
}

async function latestYouTubeVideo(channelId) {
  const apiKey = process.env.YOUTUBE_API_KEY;
  if (!apiKey || !channelId) return null;
  const response = await queuedProviderFetch('youtube', `https://www.googleapis.com/youtube/v3/search?${new URLSearchParams({
    part: 'snippet', channelId, type: 'video', order: 'date', maxResults: '1', key: apiKey,
  })}`);
  if (!response.ok) return null;
  return (await response.json()).items?.[0]?.id?.videoId ?? null;
}

function relationUrl(musicBrainz, types) {
  return musicBrainz?.relations?.find((relation) => types.includes(relation.type))?.url?.resource ?? null;
}

function publicProviderSearchSources(name, musicBrainz, discogs) {
  const encoded = new URLSearchParams({ query: name, type: 'artist', method: 'indexed' });
  return [
    ...(musicBrainz ? [] : [{
      url: `https://musicbrainz.org/search?${encoded}`,
      type: 'musicbrainz_search_no_exact_match',
      fields: ['identityCandidates'],
      attribution: 'MusicBrainz artist search returned no exact normalized-name candidate',
    }]),
    ...(discogs ? [] : [{
      url: `https://www.discogs.com/search/?${new URLSearchParams({ q: name, type: 'artist' })}`,
      type: 'discogs_search_no_exact_match',
      fields: ['identityCandidates'],
      attribution: 'Discogs artist search returned no exact normalized-name candidate',
    }]),
  ];
}

function discographyOverlap(spotifyItems, mbItems) {
  const spotifyTitles = new Set(spotifyItems.map((item) => normalizeName(item.name)).filter(Boolean));
  return mbItems
    .filter((item) => spotifyTitles.has(normalizeName(item.title)))
    .map((item) => item.title);
}

async function validateLink(url) {
  if (!url) return { valid: false, status: null };
  try {
    const response = await retryFetch(url, { method: 'HEAD', redirect: 'follow' }, { attempts: 2, timeoutMs: 10_000 });
    if (response.status === 405 || response.status === 403) {
      const fallback = await retryFetch(url, { method: 'GET', headers: { Range: 'bytes=0-0' }, redirect: 'follow' }, { attempts: 2, timeoutMs: 10_000 });
      return { valid: fallback.ok || fallback.status === 206, status: fallback.status };
    }
    return { valid: response.ok, status: response.status };
  } catch {
    return { valid: false, status: null };
  }
}

export const reportableLinkUrl = (url) => /^data:/i.test(String(url ?? ''))
  ? '[inline-data-url-redacted]'
  : url;

export const isPersistableResearchUrl = (url) => /^https?:\/\//i.test(String(url ?? ''));

async function buildLinkCheck(url) {
  if (/^data:/i.test(String(url))) {
    return { url: reportableLinkUrl(url), valid: true, status: null, skipped: 'inline_data' };
  }
  return { url: reportableLinkUrl(url), ...await validateLink(url) };
}

function evidenceFor(profile, sources, signals, details = {}) {
  return JSON.stringify({
    artistId: profile.apArtistId,
    artistName: profile.apDisplayName,
    signals: meaningfulSignals(signals),
    sources: sources.map((source) => source.url),
    retrievedAt: nowIso(),
    ...details,
  });
}

async function persistSource(api, profile, source) {
  return api('POST', '/admin/artists/enrichment/sources', {
    arscArtistId: profile.apArtistId,
    arscInventoryReferenceId: null,
    arscSourceUrl: source.url,
    arscSourceType: source.type,
    arscRetrievedAt: nowIso(),
    arscSupportedFields: source.fields.join(','),
    arscAttribution: source.attribution ?? null,
    arscContentHash: source.contentHash ?? null,
  });
}

async function persistInventorySource(api, inventoryReferenceId, source) {
  return api('POST', '/admin/artists/enrichment/sources', {
    arscArtistId: null,
    arscInventoryReferenceId: inventoryReferenceId,
    arscSourceUrl: source.url,
    arscSourceType: source.type,
    arscRetrievedAt: nowIso(),
    arscSupportedFields: source.fields.join(','),
    arscAttribution: source.attribution ?? null,
    arscContentHash: source.contentHash ?? null,
  });
}

async function persistLinkChecks(api, profile, linkChecks) {
  for (const [field, check] of Object.entries(linkChecks)) {
    if (!isPersistableResearchUrl(check.url)) continue;
    await persistSource(api, profile, {
      url: check.url,
      type: check.valid ? 'link_validation_valid' : 'link_validation_broken',
      fields: [field],
      attribution: JSON.stringify({ httpStatus: check.status, valid: check.valid }),
    });
  }
}

function isTdfManagedImageUrl(rawUrl) {
  try {
    const host = new URL(rawUrl).hostname.toLowerCase();
    return host === 'drive.google.com'
      || host.endsWith('.googleusercontent.com')
      || host === 'tdf-hq.fly.dev'
      || host === 'tdfrecords.com'
      || host.endsWith('.tdfrecords.com');
  } catch {
    return false;
  }
}

async function persistHotlinkReview(api, profile) {
  const url = profile.apHeroImageUrl;
  if (!url || isTdfManagedImageUrl(url) || !/^https?:\/\//i.test(url)) return;
  await persistSource(api, profile, {
    url,
    type: 'link_validation_broken',
    fields: ['heroImageUrl'],
    attribution: JSON.stringify({ valid: false, reason: 'third_party_hotlink_requires_authorized_ingestion' }),
  });
}

async function createSuggestion(api, profile, fieldName, currentValue, proposedValue, confidence, evidence, autoPublish) {
  if (proposedValue == null || String(proposedValue).trim() === '' || String(currentValue ?? '') === String(proposedValue)) return null;
  const suggestion = await api('POST', '/admin/artists/enrichment/suggestions', {
    aescArtistId: profile.apArtistId,
    aescInventoryReferenceId: null,
    aescFieldName: fieldName,
    aescCurrentValue: currentValue ?? null,
    aescProposedValue: String(proposedValue),
    aescConfidence: confidence,
    aescAutoPublish: autoPublish,
    aescEvidence: evidence,
  });
  if (autoPublish && suggestion?.aesAutoPublish === true && suggestion?.aesStatus === 'pending') {
    return api('PATCH', `/admin/artists/enrichment/suggestions/${suggestion.aesId}`, {
      aedDecision: 'approve',
      aedEditedValue: null,
      aedNote: 'Automatic publication: at least two independent matching signals.',
    });
  }
  return suggestion;
}

async function driveAccessToken() {
  const clientId = process.env.DRIVE_CLIENT_ID;
  const clientSecret = process.env.DRIVE_CLIENT_SECRET;
  const refreshToken = process.env.DRIVE_REFRESH_TOKEN;
  const serviceAccountEmail = process.env.GDRIVE_CLIENT_EMAIL;
  const serviceAccountKey = process.env.GDRIVE_PRIVATE_KEY?.replaceAll('\\n', '\n');
  let body;
  if (clientId && clientSecret && refreshToken) {
    body = new URLSearchParams({
      client_id: clientId,
      client_secret: clientSecret,
      refresh_token: refreshToken,
      grant_type: 'refresh_token',
    });
  } else if (serviceAccountEmail && serviceAccountKey) {
    const issuedAt = Math.floor(Date.now() / 1000);
    const encoded = (value) => Buffer.from(JSON.stringify(value)).toString('base64url');
    const unsigned = `${encoded({ alg: 'RS256', typ: 'JWT' })}.${encoded({
      iss: serviceAccountEmail,
      scope: 'https://www.googleapis.com/auth/drive',
      aud: 'https://oauth2.googleapis.com/token',
      iat: issuedAt,
      exp: issuedAt + 3600,
    })}`;
    const signature = crypto.sign('RSA-SHA256', Buffer.from(unsigned), serviceAccountKey).toString('base64url');
    body = new URLSearchParams({
      grant_type: 'urn:ietf:params:oauth:grant-type:jwt-bearer',
      assertion: `${unsigned}.${signature}`,
    });
  } else {
    return null;
  }
  const response = await retryFetch('https://oauth2.googleapis.com/token', {
    method: 'POST',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    body,
  });
  if (!response.ok) throw new Error(`Drive token refresh failed (${response.status})`);
  return (await response.json()).access_token;
}

const escapeDriveQuery = (value) => value.replaceAll('\\', '\\\\').replaceAll("'", "\\'");

async function ensureDriveFolder(token, parentId, name) {
  const query = `name='${escapeDriveQuery(name)}' and mimeType='application/vnd.google-apps.folder' and '${escapeDriveQuery(parentId)}' in parents and trashed=false`;
  const found = await retryFetch(`${DRIVE_API}/files?${new URLSearchParams({ q: query, fields: 'files(id,name)', pageSize: '2' })}`, {
    headers: { Authorization: `Bearer ${token}` },
  });
  const existing = found.ok ? (await found.json()).files?.[0] : null;
  if (existing) return existing.id;
  const created = await retryFetch(`${DRIVE_API}/files?fields=id`, {
    method: 'POST',
    headers: { Authorization: `Bearer ${token}`, 'Content-Type': 'application/json' },
    body: JSON.stringify({ name, mimeType: 'application/vnd.google-apps.folder', parents: [parentId] }),
  });
  if (!created.ok) throw new Error(`Drive folder creation failed (${created.status})`);
  return (await created.json()).id;
}

export async function uploadDriveFile(token, folderId, fileName, mimeType, bytes) {
  const query = `name='${escapeDriveQuery(fileName)}' and '${escapeDriveQuery(folderId)}' in parents and trashed=false`;
  const found = await retryFetch(`${DRIVE_API}/files?${new URLSearchParams({ q: query, fields: 'files(id,name)', pageSize: '2' })}`, {
    headers: { Authorization: `Bearer ${token}` },
  });
  let fileId = found.ok ? (await found.json()).files?.[0]?.id : null;
  if (!fileId) {
    const boundary = `tdf-${crypto.randomUUID()}`;
    const metadata = Buffer.from(JSON.stringify({ name: fileName, parents: [folderId] }));
    const body = Buffer.concat([
      Buffer.from(`--${boundary}\r\nContent-Type: application/json; charset=UTF-8\r\n\r\n`), metadata,
      Buffer.from(`\r\n--${boundary}\r\nContent-Type: ${mimeType}\r\n\r\n`), bytes,
      Buffer.from(`\r\n--${boundary}--`),
    ]);
    const uploaded = await retryFetch(`${DRIVE_UPLOAD_API}/files?uploadType=multipart&fields=id`, {
      method: 'POST',
      headers: { Authorization: `Bearer ${token}`, 'Content-Type': `multipart/related; boundary=${boundary}` },
      body,
    }, { attempts: 4, timeoutMs: 60_000 });
    if (!uploaded.ok) throw new Error(`Drive upload failed (${uploaded.status})`);
    fileId = (await uploaded.json()).id;
  }
  const permission = await retryFetch(`${DRIVE_API}/files/${encodeURIComponent(fileId)}/permissions`, {
    method: 'POST',
    headers: { Authorization: `Bearer ${token}`, 'Content-Type': 'application/json' },
    body: JSON.stringify({ role: 'reader', type: 'anyone', allowFileDiscovery: false }),
  });
  if (!permission.ok) throw new Error(`Drive public sharing failed (${permission.status})`);
  return { id: fileId, publicUrl: `https://drive.google.com/uc?export=view&id=${encodeURIComponent(fileId)}` };
}

export async function uploadTdfDriveFile(apiBase, adminToken, fileName, mimeType, bytes, idempotencyKey) {
  const form = new FormData();
  form.append('file', new Blob([bytes], { type: mimeType }), fileName);
  form.append('name', fileName);
  form.append('idempotencyKey', idempotencyKey);
  const response = await retryFetch(`${apiBase.replace(/\/+$/, '')}/drive/upload`, {
    method: 'POST',
    headers: { Authorization: `Bearer ${adminToken}` },
    body: form,
  }, { attempts: 4, timeoutMs: 60_000 });
  const responseText = await response.text();
  if (!response.ok) throw new Error(`TDF Drive upload failed (${response.status}): ${responseText.slice(0, 500)}`);
  const dto = JSON.parse(responseText);
  if (!dto.duFileId) throw new Error('TDF Drive upload response did not include a file id');
  return {
    id: dto.duFileId,
    publicUrl: dto.duPublicUrl
      ?? dto.duWebContentLink
      ?? `https://drive.google.com/uc?export=view&id=${encodeURIComponent(dto.duFileId)}`,
  };
}

export async function probeImage(filePath) {
  const { stdout } = await execFileAsync('ffprobe', [
    '-v', 'error', '-select_streams', 'v:0', '-show_entries', 'stream=width,height', '-of', 'json', filePath,
  ]);
  const stream = JSON.parse(stdout).streams?.[0];
  if (!stream?.width || !stream?.height) throw new Error('Image did not decode to positive dimensions');
  return { width: Number(stream.width), height: Number(stream.height) };
}

async function transcode(input, output, format, filter, quality) {
  const codecArgs = format === 'webp'
    ? ['-c:v', 'libwebp', '-quality', String(quality), '-compression_level', '6']
    : ['-c:v', 'libaom-av1', '-crf', String(quality), '-still-picture', '1', '-pix_fmt', 'yuv420p'];
  await execFileAsync('ffmpeg', [
    '-hide_banner', '-loglevel', 'error', '-y', '-i', input, '-vf', filter,
    '-frames:v', '1', ...codecArgs, output,
  ], { maxBuffer: 2 * 1024 * 1024 });
}

export async function transcodeWithinBudget(input, output, format, filter, budgetBytes) {
  const qualities = format === 'webp' ? [82, 76, 70, 64, 58] : [30, 34, 38, 42, 46];
  for (const quality of qualities) {
    await transcode(input, output, format, filter, quality);
    const fileStats = await stat(output);
    if (fileStats.size <= budgetBytes) return fileStats.size;
  }
  throw new Error(`${path.basename(output)} could not meet the ${budgetBytes} byte budget`);
}

async function processMedia(api, profile, imageUrl, sourceAttribution, rightsStatus, focalPoint, autoPublish, serverDriveUpload) {
  if (!['authorized', 'licensed'].includes(rightsStatus)) {
    throw new Error('Image ingestion requires explicit authorized or licensed rights');
  }
  const driveToken = await driveAccessToken();
  const driveParentId = process.env.DRIVE_UPLOAD_FOLDER_ID
    || process.env.UPLOAD_FOLDER_ID
    || process.env.GDRIVE_PARENT_ID;
  const directDriveAvailable = Boolean(driveToken && driveParentId);
  if (!directDriveAvailable && !serverDriveUpload) {
    throw new Error('Google Drive OAuth is incomplete and the TDF Drive proxy is unavailable');
  }
  const response = await retryFetch(imageUrl, { redirect: 'follow' }, { attempts: 4, timeoutMs: 30_000 });
  if (!response.ok) throw new Error(`Official image download failed (${response.status})`);
  const sourceBytes = Buffer.from(await response.arrayBuffer());
  if (sourceBytes.length > 25 * 1024 * 1024) throw new Error('Official image exceeds the 25MB ingestion limit');
  const detectedMime = detectImageMime(sourceBytes);
  if (!detectedMime) throw new Error('Official image MIME/magic bytes are unsupported');
  const tempDir = await mkdtemp(path.join(os.tmpdir(), 'tdf-artist-media-'));
  try {
    const input = path.join(tempDir, `source.${detectedMime.split('/')[1]}`);
    await writeFile(input, sourceBytes, { mode: 0o600 });
    const sourceDimensions = await probeImage(input);
    const sourceContentHash = crypto.createHash('sha256').update(sourceBytes).digest('hex');
    await persistSource(api, profile, {
      url: imageUrl,
      type: 'authorized_artist_image',
      fields: ['heroImageSource'],
      attribution: JSON.stringify({ attribution: sourceAttribution, rights: rightsStatus }),
      contentHash: sourceContentHash,
    });
    const variants = [
    { key: 'original', width: null, height: null, budget: 2 * 1024 * 1024, filter: "scale='min(2560,iw)':'min(2560,ih)':force_original_aspect_ratio=decrease,scale='trunc(iw/2)*2':'trunc(ih/2)*2'" },
    { key: 'square', width: 1024, height: 1024, budget: 400 * 1024, filter: 'scale=1024:1024:force_original_aspect_ratio=decrease,pad=1024:1024:(ow-iw)/2:(oh-ih)/2:color=0x0b1224' },
    { key: 'landscape', width: 1600, height: 900, budget: 500 * 1024, filter: 'scale=1600:900:force_original_aspect_ratio=decrease,pad=1600:900:(ow-iw)/2:(oh-ih)/2:color=0x0b1224' },
    { key: 'responsive', suffix: '800x450', width: 800, height: 450, budget: 250 * 1024, filter: 'scale=800:450:force_original_aspect_ratio=decrease,pad=800:450:(ow-iw)/2:(oh-ih)/2:color=0x0b1224' },
    { key: 'responsive', suffix: '480x270', width: 480, height: 270, budget: 160 * 1024, filter: 'scale=480:270:force_original_aspect_ratio=decrease,pad=480:270:(ow-iw)/2:(oh-ih)/2:color=0x0b1224' },
  ];
  const folderId = directDriveAvailable
    ? await ensureDriveFolder(driveToken, driveParentId, `artist-${profile.apArtistId}-${profile.apSlug || normalizeName(profile.apDisplayName).replaceAll(' ', '-')}`)
    : null;
  const uploaded = [];
  let originalParentId = null;
  for (const variant of variants) {
    for (const format of ['webp', 'avif']) {
      const output = path.join(tempDir, `${variant.key}-${variant.suffix || 'main'}.${format}`);
      await transcodeWithinBudget(input, output, format, variant.filter, variant.budget);
      const bytes = await readFile(output);
      const dimensions = await probeImage(output);
      const contentHash = crypto.createHash('sha256').update(bytes).digest('hex');
      const fileName = `${profile.apArtistId}-${variant.key}-${variant.suffix || 'main'}-${contentHash.slice(0, 16)}.${format}`;
      const uploadIdempotencyKey = crypto.createHash('sha256')
        .update(`${profile.apArtistId}|${variant.key}|${variant.suffix || 'main'}|${format}|${contentHash}`)
        .digest('hex');
      const driveFile = directDriveAvailable
        ? await uploadDriveFile(driveToken, folderId, fileName, `image/${format}`, bytes)
        : await serverDriveUpload(fileName, `image/${format}`, bytes, uploadIdempotencyKey);
      const media = await api('POST', '/admin/artists/enrichment/media', {
        amacArtistId: profile.apArtistId,
        amacAssetKind: variant.key,
        amacSourceUrl: imageUrl,
        amacSourceAttribution: sourceAttribution,
        amacRetrievedAt: nowIso(),
        amacSourceContentHash: sourceContentHash,
        amacSourceWidth: sourceDimensions.width,
        amacSourceHeight: sourceDimensions.height,
        amacSourceMimeType: detectedMime,
        amacSourceByteSize: sourceBytes.length,
        amacContentHash: contentHash,
        amacWidth: dimensions.width,
        amacHeight: dimensions.height,
        amacMimeType: `image/${format}`,
        amacByteSize: bytes.length,
        amacRightsStatus: rightsStatus,
        amacDriveFileId: driveFile.id,
        amacPublicUrl: driveFile.publicUrl,
        amacParentAssetId: variant.key === 'original' ? null : originalParentId,
        amacFocalPoint: focalPoint,
      });
      if (variant.key === 'original' && format === 'webp') originalParentId = media.amaId;
      uploaded.push({ ...media, format, suffix: variant.suffix || null });
    }
  }
  const by = (kind, format, suffix = null) => uploaded.find((item) =>
    item.amaAssetKind === kind && item.format === format && item.suffix === suffix)?.amaPublicUrl;
  const responsiveEntry = (format, width) => ({
    width,
    url: width === 1600
      ? by('landscape', format)
      : by('responsive', format, `${width}x${Math.round(width * 9 / 16)}`),
  });
  const responsive = {
    avif: [480, 800, 1600].map((width) => responsiveEntry('avif', width)),
    webp: [480, 800, 1600].map((width) => responsiveEntry('webp', width)),
  };
  const mediaEvidence = JSON.stringify({
    sourceUrl: imageUrl,
    attribution: sourceAttribution,
    rights: rightsStatus,
    signals: ['explicit_media_rights', 'decoded_content_hash'],
    contentAssets: uploaded.map((item) => item.amaId),
  });
  const suggestions = [
    ['heroImageUrl', profile.apHeroImageUrl, by('landscape', 'webp')],
    ['heroOriginalUrl', profile.apHeroOriginalUrl, by('original', 'webp')],
    ['heroSquareUrl', profile.apHeroSquareUrl, by('square', 'webp')],
    ['heroLandscapeUrl', profile.apHeroLandscapeUrl, by('landscape', 'webp')],
    ['heroResponsiveUrls', profile.apHeroResponsiveUrls, JSON.stringify(responsive)],
    ['heroFocalPoint', profile.apHeroFocalPoint, focalPoint],
  ];
  for (const [field, current, proposed] of suggestions) {
    await createSuggestion(api, profile, field, current, proposed, 0.96, mediaEvidence, autoPublish);
  }
    return uploaded;
  } finally {
    await rm(tempDir, { recursive: true, force: true });
  }
}

async function researchInventoryIdentity(api, inventoryRows, profiles, spotifyToken, options) {
  const primary = inventoryRows[0];
  const artistName = primary.airOriginalName;
  const [spotifyMatch, musicBrainzMatch, youtubeMatch, discogsMatch] = await Promise.all([
    researchSpotify(artistName, spotifyToken),
    researchMusicBrainz(artistName),
    researchYouTube(artistName),
    researchDiscogs(artistName),
  ]);
  const spotify = spotifyMatch?.candidate ?? null;
  const musicBrainz = musicBrainzMatch?.candidate ?? null;
  const youtube = youtubeMatch?.candidate ?? null;
  const discogs = discogsMatch?.candidate ?? null;
  const sources = [];
  if (spotify) sources.push({ url: spotify.external_urls.spotify, type: 'spotify_artist_profile', fields: ['officialName', 'spotifyArtistId', 'spotifyUrl', 'genres', 'heroImageCandidate'], attribution: 'Spotify artist profile candidate' });
  if (musicBrainz) sources.push({ url: `https://musicbrainz.org/artist/${musicBrainz.id}`, type: 'musicbrainz', fields: ['officialName', 'country', 'city', 'genres', 'websiteUrl', 'instagramUrl', 'discography'], attribution: 'MusicBrainz artist record' });
  if (youtube) sources.push({ url: `https://www.youtube.com/channel/${youtube.id.channelId}`, type: 'youtube_channel_candidate', fields: ['youtubeChannelId', 'youtubeUrl', 'featuredVideoUrl'], attribution: 'YouTube channel candidate' });
  if (discogs?.id) sources.push({ url: `https://www.discogs.com/artist/${discogs.id}`, type: 'discogs', fields: ['officialName', 'websiteUrl', 'socialLinks'], attribution: 'Discogs artist record' });
  sources.push(...publicProviderSearchSources(artistName, musicBrainz, discogs));
  const spotifyReleases = await spotifyAlbums(spotify?.id, spotifyToken);
  const mbReleases = musicBrainz?.['release-groups'] ?? [];
  const overlappingReleases = discographyOverlap(spotifyReleases, mbReleases);
  const website = relationUrl(musicBrainz, ['official homepage']);
  const instagram = musicBrainz?.relations?.find((relation) =>
    relation.type === 'social network' && relation.url?.resource?.includes('instagram.com'))?.url?.resource ?? null;
  if (website) sources.unshift({ url: website, type: 'official_website', fields: ['officialName', 'websiteUrl'], attribution: 'Official homepage linked from MusicBrainz' });
  if (instagram) sources.push({ url: instagram, type: 'instagram_linked_candidate', fields: ['instagramUrl'], attribution: 'Social profile linked from MusicBrainz' });
  const mbYouTube = musicBrainz?.relations?.find((relation) => relation.url?.resource?.includes('youtube.com'))?.url?.resource ?? null;
  const mbDiscogs = musicBrainz?.relations?.find((relation) => relation.url?.resource?.includes('discogs.com/artist/'))?.url?.resource ?? null;
  const signals = [];
  if (overlappingReleases.length > 0) signals.push('discography_cross_provider');
  if (mbYouTube && youtube?.id?.channelId && mbYouTube.includes(youtube.id.channelId)) signals.push('youtube_cross_link');
  if (mbDiscogs && discogs?.id && mbDiscogs.includes(String(discogs.id))) signals.push('discogs_cross_link');
  if (website && discogs?.urls?.some((url) => url.replace(/\/$/, '') === website.replace(/\/$/, ''))) signals.push('website_cross_link');
  if (instagram && discogs?.urls?.some((url) => url.includes('instagram.com'))) signals.push('instagram_cross_link');
  const homonymCount = Math.max(
    spotifyMatch?.exactMatchCount ?? 0,
    musicBrainzMatch?.exactMatchCount ?? 0,
    youtubeMatch?.exactMatchCount ?? 0,
    discogsMatch?.exactMatchCount ?? 0,
  );
  const reliable = automaticMatchAllowed(signals, homonymCount);
  const exactExistingProfiles = profiles.filter((profile) =>
    normalizeName(profile.apDisplayName) === normalizeName(artistName));
  const possibleExistingProfiles = profiles.filter((profile) =>
    artistNameAliasCandidate(profile.apDisplayName, artistName));
  const sameUrl = (left, right) => {
    try {
      return new URL(left).toString().replace(/\/$/, '') === new URL(right).toString().replace(/\/$/, '');
    } catch {
      return false;
    }
  };
  const externallyLinkedProfiles = possibleExistingProfiles.filter((profile) =>
    (spotify?.id && profile.apSpotifyArtistId === spotify.id)
    || (youtube?.id?.channelId && profile.apYoutubeChannelId === youtube.id.channelId)
    || (website && profile.apWebsiteUrl && sameUrl(profile.apWebsiteUrl, website)));
  const linkedExistingProfile = externallyLinkedProfiles.length === 1 ? externallyLinkedProfiles[0] : null;
  const automaticTargetAllowed = possibleExistingProfiles.length === 0 || linkedExistingProfile != null;
  const automaticActionAllowed = reliable && automaticTargetAllowed;
  const confidence = reliable ? Math.min(0.99, 0.88 + meaningfulSignals(signals).length * 0.04) : 0.55;
  const evidence = JSON.stringify({
    inventoryReferenceIds: inventoryRows.map((row) => row.airId),
    artistName,
    aliases: [...new Set(inventoryRows.map((row) => row.airOriginalName))],
    tdfSources: [...new Set(inventoryRows.map((row) => row.airSourceType))],
    signals: meaningfulSignals(signals),
    sources: sources.map((source) => source.url),
    exactNameCandidateCounts: {
      spotify: spotifyMatch?.exactMatchCount ?? 0,
      musicBrainz: musicBrainzMatch?.exactMatchCount ?? 0,
      youtube: youtubeMatch?.exactMatchCount ?? 0,
      discogs: discogsMatch?.exactMatchCount ?? 0,
    },
    externalIds: {
      spotify: spotify?.id ?? null,
      musicBrainz: musicBrainz?.id ?? null,
      youtube: youtube?.id?.channelId ?? null,
      discogs: discogs?.id ? String(discogs.id) : null,
    },
    exactNameTdfProfileIds: exactExistingProfiles.map((profile) => profile.apArtistId),
    possibleAliasTdfProfileIds: possibleExistingProfiles.map((profile) => profile.apArtistId),
    externallyLinkedTdfProfileId: linkedExistingProfile?.apArtistId ?? null,
    discographyOverlap: overlappingReleases,
    retrievedAt: nowIso(),
    jobRunId: options.backendRunId ?? null,
  });
  const report = {
    inventoryReferenceIds: inventoryRows.map((row) => row.airId),
    artistName,
    aliases: [...new Set(inventoryRows.map((row) => row.airOriginalName))],
    sources: sources.map((source) => source.url),
    signals: meaningfulSignals(signals),
    confidence,
    reliable,
    action: automaticActionAllowed
      ? (options.autoPublish ? (linkedExistingProfile ? 'link_existing_profile' : 'create_profile') : 'queue_review')
      : (reliable ? 'queue_review' : 'withhold'),
    candidateId: null,
    createdArtistId: null,
    exactNameTdfProfileIds: exactExistingProfiles.map((profile) => profile.apArtistId),
    possibleAliasTdfProfileIds: possibleExistingProfiles.map((profile) => profile.apArtistId),
    externallyLinkedTdfProfileId: linkedExistingProfile?.apArtistId ?? null,
    withheldReason: reliable
      ? (automaticTargetAllowed ? null : 'possible_tdf_alias_requires_external_link_selection')
      : (homonymCount > 1
        ? 'homonymous_exact_name_candidates'
        : 'fewer_than_two_independent_matching_signals'),
  };
  if (options.mode === 'production') {
    for (const source of sources) await persistInventorySource(api, primary.airId, source);
    const candidate = await api('POST', '/admin/artists/enrichment/identity-candidates', {
      aiccInventoryReferenceId: primary.airId,
      aiccArtistId: linkedExistingProfile?.apArtistId ?? null,
      aiccProvider: 'external_research',
      aiccExternalId: musicBrainz?.id ?? spotify?.id ?? null,
      aiccCandidateUrl: website ?? spotify?.external_urls?.spotify ?? (musicBrainz ? `https://musicbrainz.org/artist/${musicBrainz.id}` : null),
      aiccEvidence: evidence,
      aiccConfidence: confidence,
    });
    report.candidateId = candidate.aicId;
    if (automaticActionAllowed && options.autoPublish && candidate.aicStatus === 'pending') {
      const approved = await api('PATCH', `/admin/artists/enrichment/identity-candidates/${candidate.aicId}`, {
        aedDecision: 'approve',
        aedEditedValue: null,
        aedNote: linkedExistingProfile
          ? 'Automatic profile link: a stored external identifier plus two cross-provider signals and no detected homonym.'
          : 'Automatic profile creation: at least two cross-provider signals and no detected homonym.',
      });
      report.createdArtistId = approved.aicArtistId ?? null;
    }
  }
  return report;
}

async function researchArtist(api, profile, enrichment, spotifyToken, options) {
  const sources = [];
  const signals = [];
  const spotifyMatch = await researchSpotify(profile.apDisplayName, spotifyToken);
  const musicBrainzMatch = await researchMusicBrainz(profile.apDisplayName);
  const youtubeMatch = await researchYouTube(profile.apDisplayName);
  const discogsMatch = await researchDiscogs(profile.apDisplayName);
  const spotify = spotifyMatch?.candidate ?? null;
  const musicBrainz = musicBrainzMatch?.candidate ?? null;
  const youtube = youtubeMatch?.candidate ?? null;
  const discogs = discogsMatch?.candidate ?? null;
  if (profile.apWebsiteUrl) sources.push({ url: profile.apWebsiteUrl, type: 'tdf_existing_website', fields: ['websiteUrl'], attribution: 'Website currently stored in TDF; official status requires corroboration' });
  if (enrichment?.apeInstagramUrl) sources.push({ url: enrichment.apeInstagramUrl, type: 'tdf_existing_instagram', fields: ['instagramUrl'], attribution: 'Instagram currently stored in TDF; official status requires corroboration' });
  if (spotify) sources.push({ url: spotify.external_urls.spotify, type: 'spotify_artist_profile', fields: ['officialName', 'spotifyArtistId', 'spotifyUrl', 'genres', 'heroImageCandidate'], attribution: 'Spotify artist profile candidate' });
  if (musicBrainz) sources.push({ url: `https://musicbrainz.org/artist/${musicBrainz.id}`, type: 'musicbrainz', fields: ['officialName', 'country', 'city', 'genres', 'websiteUrl', 'instagramUrl', 'discography'], attribution: 'MusicBrainz artist record' });
  if (youtube) sources.push({ url: `https://www.youtube.com/channel/${youtube.id.channelId}`, type: 'youtube_channel_candidate', fields: ['youtubeChannelId', 'youtubeUrl', 'featuredVideoUrl'], attribution: 'YouTube channel candidate' });
  if (discogs?.id) sources.push({ url: `https://www.discogs.com/artist/${discogs.id}`, type: 'discogs', fields: ['officialName', 'websiteUrl', 'socialLinks'], attribution: 'Discogs artist record' });
  sources.push(...publicProviderSearchSources(profile.apDisplayName, musicBrainz, discogs));
  if (profile.apSpotifyArtistId && spotify?.id === profile.apSpotifyArtistId) signals.push('existing_spotify_artist_id');
  if (profile.apYoutubeChannelId && youtube?.id?.channelId === profile.apYoutubeChannelId) signals.push('existing_youtube_channel_id');
  const mbCountry = musicBrainz?.country ?? musicBrainz?.area?.['iso-3166-1-codes']?.[0] ?? null;
  if (enrichment?.apeCountry && mbCountry && normalizeName(enrichment.apeCountry) === normalizeName(mbCountry)) signals.push('country');
  const spotifyReleases = await spotifyAlbums(spotify?.id, spotifyToken);
  const mbReleases = musicBrainz?.['release-groups'] ?? [];
  const overlappingReleases = discographyOverlap(spotifyReleases, mbReleases);
  if (overlappingReleases.length > 0) signals.push('discography_cross_provider');
  const website = relationUrl(musicBrainz, ['official homepage']);
  const instagram = musicBrainz?.relations?.find((relation) =>
    relation.type === 'social network' && relation.url?.resource?.includes('instagram.com'))?.url?.resource ?? null;
  if (website) sources.unshift({ url: website, type: 'official_website', fields: ['officialName', 'websiteUrl'], attribution: 'Official homepage linked from MusicBrainz' });
  if (instagram) sources.push({ url: instagram, type: 'instagram_linked_candidate', fields: ['instagramUrl'], attribution: 'Social profile linked from MusicBrainz' });
  const bandcamp = musicBrainz?.relations?.find((relation) =>
    relation.url?.resource?.includes('bandcamp.com'))?.url?.resource ?? null;
  if (bandcamp) sources.push({ url: bandcamp, type: 'official_bandcamp', fields: ['officialName', 'discography'], attribution: 'Bandcamp linked from MusicBrainz' });
  const otherSocials = Object.fromEntries((musicBrainz?.relations ?? [])
    .map((relation) => relation.url?.resource)
    .filter((url) => url && ['facebook.com', 'soundcloud.com', 'x.com', 'twitter.com'].some((host) => url.includes(host)))
    .map((url) => [new URL(url).hostname.replace(/^www\./, ''), url]));
  const mbYouTube = musicBrainz?.relations?.find((relation) => relation.url?.resource?.includes('youtube.com'))?.url?.resource ?? null;
  if (mbYouTube && youtube?.id?.channelId && mbYouTube.includes(youtube.id.channelId)) signals.push('youtube_cross_link');
  const mbDiscogs = musicBrainz?.relations?.find((relation) => relation.url?.resource?.includes('discogs.com/artist/'))?.url?.resource ?? null;
  if (mbDiscogs && discogs?.id && mbDiscogs.includes(String(discogs.id))) signals.push('discogs_cross_link');
  if (website && discogs?.urls?.some((url) => url.replace(/\/$/, '') === website.replace(/\/$/, ''))) signals.push('website_cross_link');
  if (instagram && discogs?.urls?.some((url) => url.includes('instagram.com'))) signals.push('instagram_cross_link');
  const homonymCount = Math.max(
    spotifyMatch?.exactMatchCount ?? 0,
    musicBrainzMatch?.exactMatchCount ?? 0,
    youtubeMatch?.exactMatchCount ?? 0,
    discogsMatch?.exactMatchCount ?? 0,
  );
  const reliable = automaticMatchAllowed(signals, homonymCount);
  const confidence = reliable ? Math.min(0.99, 0.86 + meaningfulSignals(signals).length * 0.04) : 0.55;
  const hasSignal = (name) => signals.includes(name);
  const spotifyTrusted = hasSignal('existing_spotify_artist_id') || hasSignal('discography_cross_provider');
  const musicBrainzTrusted = hasSignal('discography_cross_provider')
    || hasSignal('country')
    || hasSignal('youtube_cross_link')
    || hasSignal('discogs_cross_link')
    || hasSignal('website_cross_link')
    || hasSignal('instagram_cross_link');
  const youtubeTrusted = hasSignal('existing_youtube_channel_id') || hasSignal('youtube_cross_link');
  const discogsTrusted = hasSignal('discogs_cross_link')
    || hasSignal('website_cross_link')
    || hasSignal('instagram_cross_link');
  const evidence = evidenceFor(profile, sources, signals, {
    jobRunId: options.backendRunId ?? null,
    spotifyArtistId: spotify?.id ?? null,
    musicBrainzArtistId: musicBrainz?.id ?? null,
    youtubeChannelId: youtube?.id?.channelId ?? null,
    discographyOverlap: overlappingReleases,
    exactNameCandidateCounts: {
      spotify: spotifyMatch?.exactMatchCount ?? 0,
      musicBrainz: musicBrainzMatch?.exactMatchCount ?? 0,
      youtube: youtubeMatch?.exactMatchCount ?? 0,
      discogs: discogsMatch?.exactMatchCount ?? 0,
    },
  });
  const report = {
    artistId: profile.apArtistId,
    artistName: profile.apDisplayName,
    sources: sources.map((source) => source.url),
    signals: meaningfulSignals(signals),
    confidence,
    reliable,
    suggestions: [],
    media: [],
    withheldReason: reliable ? null : 'fewer_than_two_independent_matching_signals',
  };
  if (options.mode === 'production') {
    for (const source of sources) await persistSource(api, profile, source);
  }
  const linkChecks = {};
  for (const [field, url] of [
    ['heroImageUrl', profile.apHeroImageUrl],
    ['spotifyUrl', profile.apSpotifyUrl],
    ['youtubeUrl', profile.apYoutubeUrl],
    ['websiteUrl', profile.apWebsiteUrl],
    ['instagramUrl', enrichment?.apeInstagramUrl],
  ]) {
    if (url) linkChecks[field] = await buildLinkCheck(url);
  }
  if (options.mode === 'production') {
    await persistLinkChecks(api, profile, linkChecks);
    await persistHotlinkReview(api, profile);
  }
  report.linkChecks = linkChecks;
  report.missingImage = !profile.apHeroImageUrl;
  if (profile.apHeroImageUrl && !isTdfManagedImageUrl(profile.apHeroImageUrl)) {
    report.mediaWithheldReason = 'existing_third_party_hotlink_requires_authorized_ingestion';
  }
  if (options.imageSourceUrl && options.artistId === profile.apArtistId && ['media', 'full'].includes(options.scope)) {
    if (options.mode === 'production') {
      report.media = await processMedia(
        api,
        profile,
        options.imageSourceUrl,
        options.imageAttribution,
        options.imageRights,
        options.focalPoint,
        options.autoPublish,
        options.serverDriveUpload,
      );
    } else {
      report.media = [{ sourceUrl: options.imageSourceUrl, rights: options.imageRights, action: 'would_download_optimize_and_upload' }];
    }
  }
  const imageCandidateUrl = spotifyTrusted
    ? spotify?.images?.sort((a, b) => (b.width ?? 0) - (a.width ?? 0))?.[0]?.url ?? null
    : null;
  if (!options.imageSourceUrl && imageCandidateUrl && !profile.apHeroImageUrl) {
    report.mediaWithheldReason = 'image_candidate_found_but_reuse_rights_not_confirmed';
    report.imageCandidateUrl = imageCandidateUrl;
  }
  if (!reliable) return report;
  const genres = [...new Set([
    ...(spotifyTrusted ? spotify?.genres ?? [] : []),
    ...(musicBrainzTrusted ? (musicBrainz?.genres ?? []).map((item) => item.name) : []),
  ])];
  const featuredVideoId = youtubeTrusted ? await latestYouTubeVideo(youtube?.id?.channelId) : null;
  const proposals = [
    ['officialName', profile.apOfficialName ?? profile.apDisplayName, (spotifyTrusted ? spotify?.name : null) ?? (musicBrainzTrusted ? musicBrainz?.name : null)],
    ['country', enrichment?.apeCountry, musicBrainzTrusted ? mbCountry : null],
    ['city', profile.apCity, musicBrainzTrusted ? musicBrainz?.['begin-area']?.name ?? null : null],
    ['genres', profile.apGenres, genres.length > 0 ? genres.join(', ') : null],
    ['spotifyArtistId', profile.apSpotifyArtistId, spotifyTrusted ? spotify?.id : null],
    ['spotifyUrl', profile.apSpotifyUrl, spotifyTrusted ? spotify?.external_urls?.spotify : null],
    ['youtubeChannelId', profile.apYoutubeChannelId, youtubeTrusted ? youtube?.id?.channelId : null],
    ['youtubeUrl', profile.apYoutubeUrl, youtubeTrusted ? (youtube?.id?.channelId ? `https://www.youtube.com/channel/${youtube.id.channelId}` : mbYouTube) : null],
    ['instagramUrl', enrichment?.apeInstagramUrl, musicBrainzTrusted ? instagram : null],
    ['socialLinks', enrichment?.apeSocialLinks, musicBrainzTrusted && (Object.keys(otherSocials).length > 0 || bandcamp) ? JSON.stringify({ ...otherSocials, ...(bandcamp ? { bandcamp } : {}) }) : null],
    ['websiteUrl', profile.apWebsiteUrl, musicBrainzTrusted ? website : null],
    ['featuredVideoUrl', profile.apFeaturedVideoUrl, featuredVideoId ? `https://www.youtube.com/watch?v=${featuredVideoId}` : null],
    ['discography', enrichment?.apeDiscography, musicBrainzTrusted && mbReleases.length > 0 ? JSON.stringify(mbReleases.slice(0, 30).map((item) => ({ title: item.title, type: item['primary-type'], firstReleaseDate: item['first-release-date'] ?? null }))) : null],
    ['lastVerifiedAt', enrichment?.apeLastVerifiedAt, `${nowIso().slice(0, 10)}T00:00:00Z`],
    ['confidence', enrichment?.apeConfidence, String(confidence)],
    ['reviewStatus', enrichment?.apeReviewStatus, 'verified'],
  ];
  for (const [field, current, proposed] of proposals) {
    if (proposed == null || String(current ?? '') === String(proposed)) continue;
    report.suggestions.push({ field, current: current ?? null, proposed });
    if (options.mode === 'production') {
      await createSuggestion(api, profile, field, current, proposed, confidence, evidence, options.autoPublish);
    }
  }
  return report;
}

async function auditArtistLinks(profile, enrichment) {
  const linkChecks = {};
  for (const [field, url] of [
    ['heroImageUrl', profile.apHeroImageUrl],
    ['spotifyUrl', profile.apSpotifyUrl],
    ['youtubeUrl', profile.apYoutubeUrl],
    ['websiteUrl', profile.apWebsiteUrl],
    ['instagramUrl', enrichment?.apeInstagramUrl],
  ]) {
    if (url) linkChecks[field] = await buildLinkCheck(url);
  }
  return {
    artistId: profile.apArtistId,
    artistName: profile.apDisplayName,
    reliable: null,
    sources: [],
    signals: [],
    suggestions: [],
    media: [],
    withheldReason: null,
    missingImage: !profile.apHeroImageUrl,
    linkChecks,
  };
}

export async function runPipeline(options) {
  const adminToken = process.env.ADMIN_TOKEN || process.env.API_TOKEN;
  if (!adminToken) throw new Error('ADMIN_TOKEN is required; do not pass secrets on the command line');
  const apiBase = process.env.TDF_API_BASE || process.env.API_BASE || DEFAULT_API_BASE;
  const api = createApiClient(apiBase, adminToken);
  options.serverDriveUpload = (fileName, mimeType, bytes, idempotencyKey) =>
    uploadTdfDriveFile(apiBase, adminToken, fileName, mimeType, bytes, idempotencyKey);
  const runDate = nowIso().slice(0, 10);
  const runKey = `operator:${options.mode}:${options.scope}:${options.artistId ?? 'full'}:${runDate}`;
  const backendRun = await api('POST', '/admin/artists/enrichment/runs', {
    aerrMode: options.mode,
    aerrArtistId: options.artistId,
    aerrResumeRunKey: runKey,
    aerrBatchSize: options.batchSize,
    aerrStaleDays: Number(process.env.ARTIST_ENRICHMENT_STALE_DAYS || 90),
  });
  if (backendRun.aerStatus === 'running') {
    const heartbeatAgeMs = Date.now() - new Date(backendRun.aerHeartbeatAt).getTime();
    if (!Number.isFinite(heartbeatAgeMs) || heartbeatAgeMs < 30 * 60 * 1000) {
      throw new Error(`Artist enrichment run ${backendRun.aerRunKey} is already active; refusing an overlapping execution`);
    }
    log('warn', 'resuming_stale_backend_run', {
      runId: backendRun.aerId,
      runKey: backendRun.aerRunKey,
      heartbeatAgeMs,
    });
  }
  options.backendRunId = backendRun.aerId;
  await api('PATCH', `/admin/artists/enrichment/runs/${backendRun.aerId}`, {
    aeruStatus: 'running',
    // The backend treats this phase as an atomic lease claim. A second runner
    // cannot race between the completed discovery run and this external work.
    aeruPhase: 'external_research_claim',
    aeruCheckpoint: JSON.stringify({ completedArtists: [], errors: 0 }),
  });
  const overviewRoute = `/admin/artists/enrichment/overview${options.artistId ? `?artistId=${options.artistId}` : ''}`;
  const [profiles, overview] = await Promise.all([
    api('GET', '/admin/artists/profiles'),
    api('GET', overviewRoute),
  ]);
  const enrichmentById = new Map(overview.aeoProfiles.map((profile) => [profile.apeArtistId, profile]));
  const checkpoint = prepareCheckpointForAttempt(await readCheckpoint(options.checkpoint, options.resume));
  const completed = new Set(checkpoint.completedArtists);
  const completedInventory = new Set(checkpoint.completedInventory);
  const eligibleProfiles = profiles
    .filter((profile) => options.artistId == null || profile.apArtistId === options.artistId)
    .filter((profile) => !completed.has(profile.apArtistId))
    .sort((left, right) => left.apArtistId - right.apArtistId);
  const selected = selectRunBatch(
    eligibleProfiles,
    options.batchSize,
    runDate,
    options.artistId == null && options.rotateBatches === true,
  );
  const spotifyToken = ['research', 'media', 'full'].includes(options.scope) ? await spotifyAccessToken() : null;
  const artistReports = [];
  const inventoryReports = [];
  const inventoryGroups = new Map();
  if (options.artistId == null && ['research', 'full'].includes(options.scope)) {
    for (const row of overview.aeoInventory) {
      if (row.airArtistId != null || row.airDisposition === 'obsolete_review' || completedInventory.has(row.airNormalizedName)) continue;
      const rows = inventoryGroups.get(row.airNormalizedName) ?? [];
      rows.push(row);
      inventoryGroups.set(row.airNormalizedName, rows);
    }
  }
  const eligibleInventory = [...inventoryGroups.entries()]
    .sort(([left], [right]) => left.localeCompare(right))
    .map(([, rows]) => rows);
  const selectedInventory = selectRunBatch(
    eligibleInventory,
    options.batchSize,
    runDate,
    options.artistId == null && options.rotateBatches === true,
  );
  let checkpointWrite = Promise.resolve();
  let attempted = 0;
  let halted = false;
  let executionError = null;
  const persistProgress = async () => {
    checkpoint.completedArtists = [...completed].sort((a, b) => a - b);
    checkpoint.completedInventory = [...completedInventory].sort();
    checkpointWrite = checkpointWrite.then(async () => {
      await writeJson(options.checkpoint, checkpoint);
      await api('PATCH', `/admin/artists/enrichment/runs/${backendRun.aerId}`, {
        aeruStatus: 'running',
        aeruPhase: 'external_research',
        aeruCheckpoint: JSON.stringify({
          completedArtists: checkpoint.completedArtists,
          completedInventory: checkpoint.completedInventory,
          previousErrors: checkpoint.previousErrors.length,
          errors: checkpoint.errors.length,
          halted,
        }),
        aeruCounters: JSON.stringify({
          attempted,
          completed: completed.size,
          inventoryCompleted: completedInventory.size,
          previousErrors: checkpoint.previousErrors.length,
          errors: checkpoint.errors.length,
        }),
      });
    });
    await checkpointWrite;
  };
  try {
    for (const inventoryRows of selectedInventory) {
      if (halted) break;
      const normalizedName = inventoryRows[0].airNormalizedName;
      attempted += 1;
      try {
        const result = await researchInventoryIdentity(api, inventoryRows, profiles, spotifyToken, options);
        inventoryReports.push(result);
        completedInventory.add(normalizedName);
        log('info', 'inventory_identity_completed', {
          inventoryReferenceId: inventoryRows[0].airId,
          reliable: result.reliable,
          action: result.action,
        });
      } catch (error) {
        checkpoint.errors.push({
          inventoryReferenceId: inventoryRows[0].airId,
          message: error instanceof Error ? error.message : String(error),
          at: nowIso(),
        });
        log('error', 'inventory_identity_failed', {
          inventoryReferenceId: inventoryRows[0].airId,
          message: error instanceof Error ? error.message : String(error),
        });
      }
      if (checkpoint.errors.length >= 3
        || (attempted >= 5 && checkpoint.errors.length / attempted > 0.1)) halted = true;
      await persistProgress();
    }
    await mapConcurrent(selected, options.concurrency, async (profile) => {
    if (halted) return;
    attempted += 1;
    try {
      const enrichment = enrichmentById.get(profile.apArtistId);
      const result = ['research', 'media', 'full'].includes(options.scope)
        ? await researchArtist(api, profile, enrichment, spotifyToken, options)
        : await auditArtistLinks(profile, enrichment);
      if (options.mode === 'production' && options.scope === 'audit') {
        await persistLinkChecks(api, profile, result.linkChecks);
        await persistHotlinkReview(api, profile);
      }
      artistReports.push(result);
      completed.add(profile.apArtistId);
      log('info', 'artist_completed', { artistId: profile.apArtistId, reliable: result.reliable, suggestions: result.suggestions.length, media: result.media.length });
    } catch (error) {
      checkpoint.errors.push({ artistId: profile.apArtistId, message: error instanceof Error ? error.message : String(error), at: nowIso() });
      log('error', 'artist_failed', { artistId: profile.apArtistId, message: error instanceof Error ? error.message : String(error) });
      if (checkpoint.errors.length >= 3
        || (attempted >= 5 && checkpoint.errors.length / attempted > 0.1)) {
        halted = true;
        log('error', 'safety_threshold_reached', { attempted, errors: checkpoint.errors.length });
      }
    }
    await persistProgress();
    });
  } catch (error) {
    executionError = error instanceof Error ? error : new Error(String(error));
    halted = true;
    log('error', 'execution_interrupted', { message: executionError.message });
  }
  const report = {
    runId: backendRun.aerId,
    runKey: backendRun.aerRunKey,
    mode: options.mode,
    scope: options.scope,
    generatedAt: nowIso(),
    inventory: {
      profiles: overview.aeoProfiles.length,
      references: overview.aeoInventory.length,
      candidates: overview.aeoIdentityCandidates.length,
      pendingSuggestions: overview.aeoSuggestions.filter((item) => item.aesStatus === 'pending').length,
    },
    inventoryCandidates: inventoryReports.sort((a, b) => a.artistName.localeCompare(b.artistName)),
    artists: artistReports.sort((a, b) => a.artistId - b.artistId),
    previousAttemptErrors: checkpoint.previousErrors,
    errors: checkpoint.errors,
    haltedBySafetyThreshold: halted,
  };
  await writeJson(options.report, report);
  await api('PATCH', `/admin/artists/enrichment/runs/${backendRun.aerId}`, {
    aeruStatus: halted ? 'failed' : 'completed',
    aeruPhase: 'reporting',
    aeruCheckpoint: JSON.stringify({
      completedArtists: checkpoint.completedArtists,
      completedInventory: checkpoint.completedInventory,
      previousErrors: checkpoint.previousErrors.length,
      halted,
    }),
    aeruCounters: JSON.stringify({
      attempted,
      completed: completed.size,
      inventoryCompleted: completedInventory.size,
      previousErrors: checkpoint.previousErrors.length,
      errors: checkpoint.errors.length,
      suggestions: report.artists.reduce((total, artist) => total + artist.suggestions.length, 0),
      media: report.artists.reduce((total, artist) => total + artist.media.length, 0),
    }),
    // A resumed successful run must replace, rather than retain, an error
    // summary written by an earlier failed attempt with the same run key.
    aeruErrorSummary: JSON.stringify({
      artists: checkpoint.errors.slice(0, 20),
      execution: executionError?.message ?? null,
    }),
  });
  log('info', 'run_completed', { runId: report.runId, artists: report.artists.length, errors: report.errors.length, report: options.report });
  if (executionError) throw executionError;
  if (halted) throw new Error('Artist enrichment stopped after reaching the configured safety error threshold');
  return report;
}

async function main() {
  const options = parseArgs(process.argv.slice(2));
  if (options.help) return help();
  await runPipeline(options);
}

const isMain = process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url);
if (isMain) {
  main().catch((error) => {
    log('error', 'run_failed', { message: error instanceof Error ? error.message : String(error) });
    process.exitCode = 1;
  });
}
