'use strict';
// Channel configuration: which web origin the shell hosts, which API it talks
// to, and where shell updates come from. Everything here is validated before
// use so a tampered or malformed channel file cannot widen the shell's reach.
const fs = require('node:fs');
const path = require('node:path');

const INTERNAL_SCHEME = 'tdf-desktop';
const INTERNAL_HOST = 'internal';
const DEFAULT_CHANNEL = 'production';
const LOOPBACK_HOSTS = new Set(['127.0.0.1', 'localhost', '[::1]']);

/** Accepts an absolute http(s) origin with no path, query, hash or credentials. */
function parseOrigin(value, { allowHttp = false } = {}) {
  if (typeof value !== 'string' || value.length > 253 + 16 || /\s/.test(value)) return null;
  let url;
  try { url = new URL(value); } catch { return null; }
  if (url.username || url.password || url.search || url.hash) return null;
  if (url.pathname !== '/' || value.endsWith('/') && value !== `${url.origin}/`) return null;
  if (url.protocol === 'https:') return url.origin;
  if (url.protocol === 'http:' && (allowHttp || LOOPBACK_HOSTS.has(url.hostname))) return url.origin;
  return null;
}

/** A path inside the hosted application: single leading slash, no scheme, no traversal. */
function sanitizeAppPath(value) {
  if (typeof value !== 'string' || value.length === 0 || value.length > 2048) return null;
  if (!value.startsWith('/') || value.startsWith('//') || /[\\\u0000-\u001f\u007f]/.test(value)) return null;
  // Reject traversal before URL normalization can hide it ("/a/../b" -> "/b").
  const rawPath = value.split(/[?#]/, 1)[0];
  if (rawPath.split('/').some((segment) => segment === '..' || segment === '.')) return null;
  if (/%2e|%2f|%5c/i.test(rawPath)) return null;
  let url;
  try { url = new URL(value, 'https://placeholder.invalid'); } catch { return null; }
  if (url.origin !== 'https://placeholder.invalid' || url.pathname.startsWith('//')) return null;
  return `${url.pathname}${url.search}${url.hash}`;
}

function normalizeChannel(raw, { allowHttp = false } = {}) {
  if (!raw || typeof raw !== 'object') throw new Error('Channel configuration must be an object');
  const name = typeof raw.name === 'string' && /^[a-z][a-z0-9-]{0,31}$/.test(raw.name) ? raw.name : null;
  if (!name) throw new Error('Channel name is missing or invalid');
  const appOrigin = parseOrigin(raw.appOrigin, { allowHttp: allowHttp || name === 'local' });
  if (!appOrigin) throw new Error(`Channel ${name}: appOrigin must be an https origin (http only for loopback/local)`);
  const apiOrigin = parseOrigin(raw.apiOrigin, { allowHttp: allowHttp || name === 'local' });
  if (!apiOrigin) throw new Error(`Channel ${name}: apiOrigin must be an https origin (http only for loopback/local)`);
  const startPath = sanitizeAppPath(raw.startPath ?? '/inicio');
  if (!startPath) throw new Error(`Channel ${name}: startPath is not a safe application path`);
  const updatesRaw = raw.updates && typeof raw.updates === 'object' ? raw.updates : {};
  const updateUrl = updatesRaw.url ? String(updatesRaw.url).replace(/\/+$/, '') : '';
  const updatesEnabled = updatesRaw.enabled === true;
  if (updatesEnabled) {
    if (updatesRaw.provider !== 'generic') throw new Error(`Channel ${name}: only the generic update provider is supported`);
    if (!/^https:\/\/[^\s/]+\/\S*$/.test(updateUrl)) throw new Error(`Channel ${name}: updates.url must be an https URL`);
  }
  const updateChannel = typeof updatesRaw.channel === 'string' && /^[a-z]+$/.test(updatesRaw.channel) ? updatesRaw.channel : 'latest';
  return {
    name,
    appOrigin,
    apiOrigin,
    startPath,
    updates: { enabled: updatesEnabled, provider: 'generic', url: updateUrl, channel: updateChannel },
    unsigned: raw.unsigned === true,
    builtAt: typeof raw.builtAt === 'string' ? raw.builtAt : null,
    sourceCommit: typeof raw.sourceCommit === 'string' && /^[0-9a-f]{7,40}$/.test(raw.sourceCommit) ? raw.sourceCommit : null,
  };
}

/**
 * Resolves the effective channel.
 * Packaged builds read the channel.json shipped in resources. Development runs
 * read channels/<name>.json and may override the hosted origin through
 * TDF_DESKTOP_APP_URL (loopback or https only); packaged builds ignore it.
 */
function loadChannel({ isPackaged, resourcesPath, channelsDir, env = process.env }) {
  let raw;
  if (isPackaged) {
    raw = JSON.parse(fs.readFileSync(path.join(resourcesPath, 'channel.json'), 'utf8'));
  } else {
    const name = env.TDF_DESKTOP_CHANNEL && /^[a-z][a-z0-9-]{0,31}$/.test(env.TDF_DESKTOP_CHANNEL) ? env.TDF_DESKTOP_CHANNEL : 'local';
    const file = path.join(channelsDir, `${name}.json`);
    raw = fs.existsSync(file) ? JSON.parse(fs.readFileSync(file, 'utf8')) : JSON.parse(fs.readFileSync(path.join(channelsDir, `${DEFAULT_CHANNEL}.json`), 'utf8'));
    raw = { ...raw, unsigned: true };
    if (env.TDF_DESKTOP_APP_URL) {
      const override = parseOrigin(env.TDF_DESKTOP_APP_URL);
      if (!override) throw new Error('TDF_DESKTOP_APP_URL must be an https origin or a loopback http origin');
      raw = { ...raw, appOrigin: override };
    }
    if (env.TDF_DESKTOP_API_URL) {
      const override = parseOrigin(env.TDF_DESKTOP_API_URL);
      if (!override) throw new Error('TDF_DESKTOP_API_URL must be an https origin or a loopback http origin');
      raw = { ...raw, apiOrigin: override };
    }
  }
  return normalizeChannel(raw);
}

module.exports = { DEFAULT_CHANNEL, INTERNAL_HOST, INTERNAL_SCHEME, loadChannel, normalizeChannel, parseOrigin, sanitizeAppPath };
