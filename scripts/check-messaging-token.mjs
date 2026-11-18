#!/usr/bin/env node
/**
 * Messaging Token Health Check
 *
 * Checks if Instagram/Facebook messaging tokens are valid and not expiring soon.
 * Pass --check for read-only validation. Automatic credential installation is
 * retired: rotate credentials in the current deployment (ops/hetzner/README.md).
 *
 * Environment variables:
 *   FACEBOOK_APP_ID       - Meta App ID
 *   FACEBOOK_APP_SECRET   - Meta App Secret
 *   INSTAGRAM_MESSAGING_TOKEN - Current token (Page or User)
 *   FACEBOOK_MESSAGING_TOKEN  - Current token (Page or User)
 */

import { pathToFileURL } from 'url';

const APP_ID = process.env.FACEBOOK_APP_ID || process.env.META_APP_ID;
const APP_SECRET = process.env.FACEBOOK_APP_SECRET || process.env.META_APP_SECRET;
const IG_TOKEN = process.env.INSTAGRAM_MESSAGING_TOKEN;
const FB_TOKEN = process.env.FACEBOOK_MESSAGING_TOKEN || IG_TOKEN;

const GRAPH_BASE = 'https://graph.facebook.com/v18.0';

function log(...args) {
  console.log(`[${new Date().toISOString()}]`, ...args);
}

function error(...args) {
  console.error(`[${new Date().toISOString()}] ERROR:`, ...args);
}

async function graph(path, token, fetchImpl = globalThis.fetch) {
  const url = `${GRAPH_BASE}${path}&access_token=${encodeURIComponent(token)}`;
  const res = await fetchImpl(url);
  return res.json();
}

export async function checkToken(token, name, {
  appId = APP_ID,
  appSecret = APP_SECRET,
  fetchImpl = globalThis.fetch,
} = {}) {
  log(`\n=== Checking ${name} ===`);

  if (!token) {
    error(`${name} is not set`);
    return { ok: false, error: 'missing' };
  }

  if (!appId || !appSecret) {
    error('FACEBOOK_APP_ID and FACEBOOK_APP_SECRET are required');
    return { ok: false, error: 'missing_credentials' };
  }

  try {
    const debug = await graph(
      `/debug_token?input_token=${encodeURIComponent(token)}`,
      `${appId}|${appSecret}`,
      fetchImpl
    );

    if (debug.error) {
      error(`Token check failed: ${debug.error.message}`);
      return { ok: false, error: debug.error.message };
    }

    const info = debug.data;
    log(`Valid: ${info.is_valid}`);
    log(`Type: ${info.type}`);
    log(`Profile: ${info.profile_id || 'N/A'}`);
    log(`Scopes: ${(info.scopes || []).join(', ')}`);

    if (info.is_valid !== true) {
      error('Token is invalid');
      return { ok: false, error: 'invalid' };
    }

    if (info.expires_at) {
      const expiresAt = new Date(info.expires_at * 1000);
      const now = new Date();
      const hoursLeft = Math.floor((expiresAt - now) / (1000 * 60 * 60));
      const daysLeft = Math.floor(hoursLeft / 24);

      log(`Expires at: ${expiresAt.toISOString()}`);
      log(`Hours left: ${hoursLeft}`);

      if (hoursLeft < 0) {
        error('Token has already expired!');
        return { ok: false, expired: true, hoursLeft };
      }

      if (hoursLeft < 24) {
        log('⚠️ Token expires in less than 24 hours!');
        return { ok: true, expiringSoon: true, hoursLeft, token };
      }

      if (daysLeft < 7) {
        log('⚠️ Token expires in less than 7 days');
        return { ok: true, expiringSoon: true, hoursLeft, token };
      }

      log('✅ Token is healthy');
      return { ok: true, expiringSoon: false, hoursLeft };
    } else {
      log('✅ Token does not expire (non-expiring/long-lived)');
      return { ok: true, expiringSoon: false, hoursLeft: Infinity };
    }
  } catch (err) {
    error(`Check failed: ${err.message}`);
    return { ok: false, error: err.message };
  }
}

// Automatic refresh used to install the exchanged token into the previous
// hosting provider. It now refuses before any token exchange so a new
// credential is never minted without a reviewed place to store it.
async function refreshTokenFlow() {
  throw new Error(
    'Automatic credential installation is retired; rotate credentials in the current deployment (ops/hetzner/README.md).'
  );
}

// Reject unsupported CLI input before either validation or credential mutation.
export function parseReadOnly(args) {
  if (args.length === 0) return false;
  if (args.length === 1 && args[0] === '--check') return true;
  throw new Error('Usage: check-messaging-token.mjs [--check]');
}

/**
 * Return a process exit code. Read-only execution may inspect token health but
 * must never call refresh; default execution retains refresh-when-needed.
 * check/refresh dependencies make that side-effect boundary directly testable.
 */
export async function runMessagingTokenMaintenance({
  readOnly = false,
  instagramToken = IG_TOKEN,
  facebookToken = FB_TOKEN,
  check = checkToken,
  refresh = refreshTokenFlow,
} = {}) {
  console.log('=== Messaging Token Health Check ===\n');

  // Check both tokens (they're usually the same)
  const igCheck = await check(instagramToken, 'Instagram Messaging Token');
  const fbCheck = await check(facebookToken, 'Facebook Messaging Token');

  const needsRefresh = !igCheck.ok || igCheck.expiringSoon || !fbCheck.ok || fbCheck.expiringSoon;

  if (needsRefresh) {
    // Fail before reaching any token exchange or Page-token lookup.
    // An expiring token is not a successful read-only maintenance check.
    if (readOnly) {
      error('Read-only check failed: token maintenance is required; no credentials were changed.');
      return 1;
    }
    log('\n⚠️ Token needs refresh');

    if (!instagramToken) {
      error('No token to refresh. Set INSTAGRAM_MESSAGING_TOKEN first.');
      return 1;
    }

    try {
      const newToken = await refresh(instagramToken);
      log('\n✅ Refresh complete!');

      // Final verification
      const finalCheck = await check(newToken, 'Refreshed Token');
      if (!finalCheck.ok) {
        error('Final verification failed');
        return 1;
      }
    } catch (err) {
      error(`Refresh failed: ${err.message}`);
      return 1;
    }
  } else {
    log('\n✅ All tokens are healthy. No action needed.');
  }
  return 0;
}

export async function runMessagingTokenCli(args, options = {}) {
  return runMessagingTokenMaintenance({ ...options, readOnly: parseReadOnly(args) });
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  runMessagingTokenCli(process.argv.slice(2)).then(code => {
    process.exitCode = code;
  }).catch(err => {
    error(err.message);
    process.exitCode = 1;
  });
}
