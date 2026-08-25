#!/usr/bin/env node
/**
 * Messaging Token Health Check
 *
 * Checks if Instagram/Facebook messaging tokens are valid and not expiring soon.
 * Exchanges short-lived tokens for long-lived (60-day) tokens when needed.
 *
 * Environment variables:
 *   FACEBOOK_APP_ID       - Meta App ID
 *   FACEBOOK_APP_SECRET   - Meta App Secret
 *   INSTAGRAM_MESSAGING_TOKEN - Current token (Page or User)
 *   FACEBOOK_MESSAGING_TOKEN  - Current token (Page or User)
 *   FLY_APP_NAME          - Fly.io app name (default: tdf-hq)
 */

import { execSync } from 'child_process';

const APP_ID = process.env.FACEBOOK_APP_ID || process.env.META_APP_ID;
const APP_SECRET = process.env.FACEBOOK_APP_SECRET || process.env.META_APP_SECRET;
const IG_TOKEN = process.env.INSTAGRAM_MESSAGING_TOKEN;
const FB_TOKEN = process.env.FACEBOOK_MESSAGING_TOKEN || IG_TOKEN;
const FLY_APP = process.env.FLY_APP_NAME || 'tdf-hq';

const GRAPH_BASE = 'https://graph.facebook.com/v18.0';

function log(...args) {
  console.log(`[${new Date().toISOString()}]`, ...args);
}

function error(...args) {
  console.error(`[${new Date().toISOString()}] ERROR:`, ...args);
}

async function graph(path, token) {
  const url = `${GRAPH_BASE}${path}&access_token=${encodeURIComponent(token)}`;
  const res = await fetch(url);
  return res.json();
}

async function checkToken(token, name) {
  log(`\n=== Checking ${name} ===`);

  if (!token) {
    error(`${name} is not set`);
    return { ok: false, error: 'missing' };
  }

  if (!APP_ID || !APP_SECRET) {
    error('FACEBOOK_APP_ID and FACEBOOK_APP_SECRET are required');
    return { ok: false, error: 'missing_credentials' };
  }

  try {
    const debug = await graph(`/debug_token?input_token=${encodeURIComponent(token)}`, `${APP_ID}|${APP_SECRET}`);

    if (debug.error) {
      error(`Token check failed: ${debug.error.message}`);
      return { ok: false, error: debug.error.message };
    }

    const info = debug.data;
    log(`Valid: ${info.is_valid}`);
    log(`Type: ${info.type}`);
    log(`Profile: ${info.profile_id || 'N/A'}`);
    log(`Scopes: ${(info.scopes || []).join(', ')}`);

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

async function exchangeLongLivedToken(shortToken) {
  log('Exchanging for long-lived token...');
  try {
    const data = await graph(
      `/oauth/access_token?grant_type=fb_exchange_token&client_id=${APP_ID}&client_secret=${APP_SECRET}&fb_exchange_token=${encodeURIComponent(shortToken)}`,
      `${APP_ID}|${APP_SECRET}`
    );
    if (data.error) throw new Error(data.error.message);
    log('Long-lived token obtained');
    return data.access_token;
  } catch (err) {
    error(`Exchange failed: ${err.message}`);
    throw err;
  }
}

async function getPageToken(userToken) {
  log('Fetching page tokens...');
  try {
    const data = await graph('/me/accounts?fields=name,id,access_token', userToken);
    if (data.error) throw new Error(data.error.message);

    const pages = data.data || [];
    const targetPage = pages.find(p =>
      p.id === '169846481310276' ||
      p.name === 'TDF Estudio' ||
      p.name === 'TDF Studio'
    );

    if (!targetPage) {
      throw new Error('TDF Estudio page not found. Available pages: ' + pages.map(p => p.name).join(', '));
    }

    log(`Found page: ${targetPage.name} (${targetPage.id})`);
    return targetPage.access_token;
  } catch (err) {
    error(`Get page token failed: ${err.message}`);
    throw err;
  }
}

async function refreshTokenFlow(currentToken) {
  log('\n=== Starting Token Refresh ===');

  try {
    // Step 1: Exchange for long-lived user token
    const longLivedToken = await exchangeLongLivedToken(currentToken);

    // Step 2: Get page token (should be non-expiring)
    const pageToken = await getPageToken(longLivedToken);

    // Step 3: Verify the new token
    const check = await checkToken(pageToken, 'New Page Token');
    if (!check.ok) {
      throw new Error('New token validation failed');
    }

    // Step 4: Update Fly secrets
    log('\n=== Updating Fly.io Secrets ===');
    const cmd = `flyctl secrets set INSTAGRAM_MESSAGING_TOKEN="${pageToken}" FACEBOOK_MESSAGING_TOKEN="${pageToken}" --app ${FLY_APP}`;
    execSync(cmd, { stdio: 'inherit' });

    log('✅ Secrets updated successfully');
    return pageToken;
  } catch (err) {
    error(`Refresh failed: ${err.message}`);
    throw err;
  }
}

async function main() {
  console.log('=== Messaging Token Health Check ===\n');

  // Check both tokens (they're usually the same)
  const igCheck = await checkToken(IG_TOKEN, 'Instagram Messaging Token');
  const fbCheck = await checkToken(FB_TOKEN, 'Facebook Messaging Token');

  const needsRefresh = !igCheck.ok || igCheck.expiringSoon || !fbCheck.ok || fbCheck.expiringSoon;

  if (needsRefresh) {
    log('\n⚠️ Token needs refresh');

    if (!IG_TOKEN) {
      error('No token to refresh. Set INSTAGRAM_MESSAGING_TOKEN first.');
      process.exit(1);
    }

    try {
      const newToken = await refreshTokenFlow(IG_TOKEN);
      log('\n✅ Refresh complete!');
      log(`New token prefix: ${newToken.substring(0, 10)}...`);

      // Final verification
      const finalCheck = await checkToken(newToken, 'Refreshed Token');
      if (!finalCheck.ok) {
        error('Final verification failed');
        process.exit(1);
      }
    } catch (err) {
      error(`Refresh failed: ${err.message}`);
      process.exit(1);
    }
  } else {
    log('\n✅ All tokens are healthy. No action needed.');
  }
}

main().catch(err => {
  error(err.message);
  process.exit(1);
});
