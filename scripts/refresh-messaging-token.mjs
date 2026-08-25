#!/usr/bin/env node
/**
 * Messaging Token Refresh
 *
 * Exchanges a Facebook User Access Token for a Page Access Token
 * suitable for Instagram and Facebook messaging.
 *
 * Usage:
 *   export FACEBOOK_USER_TOKEN="your-user-token"
 *   node scripts/refresh-messaging-token.mjs
 *
 * The script will:
 *   1. Exchange the user token for a long-lived token
 *   2. Get page tokens via /me/accounts
 *   3. Find TDF Studio (or prompt if multiple)
 *   4. Update Fly.io secrets
 */

import { execSync } from 'child_process';

const APP_ID = process.env.FACEBOOK_APP_ID || process.env.META_APP_ID || '1098715965613487';
const APP_SECRET = process.env.FACEBOOK_APP_SECRET || process.env.META_APP_SECRET;
const USER_TOKEN = process.env.FACEBOOK_USER_TOKEN;
const FLY_APP = process.env.FLY_APP_NAME || 'tdf-hq';
const TARGET_PAGE_NAME = process.env.TARGET_PAGE_NAME || 'TDF Studio';
const TARGET_PAGE_ID = process.env.TARGET_PAGE_ID || '169846481310276';

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

async function exchangeLongLivedToken(shortToken) {
  log('Exchanging for long-lived user token...');
  const data = await graph(
    `/oauth/access_token?grant_type=fb_exchange_token&client_id=${APP_ID}&client_secret=${APP_SECRET}&fb_exchange_token=${encodeURIComponent(shortToken)}`,
    `${APP_ID}|${APP_SECRET}`
  );
  if (data.error) throw new Error(data.error.message);
  log('Long-lived user token obtained');
  return data.access_token;
}

async function getPageTokens(userToken) {
  log('Fetching page tokens...');
  const data = await graph('/me/accounts?', userToken);
  if (data.error) throw new Error(data.error.message);
  return data.data || [];
}

async function main() {
  console.log('=== Messaging Token Refresh ===\n');

  if (!USER_TOKEN) {
    error('FACEBOOK_USER_TOKEN is required');
    console.log('\nTo get a user token:');
    console.log('1. Go to https://developers.facebook.com/tools/explorer/');
    console.log('2. Select app "TDF Bot"');
    console.log('3. Get token with permissions: pages_messaging, instagram_basic, instagram_manage_messages, pages_read_engagement');
    console.log('4. Run: export FACEBOOK_USER_TOKEN="your-token"');
    console.log('5. Run this script again');
    process.exit(1);
  }

  if (!APP_SECRET) {
    error('FACEBOOK_APP_SECRET is required');
    process.exit(1);
  }

  try {
    // Step 1: Exchange for long-lived token
    const longLivedToken = await exchangeLongLivedToken(USER_TOKEN);

    // Step 2: Get page tokens
    const pages = await getPageTokens(longLivedToken);
    if (pages.length === 0) {
      error('No pages found. Ensure the user token has pages_read_engagement permission.');
      process.exit(1);
    }

    console.log('\nAvailable pages:');
    pages.forEach(p => {
      const marker = (p.id === TARGET_PAGE_ID || p.name === TARGET_PAGE_NAME) ? ' <-- TARGET' : '';
      console.log(`  - ${p.name} (ID: ${p.id})${marker}`);
    });

    // Step 3: Find target page
    let targetPage = pages.find(p => p.id === TARGET_PAGE_ID || p.name === TARGET_PAGE_NAME);
    if (!targetPage) {
      targetPage = pages[0];
      log(`Target page not found, using first page: ${targetPage.name}`);
    }

    const pageToken = targetPage.access_token;
    log(`Selected page: ${targetPage.name}`);

    // Step 4: Verify token works for messaging
    log('Verifying token can read Instagram account...');
    const igCheck = await graph(`/${targetPage.id}?fields=instagram_business_account{username}`, pageToken);
    if (igCheck.error) {
      error('Token cannot read Instagram account:', igCheck.error.message);
      console.log('\nThe page token may be missing instagram_basic permission.');
    } else {
      const igUser = igCheck.instagram_business_account;
      if (igUser) {
        log(`Linked Instagram: @${igUser.username}`);
      } else {
        log('No Instagram business account linked to this page');
      }
    }

    // Step 5: Update Fly secrets
    console.log('\n--- Fly.io Secret Update ---');
    console.log('Run the following command to update secrets:');
    console.log(`\n  flyctl secrets set INSTAGRAM_MESSAGING_TOKEN="${pageToken}" FACEBOOK_MESSAGING_TOKEN="${pageToken}" FACEBOOK_MESSAGING_PAGE_ID="${targetPage.id}" --app ${FLY_APP}`);
    console.log(`\n  flyctl apps restart ${FLY_APP}`);

    // Optional: auto-update if --auto flag is passed
    if (process.argv.includes('--auto')) {
      log('Auto-updating Fly secrets...');
      try {
        execSync(
          `flyctl secrets set INSTAGRAM_MESSAGING_TOKEN="${pageToken}" FACEBOOK_MESSAGING_TOKEN="${pageToken}" FACEBOOK_MESSAGING_PAGE_ID="${targetPage.id}" --app ${FLY_APP}`,
          { stdio: 'inherit' }
        );
        execSync(`flyctl apps restart ${FLY_APP}`, { stdio: 'inherit' });
        log('Secrets updated and app restarted!');
      } catch (err) {
        error('Failed to update secrets:', err.message);
      }
    }

  } catch (err) {
    error(err.message);
    process.exit(1);
  }
}

main();
