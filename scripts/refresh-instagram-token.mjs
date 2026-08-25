#!/usr/bin/env node
/**
 * Instagram Token Refresh Automation
 * 
 * Exchanges short-lived Instagram access tokens for long-lived tokens
 * and refreshes them before expiration.
 * 
 * Usage:
 *   node scripts/refresh-instagram-token.mjs --check     # Check current token status
 *   node scripts/refresh-instagram-token.mjs --refresh   # Refresh token now
 *   node scripts/refresh-instagram-token.mjs --setup     # Initial setup (exchange for long-lived)
 * 
 * Environment variables:
 *   INSTAGRAM_ACCESS_TOKEN - Current token (short or long-lived)
 *   INSTAGRAM_APP_ID       - Instagram App ID
 *   INSTAGRAM_APP_SECRET   - Instagram App Secret
 *   FLY_APP_NAME           - Fly.io app name (default: tdf-hq)
 */

import { readFile, writeFile } from 'fs/promises';
import { existsSync } from 'fs';
import { join } from 'path';
import { execSync } from 'child_process';

const TOKEN_FILE = join(process.cwd(), '.instagram-token-state.json');
const INSTAGRAM_API_BASE = 'https://graph.instagram.com';
const FACEBOOK_GRAPH_BASE = 'https://graph.facebook.com/v18.0';

// Read environment variables
const TOKEN = process.env.INSTAGRAM_ACCESS_TOKEN;
const APP_ID = process.env.INSTAGRAM_APP_ID || '1206294904899273';
const APP_SECRET = process.env.INSTAGRAM_APP_SECRET;
const FLY_APP = process.env.FLY_APP_NAME || 'tdf-hq';

function log(...args) {
  const timestamp = new Date().toISOString();
  console.log(`[${timestamp}]`, ...args);
}

function error(...args) {
  const timestamp = new Date().toISOString();
  console.error(`[${timestamp}] ERROR:`, ...args);
}

async function loadTokenState() {
  try {
    if (existsSync(TOKEN_FILE)) {
      const data = await readFile(TOKEN_FILE, 'utf-8');
      return JSON.parse(data);
    }
  } catch (err) {
    error('Failed to load token state:', err.message);
  }
  return {
    token: null,
    tokenType: null,
    expiresAt: null,
    refreshedAt: null,
    instagramUserId: null,
  };
}

async function saveTokenState(state) {
  try {
    await writeFile(TOKEN_FILE, JSON.stringify(state, null, 2));
  } catch (err) {
    error('Failed to save token state:', err.message);
  }
}

async function makeRequest(url) {
  const response = await fetch(url);
  const data = await response.json();
  
  if (!response.ok || data.error) {
    throw new Error(data.error?.message || `HTTP ${response.status}: ${response.statusText}`);
  }
  
  return data;
}

async function checkTokenStatus(token) {
  log('Checking token status...');
  
  try {
    // Check token info using Facebook's debug_token endpoint
    const debugUrl = `${FACEBOOK_GRAPH_BASE}/debug_token?input_token=${token}&access_token=${APP_ID}|${APP_SECRET}`;
    const debugData = await makeRequest(debugUrl);
    
    const tokenInfo = debugData.data;
    const expiresAt = tokenInfo.expires_at ? new Date(tokenInfo.expires_at * 1000) : null;
    const isValid = tokenInfo.is_valid;
    const scopes = tokenInfo.scopes || [];
    
    log('Token status:', isValid ? 'VALID' : 'INVALID');
    log('Scopes:', scopes.join(', '));
    
    if (expiresAt) {
      const now = new Date();
      const daysUntilExpiry = Math.floor((expiresAt - now) / (1000 * 60 * 60 * 24));
      log('Expires at:', expiresAt.toISOString());
      log('Days until expiry:', daysUntilExpiry);
      
      return {
        isValid,
        expiresAt,
        daysUntilExpiry,
        scopes,
        type: tokenInfo.type || 'unknown',
      };
    } else {
      log('Token does not expire (long-lived)');
      return {
        isValid,
        expiresAt: null,
        daysUntilExpiry: Infinity,
        scopes,
        type: 'long_lived',
      };
    }
  } catch (err) {
    error('Failed to check token status:', err.message);
    return { isValid: false, error: err.message };
  }
}

async function exchangeForLongLivedToken(shortLivedToken) {
  log('Exchanging short-lived token for long-lived token...');
  
  try {
    const exchangeUrl = `${INSTAGRAM_API_BASE}/access_token?grant_type=ig_exchange_token&client_secret=${APP_SECRET}&access_token=${shortLivedToken}`;
    const data = await makeRequest(exchangeUrl);
    
    const newToken = data.access_token;
    const expiresIn = data.expires_in; // seconds
    const expiresAt = new Date(Date.now() + expiresIn * 1000);
    
    log('Successfully exchanged for long-lived token');
    log('New token expires in:', Math.floor(expiresIn / 86400), 'days');
    log('Expires at:', expiresAt.toISOString());
    
    return {
      token: newToken,
      expiresAt,
      expiresIn,
    };
  } catch (err) {
    error('Failed to exchange token:', err.message);
    throw err;
  }
}

async function refreshLongLivedToken(currentToken) {
  log('Refreshing long-lived token...');
  
  try {
    const refreshUrl = `${INSTAGRAM_API_BASE}/refresh_access_token?grant_type=ig_refresh_token&access_token=${currentToken}`;
    const data = await makeRequest(refreshUrl);
    
    const newToken = data.access_token;
    const expiresIn = data.expires_in; // seconds
    const expiresAt = new Date(Date.now() + expiresIn * 1000);
    
    log('Successfully refreshed token');
    log('New token expires in:', Math.floor(expiresIn / 86400), 'days');
    log('Expires at:', expiresAt.toISOString());
    
    return {
      token: newToken,
      expiresAt,
      expiresIn,
    };
  } catch (err) {
    error('Failed to refresh token:', err.message);
    throw err;
  }
}

async function updateFlySecret(token) {
  log('Updating Fly.io secret...');
  
  try {
    const cmd = `flyctl secrets set INSTAGRAM_ACCESS_TOKEN="${token}" --app ${FLY_APP}`;
    execSync(cmd, { stdio: 'inherit' });
    log('Fly.io secret updated successfully');
    return true;
  } catch (err) {
    error('Failed to update Fly.io secret:', err.message);
    return false;
  }
}

async function restartFlyApp() {
  log('Restarting Fly.io app...');
  
  try {
    const cmd = `flyctl apps restart ${FLY_APP}`;
    execSync(cmd, { stdio: 'inherit' });
    log('Fly.io app restarted successfully');
    return true;
  } catch (err) {
    error('Failed to restart Fly.io app:', err.message);
    return false;
  }
}

async function setup() {
  log('=== Instagram Token Setup ===');
  
  if (!TOKEN) {
    error('INSTAGRAM_ACCESS_TOKEN environment variable is required');
    process.exit(1);
  }
  
  if (!APP_SECRET) {
    error('INSTAGRAM_APP_SECRET environment variable is required');
    process.exit(1);
  }
  
  try {
    // First check if current token is already long-lived
    const status = await checkTokenStatus(TOKEN);
    
    if (status.type === 'long_lived' || status.daysUntilExpiry > 30) {
      log('Token appears to already be long-lived or valid for >30 days');
      log('Saving state and exiting');
      
      await saveTokenState({
        token: TOKEN,
        tokenType: 'long_lived',
        expiresAt: status.expiresAt?.toISOString() || null,
        refreshedAt: new Date().toISOString(),
      });
      
      return;
    }
    
    // Exchange for long-lived token
    const result = await exchangeForLongLivedToken(TOKEN);
    
    // Save state
    await saveTokenState({
      token: result.token,
      tokenType: 'long_lived',
      expiresAt: result.expiresAt.toISOString(),
      refreshedAt: new Date().toISOString(),
    });
    
    // Update Fly.io secret
    const updated = await updateFlySecret(result.token);
    
    if (updated) {
      await restartFlyApp();
    }
    
    log('Setup complete!');
    log('New token:', result.token.substring(0, 10) + '...');
    
  } catch (err) {
    error('Setup failed:', err.message);
    process.exit(1);
  }
}

async function refresh() {
  log('=== Instagram Token Refresh ===');
  
  const state = await loadTokenState();
  const token = TOKEN || state.token;
  
  if (!token) {
    error('No token available. Run with --setup first.');
    process.exit(1);
  }
  
  try {
    const result = await refreshLongLivedToken(token);
    
    // Save state
    await saveTokenState({
      token: result.token,
      tokenType: 'long_lived',
      expiresAt: result.expiresAt.toISOString(),
      refreshedAt: new Date().toISOString(),
    });
    
    // Update Fly.io secret
    const updated = await updateFlySecret(result.token);
    
    if (updated) {
      await restartFlyApp();
    }
    
    log('Refresh complete!');
    
  } catch (err) {
    error('Refresh failed:', err.message);
    process.exit(1);
  }
}

async function check() {
  log('=== Instagram Token Check ===');
  
  const state = await loadTokenState();
  const token = TOKEN || state.token;
  
  if (!token) {
    error('No token available. Run with --setup first.');
    process.exit(1);
  }
  
  try {
    const status = await checkTokenStatus(token);
    
    log('\nCurrent state:');
    log('  Token type:', state.tokenType || 'unknown');
    log('  Last refreshed:', state.refreshedAt || 'never');
    log('  Stored expires at:', state.expiresAt || 'unknown');
    
    if (status.daysUntilExpiry < 7) {
      log('\n⚠️  WARNING: Token expires in less than 7 days!');
      log('Run with --refresh to refresh the token.');
    } else if (status.daysUntilExpiry < 30) {
      log('\n⚠️  Token expires in less than 30 days. Consider refreshing soon.');
    } else {
      log('\n✅ Token is healthy');
    }
    
  } catch (err) {
    error('Check failed:', err.message);
    process.exit(1);
  }
}

// Main
const args = process.argv.slice(2);
const command = args[0] || '--check';

switch (command) {
  case '--setup':
    setup();
    break;
  case '--refresh':
    refresh();
    break;
  case '--check':
    check();
    break;
  default:
    console.log('Usage:');
    console.log('  node refresh-instagram-token.mjs --setup    # Initial setup');
    console.log('  node refresh-instagram-token.mjs --refresh  # Refresh token now');
    console.log('  node refresh-instagram-token.mjs --check    # Check token status');
    process.exit(1);
}
