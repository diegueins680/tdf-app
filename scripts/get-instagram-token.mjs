#!/usr/bin/env node
/**
 * Instagram Token Helper
 * 
 * This script helps you get a proper long-lived Instagram token.
 * 
 * The token from Meta dashboard is session-based and expires quickly.
 * You need to get a token through one of these methods:
 * 
 * Method 1: Use your app's OAuth flow (recommended)
 * Method 2: Use Facebook Graph API Explorer
 * Method 3: Use the backend's stored token
 */

import { readFile } from 'fs/promises';
import { existsSync } from 'fs';
import { join } from 'path';

const TOKEN_FILE = join(process.cwd(), '.instagram-token-state.json');

console.log(`
╔════════════════════════════════════════════════════════════════╗
║           Instagram Token Helper                               ║
╚════════════════════════════════════════════════════════════════╝

The token you got from Meta dashboard is a SESSION token that expires
when your browser session ends. You need a proper long-lived token.

┌─────────────────────────────────────────────────────────────────┐
│ METHOD 1: Use your app's OAuth flow (RECOMMENDED)               │
└─────────────────────────────────────────────────────────────────┘

1. Open your app: https://tdf-app.pages.dev/social/instagram
2. Click "Conectar" to start OAuth flow
3. Login with the Instagram Business account: tdf.records.label
4. Authorize the app
5. Check your backend logs - the token should be stored there

┌─────────────────────────────────────────────────────────────────┐
│ METHOD 2: Get token from your backend                           │
└─────────────────────────────────────────────────────────────────┘

If users have already connected their Instagram account through your app,
the token should be stored in your backend database or environment.

Check your backend:
- Database table for Instagram connections
- Environment variables on Fly.io
- Application logs

┌─────────────────────────────────────────────────────────────────┐
│ METHOD 3: Facebook Graph API Explorer (for testing only)        │
└─────────────────────────────────────────────────────────────────┘

1. Go to: https://developers.facebook.com/tools/explorer/
2. Select your app: TDF Bot
3. Click "Generate Access Token"
4. Add permissions: instagram_basic, instagram_manage_messages, 
   instagram_business_basic, instagram_business_manage_messages
5. Exchange for long-lived token using the API

┌─────────────────────────────────────────────────────────────────┐
│ METHOD 4: Use existing token file                               │
└─────────────────────────────────────────────────────────────────┘
`);

// Check if there's a stored token
if (existsSync(TOKEN_FILE)) {
  try {
    const data = await readFile(TOKEN_FILE, 'utf-8');
    const state = JSON.parse(data);
    console.log('Found stored token:');
    console.log('  Token:', state.token ? state.token.substring(0, 20) + '...' : 'none');
    console.log('  Type:', state.tokenType || 'unknown');
    console.log('  Expires:', state.expiresAt || 'unknown');
    console.log('  Last refreshed:', state.refreshedAt || 'never');
    console.log();
  } catch (err) {
    console.log('Found token file but could not read it:', err.message);
  }
} else {
  console.log('No stored token file found.');
}

console.log(`
┌─────────────────────────────────────────────────────────────────┐
│ NEXT STEPS                                                      │
└─────────────────────────────────────────────────────────────────┘

1. Get a proper token using one of the methods above
2. Set it as a Fly.io secret:
   
   flyctl secrets set INSTAGRAM_ACCESS_TOKEN="your-token-here" --app tdf-hq

3. Restart your app:
   
   flyctl apps restart tdf-hq

4. Test sending an Instagram message from your app

┌─────────────────────────────────────────────────────────────────┐
│ TROUBLESHOOTING                                                 │
└─────────────────────────────────────────────────────────────────┘

If you keep getting "Session key invalid" errors:
- The token is expired or was revoked
- You need to re-authorize through OAuth
- The Instagram account may have changed its password

For production use, consider:
- Storing tokens encrypted in your database
- Implementing automatic refresh before expiration
- Using webhook notifications for token changes
`);
