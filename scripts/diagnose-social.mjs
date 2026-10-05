#!/usr/bin/env node
/**
 * Social Infrastructure Diagnostic
 *
 * Checks Instagram + Facebook webhook subscriptions and messaging tokens.
 * Read-only checks using the operator-provided environment.
 * Follow ops/hetzner/README.md for current production access/configuration.
 */

const APP_ID = process.env.FACEBOOK_APP_ID || process.env.META_APP_ID;
const APP_SECRET = process.env.FACEBOOK_APP_SECRET || process.env.META_APP_SECRET;
const IG_MSG_TOKEN = process.env.INSTAGRAM_MESSAGING_TOKEN;
const IG_ACCOUNT_ID = process.env.INSTAGRAM_MESSAGING_ACCOUNT_ID;
const FB_MSG_TOKEN = process.env.FACEBOOK_MESSAGING_TOKEN || process.env.FACEBOOK_PAGE_ACCESS_TOKEN;
const FB_PAGE_ID = process.env.FACEBOOK_MESSAGING_PAGE_ID || process.env.FACEBOOK_PAGE_ID;
const GRAPH_BASE = process.env.FACEBOOK_GRAPH_BASE || process.env.FACEBOOK_MESSAGING_API_BASE;


// Never echo configured credentials, including provider metadata that reflects
// a submitted token. Raw fetch/JSON exceptions are not safe diagnostics either.
const privateValues = ['FACEBOOK_APP_SECRET', 'META_APP_SECRET',
  'INSTAGRAM_MESSAGING_TOKEN', 'FACEBOOK_MESSAGING_TOKEN', 'FACEBOOK_PAGE_ACCESS_TOKEN',
  'INSTAGRAM_VERIFY_TOKEN', 'IG_VERIFY_TOKEN', 'FACEBOOK_VERIFY_TOKEN']
  .map(name => process.env[name]).filter(Boolean)
  .flatMap(value => [value, encodeURIComponent(value)])
  .sort((a, b) => b.length - a.length);
function safeLog(message) {
  let output = String(message);
  for (const value of privateValues) output = output.replaceAll(value, '[redacted]');
  console.log(output);
}

async function graph(path, token) {
  const url = `${GRAPH_BASE}${path}&access_token=${encodeURIComponent(token)}`;
  const res = await fetch(url, { redirect: 'error', signal: AbortSignal.timeout(15_000) });
  return res.json();
}

async function appGraph(path) {
  return graph(path, `${APP_ID}|${APP_SECRET}`);
}

const failedChecks = [];

function check(name, ok, detail = '') {
  const icon = ok ? '✅' : '❌';
  safeLog(`${icon} ${name}${detail ? ': ' + detail : ''}`);
  if (!ok) failedChecks.push(name);
  return ok;
}

async function main() {
  safeLog('=== Social Infrastructure Diagnostic ===\n');

  // Use the operator's explicit backend version; do not silently probe a
  // different hardcoded version or send credentials to an arbitrary host.
  if (!/^https:\/\/graph\.facebook\.com\/v[0-9]+\.[0-9]+$/.test(GRAPH_BASE || '')) {
    safeLog('Set FACEBOOK_GRAPH_BASE to the reviewed versioned https://graph.facebook.com endpoint.');
    process.exitCode = 1;
    return;
  }

  // 1. App credentials
  if (!APP_ID || !APP_SECRET) {
    safeLog('❌ FACEBOOK_APP_ID and FACEBOOK_APP_SECRET must be set');
    process.exit(1);
  }
  safeLog(`App ID: ${APP_ID}\n`);

  // 2. App subscriptions
  safeLog('--- App Webhook Subscriptions ---');
  const subs = await appGraph(`/${APP_ID}/subscriptions?`);
  const igSub = subs.data?.find(s => s.object === 'instagram');
  const fbSub = subs.data?.find(s => s.object === 'page');

  check('Instagram canonical webhook active', igSub?.active === true
    && igSub.callback_url === 'https://api.tdfrecords.net/instagram/webhook');
  check('Facebook canonical webhook active', fbSub?.active === true
    && fbSub.callback_url === 'https://api.tdfrecords.net/facebook/webhook');

  if (!igSub || !fbSub) {
    safeLog('Configure missing subscriptions in the existing Meta app dashboard.');
    safeLog('Instagram callback: https://api.tdfrecords.net/instagram/webhook');
    safeLog('Facebook callback: https://api.tdfrecords.net/facebook/webhook');
    safeLog('Use the corresponding private verification token; do not paste it into logs.');
  }

  // 3. Instagram messaging token
  safeLog('\n--- Instagram Messaging Token ---');
  if (!IG_MSG_TOKEN) {
    check('INSTAGRAM_MESSAGING_TOKEN configured', false, 'not set');
  } else {
    const debug = await graph('/debug_token?input_token=' + encodeURIComponent(IG_MSG_TOKEN), `${APP_ID}|${APP_SECRET}`);
    if (debug.error) {
      check('Token valid', false, 'provider rejected token metadata');
    } else {
      const info = debug.data;
      check('Token valid', info.is_valid === true);
      safeLog(`   App ID: ${info.app_id}, Type: ${info.type}`);
      safeLog(`   Scopes: ${(info.scopes || []).join(', ')}`);
      if (info.expires_at) {
        const days = Math.floor((info.expires_at * 1000 - Date.now()) / (86400000));
        safeLog(`   Expires in: ${days} days`);
        if (days < 7) safeLog('   ⚠️ Expires soon!');
      }
    }

    if (!IG_ACCOUNT_ID) {
      check('INSTAGRAM_MESSAGING_ACCOUNT_ID configured', false, 'not set');
    } else if (!debug.error || debug.error.code !== 190) {
      const acct = await graph(`/${IG_ACCOUNT_ID}?fields=username`, IG_MSG_TOKEN);
      if (acct.error) {
        check('Can read IG account', false, 'provider rejected account metadata');
      } else {
        check('Can read IG account', typeof acct.username === 'string' && acct.username.length > 0,
          typeof acct.username === 'string' ? `@${acct.username}` : 'missing account metadata');
      }

      safeLog('Message delivery not tested: this diagnostic performs only read requests.');
    }
  }

  // 4. Facebook messaging token
  safeLog('\n--- Facebook Messaging Token ---');
  if (!FB_MSG_TOKEN) {
    check('FACEBOOK_MESSAGING_TOKEN configured', false, 'not set');
  } else {
    const debug = await graph('/debug_token?input_token=' + encodeURIComponent(FB_MSG_TOKEN), `${APP_ID}|${APP_SECRET}`);
    if (debug.error) {
      check('Token valid', false, 'provider rejected token metadata');
    } else {
      const info = debug.data;
      check('Token valid', info.is_valid === true);
      safeLog(`   Scopes: ${(info.scopes || []).join(', ')}`);
    }
  }

  if (!FB_PAGE_ID) {
    check('FACEBOOK_MESSAGING_PAGE_ID configured', false, 'not set');
  } else {
    safeLog(`   Page ID: ${FB_PAGE_ID}`);
  }

  // 5. Summary
  safeLog('\n=== Summary ===');
  const issues = [...new Set(failedChecks)];
  if (issues.length === 0) {
    safeLog('✅ All checks passed');
  } else {
    process.exitCode = 1;
    safeLog('❌ Issues found:');
    issues.forEach(i => safeLog(`   - ${i}`));
    safeLog('\n📖 Token refresh guide:');
    safeLog('   1. Go to https://developers.facebook.com/tools/explorer/');
    safeLog('   2. Select app "TDF Bot" (1098715965613487)');
    safeLog('   3. Get User Access Token with: pages_messaging, instagram_basic, instagram_manage_messages');
    safeLog('   4. Exchange for Page Token:');
    safeLog(`      GET /me/accounts?access_token=USER_TOKEN`);
    safeLog('   5. Copy the page access_token for "TDF Studio"');
    safeLog('   6. Follow ops/hetzner/README.md for protected production configuration.');
    safeLog('   7. Use the reviewed canonical release procedure; this diagnostic does not restart services.');
  }
}

main().catch(() => {
  safeLog('Social diagnostic failed; no credentials or raw provider response printed.');
  process.exitCode = 1;
});
