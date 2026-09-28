#!/usr/bin/env node
/**
 * Social Infrastructure Diagnostic
 *
 * Checks Instagram + Facebook webhook subscriptions and messaging tokens.
 * Run locally with securely supplied current-deployment environment variables.
 * This diagnostic does not apply callback or credential changes.
 */

const APP_ID = process.env.FACEBOOK_APP_ID || process.env.META_APP_ID;
const APP_SECRET = process.env.FACEBOOK_APP_SECRET || process.env.META_APP_SECRET;
const IG_MSG_TOKEN = process.env.INSTAGRAM_MESSAGING_TOKEN;
const IG_ACCOUNT_ID = process.env.INSTAGRAM_MESSAGING_ACCOUNT_ID;
const FB_MSG_TOKEN = process.env.FACEBOOK_MESSAGING_TOKEN || process.env.FACEBOOK_PAGE_ACCESS_TOKEN;
const FB_PAGE_ID = process.env.FACEBOOK_MESSAGING_PAGE_ID || process.env.FACEBOOK_PAGE_ID;

async function graph(path, token) {
  const url = `https://graph.facebook.com/v18.0${path}&access_token=${encodeURIComponent(token)}`;
  const res = await fetch(url);
  return res.json();
}

async function appGraph(path) {
  return graph(path, `${APP_ID}|${APP_SECRET}`);
}

function check(name, ok, detail = '') {
  const icon = ok ? '✅' : '❌';
  console.log(`${icon} ${name}${detail ? ': ' + detail : ''}`);
  return ok;
}

async function main() {
  console.log('=== Social Infrastructure Diagnostic ===\n');

  // 1. App credentials
  if (!APP_ID || !APP_SECRET) {
    console.log('❌ FACEBOOK_APP_ID and FACEBOOK_APP_SECRET must be set');
    process.exit(1);
  }
  console.log(`App ID: ${APP_ID}\n`);

  // 2. App subscriptions
  console.log('--- App Webhook Subscriptions ---');
  const subs = await appGraph(`/${APP_ID}/subscriptions?`);
  const igSub = subs.data?.find(s => s.object === 'instagram');
  const fbSub = subs.data?.find(s => s.object === 'page');

  const igSubscribed = igSub?.active === true && igSub.callback_url === 'https://api.tdfrecords.net/instagram/webhook';
  const fbSubscribed = fbSub?.active === true && fbSub.callback_url === 'https://api.tdfrecords.net/facebook/webhook';

  check('Instagram webhook subscribed', igSubscribed, igSub ? `callback=${igSub.callback_url}, active=${igSub.active}` : 'missing');
  check('Facebook webhook subscribed', fbSubscribed, fbSub ? `callback=${fbSub.callback_url}, active=${fbSub.active}` : 'missing');

  if (!igSubscribed) {
    console.log('\n🔧 To fix Instagram subscription:');
    console.log('  curl -X POST "https://graph.facebook.com/v18.0/${FACEBOOK_APP_ID}/subscriptions" --data-urlencode "object=instagram" --data-urlencode "callback_url=https://api.tdfrecords.net/instagram/webhook" --data-urlencode "fields=messages" --data-urlencode "verify_token=${INSTAGRAM_VERIFY_TOKEN}" --data-urlencode "access_token=${FACEBOOK_APP_ID}|${FACEBOOK_APP_SECRET}"');
  }

  if (!fbSubscribed) {
    console.log('\n🔧 To fix Facebook subscription:');
    console.log('  curl -X POST "https://graph.facebook.com/v18.0/${FACEBOOK_APP_ID}/subscriptions" --data-urlencode "object=page" --data-urlencode "callback_url=https://api.tdfrecords.net/facebook/webhook" --data-urlencode "fields=messages" --data-urlencode "verify_token=${FACEBOOK_MESSAGING_TOKEN:-${FACEBOOK_PAGE_ACCESS_TOKEN:-${INSTAGRAM_VERIFY_TOKEN}}}" --data-urlencode "access_token=${FACEBOOK_APP_ID}|${FACEBOOK_APP_SECRET}"');
  }

  // 3. Instagram messaging token
  console.log('\n--- Instagram Messaging Token ---');
  if (!IG_MSG_TOKEN) {
    check('INSTAGRAM_MESSAGING_TOKEN configured', false, 'not set');
  } else {
    const debug = await graph('/debug_token?input_token=' + encodeURIComponent(IG_MSG_TOKEN), `${APP_ID}|${APP_SECRET}`);
    if (debug.error) {
      check('Token valid', false, debug.error.message);
    } else {
      const info = debug.data;
      check('Token valid', info.is_valid);
      console.log(`   App ID: ${info.app_id}, Type: ${info.type}`);
      console.log(`   Scopes: ${(info.scopes || []).join(', ')}`);
      if (info.expires_at) {
        const days = Math.floor((info.expires_at * 1000 - Date.now()) / (86400000));
        console.log(`   Expires in: ${days} days`);
        if (days < 7) console.log('   ⚠️ Expires soon!');
      }
    }

    if (!debug.error || debug.error.code !== 190) {
      const acct = await graph(`/${IG_ACCOUNT_ID}?fields=username`, IG_MSG_TOKEN);
      if (acct.error) {
        check('Can read IG account', false, acct.error.message);
      } else {
        check('Can read IG account', true, `@${acct.username}`);
      }

      // Dry-run message send
      const send = await fetch(`https://graph.facebook.com/v18.0/${IG_ACCOUNT_ID}/messages`, {
        method: 'POST',
        headers: { Authorization: `Bearer ${IG_MSG_TOKEN}`, 'Content-Type': 'application/json' },
        body: JSON.stringify({ recipient: { id: 'TEST_INVALID' }, message: { text: 'test' }, messaging_type: 'RESPONSE' })
      });
      const sendJson = await send.json();
      if (sendJson.error?.code === 100 || sendJson.error?.message?.includes('TEST_INVALID')) {
        check('Can call message endpoint', true, 'endpoint accessible (recipient rejected as expected)');
      } else if (sendJson.error) {
        check('Can call message endpoint', false, `${sendJson.error.code}: ${sendJson.error.message}`);
      } else {
        check('Can call message endpoint', true);
      }
    }
  }

  // 4. Facebook messaging token
  console.log('\n--- Facebook Messaging Token ---');
  if (!FB_MSG_TOKEN) {
    check('FACEBOOK_MESSAGING_TOKEN configured', false, 'not set');
  } else {
    const debug = await graph('/debug_token?input_token=' + encodeURIComponent(FB_MSG_TOKEN), `${APP_ID}|${APP_SECRET}`);
    if (debug.error) {
      check('Token valid', false, debug.error.message);
    } else {
      const info = debug.data;
      check('Token valid', info.is_valid);
      console.log(`   Scopes: ${(info.scopes || []).join(', ')}`);
    }
  }

  if (!FB_PAGE_ID) {
    check('FACEBOOK_MESSAGING_PAGE_ID configured', false, 'not set');
  } else {
    console.log(`   Page ID: ${FB_PAGE_ID}`);
  }

  // 5. Summary
  console.log('\n=== Summary ===');
  const issues = [];
  if (!igSubscribed) issues.push('Re-subscribe Instagram webhook');
  if (!fbSubscribed) issues.push('Re-subscribe Facebook webhook + set FACEBOOK_MESSAGING_TOKEN');
  if (!IG_MSG_TOKEN || (await graph('/debug_token?input_token=' + encodeURIComponent(IG_MSG_TOKEN || 'x'), `${APP_ID}|${APP_SECRET}`)).error?.code === 190) {
    issues.push('Refresh INSTAGRAM_MESSAGING_TOKEN');
  }
  if (!FB_MSG_TOKEN) issues.push('Set FACEBOOK_MESSAGING_TOKEN');
  if (!FB_PAGE_ID) issues.push('Set FACEBOOK_MESSAGING_PAGE_ID');

  if (issues.length === 0) {
    console.log('✅ All checks passed');
  } else {
    console.log('❌ Issues found:');
    issues.forEach(i => console.log(`   - ${i}`));
    console.log('\n📖 Token refresh guide:');
    console.log('   1. Go to https://developers.facebook.com/tools/explorer/');
    console.log('   2. Select app "TDF Bot" (1098715965613487)');
    console.log('   3. Get User Access Token with: pages_messaging, instagram_basic, instagram_manage_messages');
    console.log('   4. Exchange for Page Token:');
    console.log(`      GET /me/accounts?access_token=USER_TOKEN`);
    console.log('   5. Copy the page access_token for "TDF Studio"');
    console.log('   6. Follow docs/INSTAGRAM_TOKEN_SETUP.md: Messaging credential rotation after the cutover.');
    console.log('   7. An authorized operator must update the current Hetzner secret store, verify live messaging, and synchronize GitHub check credentials.');
    console.log('      Do not update or restart the retired Fly app; this diagnostic performs no writes.');
  }
}

main().catch(console.error);
