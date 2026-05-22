#!/usr/bin/env node
/**
 * Instagram Token Diagnostic
 * 
 * Checks if the Instagram messaging token is valid and has the right permissions
 */

const APP_ID = process.env.INSTAGRAM_APP_ID || '1206294904899273';
const APP_SECRET = process.env.INSTAGRAM_APP_SECRET;
const TOKEN = process.env.INSTAGRAM_MESSAGING_TOKEN;
const ACCOUNT_ID = process.env.INSTAGRAM_MESSAGING_ACCOUNT_ID;

console.log('=== Instagram Token Diagnostic ===\n');

if (!TOKEN) {
  console.error('❌ INSTAGRAM_MESSAGING_TOKEN is not set');
  console.log('\nTo fix:');
  console.log('  flyctl secrets set INSTAGRAM_MESSAGING_TOKEN="your-token" --app tdf-hq');
  process.exit(1);
}

if (!ACCOUNT_ID) {
  console.error('❌ INSTAGRAM_MESSAGING_ACCOUNT_ID is not set');
  console.log('\nTo fix:');
  console.log('  flyctl secrets set INSTAGRAM_MESSAGING_ACCOUNT_ID="your-account-id" --app tdf-hq');
  process.exit(1);
}

console.log('✓ Token is configured');
console.log('✓ Account ID is configured');
console.log(`\nToken: ${TOKEN.substring(0, 10)}...`);
console.log(`Account ID: ${ACCOUNT_ID}`);

// Check token with Facebook's debug endpoint
async function checkToken() {
  console.log('\n--- Checking Token Status ---');
  
  try {
    const response = await fetch(`https://graph.facebook.com/v18.0/debug_token?input_token=${TOKEN}&access_token=${APP_ID}|${APP_SECRET}`);
    const data = await response.json();
    
    if (data.error) {
      console.error('❌ Token check failed:', data.error.message);
      return;
    }
    
    const tokenInfo = data.data;
    console.log('Token Status:', tokenInfo.is_valid ? '✅ VALID' : '❌ INVALID');
    console.log('App ID:', tokenInfo.app_id);
    console.log('Type:', tokenInfo.type);
    console.log('Scopes:', tokenInfo.scopes?.join(', ') || 'none');
    
    if (tokenInfo.expires_at) {
      const expiresAt = new Date(tokenInfo.expires_at * 1000);
      const now = new Date();
      const daysLeft = Math.floor((expiresAt - now) / (1000 * 60 * 60 * 24));
      console.log('Expires:', expiresAt.toISOString());
      console.log('Days left:', daysLeft);
      
      if (daysLeft < 7) {
        console.log('⚠️  Token expires in less than 7 days!');
      }
    } else {
      console.log('Expires: Never (long-lived token)');
    }
    
    // Check for required scopes
    const requiredScopes = [
      'instagram_basic',
      'instagram_manage_messages',
      'instagram_business_basic',
      'instagram_business_manage_messages'
    ];
    
    const hasScopes = tokenInfo.scopes || [];
    const missingScopes = requiredScopes.filter(s => !hasScopes.includes(s));
    
    if (missingScopes.length > 0) {
      console.log('\n⚠️  Missing required scopes:', missingScopes.join(', '));
    } else {
      console.log('\n✓ All required scopes present');
    }
    
  } catch (err) {
    console.error('❌ Failed to check token:', err.message);
  }
}

// Test sending a message (dry run)
async function testMessageEndpoint() {
  console.log('\n--- Testing Message Endpoint ---');
  
  try {
    const response = await fetch(`https://graph.facebook.com/v18.0/${ACCOUNT_ID}/messages`, {
      method: 'POST',
      headers: {
        'Authorization': `Bearer ${TOKEN}`,
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        recipient: { id: 'TEST' },
        message: { text: 'test' },
        messaging_type: 'RESPONSE'
      })
    });
    
    const data = await response.json();
    
    if (response.ok) {
      console.log('✓ Message endpoint is accessible');
    } else {
      console.log('❌ Message endpoint error:', data.error?.message || 'Unknown error');
      console.log('Error code:', data.error?.code);
      console.log('Error subcode:', data.error?.error_subcode);
      console.log('Error type:', data.error?.type);
      
      if (data.error?.code === 190) {
        console.log('\n⚠️  Token is invalid or expired. You need to generate a new token.');
      } else if (data.error?.code === 10) {
        console.log('\n⚠️  Permission error. The app may not have the required permissions.');
      }
    }
  } catch (err) {
    console.error('❌ Failed to test endpoint:', err.message);
  }
}

// Main
async function main() {
  if (APP_SECRET) {
    await checkToken();
  } else {
    console.log('\n⚠️  INSTAGRAM_APP_SECRET not set, skipping token validation');
    console.log('Set it to check token details:');
    console.log('  export INSTAGRAM_APP_SECRET="your-app-secret"');
  }
  
  await testMessageEndpoint();
  
  console.log('\n=== Diagnostic Complete ===');
  console.log('\nTo fix token issues:');
  console.log('1. Generate a new token from Meta Dashboard');
  console.log('2. Update the secret:');
  console.log('   flyctl secrets set INSTAGRAM_MESSAGING_TOKEN="new-token" --app tdf-hq');
  console.log('3. Restart the app:');
  console.log('   flyctl apps restart tdf-hq');
}

main().catch(console.error);
