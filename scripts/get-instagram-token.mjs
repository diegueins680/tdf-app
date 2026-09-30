#!/usr/bin/env node

// Retired before reading any stored token state or suggesting provider writes.
console.error('This legacy Instagram token helper is retired.');
console.error('Use the existing OAuth flow and docs/INSTAGRAM_TOKEN_SETUP.md for the current protected configuration.');
console.error('For read-only status use node scripts/check-messaging-token.mjs. No stored token was read or displayed.');
process.exitCode = 1;
