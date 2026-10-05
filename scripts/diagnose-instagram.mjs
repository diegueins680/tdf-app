#!/usr/bin/env node

// Superseded by the read-only health checker; never expose token prefixes or
// recommend writes/restarts against the retired Fly application.
console.error('This legacy Instagram diagnostic is retired.');
console.error('Use node scripts/check-messaging-token.mjs --check and docs/INSTAGRAM_TOKEN_SETUP.md with the current protected configuration.');
console.error('No token was displayed, provider request sent, credential changed or service restarted.');
process.exitCode = 1;
