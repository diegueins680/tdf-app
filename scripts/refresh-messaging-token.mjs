#!/usr/bin/env node

// The former implementation exchanged tokens, printed their values, and could
// restart the retired Fly API. No current-host automatic rotation is configured.
console.error('This standalone messaging-token refresher is retired, including --auto.');
console.error('Use docs/INSTAGRAM_TOKEN_SETUP.md and ops/hetzner/README.md for the current protected secret-store procedure.');
console.error('No token was exchanged, displayed or persisted, and no service was restarted.');
process.exitCode = 1;
