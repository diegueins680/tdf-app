#!/usr/bin/env node
import { pathToFileURL } from 'node:url';
import {
  LifecycleError, bootstrapLifecycle, checkLifecycle, loadBundle,
  refreshLifecycle, saveBundle, validateConfiguration,
} from './lib/instagram-token-lifecycle.mjs';

export async function runLifecycle(command, env = process.env, options = {}) {
  if (!['--check', '--setup', '--refresh'].includes(command)) throw new LifecycleError('Use --check, --setup or --refresh');
  const config = {
    appId: env.INSTAGRAM_APP_ID,
    appSecret: env.INSTAGRAM_APP_SECRET,
    context: env.GITHUB_REPOSITORY || env.INSTAGRAM_LIFECYCLE_CONTEXT,
    expectedUserId: env.INSTAGRAM_USER_ID,
  };
  validateConfiguration(config);
  const input = env.INSTAGRAM_LIFECYCLE_STATE_FILE || '.instagram-lifecycle-state.enc.json';
  const output = env.INSTAGRAM_LIFECYCLE_OUTPUT_FILE || input;
  let bundle;
  if (command === '--setup') {
    bundle = await bootstrapLifecycle({ ...config, code: env.INSTAGRAM_AUTHORIZATION_CODE, redirectUri: env.INSTAGRAM_REDIRECT_URI }, options);
  } else {
    bundle = await loadBundle(input, config);
    if (command === '--refresh') bundle = await refreshLifecycle(bundle, config, options);
  }
  const status = await checkLifecycle(bundle, config, options);
  if (command !== '--check') await saveBundle(output, bundle, config);
  return { expiresAt: status.expiresAt, persisted: command !== '--check' };
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  try {
    if (process.argv.length > 3) throw new LifecycleError('Unexpected arguments; credentials must never be command-line arguments');
    const result = await runLifecycle(process.argv[2] || '--check');
    console.log(`Instagram ownership provenance, expiry and live account verified; expires ${new Date(result.expiresAt * 1000).toISOString()}`);
    if (result.persisted) console.log('Encrypted lifecycle state persisted. No runtime credential or deployment was changed.');
    if (result.expiresAt - Date.now() / 1000 < 7 * 86400) console.log('WARNING: lifecycle deadline is within seven days; approved credential maintenance is required.');
  } catch (error) {
    console.error(error instanceof LifecycleError ? error.message : 'Instagram lifecycle operation failed; sensitive error details withheld');
    process.exitCode = 1;
  }
}
