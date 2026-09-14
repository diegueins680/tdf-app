#!/usr/bin/env node
// Read-only, fixed-target inspection. Never export secrets, raw config or CLI errors.
import { execFile } from 'node:child_process';
import { mkdir, writeFile } from 'node:fs/promises';
import { promisify } from 'node:util';
import { pathToFileURL } from 'node:url';

const exec = promisify(execFile);
export const STAGING_APPS = Object.freeze([
  'tdf-hq-studio-audit-staging',
  'tdf-studio-audit-staging-web',
]);
const SECRET_NAMES = Object.freeze([
  'DATABASE_URL', 'COMMERCE_EVENT_ENCRYPTION_KEY',
  'DATAFAST_ENTITY_ID', 'DATAFAST_BEARER_TOKEN',
  'PAYPAL_CLIENT_ID', 'PAYPAL_CLIENT_SECRET', 'PAYPAL_WEBHOOK_ID',
  'PLACETOPAY_LOGIN', 'PLACETOPAY_SECRET_KEY', 'PAYPHONE_TOKEN', 'PAYPHONE_STORE_ID',
]);
const SAFE_CONFIG = Object.freeze({
  APP_ENV: ['staging'], COMMERCE_CHECKOUT_ENV: ['sandbox'],
  PAYPAL_ENV: ['sandbox'], DATAFAST_ENV: ['sandbox'],
  DATAFAST_BASE_URL: ['https://test.oppwa.com'],
  RUN_MIGRATIONS: ['false'], AUTO_APPLY_PRODUCTION_MIGRATIONS: ['false'],
  RESET_DB: ['false'], SEED_DB: ['false'],
  CORS_DISABLE_DEFAULTS: ['true'],
  ALLOWED_ORIGINS: ['https://tdf-studio-audit-staging-web.fly.dev'],
});

export function assertStagingApp(app) {
  if (!STAGING_APPS.includes(app)) throw new Error('Only the fixed nonproduction apps are allowed');
}

export function summarizeConfig(app, value) {
  assertStagingApp(app);
  if ((value.app ?? value.App) !== app) throw new Error('Unexpected staging app identity');
  const env = value.env ?? value.Env ?? {};
  return {
    safetySettings: Object.fromEntries(Object.entries(SAFE_CONFIG).map(([key, permitted]) => [
      key, env[key] === undefined ? 'absent' : permitted.includes(env[key]) ? env[key] : 'unexpected',
    ])),
    sensitivePlaintextSettingNames: Object.keys(env)
      .filter((key) => /^[A-Z][A-Z0-9_]{0,100}$/.test(key)
        && /TOKEN|SECRET|PASSWORD|PRIVATE_KEY|DATABASE_URL|BEARER/.test(key)).sort(),
    // Never print values for an unknown/unsafe environment key.
  };
}

export function summarizeSecrets(value) {
  if (!Array.isArray(value)) throw new Error('Unexpected secret-name listing');
  const names = new Set(value.map((item) => item.Name ?? item.name));
  return Object.fromEntries(SECRET_NAMES.map((name) => [name, names.has(name)]));
}

export function summarizeStatus(app, value) {
  assertStagingApp(app);
  if ((value.Name ?? value.name) !== app) throw new Error('Unexpected staging app identity');
  const states = new Set(['pending', 'deployed', 'running', 'suspended', 'started', 'stopped', 'destroyed']);
  const status = value.Status ?? value.status;
  const machines = value.Machines ?? value.machines ?? [];
  return {
    status: states.has(status) ? status : 'unknown',
    machines: machines.map((machine) => ({
      state: states.has(machine.state ?? machine.State) ? machine.state ?? machine.State : 'unknown',
      // IDs are only operational identifiers; machine config is deliberately omitted.
      id: /^[a-f0-9]{8,32}$/.test(machine.id ?? machine.ID ?? '') ? machine.id ?? machine.ID : 'redacted',
    })),
  };
}

async function flyJson(args) {
  const { stdout } = await exec('flyctl', args, {
    encoding: 'utf8', timeout: 30_000, maxBuffer: 5 * 1024 * 1024,
  });
  return JSON.parse(stdout);
}

async function readHealth(app) {
  const response = await fetch(`https://${app}.fly.dev/health`, {
    redirect: 'error', signal: AbortSignal.timeout(20_000),
  });
  const value = await response.json();
  return {
    httpStatus: response.status,
    statusOk: response.ok && value.status === 'ok',
    databaseOk: value.db === 'ok',
  };
}

export function classifyInspectionError(error) {
  // Match known failure classes in memory; never return error text or buffers.
  const diagnostic = `${error?.message ?? ''} ${error?.stderr ?? ''}`.toLowerCase();
  if (error?.code === 'ENOENT') return 'cli_not_installed';
  if (error?.killed || /timed out|timeout/.test(diagnostic)) return 'timeout';
  if (/no access token|not logged in|token.*expir|invalid.*token|unauthenticated|authentication|must be authenticated/.test(diagnostic)) return 'hosting_authentication_unavailable';
  if (/not authorized|not allowed|permission denied|forbidden|unauthorized/.test(diagnostic)) return 'hosting_authorization_denied';
  if (/could not find app|app.*not found/.test(diagnostic)) return 'staging_app_inaccessible';
  if (/unexpected staging app identity/.test(diagnostic)) return 'unexpected_app_identity';
  if (/unexpected secret-name listing/.test(diagnostic)) return 'unexpected_secret_metadata_schema';
  if (error instanceof SyntaxError) return 'non_json_cli_response';
  if (/resolve|enotfound|econnrefused|connection/.test(diagnostic)) return 'hosting_connection_failed';
  return 'unclassified_failure_no_raw_output_retained';
}

export async function inspectStaging({ runFly = flyJson, health = readHealth } = {}) {
  const apps = [];
  for (const app of STAGING_APPS) {
    assertStagingApp(app);
    const result = { app };
    for (const [field, args, summarize] of [
      ['status', ['status', '--app', app, '--json'], (value) => summarizeStatus(app, value)],
      ['config', ['config', 'show', '--app', app], (value) => summarizeConfig(app, value)],
      ['secretPresence', ['secrets', 'list', '--app', app, '--json'], summarizeSecrets],
    ]) {
      try {
        result[field] = { accessible: true, ...summarize(await runFly(args)) };
      } catch (error) {
        // Child-process errors can contain raw stdout/stderr; never serialize them.
        result[field] = { accessible: false, reason: classifyInspectionError(error) };
      }
    }
    try { result.health = await health(app); }
    catch { result.health = { statusOk: false, reason: 'Health check unavailable' }; }
    apps.push(result);
  }
  return {
    generatedAt: new Date().toISOString(),
    sourceCommit: /^[a-f0-9]{40}$/.test(process.env.GITHUB_SHA ?? '') ? process.env.GITHUB_SHA : null,
    scope: 'read_only_staging_metadata',
    providerQualified: false,
    apps,
  };
}

if (import.meta.url === pathToFileURL(process.argv[1] ?? '').href) {
  const report = await inspectStaging();
  await mkdir('artifacts/payment-staging', { recursive: true });
  await writeFile('artifacts/payment-staging/access-report.json', `${JSON.stringify(report, null, 2)}\n`);
  console.log(JSON.stringify(report, null, 2));
  if (report.apps.some((app) => !app.status.accessible || !app.config.accessible || !app.secretPresence.accessible)) {
    process.exitCode = 1;
  }
}
