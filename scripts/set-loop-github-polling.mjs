#!/usr/bin/env node
import fs from 'node:fs/promises';
import path from 'node:path';
import process from 'node:process';
import { execFile } from 'node:child_process';
import { promisify, parseArgs } from 'node:util';
import { fileURLToPath } from 'node:url';

const execFileAsync = promisify(execFile);
const repoRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const defaultConfigPath = path.join(repoRoot, 'scripts', 'continuous-improvement-loop.codex.json');

function usage() {
  console.log(`Usage: node scripts/set-loop-github-polling.mjs <on|off|status> [--config <path>]

Examples:
  npm run loop:enable-ci-polling
  npm run loop:disable-ci-polling
  npm run loop:ci-polling-status
  node scripts/set-loop-github-polling.mjs on --config scripts/continuous-improvement-loop.example.json`);
}

async function readConfig(configPath) {
  const raw = await fs.readFile(configPath, 'utf8');
  return JSON.parse(raw);
}

async function writeConfig(configPath, value) {
  await fs.writeFile(configPath, `${JSON.stringify(value, null, 2)}\n`, 'utf8');
}

async function ghAuthSummary() {
  try {
    await execFileAsync(process.env.SHELL || '/bin/zsh', ['-lc', 'GH_TOKEN= GITHUB_TOKEN= GITHUB_PAT= gh auth token'], {
      cwd: repoRoot,
      maxBuffer: 1024 * 1024,
    });
    return 'GitHub auth looks healthy.';
  } catch {
    return 'GitHub auth is not healthy. Re-auth with `gh auth login -h github.com` or clear stale token env vars before relying on polling.';
  }
}

async function main() {
  const { positionals, values } = parseArgs({
    options: {
      config: { type: 'string' },
      help: { type: 'boolean' },
    },
    allowPositionals: true,
  });

  if (values.help || positionals.length === 0) {
    usage();
    return;
  }

  const mode = String(positionals[0]).trim().toLowerCase();
  const configPath = path.resolve(repoRoot, values.config || defaultConfigPath);
  const config = await readConfig(configPath);

  if (!['on', 'off', 'status'].includes(mode)) {
    throw new Error(`Unsupported mode: ${mode}`);
  }

  if (mode === 'status') {
    console.log(`pollGitHub=${String(Boolean(config.pollGitHub))}`);
    console.log(`config=${configPath}`);
    return;
  }

  const nextValue = mode === 'on';
  const changed = Boolean(config.pollGitHub) !== nextValue;
  config.pollGitHub = nextValue;
  await writeConfig(configPath, config);

  console.log(`${changed ? 'Updated' : 'Kept'} pollGitHub=${String(nextValue)} in ${configPath}`);
  if (nextValue) {
    console.log(await ghAuthSummary());
  }
}

main().catch((error) => {
  console.error(error.message);
  process.exit(1);
});
