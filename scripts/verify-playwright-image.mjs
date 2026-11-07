import assert from 'node:assert/strict';
import { readFile, stat } from 'node:fs/promises';
import { homedir } from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { parse as parseYaml } from 'yaml';
import { parseArgs } from 'node:util';

export const packages = Object.freeze(['@playwright/test', 'playwright', 'playwright-core']);
export const browsers = Object.freeze(['chromium', 'firefox', 'webkit']);

export function parseWorkflowSelection(args) {
  const { values } = parseArgs({ args, options: {
    workflow: { type: 'string', default: '.github/workflows/ci.yml' },
    job: { type: 'string', default: 'persona-web-e2e' },
  }, strict: true, allowPositionals: false });
  assert.ok(['.github/workflows/ci.yml', '.github/workflows/social-verification.yml'].includes(values.workflow),
    'Select a supported repository browser workflow');
  assert.match(values.job, /^[a-zA-Z0-9_-]+$/, 'Select a nonempty workflow job');
  return values;
}

export function selectedWorkflowImage(workflow, jobName) {
  const image = workflow?.jobs?.[jobName]?.container?.image;
  assert.equal(typeof image, 'string', 'Selected workflow job must configure its container image');
  return image;
}

export function validateHomeOwner(uid, homeUid) {
  assert.ok(Number.isInteger(uid) && uid >= 0 && Number.isInteger(homeUid) && homeUid >= 0,
    'Browser user and home owner must be known');
  assert.equal(uid, homeUid, 'Browser user must own its home directory');
}

export function validateImage({ image, locked, installed, metadata, browserPath }) {
  const match = /^mcr\.microsoft\.com\/playwright:v(\d+\.\d+\.\d+)-noble@sha256:[a-f0-9]{64}$/.exec(image);
  assert.ok(match, 'CI requires an immutable official Playwright Noble image');
  const version = match[1];
  for (const name of packages) {
    assert.equal(locked[name], version, 'Image and lockfile differ: ' + name);
    assert.equal(installed[name], version, 'Image and installed package differ: ' + name);
  }
  assert.equal(browserPath, '/ms-playwright', 'Use the image browser directory');
  assert.equal(metadata?.driverVersion, version, 'Bundled browser version differs');
  assert.equal(metadata?.dockerImageName, 'mcr.microsoft.com/playwright:v' + version + '-noble',
    'Bundled browser image identity differs');
  return version;
}

export async function launchBrowsers(types) {
  const results = [];
  for (const name of browsers) {
    const browser = await types[name].launch({ headless: true });
    try {
      results.push({ browser: name, version: browser.version() });
    } finally {
      await browser.close();
    }
  }
  return results;
}

async function main() {
  const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
  const selection = parseWorkflowSelection(process.argv.slice(2));
  const workflow = parseYaml(await readFile(path.join(root, selection.workflow), 'utf8'));
  const lock = JSON.parse(await readFile(path.join(root, 'package-lock.json'), 'utf8'));
  const locked = {};
  const installed = {};
  for (const name of packages) {
    locked[name] = lock.packages['node_modules/' + name]?.version;
    installed[name] = JSON.parse(await readFile(path.join(root, 'node_modules', name, 'package.json'),
      'utf8')).version;
  }
  const metadata = JSON.parse(await readFile('/ms-playwright/.docker-info', 'utf8'));
  const browserUser = process.getuid();
  validateHomeOwner(browserUser, (await stat(homedir())).uid);
  const version = validateImage({
    image: selectedWorkflowImage(workflow, selection.job),
    locked, installed, metadata, browserPath: process.env.PLAYWRIGHT_BROWSERS_PATH,
  });
  const results = await launchBrowsers(await import('playwright'));
  console.log(JSON.stringify({
    kind: 'bundled-browser-preflight', workflow: selection.workflow, job: selection.job,
    playwright: version, browserUser, browsers: results,
  }));
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  await main();
}
