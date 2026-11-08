#!/usr/bin/env node
'use strict';
// Writes generated/channel.json (shipped as a resource) and build/icon.png (the
// official TDF symbol from the web app) before electron-builder runs.
//
// Environment:
//   TDF_DESKTOP_CHANNEL     production (default) | local | any channels/<name>.json
//   TDF_DESKTOP_APP_ORIGIN  optional https origin override (staging previews)
//   TDF_DESKTOP_API_ORIGIN  optional https origin override
//   TDF_DESKTOP_UNSIGNED    "1" marks the build as an unsigned test package
//   SOURCE_COMMIT / GITHUB_SHA  recorded in the receipt
const fs = require('node:fs');
const path = require('node:path');
const { execSync } = require('node:child_process');
const { DEFAULT_CHANNEL, normalizeChannel, parseOrigin } = require('../src/main/config.cjs');

const root = path.resolve(__dirname, '..');
const name = process.env.TDF_DESKTOP_CHANNEL || DEFAULT_CHANNEL;
if (!/^[a-z][a-z0-9-]{0,31}$/.test(name)) throw new Error(`Invalid channel name ${name}`);
const file = path.join(root, 'channels', `${name}.json`);
if (!fs.existsSync(file)) throw new Error(`Unknown channel ${name}: ${file} does not exist`);
const raw = JSON.parse(fs.readFileSync(file, 'utf8'));

if (process.env.TDF_DESKTOP_APP_ORIGIN) {
  const origin = parseOrigin(process.env.TDF_DESKTOP_APP_ORIGIN);
  if (!origin) throw new Error('TDF_DESKTOP_APP_ORIGIN must be an https origin');
  raw.appOrigin = origin;
}
if (process.env.TDF_DESKTOP_API_ORIGIN) {
  const origin = parseOrigin(process.env.TDF_DESKTOP_API_ORIGIN);
  if (!origin) throw new Error('TDF_DESKTOP_API_ORIGIN must be an https origin');
  raw.apiOrigin = origin;
}
raw.unsigned = process.env.TDF_DESKTOP_UNSIGNED === '1';
raw.builtAt = new Date().toISOString();
const commit = process.env.SOURCE_COMMIT || process.env.GITHUB_SHA || (() => {
  try { return execSync('git rev-parse HEAD', { cwd: root, encoding: 'utf8' }).trim(); } catch { return null; }
})();
if (commit) raw.sourceCommit = commit;

const channel = normalizeChannel(raw);
if (channel.name === 'local') throw new Error('The local channel is for development only and cannot be packaged');
if (channel.appOrigin.startsWith('http:')) throw new Error('Packaged channels must host an https origin');

fs.mkdirSync(path.join(root, 'build'), { recursive: true });
fs.mkdirSync(path.join(root, 'generated'), { recursive: true });
fs.writeFileSync(path.join(root, 'generated', 'channel.json'), `${JSON.stringify(channel, null, 2)}\n`);

const iconSource = path.join(root, '..', 'tdf-hq-ui', 'public', 'tdf-app-icon-1024.png');
if (!fs.existsSync(iconSource)) throw new Error(`Official icon missing: ${iconSource}`);
fs.copyFileSync(iconSource, path.join(root, 'build', 'icon.png'));

process.stdout.write(`${JSON.stringify({ channel: channel.name, appOrigin: channel.appOrigin, unsigned: channel.unsigned, sourceCommit: channel.sourceCommit })}\n`);
