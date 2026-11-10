#!/usr/bin/env node
'use strict';
// Writes generated/channel.json (shipped as a resource) and build/icon.png (the
// official TDF symbol from the web app) before electron-builder runs.
//
// Environment:
//   TDF_DESKTOP_CHANNEL     production (default) | any channels/<name>.json except local
//   TDF_DESKTOP_APP_ORIGIN  optional https origin override (staging previews)
//   TDF_DESKTOP_API_ORIGIN  optional https origin override
//   TDF_DESKTOP_UNSIGNED    "1" forces an unsigned test package
//   CSC_LINK, WIN_CSC_LINK, Azure Trusted Signing variables (see signing.cjs):
//                           without them the package is an unsigned test build
//                           and channel.json says so
//   SOURCE_COMMIT / GITHUB_SHA  recorded in the receipt
const fs = require('node:fs');
const path = require('node:path');
const { execSync } = require('node:child_process');
const { DEFAULT_CHANNEL, normalizeChannel, parseOrigin } = require('../src/main/config.cjs');
const { resolveSigning } = require('./signing.cjs');

const root = path.resolve(__dirname, '..');

/**
 * Resolves the channel to embed from channels/<name>.json, the origin
 * overrides and the signing environment. The unsigned flag is the same
 * decision electron-builder.config.cjs applies to the artifact name and the
 * signing options, so an installed app can never claim a signed build when
 * its package is an unsigned test package.
 */
function buildChannel({ env = process.env, platform = process.platform, channelsDir = path.join(root, 'channels'), now = new Date(), sourceCommit = null } = {}) {
  const name = env.TDF_DESKTOP_CHANNEL || DEFAULT_CHANNEL;
  if (!/^[a-z][a-z0-9-]{0,31}$/.test(name)) throw new Error(`Invalid channel name ${name}`);
  const file = path.join(channelsDir, `${name}.json`);
  if (!fs.existsSync(file)) throw new Error(`Unknown channel ${name}: ${file} does not exist`);
  const raw = JSON.parse(fs.readFileSync(file, 'utf8'));

  if (env.TDF_DESKTOP_APP_ORIGIN) {
    const origin = parseOrigin(env.TDF_DESKTOP_APP_ORIGIN);
    if (!origin) throw new Error('TDF_DESKTOP_APP_ORIGIN must be an https origin');
    raw.appOrigin = origin;
  }
  if (env.TDF_DESKTOP_API_ORIGIN) {
    const origin = parseOrigin(env.TDF_DESKTOP_API_ORIGIN);
    if (!origin) throw new Error('TDF_DESKTOP_API_ORIGIN must be an https origin');
    raw.apiOrigin = origin;
  }
  const signing = resolveSigning(env, platform);
  raw.unsigned = signing.unsigned;
  raw.builtAt = now.toISOString();
  if (sourceCommit) raw.sourceCommit = sourceCommit;

  const channel = normalizeChannel(raw);
  if (channel.name === 'local') throw new Error('The local channel is for development only and cannot be packaged');
  if (channel.appOrigin.startsWith('http:')) throw new Error('Packaged channels must host an https origin');
  return { channel, signing };
}

function main() {
  const sourceCommit = process.env.SOURCE_COMMIT || process.env.GITHUB_SHA || (() => {
    try { return execSync('git rev-parse HEAD', { cwd: root, encoding: 'utf8' }).trim(); } catch { return null; }
  })();
  const { channel, signing } = buildChannel({ sourceCommit });

  fs.mkdirSync(path.join(root, 'build'), { recursive: true });
  fs.mkdirSync(path.join(root, 'generated'), { recursive: true });
  fs.writeFileSync(path.join(root, 'generated', 'channel.json'), `${JSON.stringify(channel, null, 2)}\n`);

  const iconSource = path.join(root, '..', 'tdf-hq-ui', 'public', 'tdf-app-icon-1024.png');
  if (!fs.existsSync(iconSource)) throw new Error(`Official icon missing: ${iconSource}`);
  fs.copyFileSync(iconSource, path.join(root, 'build', 'icon.png'));

  process.stdout.write(`${JSON.stringify({ channel: channel.name, appOrigin: channel.appOrigin, unsigned: channel.unsigned, signing: signing.reason ?? 'signed', sourceCommit: channel.sourceCommit })}\n`);
}

if (require.main === module) main();

module.exports = { buildChannel };
