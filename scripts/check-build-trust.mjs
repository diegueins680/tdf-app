#!/usr/bin/env node
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

export const imageRecipes = ['tdf-hq/Dockerfile', 'tdf-hq/Dockerfile.runtime'];

// A deliberately narrow source-policy check, not a Docker/shell interpreter or
// a proof of upstream image trust. The real image build remains a release gate.
export function checkAptTrust(source) {
  const instructions = source.split('\n').filter(line => !line.trimStart().startsWith('#')).join('\n');
  for (const bypass of [
    /--allow-unauthenticated/i,
    /(?:AllowInsecureRepositories|AllowDowngradeToInsecureRepositories|AllowUnauthenticated)\s*(?:=|:|\s)\s*["']?(?:true|yes|1)/i,
    /(?:Check-Valid-Until|Check-Date|Verify-Peer|Verify-Host)\s*(?:=|:|\s)\s*["']?(?:false|no|0)/i,
    /(?:trusted|allow-insecure|allow-weak|allow-downgrade-to-insecure)\s*(?:=|:)\s*["']?yes/i,
  ]) assert.doesNotMatch(instructions, bypass, 'DEPLOY-BUILD-001: package trust bypass');
  const commands = [...instructions.matchAll(/\bapt-get\s+([^&;\n]+)/g)].map(match => match[1]);
  assert.ok(commands.length >= 2, 'Expected explicit APT update/install policy');
  for (const command of commands) {
    if (/\bupdate\b/.test(command)) {
      assert.match(command, /-o Acquire::AllowInsecureRepositories=false\b/);
      assert.match(command, /-o Acquire::Check-Valid-Until=true\b/);
      assert.match(command, /--error-on=any\b/);
    } else if (/\binstall\b/.test(command)) {
      assert.match(command, /-o APT::Get::AllowUnauthenticated=false\b/);
    } else assert.fail('Unreviewed APT operation in image recipe');
  }
}

export function checkBuildTrust(root) {
  for (const relative of imageRecipes) checkAptTrust(fs.readFileSync(path.join(root, relative), 'utf8'));
  assert.equal(fs.existsSync(path.join(root, 'tdf-hq/package.yaml')), false,
    'SYS-BUILD-001: retired Hpack input must not compete with the active Cabal file');
  for (const relative of ['tdf-hq/stack.yaml', 'tdf-hq/tdf-hq.cabal']) {
    assert.ok(fs.statSync(path.join(root, relative)).isFile(), `Missing build authority: ${relative}`);
  }
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  checkBuildTrust(path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..'));
  console.log('Build authority and explicit APT trust policy pass (source boundary only).');
}
