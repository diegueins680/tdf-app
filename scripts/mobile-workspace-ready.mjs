#!/usr/bin/env node

import fs from 'node:fs';
import path from 'node:path';
import process from 'node:process';
import { fileURLToPath } from 'node:url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const rootDir = path.resolve(__dirname, '..');
const mobileDir = path.join(rootDir, 'tdf-mobile');
const quiet = process.argv.includes('--quiet');

const fail = (message) => {
  if (!quiet) {
    console.error(message);
  }
  process.exit(1);
};

if (!fs.existsSync(path.join(mobileDir, 'package.json'))) {
  fail('tdf-mobile/package.json is missing.');
}

if (!fs.existsSync(path.join(mobileDir, 'node_modules'))) {
  fail('tdf-mobile/node_modules is missing.');
}

const packageJson = JSON.parse(fs.readFileSync(path.join(mobileDir, 'package.json'), 'utf8'));
const directDependencies = Object.keys({
  ...(packageJson.dependencies ?? {}),
  ...(packageJson.devDependencies ?? {}),
});

const missingPackageManifests = directDependencies.filter((dependencyName) => (
  !fs.existsSync(path.join(mobileDir, 'node_modules', ...dependencyName.split('/'), 'package.json'))
));

if (missingPackageManifests.length > 0) {
  const preview = missingPackageManifests.slice(0, 8).join(', ');
  fail(
    `tdf-mobile install is incomplete: missing direct package manifests for ${preview}`
      + `${missingPackageManifests.length > 8 ? ', ...' : ''}.`,
  );
}
