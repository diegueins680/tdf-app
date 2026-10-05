import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const tuple = value => {
  if (!/^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)$/.test(value)) throw new Error(`Unreviewed version: ${value}`);
  const parts = value.split('.').map(Number);
  if (parts.some(part => !Number.isSafeInteger(part))) throw new Error(`Unreviewed version: ${value}`);
  return parts;
};
const compare = (a, b) => a[0] - b[0] || a[1] - b[1] || a[2] - b[2];

export function verifyLock(lock, policy) {
  const object = value => value !== null && typeof value === 'object' && !Array.isArray(value);
  if (!object(lock) || lock.lockfileVersion !== 3 || !object(lock.packages)) throw new Error('Expected npm v3 package lock');
  if (!object(policy) || policy.schemaVersion !== 1 || !object(policy.packages)) throw new Error('Invalid dependency policy');
  if (!Object.keys(policy.packages).length) throw new Error('Empty dependency policy');
  for (const [name, series] of Object.entries(policy.packages)) {
    if (!object(series) || !Object.keys(series).length) throw new Error(`Invalid policy series: ${name}`);
    for (const [key, floor] of Object.entries(series)) {
      const parts = tuple(floor);
      const expected = parts[0] === 0 ? `${parts[0]}.${parts[1]}` : String(parts[0]);
      if (key !== expected) throw new Error(`Mismatched policy series: ${name} ${key}`);
    }
  }
  let checked = 0;
  for (const [location, entry] of Object.entries(lock.packages)) {
    if (!object(entry)) throw new Error(`Invalid package lock entry: ${location}`);
    if (!location.includes('node_modules/')) continue;
    const installedName = location.split('node_modules/').at(-1);
    const name = Object.hasOwn(entry, 'name') ? entry.name : installedName;
    if (typeof name !== 'string' || !/^(?:@[a-z0-9._-]+\/)?[a-z0-9._-]+$/i.test(name)) throw new Error(`Invalid package identity: ${location}`);
    const series = Object.hasOwn(policy.packages, name) ? policy.packages[name] : undefined;
    if (!series) continue;
    const actual = tuple(entry.version);
    const key = actual[0] === 0 ? `${actual[0]}.${actual[1]}` : String(actual[0]);
    const minimum = series[key];
    if (!minimum) throw new Error(`${location}: unreviewed dependency series ${entry.version}`);
    if (compare(actual, tuple(minimum)) < 0) throw new Error(`${location}: ${entry.version} violates security floor ${minimum}`);
    checked++;
  }
  return checked;
}

export function checkFiles(root, includeMobile = false) {
  const read = name => JSON.parse(fs.readFileSync(path.join(root, name), 'utf8'));
  const policy = read('formal/system/dependency-security.json');
  const locks = ['package-lock.json', ...(includeMobile ? ['tdf-mobile/package-lock.json'] : [])];
  return locks.map(file => ({ file, checked: verifyLock(read(file), policy) }));
}

if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  try {
    if (process.argv.slice(2).some(arg => arg !== '--mobile')) throw new Error('Usage: check-dependency-security.mjs [--mobile]');
    console.log(JSON.stringify({ checks: checkFiles(ROOT, process.argv.includes('--mobile')), limitation: 'Known version floors only; fresh advisory scan and unresolved exposure review are separate gates.' }));
  } catch (error) {
    console.error(error.message);
    process.exitCode = 1;
  }
}
