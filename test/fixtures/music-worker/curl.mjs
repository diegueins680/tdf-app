#!/usr/bin/env node
// Deliberate transport fault injector. This is not an S3 compatibility test.
import { appendFileSync, copyFileSync, existsSync, mkdirSync, unlinkSync, writeFileSync } from 'node:fs';
import { setTimeout } from 'node:timers/promises';
import { dirname, join } from 'node:path';

const args = process.argv.slice(2);
const option = (name) => args[args.indexOf(name) + 1];
const url = new URL(args.at(-1));
if (url.hostname !== 'music-worker.test.invalid') throw new Error('Unexpected test host');
const key = decodeURIComponent(url.pathname).slice(1);
if (key.split('/').some((part) => !part || part === '..' || part === '.')) {
  throw new Error('Unsafe fixture object key');
}
const method = args.includes('-X') ? option('-X') : 'GET';
appendFileSync(process.env.MUSIC_TEST_TRACE, `${method} ${key}\n`);
const gate = process.env.MUSIC_TEST_GATE;
if (gate && ((process.env.MUSIC_TEST_GATE_METHOD ?? 'GET') === method)) {
  writeFileSync(`${gate}.started`, String(process.pid));
  const deadline = Date.now() + 180000;
  while (!existsSync(`${gate}.release`)) {
    if (Date.now() > deadline) throw new Error('Synthetic transport gate timed out');
    await setTimeout(100);
  }
}
const fault = process.env.MUSIC_TEST_FAULT;
if ((fault === 'get' && method === 'GET')
  || (fault === 'master_put' && method === 'PUT' && key.startsWith('music-test-master/'))
  || (fault === 'derivative_put' && method === 'PUT' && key.startsWith('music-test-derivatives/'))
  || (fault === 'delete' && method === 'DELETE')) {
  console.error(`Injected storage failure: ${fault}`);
  process.exit(22);
}
const object = join(process.env.MUSIC_TEST_OBJECTS, key);
if (method === 'GET') copyFileSync(object, option('-o'));
else if (method === 'PUT') {
  mkdirSync(dirname(object), { recursive: true });
  copyFileSync(option('--upload-file'), object);
} else if (method === 'DELETE') unlinkSync(object);
else throw new Error(`Unsupported test method: ${method}`);
