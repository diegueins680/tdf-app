import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import test from 'node:test';

const read = path => readFile(new URL(`../../${path}`, import.meta.url), 'utf8');
function renderer(text) {
  const stanza = text.match(/^executable tdf-ddex-render\n([\s\S]*?)(?=^\S|$(?![\s\S]))/m)?.[1];
  assert(stanza, 'Renderer component must be explicit');
  return stanza.trim().replace(/hs-source-dirs:.*\n/, 'hs-source-dirs: app, src\n');
}

test('standalone worker builds the identical renderer component, not a fork', async () => {
  assert.equal(renderer(await read('tdf-hq/music-worker/tdf-music-worker.cabal')),
    renderer(await read('tdf-hq/tdf-hq.cabal')));
});

test('standalone worker resolver stays aligned with backend and bounds parallelism', async () => {
  const backend = await read('tdf-hq/stack.yaml');
  const worker = await read('tdf-hq/music-worker/stack.yaml');
  assert.equal(worker.match(/^resolver:\s*(\S+)/m)?.[1], backend.match(/^resolver:\s*(\S+)/m)?.[1]);
  assert.match(worker, /^jobs: 2$/m);
  const snapshots = lock => lock.split('\nsnapshots:\n')[1];
  assert.equal(snapshots(await read('tdf-hq/music-worker/stack.yaml.lock')),
    snapshots(await read('tdf-hq/stack.yaml.lock')));
});

test('worker image pins base digests and keeps compiler verification enabled', async () => {
  const dockerfile = await read('tdf-hq/Dockerfile.music-worker');
  const bases = dockerfile.split('\n').filter(line => line.startsWith('FROM '));
  assert.equal(bases.length, 2);
  for (const base of bases) assert.match(base, /@sha256:[0-9a-f]{64}(?: AS builder)?$/);
  assert(!dockerfile.includes('--skip-ghc-check'));
  assert.match(dockerfile, /music-worker\/stack\.yaml\.lock/);
});
