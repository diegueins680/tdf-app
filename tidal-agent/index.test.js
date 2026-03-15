import test from 'node:test';
import assert from 'node:assert/strict';
import { allowedLine, extractCode } from './index.js';

test('extractCode reads fenced haskell blocks with real newlines', () => {
  const input = [
    '```haskell',
    'd1 $ sound "bd*4"',
    'xfadeIn 4',
    '```',
    'short explanation',
  ].join('\n');

  assert.equal(extractCode(input), 'd1 $ sound "bd*4"\nxfadeIn 4');
});

test('allowedLine rejects partial safe-prefix matches', () => {
  assert.equal(allowedLine('d10 $ sound "bd*4"'), false);
  assert.equal(allowedLine('hushLater'), false);
  assert.equal(allowedLine('soloist'), false);
});

test('extractCode keeps exact whitelisted commands', () => {
  const input = 'hush\n-- comment\nonce $ sound "bd"';

  assert.equal(extractCode(input), 'hush\nonce $ sound "bd"');
});
