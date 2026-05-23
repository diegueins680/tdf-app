import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import path from 'node:path';
import test from 'node:test';
import { fileURLToPath } from 'node:url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const source = readFileSync(
  path.resolve(__dirname, '../../archives/tdf-root-ui-legacy/auth/AuthProvider.tsx'),
  'utf8',
);

function findMatchingBrace(text, openIndex) {
  let depth = 0;
  for (let index = openIndex; index < text.length; index += 1) {
    if (text[index] === '{') depth += 1;
    if (text[index] === '}') {
      depth -= 1;
      if (depth === 0) return index;
    }
  }
  return -1;
}

function extractFunctionBody(text, name) {
  const functionStart = text.indexOf(`function ${name}`);
  assert.notEqual(functionStart, -1, `${name} should exist`);

  const bodyStart = text.indexOf('{', functionStart);
  assert.notEqual(bodyStart, -1, `${name} should have a body`);

  const bodyEnd = findMatchingBrace(text, bodyStart);
  assert.notEqual(bodyEnd, -1, `${name} body should close`);

  return text.slice(bodyStart + 1, bodyEnd);
}

test('legacy AuthProvider catches malformed stored auth JSON during hydration', () => {
  const body = extractFunctionBody(source, 'readFromStorage');
  const parseIndex = body.indexOf('JSON.parse(raw)');
  assert.notEqual(parseIndex, -1, 'stored auth payload should still be parsed from raw storage');

  const tryIndex = body.lastIndexOf('try', parseIndex);
  assert.notEqual(tryIndex, -1, 'JSON.parse(raw) should be inside a try block');

  const tryBodyStart = body.indexOf('{', tryIndex);
  const tryBodyEnd = findMatchingBrace(body, tryBodyStart);
  assert.ok(
    parseIndex > tryBodyStart && parseIndex < tryBodyEnd,
    'JSON.parse(raw) should be protected by the try block',
  );

  const afterTry = body.slice(tryBodyEnd + 1);
  assert.match(afterTry, /^\s*catch\b/, 'malformed storage should be caught instead of crashing');

  const catchBodyStart = body.indexOf('{', tryBodyEnd + 1);
  const catchBodyEnd = findMatchingBrace(body, catchBodyStart);
  const catchBody = body.slice(catchBodyStart + 1, catchBodyEnd);

  assert.match(catchBody, /window\.sessionStorage\.removeItem\(STORAGE_KEY_SESSION\)/);
  assert.match(catchBody, /window\.localStorage\.removeItem\(STORAGE_KEY_PERSISTED\)/);
  assert.match(catchBody, /setAuthToken\(null\)/);
  assert.match(catchBody, /return null/);
});
