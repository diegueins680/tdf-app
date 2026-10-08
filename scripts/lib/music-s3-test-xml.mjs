import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';

// Local S3 test responses only. Decode XML entities with an XML parser, not
// string substitutions: Go's encoder can serialize quoted ETags using &#34;.
export function readS3TestXmlScalar(body, tag) {
  assert.ok(['UploadId', 'ETag', 'Code'].includes(tag), 'Unsupported XML field');
  assert.doesNotMatch(body.toString(), /<!DOCTYPE|<!ENTITY/i, 'DTD is forbidden in test responses');
  const selector = `/*/*[local-name()="${tag}"]`;
  const parse = (xpath) => execFileSync('xmllint', ['--nonet', '--xpath', xpath, '-'],
    { input: body, encoding: 'utf8', timeout: 10000, stdio: ['pipe', 'pipe', 'pipe'] }).trim();
  assert.equal(parse(`count(${selector})`), '1', `Expected one direct ${tag} field`);
  const value = parse(`string(${selector})`);
  assert.ok(value, `Empty ${tag} in S3 response`);
  return value;
}
