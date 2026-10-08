#!/usr/bin/env node
// Explicit fault transport, not an S3 implementation or provider certificate.
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { appendFileSync, readFileSync, writeFileSync } from 'node:fs';
const args = process.argv.slice(2);
const option = (name) => args[args.indexOf(name) + 1];
const url = new URL(args.at(-1));
assert.equal(url.origin, 'https://music-upload.test.invalid');
assert.equal(args.includes('--user'), false, 'Credentials must not be process arguments');
assert.equal(args[0], '-q');
const method = option('-X');
const action = method === 'DELETE' ? 'abort' : url.searchParams.has('uploads') ? 'create'
  : method === 'PUT' ? `part${url.searchParams.get('partNumber') ?? 'single'}` : 'complete';
appendFileSync(process.env.MUSIC_TEST_TRACE, `${action}\n`);
const fault = process.env.MUSIC_TEST_FAULT;
const respond = (body) => writeFileSync(option('-o'), body);
const id = 'id+/=&';
if (!['create', 'partsingle'].includes(action)) assert.equal(url.searchParams.get('uploadId'), id);
if (action === 'create') {
  respond(`<InitiateMultipartUploadResult><UploadId>${id.replaceAll('&', '&amp;')}</UploadId></InitiateMultipartUploadResult>`);
} else if (action.startsWith('part')) {
  const body = readFileSync(option('--upload-file'));
  const header = args.find((arg) => arg.startsWith('x-amz-content-sha256:'));
  assert.equal(header, `x-amz-content-sha256: ${createHash('sha256').update(body).digest('hex')}`);
  if (fault === 'part_failure' && action === 'part2') process.exit(22);
  if (fault === 'cancel' && action === 'part2') await new Promise(() => setInterval(() => {}, 1000));
  if (fault === 'mutation' && action === 'part1') {
    const changed = readFileSync(process.env.MUSIC_TEST_SOURCE); changed[changed.length - 1] ^= 255;
    writeFileSync(process.env.MUSIC_TEST_SOURCE, changed);
  }
  writeFileSync(option('-D'), 'HTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 200 OK\r\nETag: "opaque&etag"\r\n' +
    (fault === 'duplicate_etag' ? 'ETag: "other"\r\n' : '') + '\r\n');
  respond('');
} else if (action === 'complete') {
  const body = readFileSync(option('--data-binary').slice(1), 'utf8');
  assert.ok(body.includes('&quot;opaque&amp;etag&quot;'));
  respond(fault === 'embedded_error' ? '<Error><Code>InternalError</Code></Error>'
    : fault === 'duplicate_result' ? '<CompleteMultipartUploadResult><ETag>a</ETag><ETag>b</ETag></CompleteMultipartUploadResult>'
      : fault === 'unsafe_xml' ? '<!DOCTYPE a [<!ENTITY x SYSTEM "file:///etc/passwd">]><CompleteMultipartUploadResult><ETag>&x;</ETag></CompleteMultipartUploadResult>'
        : '<CompleteMultipartUploadResult><ETag>opaque-result</ETag></CompleteMultipartUploadResult>');
} else if (action === 'abort') respond('');
else throw new Error('Unexpected test operation');
