import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readS3TestXmlScalar } from '../lib/music-s3-test-xml.mjs';

test('decodes named, decimal and hexadecimal entities without changing multipart ETag', () => {
  for (const value of ['&quot;abcd-2&quot;', '&#34;abcd-2&#34;', '&#x22;abcd-2&#x22;', '"abcd-2"']) {
    assert.equal(readS3TestXmlScalar(`<CompleteMultipartUploadResult><ETag>${value}</ETag></CompleteMultipartUploadResult>`, 'ETag'), '"abcd-2"');
  }
});
test('reads a namespaced direct UploadId without confusing nested fields', () => {
  assert.equal(readS3TestXmlScalar('<s:Result xmlns:s="urn:test"><s:UploadId>a&amp;b</s:UploadId></s:Result>', 'UploadId'), 'a&b');
  assert.throws(() => readS3TestXmlScalar('<Result><Nested><UploadId>x</UploadId></Nested></Result>', 'UploadId'));
});
test('rejects DTD, malformed XML, duplicate and empty fields', () => {
  for (const body of ['<!DOCTYPE x><Result><ETag>x</ETag></Result>', '<Result>',
    '<Result><ETag>a</ETag><ETag>b</ETag></Result>', '<Result><ETag/></Result>']) {
    assert.throws(() => readS3TestXmlScalar(body, 'ETag'));
  }
});
