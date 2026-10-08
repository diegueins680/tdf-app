import { parseMusicMultipartCompletion } from './musicStorageResponse';

describe('music multipart storage confirmation', () => {
  const success = '<?xml version="1.0"?>\n' +
    '<CompleteMultipartUploadResult xmlns="http://s3.amazonaws.com/doc/2006-03-01/">' +
    '<ETag>&quot;abc-2&quot;</ETag></CompleteMultipartUploadResult>';

  it('accepts a namespaced completion and the decoded body ETag', () => {
    expect(parseMusicMultipartCompletion(success)).toBe('"abc-2"');
    expect(parseMusicMultipartCompletion(success, ' "abc-2" ')).toBe('"abc-2"');
  });

  it('rejects an embedded S3 error even if HTTP 200 included an ETag header', () => {
    expect(() => parseMusicMultipartCompletion(
      '<Error><Code>InvalidPart</Code><Message>provider details</Message></Error>', '"abc-2"',
    )).toThrow('InvalidPart');
  });

  it('does not expose arbitrary provider messages or unsafe error codes', () => {
    try {
      parseMusicMultipartCompletion('<Error><Code>https://private.invalid/key</Code>' +
        '<Message>secret-provider-details</Message></Error>');
      throw new Error('Expected failure');
    } catch (error) {
      expect((error as Error).message).not.toMatch(/private|secret-provider/);
      expect((error as Error).message).toContain('no completó');
    }
  });

  it.each(['', '<broken', '<Unexpected><ETag>"abc-2"</ETag></Unexpected>',
    '<CompleteMultipartUploadResult/>',
    '<CompleteMultipartUploadResult><Nested><ETag>"abc-2"</ETag></Nested></CompleteMultipartUploadResult>',
    '<CompleteMultipartUploadResult><ETag>a</ETag><ETag>b</ETag></CompleteMultipartUploadResult>',
  ])('rejects malformed or ambiguous confirmation %# despite an ETag header', (body) => {
    expect(() => parseMusicMultipartCompletion(body, '"abc-2"')).toThrow();
  });

  it('rejects disagreement between the response body and header', () => {
    expect(() => parseMusicMultipartCompletion(success, '"other"')).toThrow('no coincide');
  });
});
