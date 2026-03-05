import { parseGoogleIdToken } from './googleIdToken';

const encodeBase64UrlUtf8 = (value: string): string => {
  return Buffer.from(value, 'utf8').toString('base64url');
};

const buildGoogleIdToken = (claims: unknown): string => {
  const header = encodeBase64UrlUtf8(JSON.stringify({ alg: 'none', typ: 'JWT' }));
  const payload = encodeBase64UrlUtf8(JSON.stringify(claims));
  return `${header}.${payload}.signature`;
};

describe('parseGoogleIdToken', () => {
  it('parses standard email and name claims', () => {
    const token = buildGoogleIdToken({ email: 'ana@tdf.com', name: 'Ana TDF' });
    expect(parseGoogleIdToken(token)).toEqual({ email: 'ana@tdf.com', name: 'Ana TDF' });
  });

  it('preserves UTF-8 characters in claims', () => {
    const token = buildGoogleIdToken({ email: 'jose@tdf.com', name: 'José Ñañez' });
    expect(parseGoogleIdToken(token)).toEqual({ email: 'jose@tdf.com', name: 'José Ñañez' });
  });

  it('returns null for malformed tokens', () => {
    expect(parseGoogleIdToken('not-a-jwt')).toBeNull();
    expect(parseGoogleIdToken('header.payload')).toBeNull();
    expect(parseGoogleIdToken('header.invalid-base64.signature')).toBeNull();
  });

  it('requires a strict three-part JWT structure', () => {
    const header = encodeBase64UrlUtf8(JSON.stringify({ alg: 'none', typ: 'JWT' }));
    const payload = encodeBase64UrlUtf8(JSON.stringify({ email: 'ana@tdf.com' }));

    expect(parseGoogleIdToken(`${header}.${payload}`)).toBeNull();
    expect(parseGoogleIdToken(`${header}.${payload}.sig.extra`)).toBeNull();
  });

  it('trims claims and drops blank strings', () => {
    const token = buildGoogleIdToken({ email: '  ana@tdf.com ', name: '   ' });
    expect(parseGoogleIdToken(token)).toEqual({ email: 'ana@tdf.com', name: undefined });
  });
});
