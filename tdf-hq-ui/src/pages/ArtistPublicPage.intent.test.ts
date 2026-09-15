import { buildArtistFollowAuthPath, isArtistFollowResume } from './ArtistPublicPage';

describe('artist follow authentication continuity', () => {
  it('preserves a same-origin, artist-bound explicit follow confirmation', () => {
    const authPath = buildArtistFollowAuthPath('/a/las-lolas', 42);
    const authUrl = new URL(authPath, 'https://tdf.local');

    expect(authUrl.pathname).toBe('/login');
    expect(authUrl.searchParams.get('signup')).toBe('1');
    expect(authUrl.searchParams.get('intent')).toBe('follow_artists');
    expect(authUrl.searchParams.get('redirect')).toBe('/a/las-lolas?resume=follow&artistId=42');
  });

  it('resumes only when the pending target matches the rendered artist', () => {
    expect(isArtistFollowResume('?resume=follow&artistId=42', 42)).toBe(true);
    expect(isArtistFollowResume('?resume=follow&artistId=41', 42)).toBe(false);
    expect(isArtistFollowResume('?resume=delete&artistId=42', 42)).toBe(false);
    expect(isArtistFollowResume('?resume=follow&artistId=https://evil.test', 42)).toBe(false);
  });

  it.each([
    '?resume=follow&resume=delete&artistId=42',
    '?resume=follow&artistId=42&artistId=41',
    '?resume=follow&artistId=042',
    '?resume=follow&artistId=42.0',
    '?resume=follow&artistId=4.2e1',
    '?resume=follow&artistId=9007199254740992',
    '?resume=follow&artistId=%2042',
  ])('rejects ambiguous or noncanonical resume parameters: %s', (search) => {
    expect(isArtistFollowResume(search, 42)).toBe(false);
  });

  it.each([
    '//evil.test/a/artist', 'https://evil.test/a/artist', '/a/../login',
    '/a/name?redirect=//evil.test', '/a/name#follow', '/a/name\\evil',
    '/a/%2f%2fevil.test', '/a/%5cevil', '/a/%0aname', '/a/%252f',
    '/a/name/extra', '/other/name', '/a/%',
  ])('keeps malformed profile paths out of the login return: %s', (profile) => {
    const url = new URL(buildArtistFollowAuthPath(profile, 42), 'https://tdf.local');
    expect(url.pathname).toBe('/login');
    expect(url.searchParams.get('redirect')).toBe('/fans');
  });

  it.each([null, 0, -1, 1.5, Number.MAX_SAFE_INTEGER + 1])('rejects unsafe artist identity %s', (id) => {
    expect(new URL(buildArtistFollowAuthPath('/a/artist', id), 'https://tdf.local').searchParams.get('redirect'))
      .toBe('/fans');
    expect(isArtistFollowResume('?resume=follow&artistId=1', id)).toBe(false);
  });
});
