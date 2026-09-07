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
});
