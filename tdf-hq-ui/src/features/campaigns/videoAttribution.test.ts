import { videoAttribution } from './videoAttribution';
import { buildArtistFollowAuthPath } from '../../utils/artistFollowIntent';

const search = '?utm_campaign=126f208c-2b38-47c1-952b-6ba633122c87&utm_source=instagram&utm_content=instagram-2';
describe('video campaign registration continuity', () => {
  it('keeps campaign attribution inside a safe login return path', () => {
    const auth = new URL(buildArtistFollowAuthPath('/a/51', 51, search), 'https://www.tdfrecords.net');
    const redirect = new URL(auth.searchParams.get('redirect')!, 'https://www.tdfrecords.net');
    expect(redirect.pathname).toBe('/a/51');
    expect(redirect.searchParams.get('resume')).toBe('follow');
    expect(videoAttribution(redirect.search)).toEqual(videoAttribution(search));
  });
  it('rejects ambiguous or arbitrary attribution values', () => {
    expect(videoAttribution(search + '&utm_source=tiktok')).toBeNull();
    expect(videoAttribution(search.replace('instagram-2', '<script>'))).toBeNull();
    expect(videoAttribution('?utm_campaign=not-a-uuid')).toBeNull();
  });
});
