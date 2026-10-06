export const safeArtistId = (value: number | null): value is number =>
  value !== null && Number.isSafeInteger(value) && value > 0;

export function isArtistFollowResume(search: string, artistId: number | null): boolean {
  if (!safeArtistId(artistId)) return false;
  const params = new URLSearchParams(search);
  return params.getAll('resume').length === 1 && params.get('resume') === 'follow'
    && params.getAll('artistId').length === 1 && params.get('artistId') === String(artistId);
}

export function buildArtistFollowAuthPath(profileLink: string | null, artistId: number | null): string {
  let redirect = '/fans';
  if (profileLink && safeArtistId(artistId) && /^\/a\/[^/?#\\]+$/.test(profileLink)) {
    try {
      const segment = decodeURIComponent(profileLink.slice(3));
      const safeSegment = segment !== '.' && segment !== '..' && segment.length > 0
        && !Array.from(segment).some((char) => '/\\?#%'.includes(char) || char.charCodeAt(0) <= 32 || char.charCodeAt(0) === 127);
      if (safeSegment && profileLink.length <= 400) {
        redirect = `${profileLink}?${new URLSearchParams({ resume: 'follow', artistId: String(artistId) }).toString()}`;
      }
    } catch {
      // Malformed route metadata must not become a login return target.
    }
  }
  return `/login?${new URLSearchParams({ signup: '1', intent: 'follow_artists', redirect }).toString()}`;
}

