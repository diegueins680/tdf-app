export const safeArtistId = (value: number | null): value is number =>
  value !== null && Number.isSafeInteger(value) && value > 0;

export function isArtistFollowResume(search: string, artistId: number | null): boolean {
  if (!safeArtistId(artistId)) return false;
  const params = new URLSearchParams(search);
  return params.getAll('resume').length === 1 && params.get('resume') === 'follow'
    && params.getAll('artistId').length === 1 && params.get('artistId') === String(artistId);
}

export function buildArtistFollowAuthPath(profileLink: string | null, artistId: number | null, contextOrSearch: { search?: string; hash?: string } | string = {}): string {
  let context: { search?: string; hash?: string };
  if (typeof contextOrSearch === 'string') {
    // Preserve the campaign caller's original public-attribution allowlist.
    const input = new URLSearchParams(contextOrSearch), publicFields = new URLSearchParams();
    for (const key of ['utm_campaign', 'utm_source', 'utm_medium', 'utm_content']) {
      const value = input.get(key);
      if (input.getAll(key).length === 1 && value && /^[a-zA-Z0-9_-]{1,100}$/.test(value)) publicFields.set(key, value);
    }
    context = { search: publicFields.toString() };
  } else context = contextOrSearch;
  let redirect = '/fans';
  if (profileLink && safeArtistId(artistId) && /^\/a\/[^/?#\\]+$/.test(profileLink)) {
    try {
      const segment = decodeURIComponent(profileLink.slice(3));
      const safeSegment = segment !== '.' && segment !== '..' && segment.length > 0
        && !Array.from(segment).some((char) => '/\\?#%'.includes(char) || char.charCodeAt(0) <= 32 || char.charCodeAt(0) === 127);
      if (safeSegment && profileLink.length <= 400) {
        const params = new URLSearchParams({ resume: 'follow', artistId: String(artistId) });
        if (context.search && context.search.length <= 2000) {
          const source = new URLSearchParams(context.search);
          if (Array.from(source).length <= 25) for (const [key, value] of source) {
            if (source.getAll(key).length !== 1 || value.length > 200 || Array.from(value).some(char => char.charCodeAt(0) < 32 || char.charCodeAt(0) === 127)) continue;
            if (/^utm_(?:source|medium|campaign|content|term)$/.test(key)
              || (key === 'video' && /^(?:[0-9a-f]{32}|youtube:[\w-]{11}|vimeo:\d{6,12}|instagram:[\w-]{5,64}|tiktok:\d{15,25}|file:[0-9a-f]{32})$/.test(value))) params.set(key, value);
          }
        }
        redirect = `${profileLink}?${params.toString()}${context.hash === '#artist-session' ? '#artist-session' : ''}`;
      }
    } catch {
      // Malformed route metadata must not become a login return target.
    }
  }
  return `/login?${new URLSearchParams({ signup: '1', intent: 'follow_artists', redirect }).toString()}`;
}

