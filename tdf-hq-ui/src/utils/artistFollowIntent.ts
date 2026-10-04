import { parsePositiveSafeInt } from './ids';

export const isArtistFollowResume = (search: string, artistId: number | null): boolean => {
  if (!artistId) return false;
  const params = new URLSearchParams(search);
  return params.get('resume') === 'follow'
    && parsePositiveSafeInt(params.get('artistId')) === artistId;
};

export const buildArtistFollowAuthPath = (
  profileLink: string | null,
  artistId: number | null,
): string => {
  if (!profileLink || !artistId) return '/login?signup=1&intent=follow_artists&redirect=%2Ffans';
  const resumePath = `${profileLink}?${new URLSearchParams({
    resume: 'follow',
    artistId: String(artistId),
  }).toString()}`;
  return `/login?${new URLSearchParams({
    signup: '1',
    intent: 'follow_artists',
    redirect: resumePath,
  }).toString()}`;
};

