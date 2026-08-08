export const ARTIST_GLOBAL_FALLBACK = '/artist-fallback.svg';

export function getArtistHeroImage(apHeroImageUrl: string | null | undefined, apSlug: string | null | undefined): string | null {
  void apSlug;
  return apHeroImageUrl ?? ARTIST_GLOBAL_FALLBACK;
}
