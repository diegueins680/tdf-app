/**
 * Fallback hero images for known artists seeded in the platform.
 * These are used when the database does not yet have apHeroImageUrl set.
 */
export const ARTIST_HERO_FALLBACKS: Record<string, string> = {
  verde70: 'https://i.scdn.co/image/ab6761610000e5ebce05ab580a219a575432052e',
  arkabuz: 'https://i.scdn.co/image/ab6761610000e5eb7a769c9dd8bb01f2fe1d37aa',
  'el-bloque': 'https://i.scdn.co/image/ab6761610000e5eb15c68c09518671450cc315f1',
  skankafe: 'https://i.scdn.co/image/ab6761610000e5eb172b1792c8a9096500cbceb0',
  'quimika-soul': 'https://i.scdn.co/image/ab6761610000e5eb4918df2d6a21f0388e1c092e',
  'juano-ledesma': 'https://i.scdn.co/image/ab6761610000e5eb37e1fe32d15f7741170599df',
};

export function getArtistHeroImage(apHeroImageUrl: string | null | undefined, apSlug: string | null | undefined): string | null {
  return apHeroImageUrl ?? ARTIST_HERO_FALLBACKS[(apSlug ?? '').toLowerCase()] ?? null;
}
