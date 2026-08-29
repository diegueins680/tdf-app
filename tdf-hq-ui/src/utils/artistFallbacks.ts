export const ARTIST_GLOBAL_FALLBACK = '/artist-fallback.svg';

/**
 * Verified artwork for seeded artists whose legacy profile rows predate image
 * persistence. Keep this map as a compatibility bridge until those rows are
 * enriched in the canonical artist profile store.
 */
export const ARTIST_HERO_FALLBACKS = {
  verde70: 'https://i.scdn.co/image/ab6761610000e5ebce05ab580a219a575432052e',
  arkabuz: 'https://i.scdn.co/image/ab6761610000e5eb7a769c9dd8bb01f2fe1d37aa',
  'el-bloque': 'https://i.scdn.co/image/ab6761610000e5eb15c68c09518671450cc315f1',
  skankafe: 'https://i.scdn.co/image/ab6761610000e5eb172b1792c8a9096500cbceb0',
  'e-quimika-soul': 'https://i.scdn.co/image/ab6761610000e5eb4918df2d6a21f0388e1c092e',
  'quimika-soul': 'https://i.scdn.co/image/ab6761610000e5eb4918df2d6a21f0388e1c092e',
  'e-quimika-soul': 'https://i.scdn.co/image/ab6761610000e5eb4918df2d6a21f0388e1c092e',
  'juano-ledesma': 'https://i.scdn.co/image/ab6761610000e5eb37e1fe32d15f7741170599df',
} as const satisfies Readonly<Record<string, string>>;

export function getArtistHeroImage(apHeroImageUrl: string | null | undefined, apSlug: string | null | undefined): string | null {
  const persistedImage = apHeroImageUrl?.trim();
  if (persistedImage) return persistedImage;

  const normalizedSlug = apSlug?.trim().toLowerCase() ?? '';
  return ARTIST_HERO_FALLBACKS[normalizedSlug as keyof typeof ARTIST_HERO_FALLBACKS]
    ?? ARTIST_GLOBAL_FALLBACK;
}
