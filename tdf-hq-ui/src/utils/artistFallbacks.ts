import { ACCOUNT_PLACEHOLDER_NAME } from './signupFields';
export const ARTIST_GLOBAL_FALLBACK = '/artist-fallback.svg';

// The light-theme primary/secondary shades in theme/appTheme.ts, all with white text.
const AVATAR_COLORS = ['#6d28d9', '#5b21b6', '#be123c', '#9f1239'] as const;
const graphemes = (value: string) => value.normalize('NFC').match(/\P{M}\p{M}*/gu) ?? [];

export function artistInitials(name: string | null | undefined): string {
  const words = (name ?? '').normalize('NFKC').match(/[\p{L}\p{N}][\p{L}\p{M}\p{N}]*/gu) ?? [];
  const letters = words.length > 1
    ? `${graphemes(words[0] ?? '')[0]}${graphemes(words[words.length - 1] ?? '')[0]}`
    : (words[0] ?? '♪');
  return graphemes(letters.toLocaleUpperCase('es')).slice(0, 2).join('');
}

export function artistPlaceholderImage(name: string | null | undefined): string {
  const normalized = (name ?? '').normalize('NFKC').trim().replace(/\s+/gu, ' ').toLocaleLowerCase('es');
  let hash = 0;
  for (const character of normalized) hash = (Math.imul(hash, 31) + (character.codePointAt(0) ?? 0)) >>> 0;
  const initials = artistInitials(name).replace(/[&<>"']/g, (character) => `&#${character.charCodeAt(0)};`);
  const svg = `<svg xmlns="http://www.w3.org/2000/svg" data-tdf-artist-avatar="true" viewBox="0 0 256 256"><rect width="256" height="256" fill="${AVATAR_COLORS[hash % AVATAR_COLORS.length]}"/><text x="128" y="132" fill="white" text-anchor="middle" dominant-baseline="middle" font-family="system-ui,sans-serif" font-size="88" font-weight="700">${initials}</text></svg>`;
  return `data:image/svg+xml,${encodeURIComponent(svg)}`;
}

export function isArtistPlaceholderImage(value: string | null | undefined): boolean {
  if (!value?.trim()) return true;
  const url = value.trim();
  if (url.startsWith('data:image/svg+xml,') && url.slice(0, 350).includes('data-tdf-artist-avatar')) return true;
  try {
    return new URL(url, 'https://www.tdfrecords.net').pathname === ARTIST_GLOBAL_FALLBACK;
  } catch {
    return false;
  }
}

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
  'juano-ledesma': 'https://i.scdn.co/image/ab6761610000e5eb37e1fe32d15f7741170599df',
} as const satisfies Readonly<Record<string, string>>;

export function getArtistHeroImage(apHeroImageUrl: string | null | undefined, apSlug: string | null | undefined, artistName?: string | null): string {
  const persistedImage = apHeroImageUrl?.trim();
  if (persistedImage && !isArtistPlaceholderImage(persistedImage)) return persistedImage;

  const normalizedSlug = apSlug?.trim().toLowerCase() ?? '';
  return ARTIST_HERO_FALLBACKS[normalizedSlug as keyof typeof ARTIST_HERO_FALLBACKS]
    ?? artistPlaceholderImage(artistName);
}

/**
 * An artist profile nobody has named yet: it still carries the account
 * placeholder. Listing it gives visitors a card that identifies no one.
 */
export function isUnnamedArtistProfile(artist: { apDisplayName?: string | null }): boolean {
  const name = artist.apDisplayName?.trim() ?? '';
  return name === '' || name === ACCOUNT_PLACEHOLDER_NAME;
}
