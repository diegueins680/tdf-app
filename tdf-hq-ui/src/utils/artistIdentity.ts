/** A social artist record ID is never a substitute for a linked Party ID. */
export function canonicalArtistPartyId(value: unknown): number | null {
  if (typeof value === 'number') return Number.isSafeInteger(value) && value > 0 ? value : null;
  if (typeof value !== 'string' || !/^[1-9][0-9]*$/.test(value)) return null;
  const parsed = Number(value);
  return Number.isSafeInteger(parsed) && parsed > 0 ? parsed : null;
}

export function buildArtistPublicPath(partyId: unknown, slug?: string | null): string | null {
  const id = canonicalArtistPartyId(partyId);
  if (id === null) return null;
  const safeSlug = typeof slug === 'string' && slug.length <= 96 && /^[a-z0-9]+(?:-[a-z0-9]+)*$/.test(slug) ? slug : null;
  return `/a/${safeSlug ?? id}`;
}
