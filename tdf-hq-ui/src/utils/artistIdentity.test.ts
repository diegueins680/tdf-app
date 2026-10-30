import { buildArtistPublicPath, canonicalArtistPartyId } from './artistIdentity';
it.each([1, '1', Number.MAX_SAFE_INTEGER, String(Number.MAX_SAFE_INTEGER)])('accepts an exact Party identity %s', (id) => {
  expect(canonicalArtistPartyId(id)).toBe(Number(id));
});
it.each([null, undefined, {}, '01', ' 1', '+1', '1.0', '1e2', '1/2', '', 0, -1, 1.1, Infinity,
  Number.MAX_SAFE_INTEGER + 1, '9007199254740993'])('rejects unsafe or ambiguous Party identity %s', (id) => {
  expect(buildArtistPublicPath(id)).toBeNull();
});
it('uses the canonical profile slug when valid, otherwise the exact Party ID', () => {
  expect(buildArtistPublicPath('55', 'artist-name')).toBe('/a/artist-name');
  expect(buildArtistPublicPath('55', '//other.test')).toBe('/a/55');
  expect(buildArtistPublicPath('55', '../login')).toBe('/a/55');
});
