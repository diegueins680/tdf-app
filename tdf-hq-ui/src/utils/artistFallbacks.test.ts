import { ARTIST_GLOBAL_FALLBACK, getArtistHeroImage } from './artistFallbacks';

describe('getArtistHeroImage', () => {
  it('uses the verified Quimika Soul artwork for its canonical seeded slug', () => {
    expect(getArtistHeroImage(null, 'quimika-soul')).toBe(
      'https://i.scdn.co/image/ab6761610000e5eb4918df2d6a21f0388e1c092e',
    );
  });

  it('keeps the global fallback for unknown legacy slugs', () => {
    expect(getArtistHeroImage(null, 'unknown-artist')).toBe(ARTIST_GLOBAL_FALLBACK);
  });
});
