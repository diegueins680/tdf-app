import { ARTIST_GLOBAL_FALLBACK, getArtistHeroImage } from './artistFallbacks';

const QUIMIKA_SOUL_IMAGE = 'https://i.scdn.co/image/ab6761610000e5eb4918df2d6a21f0388e1c092e';

describe('getArtistHeroImage', () => {
  it('resolves both the canonical and legacy Quimika Soul slugs', () => {
    expect(getArtistHeroImage(null, 'quimika-soul')).toBe(QUIMIKA_SOUL_IMAGE);
    expect(getArtistHeroImage(null, 'e-quimika-soul')).toBe(QUIMIKA_SOUL_IMAGE);
  });

  it('keeps persisted artwork authoritative and unknown artists honest', () => {
    expect(getArtistHeroImage(' https://cdn.example.test/artist.webp ', 'quimika-soul'))
      .toBe('https://cdn.example.test/artist.webp');
    expect(getArtistHeroImage(null, 'unknown-artist')).toBe(ARTIST_GLOBAL_FALLBACK);
  });
});
