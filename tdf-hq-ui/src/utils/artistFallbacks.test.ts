import { artistInitials, artistPlaceholderImage, getArtistHeroImage, isArtistPlaceholderImage } from './artistFallbacks';

describe('getArtistHeroImage', () => {
  it('ignores legacy page URLs in image fields and keeps the verified artwork', () => {
    expect(getArtistHeroImage('https://open.spotify.com/artist/6348Nu8zc4gZC8vtHzPP8R', 'skankafe', 'Skanka Fe')).toBe(
      'https://i.scdn.co/image/ab6761610000e5eb172b1792c8a9096500cbceb0',
    );
    expect(getArtistHeroImage('https://drive.google.com/file/d/cover123/view', 'new-artist', 'New Artist')).toBe(
      'https://drive.google.com/thumbnail?id=cover123&sz=w1600',
    );
  });
  it('uses the verified Quimika Soul artwork for its canonical seeded slug', () => {
    expect(getArtistHeroImage(null, 'quimika-soul')).toBe(
      'https://i.scdn.co/image/ab6761610000e5eb4918df2d6a21f0388e1c092e',
    );
  });

  it('preserves the same artwork for the legacy production slug', () => {
    expect(getArtistHeroImage(null, 'e-quimika-soul')).toBe(
      'https://i.scdn.co/image/ab6761610000e5eb4918df2d6a21f0388e1c092e',
    );
  });

  it('uses stable, named initials instead of a shared stock photo', () => {
    const image = getArtistHeroImage(null, 'unknown-artist', 'Diego Saá');
    expect(image).toBe(artistPlaceholderImage('Diego Saá'));
    expect(decodeURIComponent(image)).toContain('>DS</text>');
    expect(isArtistPlaceholderImage(image)).toBe(true);
    expect(getArtistHeroImage('/artist-fallback.svg', null, 'Diego Saá')).toBe(image);
  });

  it.each([['Roj(o)', 'RO'], ['Érica Pérez', 'ÉP'], ['E\u0301rica Pe\u0301rez', 'ÉP'], ['李 明', '李明'], ['', '♪'], ['<script>alert(1)</script>', 'SS']])('handles the Unicode initials of %s', (name, expected) => {
    expect(artistInitials(name)).toBe(expected);
    const svg = decodeURIComponent(artistPlaceholderImage(name));
    expect(svg).not.toContain('<script>');
  });

  it('preserves artist-provided images', () => {
    expect(getArtistHeroImage(' https://cdn.example/owned.jpg ', null, 'Artist')).toBe('https://cdn.example/owned.jpg');
    expect(isArtistPlaceholderImage('https://cdn.example/owned.jpg')).toBe(false);
  });
});
