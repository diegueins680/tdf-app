import {
  normalizeSpotifyEmbed,
  normalizeStreamingSource,
  normalizeYoutubeEmbed,
} from './media';

describe('normalizeYoutubeEmbed', () => {
  it('normalizes valid YouTube URLs', () => {
    expect(normalizeYoutubeEmbed('https://www.youtube.com/watch?v=abc123')).toBe(
      'https://www.youtube.com/embed/abc123',
    );
    expect(normalizeYoutubeEmbed('youtu.be/xyz789')).toBe('https://www.youtube.com/embed/xyz789');
    expect(normalizeYoutubeEmbed('https://youtu.be/xyz789/')).toBe('https://www.youtube.com/embed/xyz789');
    expect(normalizeYoutubeEmbed('https://youtu.be/xyz789/extra-segment')).toBe(
      'https://www.youtube.com/embed/xyz789',
    );
  });

  it('rejects non-YouTube domains that contain youtube.com as a substring', () => {
    expect(normalizeYoutubeEmbed('https://notyoutube.com/watch?v=abc123')).toBeNull();
    expect(normalizeYoutubeEmbed('https://youtube.com.evil.example/watch?v=abc123')).toBeNull();
  });

  it('rejects YouTube pages that are not directly embeddable videos', () => {
    expect(normalizeYoutubeEmbed('https://www.youtube.com/playlist?list=PL123')).toBeNull();
    expect(normalizeYoutubeEmbed('https://www.youtube.com/channel/UC123')).toBeNull();
    expect(normalizeYoutubeEmbed('https://www.youtube.com/@artist')).toBeNull();
    expect(normalizeYoutubeEmbed('https://www.youtube.com/watch')).toBeNull();
  });
});

describe('normalizeSpotifyEmbed', () => {
  it('normalizes valid Spotify URLs', () => {
    expect(normalizeSpotifyEmbed('https://open.spotify.com/track/123')).toBe(
      'https://open.spotify.com/embed/track/123',
    );
    expect(normalizeSpotifyEmbed('https://open.spotify.com/embed/track/123')).toBe(
      'https://open.spotify.com/embed/track/123',
    );
  });

  it('normalizes locale-prefixed Spotify URLs to canonical embed paths', () => {
    expect(normalizeSpotifyEmbed('https://open.spotify.com/intl-es/track/123?si=abc')).toBe(
      'https://open.spotify.com/embed/track/123?si=abc',
    );
  });

  it('normalizes Spotify resource type casing to canonical lowercase', () => {
    expect(normalizeSpotifyEmbed('https://open.spotify.com/TRACK/123')).toBe(
      'https://open.spotify.com/embed/track/123',
    );
  });

  it('forces https when normalizing Spotify embeds', () => {
    expect(normalizeSpotifyEmbed('http://open.spotify.com/track/123')).toBe(
      'https://open.spotify.com/embed/track/123',
    );
    expect(normalizeSpotifyEmbed('javascript://open.spotify.com/track/123')).toBe(
      'https://open.spotify.com/embed/track/123',
    );
  });

  it('rejects non-Spotify domains that contain spotify.com as a substring', () => {
    expect(normalizeSpotifyEmbed('https://notspotify.com/track/123')).toBeNull();
    expect(normalizeSpotifyEmbed('https://spotify.com.evil.example/track/123')).toBeNull();
  });

  it('rejects Spotify URLs that do not point to a concrete media resource', () => {
    expect(normalizeSpotifyEmbed('https://open.spotify.com')).toBeNull();
    expect(normalizeSpotifyEmbed('https://open.spotify.com/embed')).toBeNull();
    expect(normalizeSpotifyEmbed('https://open.spotify.com/track')).toBeNull();
    expect(normalizeSpotifyEmbed('https://open.spotify.com/user/test')).toBeNull();
    expect(normalizeSpotifyEmbed('https://open.spotify.com/track/123/extra')).toBeNull();
  });
});

describe('normalizeStreamingSource', () => {
  it('does not infer youtube/spotify provider from unrelated hostnames', () => {
    expect(normalizeStreamingSource({ url: 'https://notyoutube.com/watch?v=abc123' })).toEqual({
      url: 'https://notyoutube.com/watch?v=abc123',
      provider: 'audio',
      label: 'Audio',
      mimeType: undefined,
      posterUrl: undefined,
    });

    expect(normalizeStreamingSource({ url: 'https://notspotify.com/track/123' })).toEqual({
      url: 'https://notspotify.com/track/123',
      provider: 'audio',
      label: 'Audio',
      mimeType: undefined,
      posterUrl: undefined,
    });
  });

  it('infers media provider from URL path extension when query/hash is present', () => {
    expect(
      normalizeStreamingSource({
        url: 'https://cdn.example.com/media/track.mp3?download=1#player',
      }),
    ).toEqual({
      url: 'https://cdn.example.com/media/track.mp3?download=1#player',
      provider: 'audio',
      label: 'Audio',
      mimeType: undefined,
      posterUrl: undefined,
    });

    expect(
      normalizeStreamingSource({
        url: 'https://cdn.example.com/media/live-set.webm?quality=hd',
      }),
    ).toEqual({
      url: 'https://cdn.example.com/media/live-set.webm?quality=hd',
      provider: 'video',
      label: 'Video',
      mimeType: undefined,
      posterUrl: undefined,
    });
  });

  it('infers media provider from bare filenames without a URL scheme', () => {
    expect(normalizeStreamingSource({ url: 'showreel.MP4' })).toEqual({
      url: 'showreel.MP4',
      provider: 'video',
      label: 'Video',
      mimeType: undefined,
      posterUrl: undefined,
    });

    expect(normalizeStreamingSource({ url: 'teaser.webm?autoplay=1' })).toEqual({
      url: 'teaser.webm?autoplay=1',
      provider: 'video',
      label: 'Video',
      mimeType: undefined,
      posterUrl: undefined,
    });
  });
});
