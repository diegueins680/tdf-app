import {
  normalizeSpotifyEmbed,
  normalizeStreamingSource,
  normalizeYoutubeEmbed,
} from './media';

describe('normalizeYoutubeEmbed', () => {
  it('normalizes valid YouTube URLs', () => {
    expect(normalizeYoutubeEmbed('https://www.youtube.com/watch?v=dQw4w9WgXcQ')).toBe(
      'https://www.youtube.com/embed/dQw4w9WgXcQ',
    );
    expect(normalizeYoutubeEmbed('youtu.be/M7lc1UVf-VE')).toBe('https://www.youtube.com/embed/M7lc1UVf-VE');
    expect(normalizeYoutubeEmbed('https://youtu.be/M7lc1UVf-VE/')).toBe(
      'https://www.youtube.com/embed/M7lc1UVf-VE',
    );
    expect(normalizeYoutubeEmbed('https://youtu.be/M7lc1UVf-VE/extra-segment')).toBe(
      'https://www.youtube.com/embed/M7lc1UVf-VE',
    );
  });

  it('rejects non-YouTube domains that contain youtube.com as a substring', () => {
    expect(normalizeYoutubeEmbed('https://notyoutube.com/watch?v=dQw4w9WgXcQ')).toBeNull();
    expect(normalizeYoutubeEmbed('https://youtube.com.evil.example/watch?v=dQw4w9WgXcQ')).toBeNull();
  });

  it('rejects YouTube pages that are not directly embeddable videos', () => {
    expect(normalizeYoutubeEmbed('https://www.youtube.com/playlist?list=PL123')).toBeNull();
    expect(normalizeYoutubeEmbed('https://www.youtube.com/channel/UC123')).toBeNull();
    expect(normalizeYoutubeEmbed('https://www.youtube.com/@artist')).toBeNull();
    expect(normalizeYoutubeEmbed('https://www.youtube.com/watch')).toBeNull();
  });

  it('rejects malformed YouTube video identifiers', () => {
    expect(normalizeYoutubeEmbed('https://www.youtube.com/watch?v=dQw4w9WgXcQ<script>')).toBeNull();
    expect(normalizeYoutubeEmbed('https://youtu.be/dQw4w9WgXcQ%2Fdef')).toBeNull();
    expect(normalizeYoutubeEmbed('https://www.youtube.com/embed/dQw4w9WgXcQ%20bad')).toBeNull();
    expect(normalizeYoutubeEmbed('https://www.youtube.com/watch?v=short123')).toBeNull();
  });

  it('rejects non-http(s) schemes even when hostname looks like YouTube', () => {
    expect(normalizeYoutubeEmbed('javascript://www.youtube.com/watch?v=dQw4w9WgXcQ')).toBeNull();
    expect(normalizeYoutubeEmbed('ftp://www.youtube.com/watch?v=dQw4w9WgXcQ')).toBeNull();
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
  });

  it('rejects non-http(s) schemes even when hostname looks like Spotify', () => {
    expect(normalizeSpotifyEmbed('javascript://open.spotify.com/track/123')).toBeNull();
    expect(normalizeSpotifyEmbed('ftp://open.spotify.com/track/123')).toBeNull();
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
    expect(normalizeStreamingSource({ url: 'https://notyoutube.com/watch?v=dQw4w9WgXcQ' })).toEqual({
      url: 'https://notyoutube.com/watch?v=dQw4w9WgXcQ',
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

  it('rejects unsupported URL schemes for generic streams', () => {
    expect(normalizeStreamingSource({ url: 'javascript:alert(1)' })).toBeNull();
    expect(normalizeStreamingSource({ url: 'mailto:test@example.com' })).toBeNull();
  });
});
