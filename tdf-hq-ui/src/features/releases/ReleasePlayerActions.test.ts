import type { ArtistReleaseDTO } from '../../api/types';
import {
  buildReleaseStreamUpdatePayload,
  dispatchReleaseToRadio,
  getReleasePlaybackUrls,
  resolveReleaseAudioUrl,
} from './ReleasePlayerActions';

const release = (overrides: Partial<ArtistReleaseDTO> = {}): ArtistReleaseDTO => ({
  arArtistId: 7,
  arReleaseId: 42,
  arTitle: 'Luna Baja',
  arReleaseDate: '2026-07-01',
  arDescription: 'Single',
  arCoverImageUrl: 'https://cdn.example.com/luna.jpg',
  arSpotifyUrl: null,
  arYoutubeUrl: null,
  ...overrides,
});

describe('buildReleaseStreamUpdatePayload', () => {
  it('stores non-YouTube streams as Spotify/direct playback URLs without dropping existing YouTube links', () => {
    const payload = buildReleaseStreamUpdatePayload(
      release({ arYoutubeUrl: 'https://youtube.com/watch?v=abc' }),
      '  https://cdn.example.com/audio.mp3  ',
    );

    expect(payload).toMatchObject({
      aruArtistId: 7,
      aruTitle: 'Luna Baja',
      aruSpotifyUrl: 'https://cdn.example.com/audio.mp3',
      aruYoutubeUrl: 'https://youtube.com/watch?v=abc',
    });
  });

  it('stores YouTube streams in the YouTube URL field and preserves existing Spotify links', () => {
    const payload = buildReleaseStreamUpdatePayload(
      release({ arSpotifyUrl: 'https://open.spotify.com/album/123' }),
      'https://youtu.be/video-id',
    );

    expect(payload.aruSpotifyUrl).toBe('https://open.spotify.com/album/123');
    expect(payload.aruYoutubeUrl).toBe('https://youtu.be/video-id');
  });
});

describe('release playback URL resolution', () => {
  it('prefers cached uploaded audio over release and artist fallback links', () => {
    const audioUrl = resolveReleaseAudioUrl(
      release({ arSpotifyUrl: 'https://open.spotify.com/track/release' }),
      { 42: 'https://drive.example.com/audio.mp3' },
      { spotify: 'https://open.spotify.com/artist/fallback' },
    );

    expect(audioUrl).toBe('https://drive.example.com/audio.mp3');
  });

  it('returns playback button URLs with cached audio mapped to the primary stream slot', () => {
    const urls = getReleasePlaybackUrls(
      release({ arSpotifyUrl: 'https://open.spotify.com/track/release' }),
      { 42: 'https://drive.example.com/audio.mp3' },
      { youtube: 'https://youtube.com/channel/fallback' },
    );

    expect(urls).toEqual({
      spotifyUrl: 'https://drive.example.com/audio.mp3',
      youtubeUrl: 'https://youtube.com/channel/fallback',
    });
  });
});

describe('dispatchReleaseToRadio', () => {
  it('emits the radio load event with release metadata', () => {
    const dispatchEvent = jest.fn();

    dispatchReleaseToRadio(release(), 'https://cdn.example.com/audio.mp3', { dispatchEvent });

    expect(dispatchEvent).toHaveBeenCalledTimes(1);
    const event = dispatchEvent.mock.calls[0]?.[0] as CustomEvent;
    expect(event.type).toBe('tdf-radio-load-stream');
    expect(event.detail).toEqual({
      streamUrl: 'https://cdn.example.com/audio.mp3',
      stationName: 'Luna Baja',
      stationId: 'release-42',
    });
  });
});
