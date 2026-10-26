import { jest } from '@jest/globals';
import type { ArtistReleaseDTO } from '../../api/types';
import {
  buildReleaseStreamUpdatePayload,
  dispatchReleaseToRadio,
  getReleasePlaybackUrls,
  resolveReleaseAudioUrl,
  resolveReleasePlaybackAction,
} from './ReleasePlayerActions';

const FIXTURE_ARTIST_ID = 7;
const FIXTURE_RELEASE_ID = 42;
const UNSAVED_ENTITY_ID = 0;
const FIXTURE_RELEASE_TITLE = 'Luna Baja';
const FIXTURE_RELEASE_DATE = '2026-07-01';
const FIXTURE_RELEASE_DESCRIPTION = 'Single';
const FIXTURE_COVER_IMAGE_URL = 'https://cdn.example.com/luna.jpg';
const DIRECT_AUDIO_URL = 'https://cdn.example.com/audio.mp3';
const PADDED_DIRECT_AUDIO_URL = `  ${DIRECT_AUDIO_URL}  `;
const UPLOADED_AUDIO_URL = 'https://drive.example.com/audio.mp3';
const YOUTUBE_WATCH_URL = 'https://youtube.com/watch?v=abc';
const YOUTUBE_SHORT_URL = 'https://youtu.be/video-id';
const SPOTIFY_ALBUM_URL = 'https://open.spotify.com/album/luna-baja';
const RELEASE_SPOTIFY_TRACK_URL = 'https://open.spotify.com/track/release';
const ARTIST_SPOTIFY_FALLBACK_URL = 'https://open.spotify.com/artist/fallback';
const ARTIST_YOUTUBE_FALLBACK_URL = 'https://youtube.com/channel/fallback';
const RELEASE_STATION_ID = `release-${FIXTURE_RELEASE_ID}`;
const UPLOADED_AUDIO_BY_RELEASE_ID: Record<number, string> = {
  [FIXTURE_RELEASE_ID]: UPLOADED_AUDIO_URL,
};

const release = (overrides: Partial<ArtistReleaseDTO> = {}): ArtistReleaseDTO => ({
  arArtistId: FIXTURE_ARTIST_ID,
  arReleaseId: FIXTURE_RELEASE_ID,
  arTitle: FIXTURE_RELEASE_TITLE,
  arReleaseDate: FIXTURE_RELEASE_DATE,
  arDescription: FIXTURE_RELEASE_DESCRIPTION,
  arCoverImageUrl: FIXTURE_COVER_IMAGE_URL,
  arSpotifyUrl: null,
  arYoutubeUrl: null,
  ...overrides,
});

describe('buildReleaseStreamUpdatePayload', () => {
  it('stores non-YouTube streams as Spotify/direct playback URLs without dropping existing YouTube links', () => {
    const directStreamPayload = buildReleaseStreamUpdatePayload(
      release({ arYoutubeUrl: YOUTUBE_WATCH_URL }),
      PADDED_DIRECT_AUDIO_URL,
    );

    expect(directStreamPayload).toMatchObject({
      aruArtistId: FIXTURE_ARTIST_ID,
      aruTitle: FIXTURE_RELEASE_TITLE,
      aruSpotifyUrl: DIRECT_AUDIO_URL,
      aruYoutubeUrl: YOUTUBE_WATCH_URL,
    });
  });

  it('stores YouTube streams in the YouTube URL field and preserves existing Spotify links', () => {
    const youtubeStreamPayload = buildReleaseStreamUpdatePayload(
      release({ arSpotifyUrl: SPOTIFY_ALBUM_URL }),
      YOUTUBE_SHORT_URL,
    );

    expect(youtubeStreamPayload.aruSpotifyUrl).toBe(SPOTIFY_ALBUM_URL);
    expect(youtubeStreamPayload.aruYoutubeUrl).toBe(YOUTUBE_SHORT_URL);
  });

  it('rejects blank stream URLs before building an update payload', () => {
    expect(() => buildReleaseStreamUpdatePayload(release(), '   ')).toThrow('streamUrl must be non-empty');
  });

  it('requires the release to belong to a persisted artist', () => {
    expect(() =>
      buildReleaseStreamUpdatePayload(release({ arArtistId: UNSAVED_ENTITY_ID }), DIRECT_AUDIO_URL),
    ).toThrow('release.arArtistId must be a positive safe integer');
  });
});

describe('release playback URL resolution', () => {
  it('prefers cached uploaded audio over release and artist fallback links', () => {
    const audioUrl = resolveReleaseAudioUrl(
      release({ arSpotifyUrl: RELEASE_SPOTIFY_TRACK_URL }),
      UPLOADED_AUDIO_BY_RELEASE_ID,
      { spotify: ARTIST_SPOTIFY_FALLBACK_URL },
    );

    expect(audioUrl).toBe(UPLOADED_AUDIO_URL);
  });

  it('returns playback button URLs with cached audio mapped to the primary stream slot', () => {
    const urls = getReleasePlaybackUrls(
      release({ arSpotifyUrl: RELEASE_SPOTIFY_TRACK_URL }),
      UPLOADED_AUDIO_BY_RELEASE_ID,
      { youtube: ARTIST_YOUTUBE_FALLBACK_URL },
    );

    expect(urls).toEqual({
      spotifyUrl: UPLOADED_AUDIO_URL,
      youtubeUrl: ARTIST_YOUTUBE_FALLBACK_URL,
    });
  });

  it('requires a persisted release ID before resolving playback URLs', () => {
    const unsavedRelease = release({ arReleaseId: UNSAVED_ENTITY_ID });

    expect(() => resolveReleaseAudioUrl(unsavedRelease, {})).toThrow(
      'release.arReleaseId must be a positive safe integer',
    );
    expect(() => getReleasePlaybackUrls(unsavedRelease, {})).toThrow(
      'release.arReleaseId must be a positive safe integer',
    );
  });
});

describe('dispatchReleaseToRadio', () => {
  it('emits the radio load event with release metadata', () => {
    const dispatchEvent = jest.fn();

    dispatchReleaseToRadio(release(), PADDED_DIRECT_AUDIO_URL, { dispatchEvent });

    expect(dispatchEvent).toHaveBeenCalledTimes(1);
    const event = dispatchEvent.mock.calls[0]?.[0] as CustomEvent;
    expect(event.type).toBe('tdf-player-load-track');
    expect(event.detail).toEqual({
      track: {
        id: RELEASE_STATION_ID.replace('release-', 'legacy-release-'),
        releaseId: RELEASE_STATION_ID.replace('release-', 'legacy-release-'),
        title: FIXTURE_RELEASE_TITLE,
        artist: 'Artista TDF',
        artworkUrl: FIXTURE_COVER_IMAGE_URL,
        sources: [{ url: DIRECT_AUDIO_URL, quality: 'high', mediaType: 'audio/mpeg' }],
      },
      autoplay: true,
    });
  });

  it('requires release identity and stream URL preconditions before dispatching', () => {
    expect(() => dispatchReleaseToRadio(release({ arTitle: '   ' }), DIRECT_AUDIO_URL)).toThrow(
      'release.arTitle must be non-empty',
    );
    expect(() => dispatchReleaseToRadio(release(), '   ')).toThrow('streamUrl must be non-empty');
  });
});

// Regression: a provider page is HTML, not a playable native media resource.
describe('legacy native dispatch boundary', () => {
  it.each([
    'https://open.spotify.com/track/123',
    'https://www.youtube.com/watch?v=dQw4w9WgXcQ',
    'https://youtu.be/dQw4w9WgXcQ',
    'https://music.youtube.com/watch?v=dQw4w9WgXcQ',
    'https://youtube.com/channel/artist-profile',
    'https://drive.google.com/file/d/synthetic-file/view',
    'javascript:alert(1)',
    'https://open.spotify.com@evil.invalid/track/123',
  ])('never dispatches a native media command for %s', (url) => {
    const dispatchEvent = jest.fn();
    expect(() => dispatchReleaseToRadio(release(), url, { dispatchEvent })).toThrow();
    expect(dispatchEvent).not.toHaveBeenCalled();
  });

  it('keeps the selected release artist attribution in its native command', () => {
    const dispatchEvent = jest.fn();
    dispatchReleaseToRadio({ ...release(), artistName: 'Sofia Marquez' }, DIRECT_AUDIO_URL, { dispatchEvent });
    expect((dispatchEvent.mock.calls[0]?.[0] as CustomEvent).detail.track.artist).toBe('Sofia Marquez');
  });
});


describe('contextual release playback actions', () => {
  it.each([
    ['https://open.spotify.com/intl-es/track/123?si=fixture', 'Spotify'],
    ['https://youtube.com./watch?v=dQw4w9WgXcQ', 'YouTube'],
    ['https://spotify.link/synthetic', 'Spotify'],
    ['https://drive.google.com/file/d/synthetic/view?resourcekey=example', 'Google Drive'],
  ])('keeps provider page %s outside the native engine', (url, provider) => {
    expect(resolveReleasePlaybackAction(release({ arSpotifyUrl: url }), {})).toEqual({ kind: 'provider', context: 'release', provider, url });
  });

  it.each([
    'https://media.example.com/audio?X-Amz-Signature=synthetic%2Fsignature&expires=123',
    'https://drive.google.com/uc?export=download&id=synthetic&resourcekey=example',
    '/uploads/authorized-file?token=synthetic',
  ])('preserves direct media URL bytes and its stable selection: %s', (url) => {
    expect(resolveReleasePlaybackAction(release({ arSpotifyUrl: RELEASE_SPOTIFY_TRACK_URL }), { 42: url })).toEqual({ kind: 'native', url });
    const dispatchEvent = jest.fn();
    dispatchReleaseToRadio(release(), url, { dispatchEvent });
    expect((dispatchEvent.mock.calls[0]?.[0] as CustomEvent).detail.track).toMatchObject({ id: 'legacy-release-42', sources: [{ url, quality: 'high', mediaType: 'audio/mpeg' }] });
  });

  it('keeps a generic artist provider profile contextual instead of selecting it as a release', () => {
    expect(resolveReleasePlaybackAction(release(), {}, { spotify: ARTIST_SPOTIFY_FALLBACK_URL })).toEqual({
      kind: 'provider', context: 'artist', provider: 'Spotify', url: ARTIST_SPOTIFY_FALLBACK_URL,
    });
    expect(resolveReleasePlaybackAction(release(), {}, { spotify: DIRECT_AUDIO_URL })).toBeNull();
    expect(getReleasePlaybackUrls(release(), {}, { spotify: DIRECT_AUDIO_URL })).toEqual({ spotifyUrl: null, youtubeUrl: null });
  });

  it.each(['javascript:alert(1)', 'data:text/html,synthetic', 'https://open.spotify.com@evil.invalid/track/id'])('rejects unsafe playback and persisted source %s', (url) => {
    expect(resolveReleasePlaybackAction(release({ arSpotifyUrl: url }), {})).toBeNull();
    expect(getReleasePlaybackUrls(release({ arSpotifyUrl: url, arYoutubeUrl: url }), {})).toEqual({ spotifyUrl: null, youtubeUrl: null });
    expect(() => buildReleaseStreamUpdatePayload(release(), url)).toThrow();
  });

  it('does not confer provider identity on spoofed hosts or query text', () => {
    for (const url of ['https://spotify.com.evil.invalid/audio', 'https://notyoutube.com/audio?youtube.com=1']) {
      expect(resolveReleasePlaybackAction(release({ arSpotifyUrl: url }), {})).toEqual({ kind: 'native', url });
      expect(buildReleaseStreamUpdatePayload(release(), url).aruYoutubeUrl).toBeNull();
    }
  });
});
