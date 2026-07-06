import { jest } from '@jest/globals';
import type { ArtistReleaseDTO } from '../../api/types';
import {
  buildReleaseStreamUpdatePayload,
  dispatchReleaseToRadio,
  getReleasePlaybackUrls,
  resolveReleaseAudioUrl,
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
    expect(event.type).toBe('tdf-radio-load-stream');
    expect(event.detail).toEqual({
      streamUrl: DIRECT_AUDIO_URL,
      stationName: FIXTURE_RELEASE_TITLE,
      stationId: RELEASE_STATION_ID,
    });
  });

  it('requires release identity and stream URL preconditions before dispatching', () => {
    expect(() => dispatchReleaseToRadio(release({ arTitle: '   ' }), DIRECT_AUDIO_URL)).toThrow(
      'release.arTitle must be non-empty',
    );
    expect(() => dispatchReleaseToRadio(release(), '   ')).toThrow('streamUrl must be non-empty');
  });
});
