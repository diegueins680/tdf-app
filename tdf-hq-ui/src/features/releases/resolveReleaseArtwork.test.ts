import type { RecordsReleaseDTO } from '../../api/records';
import { getReleaseArtworkSources } from './resolveReleaseArtwork';

const release = {
  arReleaseId: 1, arArtistId: 2, arTitle: 'Pirotécnicos Juegos', artistName: 'HNO',
  arCoverImageUrl: null, arSpotifyUrl: null, arYoutubeUrl: null,
};
const catalogRelease: RecordsReleaseDTO = {
  id: 'record', code: 'record', title: 'Pirotécnicos Juegos )', releaseTypeId: 'single',
  sortOrder: 1, revision: 1,
  contributors: [{ id: 'credit', code: 'hno', kind: 'credited-ensemble', name: 'HNO, Lil Weed (Per-versos)' }],
  resources: [{
    id: 'resource', kind: 'audio-track', providerCode: 'spotify', externalCode: 'track-one',
    url: 'https://open.spotify.com/track/track-one', thumbnailUrl: 'https://cdn.example.com/cover.jpg',
    relationKind: 'primary-audio', primary: true, sortOrder: 0,
  }],
};
const cover = 'https://cdn.example.com/cover.jpg';

describe('release artwork sources', () => {
  it('recovers missing legacy artwork from a unique title and credited artist, tolerating punctuation', () => {
    expect(getReleaseArtworkSources(release, [catalogRelease])).toEqual([cover]);
  });

  it('keeps custom artwork first and the artist photo last, trimming empty values', () => {
    expect(getReleaseArtworkSources({ ...release, arCoverImageUrl: ' custom.jpg ', artistHeroImageUrl: 'artist.jpg' }, [catalogRelease]))
      .toEqual(['custom.jpg', cover, 'artist.jpg']);
    expect(getReleaseArtworkSources({ ...release, arCoverImageUrl: '  ' }, [catalogRelease])).toEqual([cover]);
  });

  it('does not match an identically titled recording by a different artist', () => {
    expect(getReleaseArtworkSources({ ...release, artistName: 'HN' }, [catalogRelease])).toEqual([]);
  });

  it('does not guess between different versions of the same title and artist', () => {
    expect(getReleaseArtworkSources(release, [catalogRelease, { ...catalogRelease, id: 'other-version' }])).toEqual([]);
  });

  it('uses platform identity across localized links and renamed titles', () => {
    expect(getReleaseArtworkSources({ ...release, arTitle: 'Renamed', arSpotifyUrl: 'https://open.spotify.com/intl-es/track/track-one?si=test' }, [catalogRelease]))
      .toEqual([cover]);
  });

  it('does not substitute artwork when an existing platform identity does not match', () => {
    expect(getReleaseArtworkSources({ ...release, arSpotifyUrl: 'https://open.spotify.com/track/other' }, [catalogRelease])).toEqual([]);
  });

  it('does not guess artwork when a populated platform link cannot be parsed', () => {
    expect(getReleaseArtworkSources({ ...release, arSpotifyUrl: 'spotify:track:other' }, [catalogRelease])).toEqual([]);
    expect(getReleaseArtworkSources({ ...release, arYoutubeUrl: 'unrecognized-video-link' }, [catalogRelease])).toEqual([]);
  });

  it('allows the legacy fallback when platform fields contain only whitespace', () => {
    expect(getReleaseArtworkSources({ ...release, arSpotifyUrl: '  ', arYoutubeUrl: '\t' }, [catalogRelease])).toEqual([cover]);
  });

  it('works when the optional catalog is unavailable and removes duplicate URLs', () => {
    expect(getReleaseArtworkSources({ ...release, artistHeroImageUrl: cover }, [])).toEqual([cover]);
    expect(getReleaseArtworkSources({ ...release, arCoverImageUrl: cover, artistHeroImageUrl: cover }, [catalogRelease])).toEqual([cover]);
  });
});
