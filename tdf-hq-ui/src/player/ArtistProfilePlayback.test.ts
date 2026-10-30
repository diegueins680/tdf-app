import type { ArtistReleaseDTO } from '../api/types';
import type { ArtistFeaturedTrack, MusicPublicRelease } from '../api/musicReleases';
import { artistTracks, legacyArtistTracks, prioritizeFeaturedTrack } from './ArtistProfilePlayback';

const release = (artistPartyId: number, recordingId: string, trackNumber: number) => ({
  id: `release-${artistPartyId}`, versionId: 'version', artistPartyId,
  tracks: [{ trackId: `track-${recordingId}`, recordingId, discNumber: 1, trackNumber,
    title: recordingId, displayArtist: 'Artist', durationMs: 1000,
    sources: [{ assetId: `asset-${recordingId}`, role: 'stream_audio', mediaType: 'audio/mp4', technicalMetadata: {} }] }],
}) as MusicPublicRelease;

it('rejects unrelated artists and deduplicates recordings across releases', () => {
  const tracks = artistTracks([release(1, 'a', 1), release(2, 'b', 1), release(1, 'a', 1)], 1);
  expect(tracks.map((track) => track.recordingId)).toEqual(['a']);
  expect(tracks[0]?.sources[0]?.assetId).toBe('asset-a');
  expect(tracks[0]?.sources[0]?.url).toBe('');
});

it('orders disc/track numbers and excludes tracks without sources', () => {
  const value = release(1, 'b', 2);
  value.tracks.push(release(1, 'a', 1).tracks[0]!);
  value.tracks.push({ ...release(1, 'empty', 0).tracks[0]!, sources: [] });
  expect(artistTracks([value], 1).map((track) => track.recordingId)).toEqual(['a', 'b']);
});


it('uses only direct public legacy media from the linked artist, preserving its source identity', () => {
  const legacy = (id: number, artist: number, url: string) => ({ arReleaseId: id, arArtistId: artist,
    arTitle: 'Legacy', arSpotifyUrl: url }) as ArtistReleaseDTO;
  const tracks = legacyArtistTracks([
    legacy(1, 17, 'https://media.invalid/song.mp3'), legacy(2, 18, 'https://media.invalid/other.mp3'),
    legacy(3, 17, 'https://open.spotify.com/track/reference'), legacy(4, 17, 'javascript:alert(1)'),
  ], 17, 'Linked artist');
  expect(tracks).toHaveLength(1);
  expect(tracks[0]).toMatchObject({ id: 'legacy-release-1', artist: 'Linked artist',
    sources: [{ url: 'https://media.invalid/song.mp3' }] });
});


it('moves the matching featured recording first while preserving every other track in order', () => {
  const tracks = artistTracks([release(17, 'a', 1), release(17, 'b', 1), release(17, 'c', 1)], 17);
  const selected = { artistPartyId: 17, recordingId: 'b', legacyReleaseId: null, available: true, updatedAt: null };
  expect(prioritizeFeaturedTrack(tracks, 17, selected).map((track) => track.recordingId)).toEqual(['b', 'a', 'c']);
  for (const invalid of [{ ...selected, artistPartyId: 18 }, { ...selected, available: false }, { ...selected, recordingId: 'retired' }]) {
    expect(prioritizeFeaturedTrack(tracks, 17, invalid)).toBe(tracks);
  }
});

it('can prioritize a direct legacy release without confusing its ID with a canonical recording', () => {
  const tracks = [{ id: 'canonical', recordingId: '5', title: 'Canonical', artist: 'Artist', sources: [] },
    { id: 'legacy-release-5', title: 'Legacy', artist: 'Artist', sources: [] }];
  const selected: ArtistFeaturedTrack = { artistPartyId: 17, recordingId: null, legacyReleaseId: 5, available: true, updatedAt: null };
  expect(prioritizeFeaturedTrack(tracks, 17, selected).map((track) => track.id)).toEqual(['legacy-release-5', 'canonical']);
});
