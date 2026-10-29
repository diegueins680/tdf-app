import type { MusicPublicRelease } from '../api/musicReleases';
import { artistTracks } from './ArtistProfilePlayback';

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
