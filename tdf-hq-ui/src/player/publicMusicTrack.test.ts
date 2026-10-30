import { publicMusicTrack } from './publicMusicTrack';

it('preserves exact public identities and defers every authorized source to the engine', () => {
  const input = { trackId: 'track-B', recordingId: 'recording-B', releaseId: 'release-B', releaseVersionId: 'version-B',
    title: 'Pista B', displayArtist: 'Artista B', durationMs: 90000, artworkUrl: '/artwork-B', sources: [
      { assetId: 'full-B', role: 'stream_audio' as const, mediaType: 'audio/flac', technicalMetadata: { bitrate_kbps: 0, loudness_lufs: -16 } },
      { assetId: 'preview-B', role: 'preview_audio' as const, mediaType: 'audio/mp4', technicalMetadata: { bitrate_kbps: 96 } },
    ] };
  const track = publicMusicTrack(input);
  expect(track).toEqual({ id: 'track-B', recordingId: 'recording-B', releaseId: 'release-B', releaseVersionId: 'version-B',
    title: 'Pista B', artist: 'Artista B', durationMs: 90000, artworkUrl: '/artwork-B', previewEndMs: 30000, loudnessLufs: -16,
    sources: [{ url: '', assetId: 'full-B', quality: 'lossless', mediaType: 'audio/flac', preview: false },
      { url: '', assetId: 'preview-B', quality: 'low', mediaType: 'audio/mp4', preview: true }] });
  expect(input.sources[0]?.technicalMetadata).toEqual({ bitrate_kbps: 0, loudness_lufs: -16 });
});

it('keeps full-track duration without a preview cap and ignores invalid loudness', () => {
  expect(publicMusicTrack({ trackId: 't', recordingId: 'r', title: 'Long mix', displayArtist: 'DJ', durationMs: 7200000,
    sources: [{ assetId: 'master-derivative', role: 'stream_audio', mediaType: 'audio/mp4', technicalMetadata: { loudness_lufs: Number.NaN, bitrate_kbps: 160 } }] }))
    .toMatchObject({ previewEndMs: null, loudnessLufs: null, durationMs: 7200000, artworkUrl: null,
      sources: [{ url: '', assetId: 'master-derivative', quality: 'medium', preview: false }] });
});

it('preserves unknown duration without inventing a zero-length recording', () => {
  expect(publicMusicTrack({ trackId: 't', recordingId: 'r', title: 'Unknown duration', displayArtist: 'Artist', durationMs: null,
    sources: [] })).toMatchObject({ durationMs: null, sources: [], previewEndMs: null });
});
