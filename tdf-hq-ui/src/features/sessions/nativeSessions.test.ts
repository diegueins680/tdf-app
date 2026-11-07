import { nativeArtworkForUrl, nativeSessionsFromFeed, readNativeSession, sessionPlayerTrack } from './nativeSessions';
import type { RecordsFeedDTO, RecordsResourceDTO } from '../../api/records';
import { EMPTY_QUEUE, nextQueueIndex, queueReducer } from '../../player/queue';
const entity = { id: 'session-id', title: 'Synthetic session' };
const metadata = () => ({ schemaVersion: 1, kind: 'session', artistPartyIds: [535], artist: 'Synthetic', state: 'ready', durationMs: 30000,
  sources: [{ url: 'https://api.tdfrecords.net/assets/serve/synthetic/video.mp4', mediaType: 'video/mp4', quality: 'high' },
    { url: 'https://api.tdfrecords.net/assets/serve/synthetic/audio.m4a', mediaType: 'audio/mp4', quality: 'high' }],
  chapters: [{ id: 'song-id', title: 'Song', startMs: 10000, endMs: 20000 }], credits: [] });
const resource = (data = metadata()) => ({ providerCode: 'tdf', providerMetadata: { tdfNative: data } }) as unknown as RecordsResourceDTO;
it('uses exact artist identity, deduplicates one publication and preserves distinct canonical kinds', () => {
  const item = { ...entity, resources: [resource()] };
  const feed = { sessions: [item, item], recordings: [item], releases: [item] } as unknown as RecordsFeedDTO;
  expect(nativeSessionsFromFeed(feed, 535).map(session => session.publication)).toEqual([
    { kind: 'recording_session', entityKey: entity.id }, { kind: 'recording', entityKey: entity.id },
    { kind: 'record_release', entityKey: entity.id },
  ]);
  expect(nativeSessionsFromFeed(feed, 520)).toEqual([]);
});
it('builds chapter playback with the exact source offsets, relative duration and stable identities', () => {
  const session = readNativeSession(resource(), entity, 'recording_session')!;
  expect(sessionPlayerTrack(session, session.chapters[0])).toMatchObject({ id: 'records:recording_session:session-id:chapter:song-id', recordingId: 'song-id', startMs: 10000, endMs: 20000, durationMs: 10000 });
});
it('refuses malformed, overlapping and out-of-source chapter boundaries', () => {
  const data = metadata();
  data.chapters.push({ id: 'overlap', title: 'Other', startMs: 15000, endMs: 25000 });
  expect(readNativeSession(resource(data), entity, 'recording_session')).toBeNull();
  data.chapters = [{ id: 'outside', title: 'Other', startMs: 20000, endMs: 31000 }];
  expect(readNativeSession(resource(data), entity, 'recording_session')).toBeNull();
});
it('never makes a YouTube URL or a video-only source into ready native audio/video', () => {
  const data = metadata(); data.sources[0]!.url = 'https://www.youtube.com/watch?v=123';
  expect(readNativeSession(resource(data), entity, 'recording_session')).toBeNull();
});
it('represents missing originals and processing explicitly without inventing sources', () => {
  const data = metadata(); data.state = 'awaiting_master'; data.sources = [];
  expect(readNativeSession(resource(data), entity, 'recording_session')?.state).toBe('awaiting_master');
});

it.each([
  ['sessions', 'recording_session'], ['recordings', 'recording'], ['releases', 'record_release'],
] as const)('retains the original %s publication identity for native interactions', (collection, kind) => {
  const item = { ...entity, id: '00100000-0000-4000-8000-000000000001', resources: [resource()] };
  const feed = { sessions: [], recordings: [], releases: [], [collection]: [item] } as unknown as RecordsFeedDTO;
  expect(nativeSessionsFromFeed(feed)[0]).toMatchObject({ publication: { kind, entityKey: item.id } });
});

it('refuses a non-string publication ID instead of coercing it into an interaction identity', () => {
  expect(readNativeSession(resource(), { ...entity, id: 123 as unknown as string }, 'recording')).toBeNull();
});

it('replaces provider artwork only for the exact linked source video, preserving other portraits', () => {
  const session = readNativeSession(resource(), entity, 'recording_session')!;
  session.sourceVideoIds = ['exact-video']; session.posterUrl = 'https://api.tdfrecords.net/assets/serve/poster.jpg';
  expect(nativeArtworkForUrl('https://i.ytimg.com/vi/exact-video/maxresdefault.jpg', [session])).toBe(session.posterUrl);
  expect(nativeArtworkForUrl('https://i.ytimg.com/vi/other-video/maxresdefault.jpg', [session])).toContain('other-video');
  expect(nativeArtworkForUrl('https://portrait.invalid/artist.jpg', [session])).toBe('https://portrait.invalid/artist.jpg');
});


it('selects equal-ID publications independently and scopes release repetition to their actual parent', () => {
  const item = { ...entity, resources: [resource()] };
  const sessions = nativeSessionsFromFeed({ sessions: [item], recordings: [item], releases: [item] } as unknown as RecordsFeedDTO);
  const tracks = sessions.map(session => sessionPlayerTrack(session));
  for (const selected of tracks) {
    const queue = queueReducer(EMPTY_QUEUE, { type: 'replace', tracks, currentTrackId: selected.id });
    expect(queue.tracks[queue.currentIndex]).toBe(selected);
    expect(nextQueueIndex({ ...queue, repeat: 'release' })).toBe(queue.currentIndex);
    expect(selected.recordingId).toBe(entity.id);
  }
  expect(new Set(tracks.map(track => track.id)).size).toBe(3);
  expect(new Set(tracks.map(track => track.releaseId)).size).toBe(3);
});

it('keeps equal chapter IDs in distinct publications separate without changing backend recording IDs', () => {
  const first = readNativeSession(resource(), entity, 'recording_session')!;
  const second = readNativeSession(resource(), { ...entity, id: 'another-session' }, 'recording_session')!;
  const firstTrack = sessionPlayerTrack(first, first.chapters[0]);
  const secondTrack = sessionPlayerTrack(second, second.chapters[0]);
  expect(firstTrack.id).not.toBe(secondTrack.id);
  expect(firstTrack.releaseId).not.toBe(secondTrack.releaseId);
  expect(firstTrack.recordingId).toBe('song-id');
  expect(secondTrack.recordingId).toBe('song-id');
});


it('refuses an explicitly unavailable TDF resource even when its native metadata is ready', () => {
  expect(readNativeSession({ ...resource(), availability: 'unavailable' }, entity, 'recording')).toBeNull();
});

it('retains available and unspecified native delivery independently of provider embed metadata', () => {
  expect(readNativeSession({ ...resource(), availability: 'available', providerMetadata: { tdfNative: metadata(), embeddable: false } }, entity, 'recording')?.state).toBe('ready');
  expect(readNativeSession(resource(), entity, 'recording')?.state).toBe('ready');
});
