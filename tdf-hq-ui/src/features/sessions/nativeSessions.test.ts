import { nativeArtworkForUrl, nativeSessionsFromFeed, readNativeSession, sessionPlayerTrack } from './nativeSessions';
import type { RecordsFeedDTO, RecordsResourceDTO } from '../../api/records';
const entity = { id: 'session-id', title: 'Synthetic session' };
const metadata = () => ({ schemaVersion: 1, kind: 'session', artistPartyIds: [535], artist: 'Synthetic', state: 'ready', durationMs: 30000,
  sources: [{ url: 'https://api.tdfrecords.net/assets/serve/synthetic/video.mp4', mediaType: 'video/mp4', quality: 'high' },
    { url: 'https://api.tdfrecords.net/assets/serve/synthetic/audio.m4a', mediaType: 'audio/mp4', quality: 'high' }],
  chapters: [{ id: 'song-id', title: 'Song', startMs: 10000, endMs: 20000 }], credits: [] });
const resource = (data = metadata()) => ({ providerCode: 'tdf', providerMetadata: { tdfNative: data } }) as unknown as RecordsResourceDTO;
it('uses exact persisted artist identity and deduplicates the same entity across collections', () => {
  const item = { ...entity, resources: [resource()] };
  const feed = { sessions: [item], recordings: [item], releases: [] } as unknown as RecordsFeedDTO;
  expect(nativeSessionsFromFeed(feed, 535)).toHaveLength(1);
  expect(nativeSessionsFromFeed(feed, 520)).toEqual([]);
});
it('builds chapter playback with the exact source offsets, relative duration and stable identities', () => {
  const session = readNativeSession(resource(), entity)!;
  expect(sessionPlayerTrack(session, session.chapters[0])).toMatchObject({ id: 'records:song-id', startMs: 10000, endMs: 20000, durationMs: 10000 });
});
it('refuses malformed, overlapping and out-of-source chapter boundaries', () => {
  const data = metadata();
  data.chapters.push({ id: 'overlap', title: 'Other', startMs: 15000, endMs: 25000 });
  expect(readNativeSession(resource(data), entity)).toBeNull();
  data.chapters = [{ id: 'outside', title: 'Other', startMs: 20000, endMs: 31000 }];
  expect(readNativeSession(resource(data), entity)).toBeNull();
});
it('never makes a YouTube URL or a video-only source into ready native audio/video', () => {
  const data = metadata(); data.sources[0]!.url = 'https://www.youtube.com/watch?v=123';
  expect(readNativeSession(resource(data), entity)).toBeNull();
});
it('represents missing originals and processing explicitly without inventing sources', () => {
  const data = metadata(); data.state = 'awaiting_master'; data.sources = [];
  expect(readNativeSession(resource(data), entity)?.state).toBe('awaiting_master');
});

it('replaces provider artwork only for the exact linked source video, preserving other portraits', () => {
  const session = readNativeSession(resource(), entity)!;
  session.sourceVideoIds = ['exact-video']; session.posterUrl = 'https://api.tdfrecords.net/assets/serve/poster.jpg';
  expect(nativeArtworkForUrl('https://i.ytimg.com/vi/exact-video/maxresdefault.jpg', [session])).toBe(session.posterUrl);
  expect(nativeArtworkForUrl('https://i.ytimg.com/vi/other-video/maxresdefault.jpg', [session])).toContain('other-video');
  expect(nativeArtworkForUrl('https://portrait.invalid/artist.jpg', [session])).toBe('https://portrait.invalid/artist.jpg');
});
