import type { RecordsFeedDTO, RecordsResourceDTO } from '../../api/records';
import type { InteractionIdentity } from '../../api/interactions';
import type { PlayerQuality, PlayerSource, PlayerTrack } from '../../player/types';

export type NativePublicationKind = Extract<InteractionIdentity['kind'], 'recording' | 'recording_session' | 'record_release'>;
export interface NativePublication extends InteractionIdentity { kind: NativePublicationKind }
export const nativePublicationKey = ({ kind, entityKey }: InteractionIdentity): string => `${kind}:${entityKey}`;
export interface SessionChapter { id: string; title: string; startMs: number; endMs: number }
export interface NativeSession {
  id: string; title: string; artist: string; artistPartyIds: number[]; description?: string;
  publication: NativePublication;
  recordedAt?: string; publishedAt?: string; posterUrl?: string; durationMs: number;
  state: 'awaiting_master' | 'processing' | 'ready' | 'failed';
  sourceVideoIds?: string[];
  sources: PlayerSource[]; chapters: SessionChapter[]; credits: { name: string; role: string }[];
}
const object = (v: unknown): Record<string, unknown> | null => v !== null && typeof v === 'object' && !Array.isArray(v) ? v as Record<string, unknown> : null;
const text = (v: unknown): v is string => typeof v === 'string' && v.trim().length > 0;
const positive = (v: unknown): v is number => typeof v === 'number' && Number.isFinite(v) && v > 0;
// Public delivery metadata cannot load third-party embeds or non-HTTPS media.
// Only public HTTPS delivery URLs may become browser media requests.
export function nativeMediaUrl(v: unknown): string | undefined {
  if (!text(v)) return undefined;
  try { const u = new URL(v); return !u.username && !u.password
    && u.protocol === 'https:'
    && !/(^|\.)(youtube\.com|youtu\.be|ytimg\.com|googlevideo\.com)$/.test(u.hostname) ? u.href : undefined;
  } catch { return undefined; }
}

/** Records is the source of truth: match explicit artist IDs, never titles or names. */
export function readNativeSession(resource: RecordsResourceDTO, entity: { id: string; title: string; description?: string }, kind: NativePublicationKind): NativeSession | null {
  if (resource.providerCode !== 'tdf' || resource.availability === 'unavailable' || !text(entity.id)) return null;
  const data = object(resource.providerMetadata?.['tdfNative']);
  if (data?.['schemaVersion'] !== 1 || data['kind'] !== 'session' || data['isTrack'] === true) return null;
  if (!Array.isArray(data['artistPartyIds']) || !data['artistPartyIds'].length
    || !data['artistPartyIds'].every(id => Number.isSafeInteger(id) && Number(id) > 0)
    || !positive(data['durationMs']) || !text(data['artist'])) return null;
  const state = data['state'];
  if (!['awaiting_master', 'processing', 'ready', 'failed'].includes(String(state))) return null;
  const sources: PlayerSource[] = [];
  for (const raw of Array.isArray(data['sources']) ? data['sources'].slice(0, 12) : []) {
    const item = object(raw); if (!item) continue;
    const url = nativeMediaUrl(item['url']);
    if (!url || !['video/mp4', 'audio/mp4', 'audio/mpeg'].includes(String(item['mediaType']))
      || !['low', 'medium', 'high', 'lossless'].includes(String(item['quality']))) continue;
    sources.push({ url, quality: item['quality'] as PlayerQuality, mediaType: String(item['mediaType']) });
  }
  if (state === 'ready' && (!sources.some(s => s.mediaType.startsWith('audio/')) || !sources.some(s => s.mediaType.startsWith('video/')))) return null;
  const chapters: SessionChapter[] = [];
  for (const raw of Array.isArray(data['chapters']) ? data['chapters'].slice(0, 100) : []) {
    const c = object(raw);
    if (!c || !text(c['id']) || !text(c['title']) || typeof c['startMs'] !== 'number' || !Number.isFinite(c['startMs'])
      || c['startMs'] < 0 || !positive(c['endMs']) || c['endMs'] <= c['startMs'] || c['endMs'] > data['durationMs']
      || c['startMs'] < (chapters.at(-1)?.endMs ?? 0) || chapters.some(p => p.id === c['id'])) return null;
    chapters.push({ id: c['id'], title: c['title'], startMs: c['startMs'], endMs: c['endMs'] });
  }
  const credits = (Array.isArray(data['credits']) ? data['credits'] : []).flatMap(raw => {
    const c = object(raw); return c && text(c['name']) && text(c['role']) ? [{ name: c['name'], role: c['role'] }] : [];
  });
  return { id: entity.id, publication: { kind, entityKey: entity.id }, title: entity.title, description: entity.description, artist: data['artist'],
    artistPartyIds: data['artistPartyIds'] as number[], durationMs: data['durationMs'],
    state: state as NativeSession['state'], sources, chapters, credits,
    posterUrl: nativeMediaUrl(resource.thumbnailUrl), recordedAt: text(data['recordedAt']) ? data['recordedAt'] : undefined,
    publishedAt: text(data['publishedAt']) ? data['publishedAt'] : undefined };
}

export function nativeSessionsFromFeed(feed: RecordsFeedDTO, artistId?: number): NativeSession[] {
  const sessions = new Map<string, NativeSession>();
  const collections = [
    { kind: 'recording_session', items: feed.sessions },
    { kind: 'recording', items: feed.recordings },
    { kind: 'record_release', items: feed.releases },
  ] as const;
  for (const { kind, items } of collections) for (const entity of items) {
    for (const resource of entity.resources) {
      const session = readNativeSession(resource, entity, kind);
      if (session && (artistId === undefined || session.artistPartyIds.includes(artistId))) {
        session.sourceVideoIds = entity.resources.filter(r => r.providerCode === 'youtube').map(r => r.externalCode);
        sessions.set(nativePublicationKey(session.publication), session);
      }
    }
  }
  return [...sessions.values()];
}

export function sessionPlayerTrack(session: NativeSession, chapter?: SessionChapter): PlayerTrack {
  const releaseId = `records:${session.publication.kind}:${encodeURIComponent(session.publication.entityKey)}`;
  return { id: `${releaseId}:${chapter ? `chapter:${encodeURIComponent(chapter.id)}` : 'full'}`,
    recordingId: chapter?.id ?? session.id, releaseId, title: chapter?.title ?? session.title, artist: session.artist,
    artworkUrl: session.posterUrl, durationMs: chapter ? chapter.endMs - chapter.startMs : session.durationMs,
    startMs: chapter?.startMs, endMs: chapter?.endMs, sources: session.sources,
    chapters: chapter ? undefined : session.chapters };
}

/** Replace only a provider thumbnail belonging to this exact persisted session. */
export function nativeArtworkForUrl(url: string | null | undefined, sessions: NativeSession[]): string | null {
  if (!url) return null;
  try {
    const parsed = new URL(url);
    const videoId = ['i.ytimg.com', 'img.youtube.com'].includes(parsed.hostname) ? /^\/vi(?:_webp)?\/([^/]+)\//.exec(parsed.pathname)?.[1] : undefined;
    if (videoId) return sessions.find(session => session.sourceVideoIds?.includes(videoId) && session.posterUrl)?.posterUrl ?? url;
  } catch { /* Retain the existing artwork resolver's fallback. */ }
  return url;
}
