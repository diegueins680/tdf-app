import type { TorrentAudioSession, TorrentPlaybackProgress } from '../utils/torrentAudio';
import {
  createContext,
  useCallback,
  useContext,
  useEffect, useLayoutEffect,
  useMemo,
  useRef,
  useState,
  type ReactNode,
} from 'react';
import { logger } from '../utils/logger';
import { EMPTY_QUEUE, nextQueueIndex, previousQueueIndex, queueReducer, type QueueAction, type QueueState } from './queue';
import { authorizePlayerAsset } from './assetAccess';
import {
  PLAYER_LOAD_TRACK_EVENT,
  PLAYER_ANALYTICS_EVENT,
  PLAYER_TAKEOVER_EVENT,
  RADIO_TAKEOVER_EVENT,
  type LoadPlayerTrackDetail,
  type PlaybackStatus,
  type PlayerQuality,
  type PlayerSource,
  type PlayerTrack,
  type RepeatMode,
} from './types';
import { installPlayerAnalyticsBridge } from './analytics';

const PERSISTENCE_KEY = 'tdf-global-player/v1';
const TARGET_LOUDNESS_LUFS = -14;
const MAX_NETWORK_RETRIES = 2;

interface PersistedPlayerState {
  ownerPartyId?: number | null;
  queue: QueueState;
  volume: number;
  muted: boolean;
  quality: PlayerQuality | 'auto';
  positions: Record<string, number>;
  pausedByUser?: boolean;
}

interface PlayerContextValue {
  unavailableTrackIds: string[];
  torrentProgress: TorrentPlaybackProgress | null;
  queue: QueueState;
  currentTrack: PlayerTrack | null;
  status: PlaybackStatus;
  playbackRequested: boolean;
  error: string | null;
  currentTime: number;
  duration: number;
  volume: number;
  muted: boolean;
  quality: PlayerQuality | 'auto';
  resolvedQuality: PlayerQuality | null;
  beginArtist: (key: string) => void;
  loadArtist: (key: string, tracks: PlayerTrack[]) => void;
  mediaMode: 'audio' | 'video';
  playbackRate: number;
  setMediaMode: (mode: 'audio' | 'video') => void;
  setPlaybackRate: (rate: number) => void;
  requestFullscreen: () => Promise<void>;
  requestPictureInPicture: () => Promise<void>;
  playTrack: (track: PlayerTrack, queue?: PlayerTrack[], autoplay?: boolean) => void;
  enqueue: (tracks: PlayerTrack[]) => void;
  selectTrack: (index: number) => void;
  removeTrack: (index: number) => void;
  moveTrack: (from: number, to: number) => void;
  clearQueue: () => void;
  togglePlayback: () => void;
  play: () => Promise<void>;
  pause: () => void;
  next: () => void;
  previous: () => void;
  seek: (seconds: number) => void;
  setVolume: (volume: number) => void;
  setMuted: (muted: boolean) => void;
  setQuality: (quality: PlayerQuality | 'auto') => void;
  setShuffle: (enabled: boolean) => void;
  setRepeat: (mode: RepeatMode) => void;
}

const PlayerContext = createContext<PlayerContextValue | null>(null);

const isPlayerQuality = (value: unknown): value is PlayerQuality =>
  value === 'low' || value === 'medium' || value === 'high' || value === 'lossless';

const isRepeatMode = (value: unknown): value is RepeatMode =>
  value === 'off' || value === 'track' || value === 'queue' || value === 'release';

const isTrack = (value: unknown): value is PlayerTrack => {
  if (!value || typeof value !== 'object') return false;
  const track = value as Partial<PlayerTrack>;
  return typeof track.id === 'string'
    && track.id.trim() !== ''
    && typeof track.title === 'string'
    && typeof track.artist === 'string'
    && Array.isArray(track.sources)
    && track.sources.length > 0
    && track.sources.every((source) => source
      && isPlayerQuality(source.quality)
      && ((typeof source.url === 'string' && source.url.length > 0)
        || (typeof source.assetId === 'string' && source.assetId.length > 0)));
};

const loadPersistedState = (ownerPartyId: number | null | undefined, identityReady: boolean): PersistedPlayerState => {
  if (typeof window === 'undefined') {
    return { queue: EMPTY_QUEUE, volume: 0.8, muted: false, quality: 'auto', positions: {} };
  }
  try {
    const parsed = JSON.parse(window.localStorage.getItem(PERSISTENCE_KEY) ?? 'null') as Partial<PersistedPlayerState> | null;
    // Missing ownership denotes legacy, device-wide listening state. In
    // particular, it must not be interpreted as an anonymous owner's null tag.
    const restoreListening = identityReady && ownerPartyId !== undefined
      && parsed != null && Object.prototype.hasOwnProperty.call(parsed, 'ownerPartyId')
      && parsed.ownerPartyId === ownerPartyId;
    const tracks = restoreListening && Array.isArray(parsed?.queue?.tracks)
      ? parsed.queue.tracks.filter(isTrack).slice(0, 200) : [];
    const currentIndex = Number.isSafeInteger(parsed?.queue?.currentIndex)
      ? Math.min(Math.max(parsed?.queue?.currentIndex ?? -1, -1), tracks.length - 1)
      : -1;
    const savedRadioVolume = window.localStorage.getItem('radio-volume');
    const legacyVolume = savedRadioVolume?.trim() ? Number(savedRadioVolume) : Number.NaN;
    const volume = typeof parsed?.volume === 'number' && Number.isFinite(parsed.volume)
      ? Math.min(Math.max(parsed.volume, 0), 1)
      : Number.isFinite(legacyVolume) ? Math.min(Math.max(legacyVolume, 0), 1) : 0.8;
    const quality = parsed?.quality === 'auto' || isPlayerQuality(parsed?.quality) ? parsed.quality : 'auto';
    const repeat = restoreListening && isRepeatMode(parsed?.queue?.repeat) ? parsed.queue.repeat : 'off';
    const positions = restoreListening && parsed?.positions && typeof parsed.positions === 'object'
      ? Object.fromEntries(Object.entries(parsed.positions).filter(([, position]) => typeof position === 'number'
        && Number.isFinite(position) && position >= 0)) : {};
    return {
      queue: { tracks, currentIndex, shuffle: restoreListening && parsed?.queue?.shuffle === true, repeat },
      volume,
      muted: parsed?.muted === true,
      quality,
      positions,
      pausedByUser: restoreListening && parsed?.pausedByUser === true,
    };
  } catch {
    return { queue: EMPTY_QUEUE, volume: 0.8, muted: false, quality: 'auto', positions: {} };
  }
};

const qualityRank: Record<PlayerQuality, number> = {
  low: 0,
  medium: 1,
  high: 2,
  lossless: 3,
};

const automaticQuality = (): PlayerQuality => {
  if (typeof navigator === 'undefined') return 'high';
  const connection = (navigator as Navigator & { connection?: { effectiveType?: string; saveData?: boolean } }).connection;
  if (connection?.saveData || connection?.effectiveType === '2g' || connection?.effectiveType === 'slow-2g') return 'low';
  if (connection?.effectiveType === '3g') return 'medium';
  return 'high';
};

const orderedSources = (track: PlayerTrack | null, selectedQuality: PlayerQuality | 'auto'): PlayerSource[] => {
  if (!track) return [];
  const requested = selectedQuality === 'auto' ? automaticQuality() : selectedQuality;
  // Asset-bound tracks may only fall back to another freshly authorized asset.
  // A stale direct URL must not downgrade a protected recording after denial.
  const declaresIdentity = (item: PlayerSource) =>
    Object.prototype.hasOwnProperty.call(item, 'assetId') || Object.prototype.hasOwnProperty.call(item, 'legacySourceId');
  const validDeclaredIdentity = (item: PlayerSource) => {
    const assetDeclared = Object.prototype.hasOwnProperty.call(item, 'assetId');
    const legacyDeclared = Object.prototype.hasOwnProperty.call(item, 'legacySourceId');
    return (assetDeclared || legacyDeclared)
      && (!assetDeclared || (typeof item.assetId === 'string' && item.assetId.trim().length > 0))
      && (!legacyDeclared || (typeof item.legacySourceId === 'string' && item.legacySourceId.trim().length > 0));
  };
  // An explicit but malformed protected identity is not permission to use a
  // stale URL, including after restoring a queue while rollout is disabled.
  const sources = track.sources.some(declaresIdentity)
    ? track.sources.filter(validDeclaredIdentity) : track.sources;
  const seen = new Set<string>();
  const unique = sources.filter((item) => {
    const key = item.assetId ?? (item.legacySourceId ? `legacy:${item.legacySourceId}` : item.url);
    if (seen.has(key)) return false;
    seen.add(key); return true;
  });
  const order = (sources: PlayerSource[]) => [...sources].sort((a, b) => {
    // Prefer the best quality within the requested ceiling, then lower-cost
    // fallbacks above it. Preview access is always attempted after full audio.
    const aRank = qualityRank[a.quality], bRank = qualityRank[b.quality];
    const ceiling = qualityRank[requested];
    if (aRank <= ceiling && bRank <= ceiling) return bRank - aRank;
    if (aRank <= ceiling) return -1;
    if (bRank <= ceiling) return 1;
    return aRank - bRank;
  });
  return [...order(unique.filter((item) => !item.preview)),
    ...order(unique.filter((item) => item.preview))];
};

const normalizedVolume = (volume: number, loudnessLufs: number | null | undefined): number => {
  if (loudnessLufs === null || loudnessLufs === undefined || !Number.isFinite(loudnessLufs)) return volume;
  const gainDb = Math.min(0, TARGET_LOUDNESS_LUFS - loudnessLufs);
  const multiplier = 10 ** (gainDb / 20);
  return Math.min(1, Math.max(0, volume * multiplier));
};

const emitPlayerEvent = (eventType: string, track: PlayerTrack, positionSeconds: number, extra = {}) => {
  if (typeof window === 'undefined') return;
  window.dispatchEvent(new CustomEvent(PLAYER_ANALYTICS_EVENT, {
    detail: {
      schemaVersion: 1,
      eventId: crypto.randomUUID(),
      eventType,
      recordingId: track.recordingId ?? track.id,
      releaseVersionId: track.releaseVersionId,
      positionMs: Math.round(positionSeconds * 1000),
      occurredAt: new Date().toISOString(),
      ...extra,
    },
  }));
};

interface PlayerIdentity {
  ready: boolean;
  // null means resolved anonymous; undefined means an account without a valid ID.
  partyId: number | null | undefined;
}

interface PlayerPreferences {
  volume: number;
  muted: boolean;
  quality: PlayerQuality | 'auto';
}

const ANONYMOUS_IDENTITY: PlayerIdentity = { ready: true, partyId: null };

export function PlayerProvider({ children, identity = ANONYMOUS_IDENTITY }: { children: ReactNode; identity?: PlayerIdentity }) {
  const ownerPartyId = identity.partyId === null ? null
    : Number.isSafeInteger(identity.partyId) && (identity.partyId ?? 0) > 0 ? identity.partyId : undefined;
  // Device preferences survive listener changes even before the save debounce.
  const [preferences, setPreferences] = useState<PlayerPreferences>(() => loadPersistedState(undefined, false));
  const [boundary, setBoundary] = useState({
    ownerPartyId, epoch: 0, awaitingFirstOwner: !identity.ready && ownerPartyId === null,
  });
  const ownerChanged = boundary.ownerPartyId !== ownerPartyId;
  if (ownerChanged || (boundary.awaitingFirstOwner && identity.ready)) {
    // An uncached cookie session initially has no established listener. Its
    // first resolution retains hydration eligibility and newer explicit intent.
    // Resolving anonymous consumes that exception too; subsequent login is a
    // real owner change and must reset before previous titles can commit.
    setBoundary({
      ownerPartyId,
      epoch: ownerChanged && !boundary.awaitingFirstOwner ? boundary.epoch + 1 : boundary.epoch,
      awaitingFirstOwner: false,
    });
    return null;
  }
  return <PlayerSession key={boundary.epoch} ownerPartyId={ownerPartyId} identityReady={identity.ready}
    restoreListening={boundary.epoch === 0}
    preferences={preferences} setPreferences={setPreferences}>{children}</PlayerSession>;
}

function PlayerSession({ children, ownerPartyId, identityReady, restoreListening, preferences, setPreferences }: {
  children: ReactNode; ownerPartyId: number | null | undefined; identityReady: boolean;
  restoreListening: boolean;
  preferences: PlayerPreferences;
  setPreferences: (update: (current: PlayerPreferences) => PlayerPreferences) => void;
}) {
  // Only a fresh application context can restore. A -> B -> A must remain
  // empty even if an old A envelope survives the short persistence debounce.
  const [persisted] = useState(() => loadPersistedState(ownerPartyId, identityReady && restoreListening));
  const [queue, setQueue] = useState(persisted.queue);
  // Commands reduce synchronously against one immutable authoritative snapshot;
  // React renders that snapshot, including several commands in the same batch.
  const recoveryQueueRef = useRef(queue);
  const reduceQueue = useCallback((action: QueueAction) => {
    const next = queueReducer(recoveryQueueRef.current, action);
    recoveryQueueRef.current = next;
    setQueue(next);
  }, []);
  const queueIntentRef = useRef(false);
  const [listeningHydrated, setListeningHydrated] = useState(identityReady);
  const dispatchQueue = useCallback((action: QueueAction) => {
    queueIntentRef.current = true;
    reduceQueue(action);
  }, [reduceQueue]);
  const [status, setStatus] = useState<PlaybackStatus>('idle');
  const [error, setError] = useState<string | null>(null);
  const [currentTime, setCurrentTime] = useState(0);
  const [duration, setDuration] = useState(0);
  const { volume, muted, quality } = preferences;
  const updateVolume = useCallback((value: number) => setPreferences((current) => ({ ...current, volume: value })), [setPreferences]);
  const updateMuted = useCallback((value: boolean | ((current: boolean) => boolean)) => setPreferences((current) => ({
    ...current, muted: typeof value === 'function' ? value(current.muted) : value,
  })), [setPreferences]);
  const updateQuality = useCallback((value: PlayerQuality | 'auto') => setPreferences((current) => ({ ...current, quality: value })), [setPreferences]);
  const [sourceRefresh, setSourceRefresh] = useState(0);
  const legacyAudioRef = useRef<HTMLAudioElement | null>(null);
  const nativeVideoRef = useRef<HTMLVideoElement | null>(null);
  const [mediaMode, updateMediaMode] = useState<'audio' | 'video'>('video');
  const [playbackRate, updatePlaybackRate] = useState(1);
  const positionsRef = useRef<Record<string, number>>(persisted.positions);
  const autoplayRef = useRef(false);
  const [playbackRequested, setPlaybackRequested] = useState(false);
  const [pausedByUser, setPausedByUser] = useState(persisted.pausedByUser === true);
  const explicitPauseRef = useRef(persisted.pausedByUser === true);
  const artistContextRef = useRef<string | null>(null);
  const failedTracksRef = useRef(new Set<string>());
  const failedSourcesRef = useRef(new Set<PlayerSource>());
  const [unavailableTrackIds, setUnavailableTrackIds] = useState<string[]>([]);
  // Native controls provide a gesture token; playback intent remains shared.
  const nativeGestureRef = useRef(false);
  const retryRef = useRef(0);
  const cancelWatchdogRef = useRef<() => void>(() => undefined);
  const armWatchdogRef = useRef<() => void>(() => undefined);
  const retryTimerRef = useRef<number | null>(null);
  const [torrentProgress, setTorrentProgress] = useState<TorrentPlaybackProgress | null>(null);
  const [resolvedQuality, setResolvedQuality] = useState<PlayerQuality | null>(null);
  const sourceGenerationRef = useRef(0);
  const playAttemptRef = useRef(0);
  const startedPlayAttemptRef = useRef<number | null>(null);
  const loadedSourceRef = useRef<{ generation: number; trackId: string; url: string; version?: string; preview?: boolean; quality: PlayerQuality; candidate: PlayerSource } | null>(null);
  const pendingPositionRef = useRef<{ generation: number; trackId: string; url: string; position: number } | null>(null);
  const internalPauseEventsRef = useRef(new WeakSet<Event>());
  const pendingInternalPausesRef = useRef(new Map<HTMLMediaElement, { completion: Promise<void>; cancel: () => void }>());
  const pauseForSourceTransition = useCallback((media: HTMLMediaElement): Promise<void> => {
    // Only native video controls need pause-event attribution. Legacy audio
    // has no separate native transport surface and retains its existing load path.
    if (!(media instanceof HTMLVideoElement)) { media.pause(); return Promise.resolve(); }
    const pending = pendingInternalPausesRef.current.get(media);
    if (pending) return pending.completion;
    if (media.paused) return Promise.resolve();
    let finish: () => void = () => undefined;
    const completion = new Promise<void>(resolve => { finish = resolve; });
    const consume = (event?: Event) => {
      if (event) internalPauseEventsRef.current.add(event);
      media.removeEventListener('pause', consume, true);
      pendingInternalPausesRef.current.delete(media);
      finish();
    };
    pendingInternalPausesRef.current.set(media, { completion, cancel: () => consume() });
    // Observe this exact native task before the ordinary pause listener. Wait
    // before load(): aborting a resource must not discard an expected event and
    // leave a suppression token that could swallow a later user pause.
    media.addEventListener('pause', consume, { once: true, capture: true });
    media.pause();
    return completion;
  }, []);

  const lastContinuousPositionRef = useRef(0);
  const listenedSinceProgressRef = useRef(0);
  const currentTrack = queue.currentIndex >= 0 ? queue.tracks[queue.currentIndex] ?? null : null;
  const nativeVideo = Boolean(currentTrack?.sources.some(candidate => candidate.mediaType?.startsWith('video/')));
  const audioRef = useRef<HTMLMediaElement | null>(null);
  useLayoutEffect(() => { audioRef.current = nativeVideo ? nativeVideoRef.current : legacyAudioRef.current; }, [nativeVideo]);
  const candidates = useMemo(() => {
    if (!currentTrack || !nativeVideo) return orderedSources(currentTrack, quality);
    // Apply the complete track's protected-identity boundary before filtering
    // formats; a denied video grant cannot authorize an unbound audio URL.
    const eligible = orderedSources(currentTrack, quality);
    const forMode = (mode: 'audio' | 'video') => eligible.filter(candidate => candidate.mediaType?.startsWith(`${mode}/`));
    // Video may degrade to the independent audio rendition. An explicit audio
    // choice must never spend bandwidth downloading video as a fallback.
    return mediaMode === 'video' ? [...forMode('video'), ...forMode('audio')] : forMode('audio');
  }, [currentTrack, mediaMode, nativeVideo, quality]);
  const source = candidates[0] ?? null;

  useEffect(() => {
    if (!identityReady || listeningHydrated) return;
    // Any explicit queue command, including clearing an empty queue, supersedes
    // persisted state. Hydration cannot replace a user's newer selection.
    if (!queueIntentRef.current) {
      const saved = loadPersistedState(ownerPartyId, restoreListening);
      positionsRef.current = saved.positions;
      explicitPauseRef.current = saved.pausedByUser === true; setPausedByUser(saved.pausedByUser === true);
      reduceQueue({ type: 'restore', queue: saved.queue });
    }
    setListeningHydrated(true);
  }, [identityReady, listeningHydrated, ownerPartyId, reduceQueue, restoreListening]);

  const cancelRetry = useCallback(() => {
    if (retryTimerRef.current !== null) window.clearTimeout(retryTimerRef.current);
    retryTimerRef.current = null;
  }, []);

  const stopDeniedSelection = useCallback((trackId: string) => {
    const media = audioRef.current;
    if (media && loadedSourceRef.current?.trackId === trackId) {
      positionsRef.current[trackId] = pendingPositionRef.current?.position ?? media.currentTime;
    }
    cancelRetry();
    cancelWatchdogRef.current();
    const generation = ++sourceGenerationRef.current;
    playAttemptRef.current += 1;
    loadedSourceRef.current = null;
    pendingPositionRef.current = null;
    nativeGestureRef.current = false;
    autoplayRef.current = false; setPlaybackRequested(false);
    failedTracksRef.current.add(trackId);
    setUnavailableTrackIds([...failedTracksRef.current]);
    if (media) void pauseForSourceTransition(media).then(() => {
      // A newer selection/owner must never lose its source to this pause task.
      if (generation !== sourceGenerationRef.current || audioRef.current !== media) return;
      media.removeAttribute('src'); media.srcObject = null; media.load();
    });
  }, [cancelRetry, pauseForSourceTransition]);

  const retireSelection = useCallback(() => {
    const audio = audioRef.current;
    const loaded = loadedSourceRef.current;
    if (audio && loaded?.generation === sourceGenerationRef.current) {
      positionsRef.current[loaded.trackId] = pendingPositionRef.current?.position ?? audio.currentTime;
    }
    cancelRetry();
    cancelWatchdogRef.current();
    sourceGenerationRef.current += 1;
    playAttemptRef.current += 1;
    loadedSourceRef.current = null;
    pendingPositionRef.current = null;
    nativeGestureRef.current = false;
    if (audio) void pauseForSourceTransition(audio);
    // The source effect drains this exact pause task before replacing src.
    // Also reload when a new command selects the same immutable queue object.
    setSourceRefresh((value) => value + 1);
  }, [cancelRetry, pauseForSourceTransition]);

  const scheduleRecovery = useCallback((track: PlayerTrack, position: number, generation: number): boolean => {
    if (!autoplayRef.current) return false;
    if (retryTimerRef.current !== null) return true;
    if (retryRef.current >= MAX_NETWORK_RETRIES) return false;
    retryRef.current += 1;
    playAttemptRef.current += 1; // a failed native play promise no longer owns recovery
    positionsRef.current[track.id] = position;
    setStatus('loading');
    emitPlayerEvent('buffering', track, position, { recoveryPhase: 'attempt', attempt: retryRef.current });
    retryTimerRef.current = window.setTimeout(() => {
      retryTimerRef.current = null;
      if (generation === sourceGenerationRef.current) setSourceRefresh((value) => value + 1);
    }, 500 * 2 ** retryRef.current);
    return true;
  }, []);

  const skipFailed = useCallback((): boolean => {
    const queue = recoveryQueueRef.current;
    if (!currentTrack || navigator.onLine === false) return false;
    const loaded = loadedSourceRef.current;
    if (loaded?.trackId === currentTrack.id) {
      failedSourcesRef.current.add(loaded.candidate);
      const remaining = candidates.filter((candidate) => !failedSourcesRef.current.has(candidate));
      if (remaining.length) {
        if (nativeVideo && remaining.every((candidate) => candidate.mediaType?.startsWith('audio/'))) updateMediaMode('audio');
        const audio = audioRef.current;
        positionsRef.current[currentTrack.id] = pendingPositionRef.current?.position ?? audio?.currentTime ?? 0;
        cancelRetry();
        retryRef.current = 0;
        sourceGenerationRef.current += 1;
        playAttemptRef.current += 1;
        if (audio) void pauseForSourceTransition(audio);
        setSourceRefresh((value) => value + 1);
        return true;
      }
    }
    failedTracksRef.current.add(currentTrack.id);
    setUnavailableTrackIds([...failedTracksRef.current]);
    const nextIndex = queue.tracks.findIndex((track, index) => index > queue.currentIndex
      && !failedTracksRef.current.has(track.id));
    if (nextIndex < 0) return false;
    cancelRetry();
    sourceGenerationRef.current += 1;
    playAttemptRef.current += 1;
    if (audioRef.current) void pauseForSourceTransition(audioRef.current);
    dispatchQueue({ type: 'select', index: nextIndex });
    return true;
  }, [cancelRetry, candidates, currentTrack, dispatchQueue, nativeVideo, pauseForSourceTransition]);

  const flushProgress = useCallback((track: PlayerTrack, audio: HTMLAudioElement) => {
    const listenedDeltaMs = Math.round(listenedSinceProgressRef.current);
    if (listenedDeltaMs <= 0) return;
    listenedSinceProgressRef.current = 0;
    emitPlayerEvent('progress', track, audio.currentTime, { listenedDeltaMs });
  }, []);

  const pause = useCallback((explicit = true) => {
    if (explicit) { queueIntentRef.current = true; explicitPauseRef.current = true; setPausedByUser(true); }
    cancelWatchdogRef.current();
    const audio = audioRef.current;
    if (!audio) return;
    playAttemptRef.current += 1;
    autoplayRef.current = false; setPlaybackRequested(false);
    nativeGestureRef.current = false;
    cancelRetry();
    void pauseForSourceTransition(audio);
    setStatus(currentTrack ? 'paused' : 'idle');
    if (currentTrack) {
      flushProgress(currentTrack, audio);
      emitPlayerEvent('pause', currentTrack, audio.currentTime);
    }
  }, [cancelRetry, currentTrack, flushProgress, pauseForSourceTransition]);

  const play = useCallback(async () => {
    explicitPauseRef.current = false; setPausedByUser(false);
    const audio = audioRef.current;
    const selected = recoveryQueueRef.current.tracks[recoveryQueueRef.current.currentIndex];
    if (audio && selected && selected !== currentTrack) {
      autoplayRef.current = true; setPlaybackRequested(true);
      setStatus('loading');
      setSourceRefresh((value) => value + 1);
      return;
    }
    if (!audio || !currentTrack || !source) {
      setStatus('unavailable');
      setError('No hay una fuente autorizada disponible para esta pista.');
      return;
    }
    autoplayRef.current = true; setPlaybackRequested(true);
    // A user retry starts a fresh bounded source attempt after exhaustion.
    // Automatic rendition/track recovery never enters with a failed current ID.
    if (failedTracksRef.current.delete(currentTrack.id)) {
      failedSourcesRef.current.clear();
      setUnavailableTrackIds([...failedTracksRef.current]);
      retryRef.current = 0;
      setStatus('loading');
      setSourceRefresh((value) => value + 1);
      return;
    }
    armWatchdogRef.current();
    if (pendingPositionRef.current?.generation === sourceGenerationRef.current) {
      setStatus('loading');
      return;
    }
    if (!audio.getAttribute('src') || audio.error) {
      retryRef.current = 0; // explicit retry starts a new bounded attempt
      setStatus('loading');
      setSourceRefresh((value) => value + 1);
      return;
    }
    const generation = sourceGenerationRef.current;
    const loadedSource = loadedSourceRef.current;
    if (currentTrack.endMs != null && audio.currentTime * 1000 >= currentTrack.endMs) {
      if (loadedSource?.generation !== generation || loadedSource.trackId !== currentTrack.id
        || loadedSource.url !== audio.currentSrc) {
        setStatus('loading');
        return;
      }
      // The last bounded item keeps its loaded element after completion.
      // Rewind that owned source before play can emit adjacent chapter audio.
      const start = (currentTrack.startMs ?? 0) / 1000;
      audio.currentTime = start;
      if (generation !== sourceGenerationRef.current || loadedSourceRef.current !== loadedSource || audioRef.current !== audio) return;
      positionsRef.current[currentTrack.id] = start;
      lastContinuousPositionRef.current = start;
      setCurrentTime(0);
    }
    const attempt = playAttemptRef.current + 1;
    playAttemptRef.current = attempt;
    window.dispatchEvent(new CustomEvent(PLAYER_TAKEOVER_EVENT));
    try {
      await audio.play();
      if (generation !== sourceGenerationRef.current || attempt !== playAttemptRef.current
        || recoveryQueueRef.current.tracks[recoveryQueueRef.current.currentIndex] !== currentTrack) return;
      // Native 'playing' owns the visible playback state. A fulfilled promise
      // alone is insufficient, especially when a pause races its settlement.
      setError(null);
    } catch (caught) {
      if (generation !== sourceGenerationRef.current || attempt !== playAttemptRef.current
        || recoveryQueueRef.current.tracks[recoveryQueueRef.current.currentIndex] !== currentTrack) return;
      // Browsers may reject play() before dispatching their native error
      // event. Preserve recovery intent in either event order.
      // The native engine can set error while play() is pending; TypeScript's
      // pre-await narrowing does not describe that external mutation.
      const mediaError = audio.error as MediaError | null;
      const recoverable = mediaError?.code === 2 || (Boolean(loadedSourceRef.current?.candidate.assetId ?? source.assetId) && mediaError?.code === 4);
      const blocked = caught instanceof DOMException && caught.name === 'NotAllowedError';
      const unsupported = caught instanceof DOMException && caught.name === 'NotSupportedError';
      if (blocked) cancelWatchdogRef.current();
      if (!blocked && recoverable && scheduleRecovery(currentTrack, audio.currentTime, generation)) return;
      if (!blocked && (mediaError || unsupported) && skipFailed()) return;
      autoplayRef.current = false; setPlaybackRequested(false);
      void pauseForSourceTransition(audio);
      // Browser policy rejection is recoverable by a fresh user gesture. Keep
      // the selected source so that retrying direct/legacy audio also works.
      setStatus(blocked ? 'paused' : mediaError || unsupported ? 'unavailable' : 'error');
      setError(blocked ? 'Pulsa Reproducir para permitir el audio en este navegador.'
        : recoverable ? 'La fuente dejó de estar disponible. Puedes reintentar o pasar a la siguiente pista.'
          : 'No se pudo iniciar la reproducción. Revisa la conexión o vuelve a intentarlo.');
      logger.warn('Global player could not start playback', caught);
    }
  }, [currentTrack, pauseForSourceTransition, scheduleRecovery, skipFailed, source]);

  const selectTrack = useCallback((index: number) => {
    explicitPauseRef.current = false; setPausedByUser(false);
    failedTracksRef.current.clear();
    failedSourcesRef.current.clear();
    setUnavailableTrackIds([]);
    const current = recoveryQueueRef.current;
    if (!current.tracks[index]) return;
    if (index === current.currentIndex && current.tracks[index] === currentTrack) { void play(); return; }
    retireSelection();
    autoplayRef.current = true; setPlaybackRequested(true);
    dispatchQueue({ type: 'select', index });
  }, [currentTrack, dispatchQueue, play, retireSelection]);

  const seek = useCallback((seconds: number) => {
    const audio = audioRef.current;
    if (!audio || !Number.isFinite(seconds)) return;
    const start = (currentTrack?.startMs ?? 0) / 1000;
    const end = currentTrack?.endMs ? currentTrack.endMs / 1000 : Number.isFinite(audio.duration) ? audio.duration : start + seconds;
    const nextTime = Math.min(Math.max(start + seconds, start), end);
    const pending = pendingPositionRef.current;
    if (pending?.generation === sourceGenerationRef.current) pending.position = nextTime;
    else audio.currentTime = nextTime;
    if (currentTrack) positionsRef.current[currentTrack.id] = nextTime;
    lastContinuousPositionRef.current = nextTime;
    setCurrentTime(nextTime - (currentTrack?.startMs ?? 0) / 1000);
    if (currentTrack) emitPlayerEvent('seek', currentTrack, nextTime);
  }, [audioRef, currentTrack]);

  const next = useCallback(() => {
    const current = recoveryQueueRef.current;
    const nextIndex = nextQueueIndex(current);
    if (nextIndex < 0) {
      pause(false);
      return;
    }
    if (nextIndex === current.currentIndex && current.tracks[nextIndex] === currentTrack) {
      seek(0);
      if (!explicitPauseRef.current) void play();
      return;
    }
    retireSelection();
    autoplayRef.current = !explicitPauseRef.current; setPlaybackRequested(!explicitPauseRef.current);
    dispatchQueue({ type: 'select', index: nextIndex });
  }, [currentTrack, dispatchQueue, pause, play, retireSelection, seek]);

  const previous = useCallback(() => {
    const audio = audioRef.current;
    const current = recoveryQueueRef.current;
    if (audio && loadedSourceRef.current?.generation === sourceGenerationRef.current
      && loadedSourceRef.current.trackId === current.tracks[current.currentIndex]?.id
      && (pendingPositionRef.current?.position ?? audio.currentTime) - (currentTrack?.startMs ?? 0) / 1000 > 3) {
      seek(0);
      return;
    }
    const previousIndex = previousQueueIndex(current);
    if (previousIndex >= 0) {
      retireSelection();
      autoplayRef.current = !explicitPauseRef.current; setPlaybackRequested(!explicitPauseRef.current);
      dispatchQueue({ type: 'select', index: previousIndex });
    }
  }, [audioRef, currentTrack, dispatchQueue, retireSelection, seek]);

  useEffect(() => {
    const audio = audioRef.current;
    if (!audio) return undefined;

    const handleTime = () => {
      const track = queue.tracks[queue.currentIndex];
      const loadedSource = loadedSourceRef.current;
      // pause/load can deliver old timeupdate events while the next track's
      // signed URL is pending. Never attribute that position/listening to it.
      if (!loadedSource || loadedSource.generation !== sourceGenerationRef.current
        || loadedSource.trackId !== track?.id || audio.currentSrc !== loadedSource.url) return;
      if (pendingPositionRef.current?.generation === sourceGenerationRef.current) return;
      const position = audio.currentTime;
      const start = (track?.startMs ?? 0) / 1000;
      if (position < start) { audio.currentTime = start; lastContinuousPositionRef.current = start; return; }
      const delta = position - lastContinuousPositionRef.current;
      lastContinuousPositionRef.current = position;
      if (!audio.paused && delta > 0 && delta <= 1.5) {
        listenedSinceProgressRef.current += delta * 1000;
        if (track && listenedSinceProgressRef.current >= 5000) flushProgress(track, audio);
      }
      if (loadedSource.preview && track?.previewEndMs && audio.currentTime * 1000 >= track.previewEndMs) {
        autoplayRef.current = false; setPlaybackRequested(false);
        playAttemptRef.current += 1;
        void pauseForSourceTransition(audio);
        setStatus('paused');
        emitPlayerEvent('complete', track, audio.currentTime, {
          preview: true,
          listenedDeltaMs: Math.round(listenedSinceProgressRef.current),
        });
        listenedSinceProgressRef.current = 0;
        return;
      }
      if (track?.endMs && position * 1000 >= track.endMs && !audio.paused) {
        void pauseForSourceTransition(audio);
        handleEnded();
        return;
      }
      setCurrentTime(Math.max(0, audio.currentTime - (track?.startMs ?? 0) / 1000));
      if (track) positionsRef.current[track.id] = audio.currentTime;
    };
    const handleDuration = () => {
      if (!ownsNativeSource()) return;
      const end = currentTrack?.endMs ? currentTrack.endMs / 1000 : audio.duration;
      setDuration(Number.isFinite(end) ? Math.max(0, end - (currentTrack?.startMs ?? 0) / 1000) : 0);
    };
    const handlePrepared = () => {
      const loadedSource = loadedSourceRef.current;
      if (loadedSource?.generation !== sourceGenerationRef.current
        || loadedSource?.trackId !== queue.tracks[queue.currentIndex]?.id
        || loadedSource?.url !== audio.currentSrc || !audio.paused || autoplayRef.current) return;
      // preload=metadata may stop before canplay. A prepared source awaiting
      // the user's play gesture is paused, not indefinitely loading.
      setStatus((current) => current === 'loading' || current === 'buffering' ? 'paused' : current);
    };
    // Recovery retires a source immediately, before React's passive listener
    // cleanup. A second queued native error must not consume the new queue's
    // recovery intent through the old track's closure.
    const ownsNativeSource = () => loadedSourceRef.current?.generation === sourceGenerationRef.current
      && recoveryQueueRef.current.tracks[recoveryQueueRef.current.currentIndex] === currentTrack
      && loadedSourceRef.current?.trackId === currentTrack?.id
      && loadedSourceRef.current?.url === audio.currentSrc;
    const handlePlaying = () => {
      if (!ownsNativeSource() || pendingPositionRef.current || !autoplayRef.current || audio.paused) return;
      lastContinuousPositionRef.current = audio.currentTime;
      setStatus('playing');
      // A resolved play() promise is not evidence that media is playing.
      // Buffer recovery can emit 'playing' repeatedly for the same attempt.
      if (currentTrack && startedPlayAttemptRef.current !== playAttemptRef.current) {
        startedPlayAttemptRef.current = playAttemptRef.current;
        emitPlayerEvent('play_start', currentTrack, audio.currentTime, { quality: loadedSourceRef.current?.quality });
      }
    };
    const handleWaiting = () => {
      if (!ownsNativeSource() || pendingPositionRef.current || !autoplayRef.current || audio.paused) return;
      setStatus('buffering');
      const track = queue.tracks[queue.currentIndex];
      if (track) emitPlayerEvent('buffering', track, audio.currentTime);
    };
    const handlePause = (event: Event) => {
      if (internalPauseEventsRef.current.has(event) || !ownsNativeSource() || !audio.paused) return;
      if (nativeVideo && !audio.ended) {
        explicitPauseRef.current = true; setPausedByUser(true);
        queueIntentRef.current = true;
        autoplayRef.current = false; setPlaybackRequested(false);
        nativeGestureRef.current = false;
        playAttemptRef.current += 1;
        cancelRetry(); cancelWatchdogRef.current();
      }
      setStatus((current) => current === 'error' || current === 'unavailable' || current === 'loading' ? current : 'paused');
    };
    const handleEnded = () => {
      if (!ownsNativeSource() || pendingPositionRef.current || !autoplayRef.current || explicitPauseRef.current) return;
      const track = queue.tracks[queue.currentIndex];
      if (track) emitPlayerEvent('complete', track, audio.currentTime, {
        listenedDeltaMs: Math.round(listenedSinceProgressRef.current),
      });
      listenedSinceProgressRef.current = 0;
      if (track) positionsRef.current[track.id] = 0;
      if (nextQueueIndex(queue) === queue.currentIndex && track) {
        // Selecting the same queue object does not reload its source. Native
        // ended playback must explicitly rewind/restart, including a singleton
        // queue or a release containing only the current track.
        audio.currentTime = (track.startMs ?? 0) / 1000;
        lastContinuousPositionRef.current = 0;
        setCurrentTime(0);
        void play();
        return;
      }
      next();
    };
    const handleError = () => {
      if (!ownsNativeSource() || retryTimerRef.current !== null) return;
      playAttemptRef.current += 1; // native error owns the terminal/recovery outcome
      const generation = sourceGenerationRef.current;
      // Decode/unsupported failures will not improve by downloading again.
      // A protected URL can also surface an expired HTTP grant as unsupported
      // media. Reauthorization remains bounded and may deny access outright.
      const retryable = !audio.error || audio.error.code === 2
        || (Boolean(loadedSourceRef.current?.candidate.assetId ?? source?.assetId) && audio.error.code === 4);
      const position = pendingPositionRef.current?.generation === generation
        ? pendingPositionRef.current.position : audio.currentTime;
      pendingPositionRef.current = null;
      if (retryable && currentTrack && scheduleRecovery(currentTrack, position, generation)) return;
      if (skipFailed()) return;
      autoplayRef.current = false; setPlaybackRequested(false);
      void pauseForSourceTransition(audio);
      setStatus('unavailable');
      setError('La fuente dejó de estar disponible. Puedes reintentar o pasar a la siguiente pista.');
      const track = queue.tracks[queue.currentIndex];
      if (track) emitPlayerEvent('error', track, audio.currentTime, { code: audio.error?.code });
    };

    const handleSeeking = () => {
      if (!ownsNativeSource() || pendingPositionRef.current) return;
      const start = (currentTrack?.startMs ?? 0) / 1000;
      if (audio.currentTime < start) audio.currentTime = start;
      if (currentTrack?.endMs && audio.currentTime > currentTrack.endMs / 1000) audio.currentTime = currentTrack.endMs / 1000;
    };
    const handleRate = () => { if (nativeVideo && ownsNativeSource()) updatePlaybackRate(audio.playbackRate); };
    const handleVolume = () => { if (nativeVideo && ownsNativeSource()) { updateVolume(audio.volume); updateMuted(audio.muted); } };
    audio.addEventListener('seeking', handleSeeking);
    audio.addEventListener('ratechange', handleRate);
    audio.addEventListener('volumechange', handleVolume);
    audio.addEventListener('timeupdate', handleTime);
    audio.addEventListener('durationchange', handleDuration);
    audio.addEventListener('loadedmetadata', handlePrepared);
    audio.addEventListener('canplay', handlePrepared);
    audio.addEventListener('playing', handlePlaying);
    audio.addEventListener('waiting', handleWaiting);
    audio.addEventListener('pause', handlePause);
    audio.addEventListener('ended', handleEnded);
    audio.addEventListener('error', handleError);
    return () => {
      audio.removeEventListener('seeking', handleSeeking);
      audio.removeEventListener('ratechange', handleRate);
      audio.removeEventListener('volumechange', handleVolume);
      audio.removeEventListener('timeupdate', handleTime);
      audio.removeEventListener('durationchange', handleDuration);
      audio.removeEventListener('loadedmetadata', handlePrepared);
      audio.removeEventListener('canplay', handlePrepared);
      audio.removeEventListener('playing', handlePlaying);
      audio.removeEventListener('waiting', handleWaiting);
      audio.removeEventListener('pause', handlePause);
      audio.removeEventListener('ended', handleEnded);
      audio.removeEventListener('error', handleError);
    };
  }, [cancelRetry, currentTrack, flushProgress, nativeVideo, next, pauseForSourceTransition, play, queue, scheduleRecovery, skipFailed, source?.assetId, updateMuted, updateVolume]);

  useEffect(() => {
    const engines = [legacyAudioRef.current, nativeVideoRef.current];
    const pendingPauses = pendingInternalPausesRef.current;
    return () => {
      cancelRetry();
      sourceGenerationRef.current += 1;
      playAttemptRef.current += 1;
      loadedSourceRef.current = null;
      autoplayRef.current = false; setPlaybackRequested(false);
      for (const pending of pendingPauses.values()) pending.cancel();
      for (const audio of engines) {
        if (!audio) continue;
        if (!audio.paused) audio.pause();
        if (!audio.hasAttribute('src') && !audio.srcObject) continue;
        audio.removeAttribute('src'); audio.srcObject = null; audio.load();
      }
    };
  }, [cancelRetry]);

  useEffect(() => {
    const engine = audioRef.current;
    const pendingPauses = pendingInternalPausesRef.current;
    return () => {
      if (!engine) return;
      // Releasing an engine calls load(), which discards its queued media
      // tasks. Resolve/remove its wait before that task can be discarded.
      pendingPauses.get(engine)?.cancel();
      if (!engine.hasAttribute('src')) return;
      engine.pause(); engine.removeAttribute('src'); engine.load();
    };
  }, [nativeVideo]);

  useEffect(() => { retryRef.current = 0; }, [currentTrack?.id, quality]);

  useEffect(() => {
    const audio = audioRef.current;
    cancelRetry();
    const controller = new AbortController();
    let torrentSession: TorrentAudioSession | null = null;
    setTorrentProgress(null);
    const generation = sourceGenerationRef.current + 1;
    sourceGenerationRef.current = generation;
    loadedSourceRef.current = null;
    pendingPositionRef.current = null;
    nativeGestureRef.current = false;
    let removePreparationListener: () => void = () => undefined;
    lastContinuousPositionRef.current = 0;
    listenedSinceProgressRef.current = 0;
    setCurrentTime(0);
    setDuration(0);
    setResolvedQuality(null);
    setError(null);
    if (!audio) return;
    const paused = pauseForSourceTransition(audio);
    if (!currentTrack || !source) {
      if (!currentTrack) { autoplayRef.current = false; setPlaybackRequested(false); }
      void paused.then(() => {
        if (controller.signal.aborted || generation !== sourceGenerationRef.current) return;
        audio.removeAttribute('src');
        audio.load();
      });
      setStatus(currentTrack ? 'unavailable' : 'idle');
      return () => controller.abort();
    }
    setStatus('loading');
    let watchdog: number | undefined;
    let lastMediaPosition = audio.currentTime;
    const clearWatchdog = () => {
      if (watchdog !== undefined) window.clearTimeout(watchdog);
      watchdog = undefined;
    };
    cancelWatchdogRef.current = clearWatchdog;
    const waitForMedia = () => {
      // Repeated buffering events must not postpone the same failure deadline.
      if (watchdog !== undefined) return;
      watchdog = window.setTimeout(() => {
        if (controller.signal.aborted || generation !== sourceGenerationRef.current) return;
        if (skipFailed()) return;
        autoplayRef.current = false; setPlaybackRequested(false);
        playAttemptRef.current += 1;
        void pauseForSourceTransition(audio);
        setStatus('unavailable');
        setError(navigator.onLine === false ? 'Sin conexión. Vuelve a intentar cuando estés en línea.'
          : 'No quedan fuentes reproducibles. Puedes reintentar o elegir otra música.');
      }, 15000);
    };
    armWatchdogRef.current = waitForMedia;
    const ownsPreparedSource = () => !controller.signal.aborted && generation === sourceGenerationRef.current
      && loadedSourceRef.current?.generation === generation
      && loadedSourceRef.current?.url === audio.currentSrc
      && !pendingPositionRef.current;
    const confirmedPlaying = () => {
      if (ownsPreparedSource() && autoplayRef.current && !audio.paused) clearWatchdog();
    };
    const confirmedWaiting = () => {
      if (ownsPreparedSource() && autoplayRef.current && !audio.paused) waitForMedia();
    };
    const mediaProgress = () => {
      if (!ownsPreparedSource() || audio.paused || !autoplayRef.current) return;
      const position = audio.currentTime;
      if (position > lastMediaPosition) clearWatchdog();
      lastMediaPosition = position;
    };
    const prepared = () => { if (ownsPreparedSource() && !autoplayRef.current) clearWatchdog(); };
    audio.addEventListener('playing', confirmedPlaying);
    audio.addEventListener('timeupdate', mediaProgress);
    audio.addEventListener('waiting', confirmedWaiting);
    audio.addEventListener('canplay', prepared);
    audio.addEventListener('pause', prepared);
    waitForMedia();
    void (async () => {
      try {
        await paused;
        if (controller.signal.aborted || generation !== sourceGenerationRef.current) return;
        audio.removeAttribute('src');
        let resolved: { source: PlayerSource; url: string; version?: string } | null = null;
        for (const candidate of candidates) {
          if (failedSourcesRef.current.has(candidate)) continue;
          if (generation !== sourceGenerationRef.current || controller.signal.aborted
            || recoveryQueueRef.current.tracks[recoveryQueueRef.current.currentIndex] !== currentTrack) return;
          try {
            if (candidate.torrentSource) {
              const { createTorrentAudioSession } = await import('../utils/torrentAudio');
              if (controller.signal.aborted) return;
              torrentSession = await createTorrentAudioSession({ source: candidate.torrentSource, audio,
                signal: controller.signal, onProgress: (progress) => {
                  if (generation === sourceGenerationRef.current && !controller.signal.aborted) setTorrentProgress(progress);
                } });
              if (controller.signal.aborted || generation !== sourceGenerationRef.current) { torrentSession.destroy(); return; }
              resolved = { source: candidate, url: audio.src };
              break;
            }
            const accessId = candidate.assetId ?? (candidate.legacySourceId ? `legacy:${candidate.legacySourceId}` : null);
            const access = accessId ? await authorizePlayerAsset(accessId, controller.signal) : null;
            if (generation !== sourceGenerationRef.current || controller.signal.aborted
            || recoveryQueueRef.current.tracks[recoveryQueueRef.current.currentIndex] !== currentTrack) return;
            resolved = { source: candidate, url: access?.url ?? candidate.url, version: access?.sourceVersion };
            break;
          } catch (reason) {
            if (generation !== sourceGenerationRef.current || controller.signal.aborted
            || recoveryQueueRef.current.tracks[recoveryQueueRef.current.currentIndex] !== currentTrack) return;
            const status = reason && typeof reason === 'object' && 'status' in reason ? reason.status : null;
            // A full rendition can be denied while a preview of this same
            // recording is allowed. Never fall back to a different track.
            if (status !== 401 && status !== 403 && status !== 404) throw reason;
          }
        }
        if (!resolved?.url) throw Object.assign(new Error('No authorized source'), { status: 403 });
        if (!resolved.source.torrentSource) audio.src = resolved.url;
        loadedSourceRef.current = { generation, trackId: currentTrack.id, url: audio.src, version: resolved.version,
          preview: resolved.source.preview, quality: resolved.source.quality, candidate: resolved.source };
        setResolvedQuality(resolved.source.quality);
        const start = (currentTrack.startMs ?? 0) / 1000;
        const stored = positionsRef.current[currentTrack.id] ?? start;
        const storedPosition = stored < start || (currentTrack.endMs && stored >= currentTrack.endMs / 1000) ? start : stored;
        const savedPosition = resolved.source.preview && currentTrack.previewEndMs
          && storedPosition * 1000 >= currentTrack.previewEndMs ? 0 : storedPosition;
        if (savedPosition > 0) {
          const pending = { generation, trackId: currentTrack.id, url: audio.src, position: savedPosition };
          pendingPositionRef.current = pending;
          const restorePosition = () => {
            if (controller.signal.aborted || generation !== sourceGenerationRef.current
              || pendingPositionRef.current !== pending || loadedSourceRef.current?.trackId !== pending.trackId
              || loadedSourceRef.current.url !== pending.url || audio.currentSrc !== pending.url
              || audio.readyState < HTMLMediaElement.HAVE_METADATA) return;
            // Nonzero default-position seeks before metadata can stall WebKit
            // after a rendition change. Restore only the current source, once.
            audio.currentTime = pending.position;
            positionsRef.current[pending.trackId] = pending.position;
            lastContinuousPositionRef.current = pending.position;
            setCurrentTime(Math.max(0, pending.position - (currentTrack.startMs ?? 0) / 1000));
            pendingPositionRef.current = null;
            removePreparationListener();
            if (autoplayRef.current) void play();
            else setStatus('paused');
          };
          audio.addEventListener('loadedmetadata', restorePosition);
          removePreparationListener = () => { audio.removeEventListener('loadedmetadata', restorePosition); };
        }
        audio.load();
        emitPlayerEvent('quality_selected', currentTrack, savedPosition, { quality: resolved.source.quality, automatic: quality === 'auto' });
        if (!(savedPosition > 0) && autoplayRef.current) void play();
      } catch (caught) {
        if (generation !== sourceGenerationRef.current) return;
        const httpStatus = caught && typeof caught === 'object' && 'status' in caught ? caught.status : null;
        const transient = httpStatus === null || httpStatus === 408 || httpStatus === 429
          || (typeof httpStatus === 'number' && httpStatus >= 500);
        if (transient && scheduleRecovery(currentTrack, positionsRef.current[currentTrack.id] ?? 0, generation)) return;
        const denied = httpStatus === 401 || httpStatus === 403 || httpStatus === 404;
        if (!denied && skipFailed()) return;
        if (denied) stopDeniedSelection(currentTrack.id);
        autoplayRef.current = false; setPlaybackRequested(false);
        setStatus('unavailable');
        setError('El servidor no autorizó una fuente vigente para esta pista.');
        logger.warn('Global player could not refresh asset authorization', caught);
      }
    })();
    return () => {
      controller.abort(); torrentSession?.destroy(); clearWatchdog();
      removePreparationListener();
      if (pendingPositionRef.current?.generation === generation) pendingPositionRef.current = null;
      audio.removeEventListener('playing', confirmedPlaying);
      audio.removeEventListener('timeupdate', mediaProgress);
      audio.removeEventListener('waiting', confirmedWaiting);
      if (armWatchdogRef.current === waitForMedia) armWatchdogRef.current = () => undefined;
      audio.removeEventListener('canplay', prepared);
      audio.removeEventListener('pause', prepared);
    };
  }, [audioRef, pauseForSourceTransition, cancelRetry, candidates, currentTrack, play, quality, scheduleRecovery, skipFailed, source, sourceRefresh, stopDeniedSelection]);

  useEffect(() => {
    const connection = (navigator as Navigator & { connection?: { saveData?: boolean } }).connection;
    if (!artistContextRef.current || connection?.saveData || navigator.onLine === false) return;
    const upcoming = queue.tracks[queue.currentIndex + 1];
    const candidate = orderedSources(upcoming ?? null, quality).find((item) => item.assetId);
    if (!candidate?.assetId) return;
    const controller = new AbortController();
    const timer = window.setTimeout(() => controller.abort(), 5000);
    // Warm authorization only; selection still obtains a fresh grant and never
    // trusts this result as proof of decoding or a reason to hide content.
    void Promise.resolve(authorizePlayerAsset(candidate.assetId, controller.signal)).catch(() => undefined)
      .finally(() => window.clearTimeout(timer));
    return () => { controller.abort(); window.clearTimeout(timer); };
  }, [quality, queue.currentIndex, queue.tracks]);

  useEffect(() => {
    const loaded = loadedSourceRef.current;
    const candidate = loaded?.candidate;
    const accessId = candidate?.assetId ?? (candidate?.legacySourceId ? `legacy:${candidate.legacySourceId}` : null);
    if (!loaded || !accessId || !currentTrack) return;
    const controller = new AbortController();
    let busy = false;
    const validate = async () => {
      if (busy || controller.signal.aborted || document.visibilityState === 'hidden' || navigator.onLine === false) return;
      busy = true;
      try {
        const checked = await authorizePlayerAsset(accessId, controller.signal);
        if (controller.signal.aborted || loaded.generation !== sourceGenerationRef.current) return;
        if (loaded.version && checked.sourceVersion && checked.sourceVersion !== loaded.version) {
          positionsRef.current[currentTrack.id] = pendingPositionRef.current?.position ?? audioRef.current?.currentTime ?? 0;
          sourceGenerationRef.current += 1;
          playAttemptRef.current += 1;
          if (audioRef.current) void pauseForSourceTransition(audioRef.current);
          setStatus('loading');
          setSourceRefresh((value) => value + 1);
        }
      }
      catch (reason) {
        if (controller.signal.aborted || loaded.generation !== sourceGenerationRef.current) return;
        const code = reason && typeof reason === 'object' && 'status' in reason ? reason.status : null;
        if (code === 401 || code === 403 || code === 404) {
          controller.abort();
          stopDeniedSelection(currentTrack.id);
          setStatus('unavailable');
          setError('Esta música ya no está disponible para escuchar. Elige otra pista.');
        }
      } finally { busy = false; }
    };
    const onVisible = () => { void validate(); };
    const interval = window.setInterval(onVisible, 60000);
    document.addEventListener('visibilitychange', onVisible);
    return () => { controller.abort(); window.clearInterval(interval); document.removeEventListener('visibilitychange', onVisible); };
  }, [currentTrack, pauseForSourceTransition, resolvedQuality, sourceRefresh, stopDeniedSelection]);

  useEffect(() => {
    const audio = audioRef.current;
    if (!audio) return;
    audio.muted = muted;
    audio.volume = normalizedVolume(volume, currentTrack?.loudnessLufs);
  }, [nativeVideo, currentTrack?.loudnessLufs, muted, volume]);

  useEffect(() => {
    const handleLoad = (event: Event) => {
      const detail = (event as CustomEvent<LoadPlayerTrackDetail>).detail;
      if (!detail?.track || !isTrack(detail.track)) return;
      const tracks = detail.queue?.filter(isTrack) ?? [detail.track];
      retireSelection();
      if (typeof detail.startPositionMs === 'number' && Number.isFinite(detail.startPositionMs) && detail.startPositionMs >= 0) {
        positionsRef.current[detail.track.id] = detail.startPositionMs / 1000;
      }
      artistContextRef.current = null;
      if (detail.autoplay !== false) { explicitPauseRef.current = false; setPausedByUser(false); }
      failedTracksRef.current.clear();
      failedSourcesRef.current.clear();
      setUnavailableTrackIds([]);
      autoplayRef.current = detail.autoplay !== false; setPlaybackRequested(detail.autoplay !== false);
      if (detail.mediaMode) updateMediaMode(detail.mediaMode);
      dispatchQueue({ type: 'replace', tracks, currentTrackId: detail.track.id });
    };
    const handleRadioTakeover = () => { artistContextRef.current = null; pause(false); };
    window.addEventListener(PLAYER_LOAD_TRACK_EVENT, handleLoad as EventListener);
    window.addEventListener(RADIO_TAKEOVER_EVENT, handleRadioTakeover);
    return () => {
      window.removeEventListener(PLAYER_LOAD_TRACK_EVENT, handleLoad as EventListener);
      window.removeEventListener(RADIO_TAKEOVER_EVENT, handleRadioTakeover);
    };
  }, [dispatchQueue, pause, retireSelection]);

  useEffect(() => installPlayerAnalyticsBridge(), []);

  useEffect(() => {
    if (!('mediaSession' in navigator)) return;
    const mediaSession = navigator.mediaSession;
    if (currentTrack && typeof MediaMetadata !== 'undefined') {
      mediaSession.metadata = new MediaMetadata({
        title: currentTrack.title,
        artist: currentTrack.artist,
        album: currentTrack.releaseId,
        artwork: currentTrack.artworkUrl ? [{ src: currentTrack.artworkUrl }] : undefined,
      });
    } else {
      mediaSession.metadata = null;
    }
    const handlers: Partial<Record<MediaSessionAction, MediaSessionActionHandler>> = {
      play: () => { void play(); }, pause: () => pause(), previoustrack: previous, nexttrack: next,
      seekto: (details) => {
        if (details.seekTime !== undefined) seek(details.seekTime);
      },
    };
    for (const [action, handler] of Object.entries(handlers)) {
      try { mediaSession.setActionHandler(action as MediaSessionAction, handler); }
      catch { /* Some browsers expose only a subset of Media Session actions. */ }
    }
    return () => {
      ['play', 'pause', 'previoustrack', 'nexttrack', 'seekto'].forEach((action) => {
        try {
          mediaSession.setActionHandler(action as MediaSessionAction, null);
        } catch {
          // Older browsers expose MediaSession with a smaller action set.
        }
      });
    };
  }, [currentTrack, next, pause, play, previous, seek]);

  useEffect(() => {
    if (!('mediaSession' in navigator)) return;
    navigator.mediaSession.playbackState = status === 'playing' ? 'playing' : currentTrack ? 'paused' : 'none';
    if (duration > 0 && navigator.mediaSession.setPositionState) {
      try { navigator.mediaSession.setPositionState({ duration, playbackRate, position: Math.min(duration, Math.max(0, currentTime)) }); }
      catch { /* OS media position reporting is optional. */ }
    }
  }, [currentTime, currentTrack, duration, playbackRate, status]);

  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.defaultPrevented || event.repeat || event.isComposing || event.ctrlKey || event.metaKey) return;
      const target = event.target instanceof Element ? event.target : null;
      if ((target instanceof HTMLElement && target.isContentEditable) || target?.closest(
        'input, textarea, select, button, video, audio, a[href], [role="button"], [role="slider"], [role="combobox"], [role="menu"], [role="listbox"], [role="option"], [role="dialog"], [role="alertdialog"]',
      )) return;
      if (!currentTrack) return;
      if (event.code === 'Space' && !event.altKey) {
        event.preventDefault();
        if (autoplayRef.current || status === 'playing' || status === 'buffering') pause(); else void play();
      } else if (event.altKey && event.key === 'ArrowRight') {
        event.preventDefault();
        next();
      } else if (event.altKey && event.key === 'ArrowLeft') {
        event.preventDefault();
        previous();
      } else if (event.key.toLowerCase() === 'm' && !event.altKey) {
        updateMuted((current) => !current);
      }
    };
    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [currentTrack, next, pause, play, previous, status, updateMuted]);

  useEffect(() => {
    // Never replace a saved listener queue with the initial unresolved empty
    // state, or label an unknown authenticated account as anonymous.
    if (!identityReady || !listeningHydrated || ownerPartyId === undefined) return undefined;
    const timeout = window.setTimeout(() => {
      try {
        const persistedQueue: QueueState = {
          ...queue,
          tracks: queue.tracks.filter((track) => !track.sources.some((source) => source.torrentSource instanceof File)).map((track) => ({
            ...track,
            artworkUrl: null,
            sources: track.sources.map((item) => item.assetId
              ? { ...item, url: '', expiresAt: undefined }
              : item),
          })),
        };
        persistedQueue.currentIndex = currentTrack
          ? persistedQueue.tracks.findIndex((track) => track.id === currentTrack.id) : -1;
        window.localStorage.setItem(PERSISTENCE_KEY, JSON.stringify({
          ownerPartyId,
          queue: persistedQueue,
          volume,
          muted,
          quality,
          positions: positionsRef.current,
          pausedByUser,
        } satisfies PersistedPlayerState));
      } catch {
        // Storage can be unavailable in private mode. Playback remains usable.
      }
    }, 250);
    return () => window.clearTimeout(timeout);
  }, [currentTime, currentTrack, identityReady, listeningHydrated, muted, ownerPartyId, pausedByUser, quality, queue, volume]);

  const playTrack = useCallback((track: PlayerTrack, replacementQueue?: PlayerTrack[], autoplay = true) => {
    queueIntentRef.current = true;
    artistContextRef.current = null;
    explicitPauseRef.current = !autoplay; setPausedByUser(!autoplay);
    failedTracksRef.current.clear();
    failedSourcesRef.current.clear();
    setUnavailableTrackIds([]);
    if (track === currentTrack && !replacementQueue) {
      if (autoplay) void play(); else pause();
      return;
    }
    const tracks = replacementQueue?.filter(isTrack) ?? [track];
    retireSelection();
    autoplayRef.current = autoplay; setPlaybackRequested(autoplay);
    dispatchQueue({ type: 'replace', tracks, currentTrackId: track.id });
  }, [currentTrack, dispatchQueue, pause, play, retireSelection]);

  const beginArtist = useCallback((key: string) => {
    if (artistContextRef.current === key) return;
    artistContextRef.current = key;
    failedTracksRef.current.clear();
    failedSourcesRef.current.clear();
    setUnavailableTrackIds([]);
    autoplayRef.current = false; setPlaybackRequested(false);
    retireSelection();
    window.dispatchEvent(new CustomEvent(PLAYER_TAKEOVER_EVENT));
    dispatchQueue({ type: 'clear' });
  }, [dispatchQueue, retireSelection]);

  const loadArtist = useCallback((key: string, tracks: PlayerTrack[]) => {
    if (artistContextRef.current !== key) return;
    const seen = new Set<string>();
    const eligible = tracks.filter((track) => isTrack(track) && !seen.has(track.id) && Boolean(seen.add(track.id)));
    autoplayRef.current = !explicitPauseRef.current; setPlaybackRequested(!explicitPauseRef.current);
    dispatchQueue({ type: 'replace', tracks: eligible });
  }, [dispatchQueue]);

  const setMediaMode = useCallback((mode: 'audio' | 'video') => {
    if (audioRef.current && currentTrack) positionsRef.current[currentTrack.id] = audioRef.current.currentTime;
    updateMediaMode(mode);
  }, [audioRef, currentTrack]);
  // Native load() restores defaultPlaybackRate. Keep it aligned with the
  // shared preference before another rendition or chapter is loaded.
  useEffect(() => {
    const audio = audioRef.current;
    if (!audio) return;
    audio.defaultPlaybackRate = playbackRate;
    audio.playbackRate = playbackRate;
  }, [nativeVideo, playbackRate, candidates]);

  const contextValue = useMemo<PlayerContextValue>(() => ({
    queue,
    torrentProgress,
    unavailableTrackIds,
    currentTrack,
    status,
    playbackRequested,
    error,
    currentTime,
    duration,
    volume,
    muted,
    quality,
    resolvedQuality,
    mediaMode, playbackRate, setMediaMode,
    setPlaybackRate: (rate) => { if (Number.isFinite(rate) && rate >= 0.5 && rate <= 2) updatePlaybackRate(rate); },
    requestFullscreen: async () => {
      const video = nativeVideoRef.current as (HTMLVideoElement & { webkitEnterFullscreen?: () => void }) | null;
      if (!video) return;
      if (video.requestFullscreen) await video.requestFullscreen();
      else if (video.webkitEnterFullscreen) video.webkitEnterFullscreen();
      else throw new DOMException('Fullscreen unavailable', 'NotSupportedError');
    },
    requestPictureInPicture: async () => {
      const video = nativeVideoRef.current;
      if (!video?.requestPictureInPicture) throw new DOMException('Picture in picture unavailable', 'NotSupportedError');
      await video.requestPictureInPicture();
    },
    playTrack,
    beginArtist,
    loadArtist,
    enqueue: (tracks) => dispatchQueue({ type: 'enqueue', tracks: tracks.filter(isTrack) }),
    selectTrack,
    removeTrack: (index) => {
      if (index === recoveryQueueRef.current.currentIndex) retireSelection();
      dispatchQueue({ type: 'remove', index });
    },
    moveTrack: (from, to) => dispatchQueue({ type: 'move', from, to }),
    clearQueue: () => {
      retireSelection();
      pause();
      dispatchQueue({ type: 'clear' });
    },
    togglePlayback: () => { if (autoplayRef.current || status === 'playing' || status === 'buffering') pause(); else void play(); },
    play,
    pause,
    next,
    previous,
    seek,
    setVolume: (nextVolume) => updateVolume(Math.min(Math.max(nextVolume, 0), 1)),
    setMuted: updateMuted,
    setQuality: updateQuality,
    setShuffle: (enabled) => dispatchQueue({ type: 'shuffle', enabled }),
    setRepeat: (mode) => dispatchQueue({ type: 'repeat', mode }),
  }), [
    beginArtist, loadArtist, playbackRequested, retireSelection, torrentProgress, unavailableTrackIds, currentTime, currentTrack, duration, error, muted, next, pause, play, playTrack, previous,
    quality, queue, resolvedQuality, seek, selectTrack, status, volume, dispatchQueue, updateMuted, updateQuality, updateVolume, mediaMode, playbackRate, setMediaMode,
  ]);

  return (
    <PlayerContext.Provider value={contextValue}>
      {children}
      {/* Engines persist across route changes; only the active engine receives a source. */}
      {/* eslint-disable-next-line jsx-a11y/media-has-caption */}
      <audio ref={legacyAudioRef} preload="metadata" aria-hidden="true" />
      <div style={{ display: nativeVideo && mediaMode === 'video' ? 'block' : 'none',
        position: 'fixed', right: 12, bottom: 'calc(var(--tdf-global-player-height, 110px) + 12px)',
        width: 'min(480px, calc(100vw - 24px))', zIndex: 1299, background: '#000', borderRadius: 12, overflow: 'hidden' }}>
        {/* eslint-disable-next-line jsx-a11y/media-has-caption */}
        <video ref={nativeVideoRef} controls playsInline preload="metadata" poster={currentTrack?.artworkUrl ?? undefined}
          aria-label={currentTrack ? `Video: ${currentTrack.title}` : 'Video TDF'}
          onPointerDown={() => { nativeGestureRef.current = true; }}
          onKeyDown={() => { nativeGestureRef.current = true; }}
          onPlay={() => {
            const video = nativeVideoRef.current;
            if (!video || !nativeVideo || video.paused
              || loadedSourceRef.current?.generation !== sourceGenerationRef.current
              || loadedSourceRef.current?.trackId !== currentTrack?.id
              || recoveryQueueRef.current.tracks[recoveryQueueRef.current.currentIndex] !== currentTrack
              || loadedSourceRef.current?.url !== video.currentSrc || pendingPositionRef.current) return;
            if (!autoplayRef.current && !nativeGestureRef.current) { void pauseForSourceTransition(video); return; }
            if (nativeGestureRef.current) {
              explicitPauseRef.current = false; setPausedByUser(false);
              queueIntentRef.current = true;
              if (!autoplayRef.current) playAttemptRef.current += 1;
            }
            nativeGestureRef.current = false;
            autoplayRef.current = true; setPlaybackRequested(true);
            armWatchdogRef.current();
            window.dispatchEvent(new CustomEvent(PLAYER_TAKEOVER_EVENT));
          }}
          style={{ display: 'block', width: '100%', maxHeight: '45vh' }} />
      </div>
    </PlayerContext.Provider>
  );
}

export function usePlayer(): PlayerContextValue {
  const value = useContext(PlayerContext);
  if (!value) throw new Error('usePlayer must be used within PlayerProvider');
  return value;
}

export function useOptionalPlayer() { return useContext(PlayerContext); }
