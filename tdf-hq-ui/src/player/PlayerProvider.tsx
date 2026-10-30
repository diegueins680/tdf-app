import type { TorrentAudioSession, TorrentPlaybackProgress } from '../utils/torrentAudio';
import {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useReducer,
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
}

interface PlayerContextValue {
  unavailableTrackIds: string[];
  torrentProgress: TorrentPlaybackProgress | null;
  queue: QueueState;
  currentTrack: PlayerTrack | null;
  status: PlaybackStatus;
  error: string | null;
  currentTime: number;
  duration: number;
  volume: number;
  muted: boolean;
  quality: PlayerQuality | 'auto';
  resolvedQuality: PlayerQuality | null;
  beginArtist: (key: string) => void;
  loadArtist: (key: string, tracks: PlayerTrack[]) => void;
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
    const volume = typeof parsed?.volume === 'number' && Number.isFinite(parsed.volume)
      ? Math.min(Math.max(parsed.volume, 0), 1)
      : 0.8;
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
  const sources = track.sources.some((item) => item.assetId)
    ? track.sources.filter((item) => item.assetId) : track.sources;
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
  return [...order(sources.filter((item) => !item.preview)),
    ...order(sources.filter((item) => item.preview))];
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
  const [queue, reduceQueue] = useReducer(queueReducer, persisted.queue);
  const queueIntentRef = useRef(false);
  const [listeningHydrated, setListeningHydrated] = useState(identityReady);
  const dispatchQueue = useCallback((action: QueueAction) => {
    queueIntentRef.current = true;
    reduceQueue(action);
  }, []);
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
  const audioRef = useRef<HTMLAudioElement | null>(null);
  const positionsRef = useRef<Record<string, number>>(persisted.positions);
  const autoplayRef = useRef(false);
  const explicitPauseRef = useRef(false);
  const artistContextRef = useRef<string | null>(null);
  const failedTracksRef = useRef(new Set<string>());
  const failedSourcesRef = useRef(new Set<PlayerSource>());
  const [unavailableTrackIds, setUnavailableTrackIds] = useState<string[]>([]);
  const retryRef = useRef(0);
  const cancelWatchdogRef = useRef<() => void>(() => undefined);
  const retryTimerRef = useRef<number | null>(null);
  const [torrentProgress, setTorrentProgress] = useState<TorrentPlaybackProgress | null>(null);
  const [resolvedQuality, setResolvedQuality] = useState<PlayerQuality | null>(null);
  const sourceGenerationRef = useRef(0);
  const playAttemptRef = useRef(0);
  const loadedSourceRef = useRef<{ trackId: string; url: string; preview?: boolean; quality: PlayerQuality; candidate: PlayerSource } | null>(null);
  const pendingPositionRef = useRef<{ generation: number; trackId: string; url: string; position: number } | null>(null);
  const lastContinuousPositionRef = useRef(0);
  const listenedSinceProgressRef = useRef(0);
  const currentTrack = queue.currentIndex >= 0 ? queue.tracks[queue.currentIndex] ?? null : null;
  const candidates = useMemo(() => orderedSources(currentTrack, quality), [currentTrack, quality]);
  const source = candidates[0] ?? null;

  useEffect(() => {
    if (!identityReady || listeningHydrated) return;
    // Any explicit queue command, including clearing an empty queue, supersedes
    // persisted state. Hydration cannot replace a user's newer selection.
    if (!queueIntentRef.current) {
      const saved = loadPersistedState(ownerPartyId, restoreListening);
      positionsRef.current = saved.positions;
      reduceQueue({ type: 'restore', queue: saved.queue });
    }
    setListeningHydrated(true);
  }, [identityReady, listeningHydrated, ownerPartyId, restoreListening]);

  const cancelRetry = useCallback(() => {
    if (retryTimerRef.current !== null) window.clearTimeout(retryTimerRef.current);
    retryTimerRef.current = null;
  }, []);

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

  const recoveryQueueRef = useRef(queue);
  recoveryQueueRef.current = queue;
  const skipFailed = useCallback((): boolean => {
    const queue = recoveryQueueRef.current;
    if (!currentTrack || navigator.onLine === false) return false;
    const loaded = loadedSourceRef.current;
    if (loaded?.trackId === currentTrack.id) {
      failedSourcesRef.current.add(loaded.candidate);
      if (candidates.some((candidate) => !failedSourcesRef.current.has(candidate))) {
        const audio = audioRef.current;
        positionsRef.current[currentTrack.id] = pendingPositionRef.current?.position ?? audio?.currentTime ?? 0;
        cancelRetry();
        retryRef.current = 0;
        sourceGenerationRef.current += 1;
        playAttemptRef.current += 1;
        audio?.pause();
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
    audioRef.current?.pause();
    dispatchQueue({ type: 'select', index: nextIndex });
    return true;
  }, [cancelRetry, candidates, currentTrack, dispatchQueue]);

  const flushProgress = useCallback((track: PlayerTrack, audio: HTMLAudioElement) => {
    const listenedDeltaMs = Math.round(listenedSinceProgressRef.current);
    if (listenedDeltaMs <= 0) return;
    listenedSinceProgressRef.current = 0;
    emitPlayerEvent('progress', track, audio.currentTime, { listenedDeltaMs });
  }, []);

  const pause = useCallback((explicit = true) => {
    if (explicit) explicitPauseRef.current = true;
    cancelWatchdogRef.current();
    const audio = audioRef.current;
    if (!audio) return;
    playAttemptRef.current += 1;
    autoplayRef.current = false;
    cancelRetry();
    audio.pause();
    setStatus(currentTrack ? 'paused' : 'idle');
    if (currentTrack) {
      flushProgress(currentTrack, audio);
      emitPlayerEvent('pause', currentTrack, audio.currentTime);
    }
  }, [cancelRetry, currentTrack, flushProgress]);

  const play = useCallback(async () => {
    explicitPauseRef.current = false;
    const audio = audioRef.current;
    if (!audio || !currentTrack || !source) {
      setStatus('unavailable');
      setError('No hay una fuente autorizada disponible para esta pista.');
      return;
    }
    autoplayRef.current = true;
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
    const attempt = playAttemptRef.current + 1;
    playAttemptRef.current = attempt;
    window.dispatchEvent(new CustomEvent(PLAYER_TAKEOVER_EVENT));
    try {
      await audio.play();
      if (generation !== sourceGenerationRef.current || attempt !== playAttemptRef.current) return;
      // Native 'playing' owns the visible playback state. A fulfilled promise
      // alone is insufficient, especially when a pause races its settlement.
      setError(null);
      emitPlayerEvent('play_start', currentTrack, audio.currentTime, { quality: loadedSourceRef.current?.quality ?? source.quality });
    } catch (caught) {
      if (generation !== sourceGenerationRef.current || attempt !== playAttemptRef.current) return;
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
      autoplayRef.current = false;
      audio.pause();
      // Browser policy rejection is recoverable by a fresh user gesture. Keep
      // the selected source so that retrying direct/legacy audio also works.
      setStatus(blocked ? 'paused' : mediaError || unsupported ? 'unavailable' : 'error');
      setError(blocked ? 'Pulsa Reproducir para permitir el audio en este navegador.'
        : recoverable ? 'La fuente dejó de estar disponible. Puedes reintentar o pasar a la siguiente pista.'
          : 'No se pudo iniciar la reproducción. Revisa la conexión o vuelve a intentarlo.');
      logger.warn('Global player could not start playback', caught);
    }
  }, [currentTrack, scheduleRecovery, skipFailed, source]);

  const selectTrack = useCallback((index: number) => {
    explicitPauseRef.current = false;
    failedTracksRef.current.clear();
    failedSourcesRef.current.clear();
    setUnavailableTrackIds([]);
    if (index === queue.currentIndex) { void play(); return; }
    autoplayRef.current = true;
    dispatchQueue({ type: 'select', index });
  }, [dispatchQueue, play, queue.currentIndex]);

  const seek = useCallback((seconds: number) => {
    const audio = audioRef.current;
    if (!audio || !Number.isFinite(seconds)) return;
    const nextTime = Math.min(Math.max(seconds, 0), Number.isFinite(audio.duration) ? audio.duration : seconds);
    const pending = pendingPositionRef.current;
    if (pending?.generation === sourceGenerationRef.current) pending.position = nextTime;
    else audio.currentTime = nextTime;
    if (currentTrack) positionsRef.current[currentTrack.id] = nextTime;
    lastContinuousPositionRef.current = nextTime;
    setCurrentTime(nextTime);
    if (currentTrack) emitPlayerEvent('seek', currentTrack, nextTime);
  }, [currentTrack]);

  const next = useCallback(() => {
    const nextIndex = nextQueueIndex(queue);
    if (nextIndex < 0) {
      pause(false);
      return;
    }
    if (nextIndex === queue.currentIndex) {
      seek(0);
      void play();
      return;
    }
    autoplayRef.current = true;
    dispatchQueue({ type: 'select', index: nextIndex });
  }, [dispatchQueue, pause, play, queue, seek]);

  const previous = useCallback(() => {
    const audio = audioRef.current;
    if (audio && (pendingPositionRef.current?.position ?? audio.currentTime) > 3) {
      seek(0);
      return;
    }
    const previousIndex = previousQueueIndex(queue);
    if (previousIndex >= 0) {
      autoplayRef.current = true;
      dispatchQueue({ type: 'select', index: previousIndex });
    }
  }, [dispatchQueue, queue, seek]);

  useEffect(() => {
    const audio = audioRef.current;
    if (!audio) return undefined;

    const handleTime = () => {
      const track = queue.tracks[queue.currentIndex];
      const loadedSource = loadedSourceRef.current;
      // pause/load can deliver old timeupdate events while the next track's
      // signed URL is pending. Never attribute that position/listening to it.
      if (!loadedSource || loadedSource.trackId !== track?.id || audio.currentSrc !== loadedSource.url) return;
      if (pendingPositionRef.current?.generation === sourceGenerationRef.current) return;
      const position = audio.currentTime;
      const delta = position - lastContinuousPositionRef.current;
      lastContinuousPositionRef.current = position;
      if (!audio.paused && delta > 0 && delta <= 1.5) {
        listenedSinceProgressRef.current += delta * 1000;
        if (track && listenedSinceProgressRef.current >= 5000) flushProgress(track, audio);
      }
      if (loadedSource.preview && track?.previewEndMs && audio.currentTime * 1000 >= track.previewEndMs) {
        autoplayRef.current = false;
        playAttemptRef.current += 1;
        audio.pause();
        setStatus('paused');
        emitPlayerEvent('complete', track, audio.currentTime, {
          preview: true,
          listenedDeltaMs: Math.round(listenedSinceProgressRef.current),
        });
        listenedSinceProgressRef.current = 0;
        return;
      }
      setCurrentTime(audio.currentTime);
      if (track) positionsRef.current[track.id] = audio.currentTime;
    };
    const handleDuration = () => setDuration(Number.isFinite(audio.duration) ? audio.duration : 0);
    const handlePrepared = () => {
      const loadedSource = loadedSourceRef.current;
      if (loadedSource?.trackId !== queue.tracks[queue.currentIndex]?.id
        || loadedSource?.url !== audio.currentSrc || !audio.paused || autoplayRef.current) return;
      // preload=metadata may stop before canplay. A prepared source awaiting
      // the user's play gesture is paused, not indefinitely loading.
      setStatus((current) => current === 'loading' || current === 'buffering' ? 'paused' : current);
    };
    const ownsNativeSource = () => loadedSourceRef.current?.trackId === currentTrack?.id
      && loadedSourceRef.current?.url === audio.currentSrc;
    const handlePlaying = () => {
      if (!ownsNativeSource() || pendingPositionRef.current || !autoplayRef.current || audio.paused) return;
      lastContinuousPositionRef.current = audio.currentTime;
      setStatus('playing');
    };
    const handleWaiting = () => {
      if (!ownsNativeSource() || pendingPositionRef.current || !autoplayRef.current || audio.paused) return;
      setStatus('buffering');
      const track = queue.tracks[queue.currentIndex];
      if (track) emitPlayerEvent('buffering', track, audio.currentTime);
    };
    const handlePause = () => {
      if (!ownsNativeSource() || !audio.paused) return;
      setStatus((current) => current === 'error' || current === 'unavailable' || current === 'loading' ? current : 'paused');
    };
    const handleEnded = () => {
      if (!ownsNativeSource() || pendingPositionRef.current) return;
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
        audio.currentTime = 0;
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
      autoplayRef.current = false;
      audio.pause();
      setStatus('unavailable');
      setError('La fuente dejó de estar disponible. Puedes reintentar o pasar a la siguiente pista.');
      const track = queue.tracks[queue.currentIndex];
      if (track) emitPlayerEvent('error', track, audio.currentTime, { code: audio.error?.code });
    };

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
  }, [currentTrack, flushProgress, next, play, queue, scheduleRecovery, skipFailed, source?.assetId]);

  useEffect(() => {
    const audio = audioRef.current;
    return () => {
      cancelRetry();
      sourceGenerationRef.current += 1;
      playAttemptRef.current += 1;
      loadedSourceRef.current = null;
      autoplayRef.current = false;
      if (audio) {
        audio.pause();
        audio.removeAttribute('src');
        audio.load();
      }
    };
  }, [cancelRetry]);

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
    let removePreparationListener: () => void = () => undefined;
    lastContinuousPositionRef.current = 0;
    listenedSinceProgressRef.current = 0;
    setCurrentTime(0);
    setDuration(0);
    setResolvedQuality(null);
    setError(null);
    if (!audio) return;
    audio.pause();
    audio.removeAttribute('src');
    if (!currentTrack || !source) {
      audio.load();
      setStatus(currentTrack ? 'unavailable' : 'idle');
      return;
    }
    setStatus('loading');
    let watchdog: number | undefined;
    const clearWatchdog = () => { if (watchdog !== undefined) window.clearTimeout(watchdog); };
    cancelWatchdogRef.current = clearWatchdog;
    const waitForMedia = () => {
      clearWatchdog();
      watchdog = window.setTimeout(() => {
        if (controller.signal.aborted || generation !== sourceGenerationRef.current) return;
        if (skipFailed()) return;
        autoplayRef.current = false;
        playAttemptRef.current += 1;
        audio.pause();
        setStatus('unavailable');
        setError(navigator.onLine === false ? 'Sin conexión. Vuelve a intentar cuando estés en línea.'
          : 'No quedan fuentes reproducibles. Puedes reintentar o elegir otra música.');
      }, 15000);
    };
    const prepared = () => { if (!autoplayRef.current) clearWatchdog(); };
    audio.addEventListener('playing', clearWatchdog);
    audio.addEventListener('timeupdate', clearWatchdog);
    audio.addEventListener('waiting', waitForMedia);
    audio.addEventListener('canplay', prepared);
    audio.addEventListener('pause', prepared);
    waitForMedia();
    void (async () => {
      try {
        let resolved: { source: PlayerSource; url: string } | null = null;
        for (const candidate of candidates) {
          if (failedSourcesRef.current.has(candidate)) continue;
          if (generation !== sourceGenerationRef.current || controller.signal.aborted) return;
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
            const access = candidate.assetId ? await authorizePlayerAsset(candidate.assetId, controller.signal) : null;
            if (generation !== sourceGenerationRef.current || controller.signal.aborted) return;
            resolved = { source: candidate, url: access?.url ?? candidate.url };
            break;
          } catch (reason) {
            if (generation !== sourceGenerationRef.current || controller.signal.aborted) return;
            const status = reason && typeof reason === 'object' && 'status' in reason ? reason.status : null;
            // A full rendition can be denied while a preview of this same
            // recording is allowed. Never fall back to a different track.
            if (status !== 401 && status !== 403 && status !== 404) throw reason;
          }
        }
        if (!resolved?.url) throw Object.assign(new Error('No authorized source'), { status: 403 });
        if (!resolved.source.torrentSource) audio.src = resolved.url;
        loadedSourceRef.current = { trackId: currentTrack.id, url: audio.src,
          preview: resolved.source.preview, quality: resolved.source.quality, candidate: resolved.source };
        setResolvedQuality(resolved.source.quality);
        const storedPosition = positionsRef.current[currentTrack.id] ?? 0;
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
            setCurrentTime(pending.position);
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
        if (skipFailed()) return;
        autoplayRef.current = false;
        setStatus('unavailable');
        setError('El servidor no autorizó una fuente vigente para esta pista.');
        logger.warn('Global player could not refresh asset authorization', caught);
      }
    })();
    return () => {
      controller.abort(); torrentSession?.destroy(); clearWatchdog();
      removePreparationListener();
      if (pendingPositionRef.current?.generation === generation) pendingPositionRef.current = null;
      audio.removeEventListener('playing', clearWatchdog);
      audio.removeEventListener('timeupdate', clearWatchdog);
      audio.removeEventListener('waiting', waitForMedia);
      audio.removeEventListener('canplay', prepared);
      audio.removeEventListener('pause', prepared);
    };
  }, [cancelRetry, candidates, currentTrack, play, quality, scheduleRecovery, skipFailed, source, sourceRefresh]);

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
    const audio = audioRef.current;
    if (!audio) return;
    audio.muted = muted;
    audio.volume = normalizedVolume(volume, currentTrack?.loudnessLufs);
  }, [currentTrack?.loudnessLufs, muted, volume]);

  useEffect(() => {
    const handleLoad = (event: Event) => {
      const detail = (event as CustomEvent<LoadPlayerTrackDetail>).detail;
      if (!detail?.track || !isTrack(detail.track)) return;
      const tracks = detail.queue?.filter(isTrack) ?? [detail.track];
      if (typeof detail.startPositionMs === 'number' && Number.isFinite(detail.startPositionMs) && detail.startPositionMs >= 0) {
        positionsRef.current[detail.track.id] = detail.startPositionMs / 1000;
      }
      artistContextRef.current = null;
      explicitPauseRef.current = false;
      failedTracksRef.current.clear();
      failedSourcesRef.current.clear();
      setUnavailableTrackIds([]);
      autoplayRef.current = detail.autoplay !== false;
      dispatchQueue({ type: 'replace', tracks, currentTrackId: detail.track.id });
    };
    const handleRadioTakeover = () => { artistContextRef.current = null; pause(false); };
    window.addEventListener(PLAYER_LOAD_TRACK_EVENT, handleLoad as EventListener);
    window.addEventListener(RADIO_TAKEOVER_EVENT, handleRadioTakeover);
    return () => {
      window.removeEventListener(PLAYER_LOAD_TRACK_EVENT, handleLoad as EventListener);
      window.removeEventListener(RADIO_TAKEOVER_EVENT, handleRadioTakeover);
    };
  }, [dispatchQueue, pause]);

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
    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.defaultPrevented || event.repeat || event.isComposing || event.ctrlKey || event.metaKey) return;
      const target = event.target instanceof Element ? event.target : null;
      if ((target instanceof HTMLElement && target.isContentEditable) || target?.closest(
        'input, textarea, select, button, a[href], [role="button"], [role="slider"], [role="combobox"], [role="menu"], [role="listbox"], [role="option"], [role="dialog"], [role="alertdialog"]',
      )) return;
      if (!currentTrack) return;
      if (event.code === 'Space' && !event.altKey) {
        event.preventDefault();
        if (status === 'playing' || status === 'buffering') pause(); else void play();
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
        } satisfies PersistedPlayerState));
      } catch {
        // Storage can be unavailable in private mode. Playback remains usable.
      }
    }, 250);
    return () => window.clearTimeout(timeout);
  }, [currentTime, currentTrack, identityReady, listeningHydrated, muted, ownerPartyId, quality, queue, volume]);

  const playTrack = useCallback((track: PlayerTrack, replacementQueue?: PlayerTrack[], autoplay = true) => {
    queueIntentRef.current = true;
    artistContextRef.current = null;
    explicitPauseRef.current = !autoplay;
    failedTracksRef.current.clear();
    failedSourcesRef.current.clear();
    setUnavailableTrackIds([]);
    if (track === currentTrack && !replacementQueue) {
      if (autoplay) void play(); else pause();
      return;
    }
    const tracks = replacementQueue?.filter(isTrack) ?? [track];
    autoplayRef.current = autoplay;
    dispatchQueue({ type: 'replace', tracks, currentTrackId: track.id });
  }, [currentTrack, dispatchQueue, pause, play]);

  const beginArtist = useCallback((key: string) => {
    if (artistContextRef.current === key) return;
    artistContextRef.current = key;
    failedTracksRef.current.clear();
    failedSourcesRef.current.clear();
    setUnavailableTrackIds([]);
    autoplayRef.current = false;
    playAttemptRef.current += 1;
    sourceGenerationRef.current += 1;
    cancelRetry();
    audioRef.current?.pause();
    window.dispatchEvent(new CustomEvent(PLAYER_TAKEOVER_EVENT));
    dispatchQueue({ type: 'clear' });
  }, [cancelRetry, dispatchQueue]);

  const loadArtist = useCallback((key: string, tracks: PlayerTrack[]) => {
    if (artistContextRef.current !== key) return;
    const seen = new Set<string>();
    const eligible = tracks.filter((track) => isTrack(track) && !seen.has(track.id) && Boolean(seen.add(track.id)));
    autoplayRef.current = !explicitPauseRef.current;
    dispatchQueue({ type: 'replace', tracks: eligible });
  }, [dispatchQueue]);

  const contextValue = useMemo<PlayerContextValue>(() => ({
    queue,
    torrentProgress,
    unavailableTrackIds,
    currentTrack,
    status,
    error,
    currentTime,
    duration,
    volume,
    muted,
    quality,
    resolvedQuality,
    playTrack,
    beginArtist,
    loadArtist,
    enqueue: (tracks) => dispatchQueue({ type: 'enqueue', tracks: tracks.filter(isTrack) }),
    selectTrack,
    removeTrack: (index) => dispatchQueue({ type: 'remove', index }),
    moveTrack: (from, to) => dispatchQueue({ type: 'move', from, to }),
    clearQueue: () => {
      pause();
      dispatchQueue({ type: 'clear' });
    },
    togglePlayback: () => { if (status === 'playing' || status === 'buffering') pause(); else void play(); },
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
    beginArtist, loadArtist, torrentProgress, unavailableTrackIds, currentTime, currentTrack, duration, error, muted, next, pause, play, playTrack, previous,
    quality, queue, resolvedQuality, seek, selectTrack, status, volume, dispatchQueue, updateMuted, updateQuality, updateVolume,
  ]);

  return (
    <PlayerContext.Provider value={contextValue}>
      {children}
      {/* One release-audio element for the entire app shell. */}
      {/* eslint-disable-next-line jsx-a11y/media-has-caption */}
      <audio ref={audioRef} preload="metadata" aria-hidden="true" />
    </PlayerContext.Provider>
  );
}

export function usePlayer(): PlayerContextValue {
  const value = useContext(PlayerContext);
  if (!value) throw new Error('usePlayer must be used within PlayerProvider');
  return value;
}

export function useOptionalPlayer() { return useContext(PlayerContext); }
