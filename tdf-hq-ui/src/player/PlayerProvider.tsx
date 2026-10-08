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
import { EMPTY_QUEUE, nextQueueIndex, previousQueueIndex, queueReducer, type QueueState } from './queue';
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
  queue: QueueState;
  volume: number;
  muted: boolean;
  quality: PlayerQuality | 'auto';
  positions: Record<string, number>;
}

interface PlayerContextValue {
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
    && track.sources.some((source) => source
      && isPlayerQuality(source.quality)
      && ((typeof source.url === 'string' && source.url.length > 0)
        || (typeof source.assetId === 'string' && source.assetId.length > 0)));
};

const loadPersistedState = (): PersistedPlayerState => {
  if (typeof window === 'undefined') {
    return { queue: EMPTY_QUEUE, volume: 0.8, muted: false, quality: 'auto', positions: {} };
  }
  try {
    const parsed = JSON.parse(window.localStorage.getItem(PERSISTENCE_KEY) ?? 'null') as Partial<PersistedPlayerState> | null;
    const tracks = Array.isArray(parsed?.queue?.tracks) ? parsed.queue.tracks.filter(isTrack).slice(0, 200) : [];
    const currentIndex = Number.isSafeInteger(parsed?.queue?.currentIndex)
      ? Math.min(Math.max(parsed?.queue?.currentIndex ?? -1, -1), tracks.length - 1)
      : -1;
    const volume = typeof parsed?.volume === 'number' && Number.isFinite(parsed.volume)
      ? Math.min(Math.max(parsed.volume, 0), 1)
      : 0.8;
    const quality = parsed?.quality === 'auto' || isPlayerQuality(parsed?.quality) ? parsed.quality : 'auto';
    const repeat = isRepeatMode(parsed?.queue?.repeat) ? parsed.queue.repeat : 'off';
    const positions = parsed?.positions && typeof parsed.positions === 'object' ? parsed.positions : {};
    return {
      queue: { tracks, currentIndex, shuffle: parsed?.queue?.shuffle === true, repeat },
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

const selectSource = (track: PlayerTrack | null, selectedQuality: PlayerQuality | 'auto'): PlayerSource | null => {
  if (!track || track.sources.length === 0) return null;
  const requested = selectedQuality === 'auto' ? automaticQuality() : selectedQuality;
  // A preview is an access fallback, not another bitrate of the full track.
  // Restrict selection to full sources when any were authorized; otherwise
  // retain the visitor's preview-only sources and their existing duration cap.
  const fullSources = track.sources.filter((candidate) => !candidate.preview);
  const ordered = [...(fullSources.length ? fullSources : track.sources)]
    .sort((a, b) => qualityRank[a.quality] - qualityRank[b.quality]);
  const notAboveRequested = ordered.filter((source) => qualityRank[source.quality] <= qualityRank[requested]);
  return notAboveRequested.at(-1) ?? ordered[0] ?? null;
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

export function PlayerProvider({ children }: { children: ReactNode }) {
  const persisted = useMemo(loadPersistedState, []);
  const [queue, dispatchQueue] = useReducer(queueReducer, persisted.queue);
  const [status, setStatus] = useState<PlaybackStatus>('idle');
  const [error, setError] = useState<string | null>(null);
  const [currentTime, setCurrentTime] = useState(0);
  const [duration, setDuration] = useState(0);
  const [volume, updateVolume] = useState(persisted.volume);
  const [muted, updateMuted] = useState(persisted.muted);
  const [quality, updateQuality] = useState<PlayerQuality | 'auto'>(persisted.quality);
  const [sourceRefresh, setSourceRefresh] = useState(0);
  const audioRef = useRef<HTMLAudioElement | null>(null);
  const positionsRef = useRef<Record<string, number>>(persisted.positions);
  const autoplayRef = useRef(false);
  const retryRef = useRef(0);
  const sourceGenerationRef = useRef(0);
  const playAttemptRef = useRef(0);
  const loadedSourceRef = useRef<{ trackId: string; url: string } | null>(null);
  const lastContinuousPositionRef = useRef(0);
  const listenedSinceProgressRef = useRef(0);
  const currentTrack = queue.currentIndex >= 0 ? queue.tracks[queue.currentIndex] ?? null : null;
  const source = useMemo(() => selectSource(currentTrack, quality), [currentTrack, quality]);

  const flushProgress = useCallback((track: PlayerTrack, audio: HTMLAudioElement) => {
    const listenedDeltaMs = Math.round(listenedSinceProgressRef.current);
    if (listenedDeltaMs <= 0) return;
    listenedSinceProgressRef.current = 0;
    emitPlayerEvent('progress', track, audio.currentTime, { listenedDeltaMs });
  }, []);

  const pause = useCallback(() => {
    const audio = audioRef.current;
    if (!audio) return;
    playAttemptRef.current += 1;
    autoplayRef.current = false;
    audio.pause();
    setStatus(currentTrack ? 'paused' : 'idle');
    if (currentTrack) {
      flushProgress(currentTrack, audio);
      emitPlayerEvent('pause', currentTrack, audio.currentTime);
    }
  }, [currentTrack, flushProgress]);

  const play = useCallback(async () => {
    const audio = audioRef.current;
    if (!audio || !currentTrack || !source) {
      setStatus('unavailable');
      setError('No hay una fuente autorizada disponible para esta pista.');
      return;
    }
    autoplayRef.current = true;
    if (source.assetId && !audio.getAttribute('src')) {
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
      setStatus('playing');
      setError(null);
      emitPlayerEvent('play_start', currentTrack, audio.currentTime, { quality: source.quality });
    } catch (caught) {
      if (generation !== sourceGenerationRef.current || attempt !== playAttemptRef.current) return;
      autoplayRef.current = false;
      audio.removeAttribute('src');
      setStatus('error');
      setError('No se pudo iniciar la reproducción. Revisa la conexión o vuelve a intentarlo.');
      logger.warn('Global player could not start playback', caught);
    }
  }, [currentTrack, source]);

  const selectTrack = useCallback((index: number) => {
    autoplayRef.current = true;
    dispatchQueue({ type: 'select', index });
  }, []);

  const next = useCallback(() => {
    const nextIndex = nextQueueIndex(queue);
    if (nextIndex < 0) {
      pause();
      return;
    }
    autoplayRef.current = true;
    dispatchQueue({ type: 'select', index: nextIndex });
  }, [pause, queue]);

  const previous = useCallback(() => {
    const audio = audioRef.current;
    if (audio && audio.currentTime > 3) {
      audio.currentTime = 0;
      setCurrentTime(0);
      return;
    }
    const previousIndex = previousQueueIndex(queue);
    if (previousIndex >= 0) {
      autoplayRef.current = true;
      dispatchQueue({ type: 'select', index: previousIndex });
    }
  }, [queue]);

  useEffect(() => {
    const audio = audioRef.current;
    if (!audio) return undefined;

    const handleTime = () => {
      const track = queue.tracks[queue.currentIndex];
      const loadedSource = loadedSourceRef.current;
      // pause/load can deliver old timeupdate events while the next track's
      // signed URL is pending. Never attribute that position/listening to it.
      if (!loadedSource || loadedSource.trackId !== track?.id || audio.currentSrc !== loadedSource.url) return;
      const position = audio.currentTime;
      const delta = position - lastContinuousPositionRef.current;
      lastContinuousPositionRef.current = position;
      if (!audio.paused && delta > 0 && delta <= 1.5) {
        listenedSinceProgressRef.current += delta * 1000;
        if (track && listenedSinceProgressRef.current >= 5000) flushProgress(track, audio);
      }
      if (track?.previewEndMs && audio.currentTime * 1000 >= track.previewEndMs) {
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
    const handlePlaying = () => {
      lastContinuousPositionRef.current = audio.currentTime;
      setStatus('playing');
    };
    const handleWaiting = () => {
      setStatus('buffering');
      const track = queue.tracks[queue.currentIndex];
      if (track) emitPlayerEvent('buffering', track, audio.currentTime);
    };
    const handlePause = () => setStatus((current) => current === 'error' || current === 'unavailable' ? current : 'paused');
    const handleEnded = () => {
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
      const generation = sourceGenerationRef.current;
      if (retryRef.current < MAX_NETWORK_RETRIES) {
        retryRef.current += 1;
        setStatus('loading');
        window.setTimeout(() => {
          if (generation !== sourceGenerationRef.current) return;
          if (source?.assetId) setSourceRefresh((value) => value + 1);
          else {
            audio.load();
            if (autoplayRef.current) void play();
          }
        }, 500 * 2 ** retryRef.current);
        return;
      }
      autoplayRef.current = false;
      setStatus('error');
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
  }, [flushProgress, next, play, queue, source?.assetId]);

  useEffect(() => {
    const audio = audioRef.current;
    return () => {
      sourceGenerationRef.current += 1;
      playAttemptRef.current += 1;
      loadedSourceRef.current = null;
      autoplayRef.current = false;
      audio?.pause();
    };
  }, []);

  useEffect(() => { retryRef.current = 0; }, [currentTrack?.id, quality]);

  useEffect(() => {
    const audio = audioRef.current;
    const generation = sourceGenerationRef.current + 1;
    sourceGenerationRef.current = generation;
    loadedSourceRef.current = null;
    lastContinuousPositionRef.current = 0;
    listenedSinceProgressRef.current = 0;
    setCurrentTime(0);
    setDuration(0);
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
    void (async () => {
      try {
        const authorized = source.assetId ? await authorizePlayerAsset(source.assetId) : null;
        if (generation !== sourceGenerationRef.current) return;
        audio.src = authorized?.url ?? source.url;
        loadedSourceRef.current = { trackId: currentTrack.id, url: audio.src };
        audio.load();
        const savedPosition = positionsRef.current[currentTrack.id] ?? 0;
        if (savedPosition > 0) audio.currentTime = savedPosition;
        emitPlayerEvent('quality_selected', currentTrack, savedPosition, { quality: source.quality, automatic: quality === 'auto' });
        if (autoplayRef.current) void play();
      } catch (caught) {
        if (generation !== sourceGenerationRef.current) return;
        autoplayRef.current = false;
        setStatus('unavailable');
        setError('El servidor no autorizó una fuente vigente para esta pista.');
        logger.warn('Global player could not refresh asset authorization', caught);
      }
    })();
  }, [currentTrack, play, quality, source, sourceRefresh]);

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
      autoplayRef.current = detail.autoplay !== false;
      dispatchQueue({ type: 'replace', tracks, currentTrackId: detail.track.id });
    };
    const handleRadioTakeover = () => pause();
    window.addEventListener(PLAYER_LOAD_TRACK_EVENT, handleLoad as EventListener);
    window.addEventListener(RADIO_TAKEOVER_EVENT, handleRadioTakeover);
    return () => {
      window.removeEventListener(PLAYER_LOAD_TRACK_EVENT, handleLoad as EventListener);
      window.removeEventListener(RADIO_TAKEOVER_EVENT, handleRadioTakeover);
    };
  }, [pause]);

  useEffect(() => installPlayerAnalyticsBridge(), []);

  useEffect(() => {
    if (!('mediaSession' in navigator)) return;
    const mediaSession = navigator.mediaSession;
    if (currentTrack) {
      mediaSession.metadata = new MediaMetadata({
        title: currentTrack.title,
        artist: currentTrack.artist,
        album: currentTrack.releaseId,
        artwork: currentTrack.artworkUrl ? [{ src: currentTrack.artworkUrl }] : undefined,
      });
    } else {
      mediaSession.metadata = null;
    }
    mediaSession.setActionHandler('play', () => { void play(); });
    mediaSession.setActionHandler('pause', pause);
    mediaSession.setActionHandler('previoustrack', previous);
    mediaSession.setActionHandler('nexttrack', next);
    mediaSession.setActionHandler('seekto', (details) => {
      if (audioRef.current && details.seekTime !== undefined) audioRef.current.currentTime = details.seekTime;
    });
    return () => {
      ['play', 'pause', 'previoustrack', 'nexttrack', 'seekto'].forEach((action) => {
        try {
          mediaSession.setActionHandler(action as MediaSessionAction, null);
        } catch {
          // Older browsers expose MediaSession with a smaller action set.
        }
      });
    };
  }, [currentTrack, next, pause, play, previous]);

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
        if (status === 'playing') pause(); else void play();
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
  }, [currentTrack, next, pause, play, previous, status]);

  useEffect(() => {
    const timeout = window.setTimeout(() => {
      try {
        const persistedQueue: QueueState = {
          ...queue,
          tracks: queue.tracks.map((track) => ({
            ...track,
            artworkUrl: null,
            sources: track.sources.map((item) => item.assetId
              ? { ...item, url: '', expiresAt: undefined }
              : item),
          })),
        };
        window.localStorage.setItem(PERSISTENCE_KEY, JSON.stringify({
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
  }, [currentTime, muted, quality, queue, volume]);

  const playTrack = useCallback((track: PlayerTrack, replacementQueue?: PlayerTrack[], autoplay = true) => {
    const tracks = replacementQueue?.filter(isTrack) ?? [track];
    autoplayRef.current = autoplay;
    dispatchQueue({ type: 'replace', tracks, currentTrackId: track.id });
  }, []);

  const contextValue = useMemo<PlayerContextValue>(() => ({
    queue,
    currentTrack,
    status,
    error,
    currentTime,
    duration,
    volume,
    muted,
    quality,
    resolvedQuality: source?.quality ?? null,
    playTrack,
    enqueue: (tracks) => dispatchQueue({ type: 'enqueue', tracks: tracks.filter(isTrack) }),
    selectTrack,
    removeTrack: (index) => dispatchQueue({ type: 'remove', index }),
    moveTrack: (from, to) => dispatchQueue({ type: 'move', from, to }),
    clearQueue: () => {
      pause();
      dispatchQueue({ type: 'clear' });
    },
    togglePlayback: () => { if (status === 'playing') pause(); else void play(); },
    play,
    pause,
    next,
    previous,
    seek: (seconds) => {
      const audio = audioRef.current;
      if (!audio || !Number.isFinite(seconds)) return;
      const nextTime = Math.min(Math.max(seconds, 0), Number.isFinite(audio.duration) ? audio.duration : seconds);
      audio.currentTime = nextTime;
      lastContinuousPositionRef.current = nextTime;
      setCurrentTime(nextTime);
      if (currentTrack) emitPlayerEvent('seek', currentTrack, nextTime);
    },
    setVolume: (nextVolume) => updateVolume(Math.min(Math.max(nextVolume, 0), 1)),
    setMuted: updateMuted,
    setQuality: updateQuality,
    setShuffle: (enabled) => dispatchQueue({ type: 'shuffle', enabled }),
    setRepeat: (mode) => dispatchQueue({ type: 'repeat', mode }),
  }), [
    currentTime, currentTrack, duration, error, muted, next, pause, play, playTrack, previous,
    quality, queue, selectTrack, source?.quality, status, volume,
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
