import {
  getDesktopShell,
  type DesktopPlaybackCommand,
  type DesktopPlaybackOwner,
  type DesktopPlaybackStatus,
} from '../desktop/shell';

/**
 * One system-level playback session for the whole web application.
 *
 * The release player and the radio widget each own an HTMLAudioElement. The
 * operating system (Media Session API: hardware media keys, macOS Now Playing,
 * Windows media transport controls) and the optional desktop shell menu only
 * see one "current" owner: the one that most recently started or selected
 * audio. Commands from either surface go to that owner, so a media key never
 * pauses the release player while the radio is the audible one.
 */
export interface PlaybackSessionHandlers {
  play: () => void;
  pause: () => void;
  next?: () => void;
  previous?: () => void;
  seekTo?: (seconds: number) => void;
}

export interface PlaybackSessionState {
  status: DesktopPlaybackStatus;
  title: string | null;
  artist: string | null;
  album?: string | null;
  artworkUrl?: string | null;
}

interface PlaybackOwner {
  id: DesktopPlaybackOwner;
  handlers: PlaybackSessionHandlers;
  state: PlaybackSessionState;
}

const MEDIA_ACTIONS = ['play', 'pause', 'previoustrack', 'nexttrack', 'seekto'] as const;
type ManagedMediaAction = (typeof MEDIA_ACTIONS)[number];

// The last entry is the active owner. Earlier entries regain the session when
// a later owner releases it (for example when the radio widget unmounts).
const owners: PlaybackOwner[] = [];
let desktopCommandUnsubscribe: (() => void) | null = null;

const activeOwner = (): PlaybackOwner | null => owners[owners.length - 1] ?? null;

const applyMediaSession = (active: PlaybackOwner | null): void => {
  if (typeof navigator === 'undefined' || !('mediaSession' in navigator)) return;
  const mediaSession = navigator.mediaSession;
  try {
    if (active && typeof MediaMetadata !== 'undefined') {
      mediaSession.metadata = new MediaMetadata({
        title: active.state.title ?? '',
        artist: active.state.artist ?? '',
        album: active.state.album ?? undefined,
        artwork: active.state.artworkUrl ? [{ src: active.state.artworkUrl }] : undefined,
      });
    } else {
      mediaSession.metadata = null;
    }
  } catch {
    // Metadata is informational; an unsupported field must not stop playback.
  }
  try {
    mediaSession.playbackState = !active ? 'none' : active.state.status === 'playing' ? 'playing' : 'paused';
  } catch {
    // Older engines expose Media Session without playbackState.
  }
  const handlers: Record<ManagedMediaAction, MediaSessionActionHandler | null> = {
    play: active ? () => { active.handlers.play(); } : null,
    pause: active ? () => { active.handlers.pause(); } : null,
    previoustrack: active?.handlers.previous ? () => { active.handlers.previous?.(); } : null,
    nexttrack: active?.handlers.next ? () => { active.handlers.next?.(); } : null,
    seekto: active?.handlers.seekTo
      ? (details) => { if (details.seekTime !== undefined && details.seekTime !== null) active.handlers.seekTo?.(details.seekTime); }
      : null,
  };
  for (const action of MEDIA_ACTIONS) {
    try { mediaSession.setActionHandler(action, handlers[action]); }
    catch { /* Some engines expose only a subset of Media Session actions. */ }
  }
};

const reportToDesktop = (active: PlaybackOwner | null): void => {
  const shell = getDesktopShell();
  if (!shell) return;
  try {
    shell.playback.report({
      owner: active?.id ?? null,
      status: active?.state.status ?? 'idle',
      title: active?.state.title ?? null,
      artist: active?.state.artist ?? null,
      canNext: Boolean(active?.handlers.next),
      canPrevious: Boolean(active?.handlers.previous),
    });
  } catch {
    // The shell is an optional observer; it must never break web playback.
  }
};

/** Routes a shell or test command to the active owner. Returns false when nothing can handle it. */
export const dispatchPlaybackCommand = (command: DesktopPlaybackCommand): boolean => {
  const active = activeOwner();
  if (!active) return false;
  switch (command) {
    case 'play': active.handlers.play(); return true;
    case 'pause': active.handlers.pause(); return true;
    case 'toggle':
      if (active.state.status === 'playing' || active.state.status === 'loading') active.handlers.pause();
      else active.handlers.play();
      return true;
    case 'next':
      if (!active.handlers.next) return false;
      active.handlers.next();
      return true;
    case 'previous':
      if (!active.handlers.previous) return false;
      active.handlers.previous();
      return true;
    default:
      return false;
  }
};

const ensureDesktopCommands = (): void => {
  if (desktopCommandUnsubscribe) return;
  const shell = getDesktopShell();
  if (!shell) return;
  try {
    desktopCommandUnsubscribe = shell.playback.onCommand((command) => { dispatchPlaybackCommand(command); });
  } catch {
    desktopCommandUnsubscribe = null;
  }
};

const apply = (): void => {
  const active = activeOwner();
  applyMediaSession(active);
  reportToDesktop(active);
};

/** Makes `id` the active owner (a new selection or an explicit play). */
export const claimPlaybackSession = (
  id: DesktopPlaybackOwner,
  handlers: PlaybackSessionHandlers,
  state: PlaybackSessionState,
): void => {
  ensureDesktopCommands();
  const index = owners.findIndex((owner) => owner.id === id);
  if (index >= 0) owners.splice(index, 1);
  owners.push({ id, handlers, state });
  apply();
};

/** Refreshes an owner's state without changing which owner is active. */
export const updatePlaybackSession = (
  id: DesktopPlaybackOwner,
  state: PlaybackSessionState,
  handlers?: PlaybackSessionHandlers,
): void => {
  const owner = owners.find((entry) => entry.id === id);
  if (!owner) return;
  owner.state = state;
  if (handlers) owner.handlers = handlers;
  if (activeOwner() === owner) apply();
};

/** Removes an owner; the previous owner, if any, regains the system session. */
export const releasePlaybackSession = (id: DesktopPlaybackOwner): void => {
  const index = owners.findIndex((owner) => owner.id === id);
  if (index < 0) {
    if (owners.length === 0) apply();
    return;
  }
  const wasActive = index === owners.length - 1;
  owners.splice(index, 1);
  if (wasActive) apply();
};

export const getActivePlaybackOwner = (): DesktopPlaybackOwner | null => activeOwner()?.id ?? null;

/** Test hook: forget every owner and desktop subscription. */
export const resetPlaybackSessionForTests = (): void => {
  owners.length = 0;
  desktopCommandUnsubscribe?.();
  desktopCommandUnsubscribe = null;
};
