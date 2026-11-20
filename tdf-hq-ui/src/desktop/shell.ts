/**
 * Optional bridge to the TDF Desktop shell (Electron).
 *
 * The shell's preload exposes `window.tdfDesktop` only on the authorized
 * application origin. The web application never requires it: every call site
 * must keep working in an ordinary browser when this returns `null`.
 */
export type DesktopPlaybackOwner = 'music' | 'radio';
export type DesktopPlaybackStatus = 'idle' | 'loading' | 'playing' | 'paused' | 'error';
export type DesktopPlaybackCommand = 'play' | 'pause' | 'toggle' | 'next' | 'previous';

export interface DesktopPlaybackReport {
  owner: DesktopPlaybackOwner | null;
  status: DesktopPlaybackStatus;
  title: string | null;
  artist: string | null;
  canNext: boolean;
  canPrevious: boolean;
}

export interface DesktopShellVersion {
  app: string;
  electron: string;
  chromium: string;
  platform: string;
  arch: string;
  channel: string;
  unsigned: boolean;
}

export interface DesktopShellApi {
  readonly version: DesktopShellVersion;
  readonly playback: {
    report(state: DesktopPlaybackReport): void;
    onCommand(listener: (command: DesktopPlaybackCommand) => void): () => void;
  };
  readonly links: {
    openExternal(url: string): Promise<boolean>;
  };
}

declare global {
  interface Window {
    tdfDesktop?: unknown;
  }
}

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === 'object' && value !== null;

const isShellApi = (value: unknown): value is DesktopShellApi => {
  if (!isRecord(value)) return false;
  const version = value['version'];
  const playback = value['playback'];
  const links = value['links'];
  return isRecord(version)
    && typeof version['app'] === 'string'
    && typeof version['channel'] === 'string'
    && isRecord(playback)
    && typeof playback['report'] === 'function'
    && typeof playback['onCommand'] === 'function'
    && isRecord(links)
    && typeof links['openExternal'] === 'function';
};

/** Returns the shell bridge, or `null` in a browser or when the global is malformed. */
export const getDesktopShell = (): DesktopShellApi | null => {
  if (typeof window === 'undefined') return null;
  const candidate = window.tdfDesktop;
  return isShellApi(candidate) ? candidate : null;
};

export const isDesktopShell = (): boolean => getDesktopShell() !== null;
