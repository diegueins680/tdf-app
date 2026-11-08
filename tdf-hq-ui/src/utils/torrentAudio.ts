import type WebTorrent from 'webtorrent';
import webTorrentBundleUrl from '../vendor/webtorrent/webtorrent.min.js?url';
import { ensureTorrentWorker } from './torrentWorker';

const PLAYABLE_AUDIO_EXTENSIONS = new Set([
  'aac',
  'flac',
  'm4a',
  'mp3',
  'mp4',
  'oga',
  'ogg',
  'opus',
  'wav',
  'webm',
]);

export const MAX_TORRENT_FILE_BYTES = 10 * 1024 * 1024;

export interface TorrentFileCandidate {
  name: string;
  length: number;
  type?: string;
}

type StreamableTorrentFile = WebTorrent.TorrentFile & {
  readonly type?: string;
  streamTo(element: HTMLMediaElement): HTMLMediaElement;
};

export interface TorrentPlaybackProgress {
  fileName: string;
  progress: number;
  peers: number;
  downloadSpeed: number;
}

export interface TorrentAudioSession {
  fileName: string;
  torrentName: string;
  destroy: () => void;
}

interface CreateTorrentAudioSessionOptions {
  source: string | File;
  audio: HTMLAudioElement;
  signal?: AbortSignal;
  onProgress?: (progress: TorrentPlaybackProgress) => void;
  onWarning?: (message: string) => void;
}

let webTorrentConstructorPromise: Promise<WebTorrent.WebTorrent> | null = null;

export function isMagnetLink(value: string): boolean {
  const normalized = value.trim();
  if (!normalized.toLowerCase().startsWith('magnet:?')) return false;
  try {
    const query = normalized.slice(normalized.indexOf('?') + 1);
    return new URLSearchParams(query)
      .getAll('xt')
      .some((topic) => /^urn:btih:[a-z0-9]+$/i.test(topic));
  } catch {
    return false;
  }
}

export function magnetDisplayName(value: string): string | null {
  if (!isMagnetLink(value)) return null;
  try {
    const query = value.trim().slice(value.indexOf('?') + 1);
    const name = new URLSearchParams(query).get('dn')?.trim() ?? '';
    return name || null;
  } catch {
    return null;
  }
}

export function isTorrentFile(file: Pick<File, 'name' | 'size' | 'type'>): boolean {
  const normalizedType = file.type.trim().toLowerCase();
  return (
    file.size > 0
    && file.size <= MAX_TORRENT_FILE_BYTES
    && (file.name.toLowerCase().endsWith('.torrent') || normalizedType === 'application/x-bittorrent')
  );
}

export function pickPlayableAudioFile<T extends TorrentFileCandidate>(files: readonly T[]): T | null {
  const playable = files.filter((file) => {
    if (file.type?.toLowerCase().startsWith('audio/')) return true;
    const extension = file.name.split('.').pop()?.toLowerCase() ?? '';
    return PLAYABLE_AUDIO_EXTENSIONS.has(extension);
  });

  return playable.reduce<T | null>(
    (largest, file) => (!largest || file.length > largest.length ? file : largest),
    null,
  );
}

function errorMessage(error: Error | string): string {
  return typeof error === 'string' ? error : error.message;
}

async function loadWebTorrentConstructor(): Promise<WebTorrent.WebTorrent> {
  webTorrentConstructorPromise ??= import(/* @vite-ignore */ webTorrentBundleUrl)
    .then((module: unknown) => {
      const candidate = (module as { default?: WebTorrent.WebTorrent }).default;
      if (!candidate) {
        throw new Error('No se pudo cargar el reproductor de torrents.');
      }
      return candidate;
    })
    .catch((error: unknown) => {
      webTorrentConstructorPromise = null;
      throw error;
    });

  return webTorrentConstructorPromise;
}

export async function createTorrentAudioSession({
  source,
  audio,
  signal,
  onProgress,
  onWarning,
}: CreateTorrentAudioSessionOptions): Promise<TorrentAudioSession> {
  if (signal?.aborted) {
    throw new Error('La reproducción del torrent fue cancelada.');
  }

  const WebTorrentClient = await loadWebTorrentConstructor();

  if (signal?.aborted) {
    throw new Error('La reproducción del torrent fue cancelada.');
  }
  if (!WebTorrentClient.WEBRTC_SUPPORT) {
    throw new Error('Este navegador no admite conexiones WebRTC para torrents.');
  }

  const registration = await ensureTorrentWorker();
  if (signal?.aborted) {
    throw new Error('La reproducción del torrent fue cancelada.');
  }

  const client = new WebTorrentClient({
    dht: false,
    lsd: false,
  });
  let destroyed = false;
  let progressTimer: number | null = null;
  let rejectPendingAbort: ((reason: Error) => void) | null = null;
  let abortHandler: (() => void) | null = null;

  const destroy = () => {
    if (destroyed) return;
    destroyed = true;
    if (signal && abortHandler) {
      signal.removeEventListener('abort', abortHandler);
    }
    if (progressTimer !== null) {
      window.clearInterval(progressTimer);
      progressTimer = null;
    }
    try {
      client.destroy();
    } catch {
      // The client may already have closed after a transport error.
    }
  };
  abortHandler = () => {
    rejectPendingAbort?.(new Error('La reproducción del torrent fue cancelada.'));
    destroy();
  };
  signal?.addEventListener('abort', abortHandler, { once: true });

  try {
    client.createServer({ controller: registration }, 'browser');

    const torrentReady = new Promise<WebTorrent.Torrent>((resolve, reject) => {
      let settled = false;
      const fail = (error: Error | string) => {
        const message = errorMessage(error);
        onWarning?.(message);
        if (!settled) {
          settled = true;
          reject(new Error(message));
        }
      };

      client.on('error', fail);
      const pendingTorrent = client.add(
        source,
        { destroyStoreOnDestroy: true },
        (readyTorrent) => {
          if (settled) return;
          settled = true;
          resolve(readyTorrent);
        },
      );
      pendingTorrent.on('warning', (warning) => onWarning?.(errorMessage(warning)));
      pendingTorrent.on('error', fail);
    });
    const aborted = new Promise<never>((_, reject) => {
      rejectPendingAbort = reject;
    });
    const torrent = await Promise.race([torrentReady, aborted]);
    rejectPendingAbort = null;

    if (destroyed) {
      throw new Error('La reproducción del torrent fue cancelada.');
    }

    const file = pickPlayableAudioFile(torrent.files as StreamableTorrentFile[]);
    if (!file) {
      throw new Error('El torrent no contiene archivos de audio compatibles con este navegador.');
    }

    const reportProgress = () => {
      if (destroyed) return;
      onProgress?.({
        fileName: file.name,
        progress: file.progress,
        peers: torrent.numPeers,
        downloadSpeed: torrent.downloadSpeed,
      });
    };

    file.select();
    file.streamTo(audio);
    audio.load();
    reportProgress();
    torrent.on('download', reportProgress);
    progressTimer = window.setInterval(reportProgress, 1_000);

    return {
      fileName: file.name,
      torrentName: torrent.name,
      destroy,
    };
  } catch (error) {
    destroy();
    throw error;
  }
}
