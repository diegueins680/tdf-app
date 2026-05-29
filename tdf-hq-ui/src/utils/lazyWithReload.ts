import { lazy } from 'react';

const LEGACY_CHUNK_RELOAD_KEY = 'chunk_reload_attempted';
const CHUNK_RELOAD_KEY = 'tdf-hq-ui/chunk-reload-attempted';
const CHUNK_ERROR_PATTERNS = [
  /Failed to fetch dynamically imported module/i,
  /error loading dynamically imported module/i,
  /Importing a module script failed/i,
  /Expected a JavaScript module script/i,
  /Unable to preload CSS/i,
];

export function isChunkLoadError(error: unknown): boolean {
  const message = error instanceof Error ? error.message : String(error);
  const name = error instanceof Error ? error.name : '';
  return (
    CHUNK_ERROR_PATTERNS.some((pattern) => pattern.test(message))
    || /ChunkLoadError|CSS_CHUNK_LOAD_FAILED/i.test(name)
  );
}

const clearChunkReloadMarker = () => {
  if (typeof window === 'undefined') return;
  try {
    window.sessionStorage.removeItem(CHUNK_RELOAD_KEY);
    window.sessionStorage.removeItem(LEGACY_CHUNK_RELOAD_KEY);
  } catch {
    // Session storage can be unavailable in private or restricted browser modes.
  }
};

const shouldReloadForChunkError = () => {
  if (typeof window === 'undefined') return false;
  try {
    const reloadMarker = (__APP_COMMIT__ || 'dev').trim() || 'dev';
    if (window.sessionStorage.getItem(CHUNK_RELOAD_KEY) === reloadMarker) {
      return false;
    }
    window.sessionStorage.setItem(CHUNK_RELOAD_KEY, reloadMarker);
    return true;
  } catch {
    return true;
  }
};

export function lazyWithReload<T extends React.ComponentType<any>>(
  factory: () => Promise<{ default: T }>,
) {
  return lazy(() =>
    factory()
      .then((module) => {
        clearChunkReloadMarker();
        return module;
      })
      .catch((error) => {
        if (isChunkLoadError(error) && shouldReloadForChunkError()) {
          window.location.reload();
          // Keep React quiet while the browser swaps in the fresh asset graph.
          return new Promise(() => {}) as Promise<{ default: T }>;
        }

        throw error;
      }),
  );
}
