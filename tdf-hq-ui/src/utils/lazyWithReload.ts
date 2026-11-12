import { lazy, type ComponentType } from 'react';

// React.lazy uses ComponentType<any>; keep that compatibility for routed pages with props.
// eslint-disable-next-line @typescript-eslint/no-explicit-any
type LazyComponent = ComponentType<any>;

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
    // Without storage there is no durable reload budget. Keep the manual
    // recovery screen instead of repeatedly discarding the current document.
    return false;
  }
};

/**
 * Reload into the current deploy once per commit after a stale-chunk failure.
 * Returns false when a reload was already attempted, so callers surface the
 * error instead of looping.
 */
export function reloadOnceForChunkError(): boolean {
  if (!shouldReloadForChunkError()) return false;
  window.location.reload();
  return true;
}

export function lazyWithReload<T extends LazyComponent>(
  factory: () => Promise<{ default: T }>,
) {
  return lazy(() =>
    factory()
      .catch((error) => {
        if (isChunkLoadError(error) && reloadOnceForChunkError()) {
          // Keep React quiet while the browser swaps in the fresh asset graph.
          return new Promise<{ default: T }>(() => {
            // Intentionally unresolved.
          });
        }

        throw error;
      }),
  );
}
