import { lazy } from 'react';

const CHUNK_RELOAD_KEY = 'chunk_reload_attempted';

export function lazyWithReload<T extends React.ComponentType<any>>(
  factory: () => Promise<{ default: T }>,
) {
  return lazy(() =>
    factory().catch((error) => {
      const isChunkError =
        error instanceof TypeError &&
        (error.message.includes('Failed to fetch dynamically imported module') ||
          error.message.includes('Expected a JavaScript module script'));

      if (isChunkError) {
        const hasReloaded = sessionStorage.getItem(CHUNK_RELOAD_KEY);
        if (!hasReloaded) {
          sessionStorage.setItem(CHUNK_RELOAD_KEY, '1');
          window.location.reload();
          // Return a never-resolving promise to keep React quiet while we reload
          return new Promise(() => {}) as Promise<{ default: T }>;
        }
      }

      throw error;
    }),
  );
}
