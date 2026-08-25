import { isChunkLoadError } from './lazyWithReload';

describe('isChunkLoadError', () => {
  it('recognizes browser dynamic import failures that can be fixed by a reload', () => {
    expect(isChunkLoadError(new TypeError('Failed to fetch dynamically imported module'))).toBe(true);
    expect(isChunkLoadError(new TypeError('error loading dynamically imported module'))).toBe(true);
    expect(isChunkLoadError(new TypeError('Importing a module script failed.'))).toBe(true);
    expect(isChunkLoadError(new Error('Unable to preload CSS for /assets/page.css'))).toBe(true);
    expect(isChunkLoadError(Object.assign(new Error('stale chunk'), { name: 'ChunkLoadError' }))).toBe(true);
  });

  it('does not classify ordinary render errors as chunk load failures', () => {
    expect(isChunkLoadError(new Error('Cannot read properties of undefined'))).toBe(false);
    expect(isChunkLoadError('plain failure')).toBe(false);
  });
});
