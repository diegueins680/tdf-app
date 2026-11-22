import { jest } from '@jest/globals';
import { isChunkLoadError, reloadOnceForChunkError } from './lazyWithReload';

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


describe('chunk reload budget', () => {
  beforeEach(() => {
    window.sessionStorage.clear();
    Object.defineProperty(globalThis, '__APP_COMMIT__', { value: 'synthetic-audit-commit', configurable: true });
  });
  afterEach(() => jest.restoreAllMocks());

  it('keeps a per-commit budget instead of resetting it when another route succeeds', () => {
    // jsdom records reload as an unimplemented navigation; storage establishes
    // the decision and the browser integration exercises the actual reload.
    jest.spyOn(console, 'error').mockImplementation(() => undefined);
    expect(reloadOnceForChunkError()).toBe(true);
    expect(window.sessionStorage.getItem('tdf-hq-ui/chunk-reload-attempted')).toBe('synthetic-audit-commit');
    expect(reloadOnceForChunkError()).toBe(false);
  });

  it('leaves manual recovery available when storage cannot retain a reload budget', () => {
    jest.spyOn(Storage.prototype, 'getItem').mockImplementation(() => { throw new Error('Storage refused'); });
    expect(reloadOnceForChunkError()).toBe(false);
  });
});
