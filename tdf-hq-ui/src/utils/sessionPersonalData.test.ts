import {
  clearSessionPersonalData,
  readSessionPersonalData,
  writeSessionPersonalData,
} from './sessionPersonalData';

const STORAGE_KEY = 'test-personal-data';

describe('sessionPersonalData', () => {
  beforeEach(() => {
    window.localStorage.clear();
    window.sessionStorage.clear();
  });

  it('discards a legacy persistent value instead of exposing it in the current tab', () => {
    window.localStorage.setItem(STORAGE_KEY, '{"email":"test@example.com"}');

    expect(readSessionPersonalData(STORAGE_KEY)).toBeNull();
    expect(window.localStorage.getItem(STORAGE_KEY)).toBeNull();
    expect(window.sessionStorage.getItem(STORAGE_KEY)).toBeNull();
  });

  it('keeps a current-tab value while removing a stale persistent copy', () => {
    window.localStorage.setItem(STORAGE_KEY, 'legacy');
    window.sessionStorage.setItem(STORAGE_KEY, 'current');

    expect(readSessionPersonalData(STORAGE_KEY)).toBe('current');
    expect(window.localStorage.getItem(STORAGE_KEY)).toBeNull();
    expect(window.sessionStorage.getItem(STORAGE_KEY)).toBe('current');
  });

  it('writes only to session storage and clears both current and legacy copies', () => {
    window.localStorage.setItem(STORAGE_KEY, 'legacy');

    expect(writeSessionPersonalData(STORAGE_KEY, 'current')).toBe(true);
    expect(window.localStorage.getItem(STORAGE_KEY)).toBeNull();
    expect(window.sessionStorage.getItem(STORAGE_KEY)).toBe('current');

    window.localStorage.setItem(STORAGE_KEY, 'stale');
    clearSessionPersonalData(STORAGE_KEY);
    expect(window.localStorage.getItem(STORAGE_KEY)).toBeNull();
    expect(window.sessionStorage.getItem(STORAGE_KEY)).toBeNull();
  });
});
