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

  it('migrates a legacy persistent value into the current tab and removes the original', () => {
    window.localStorage.setItem(STORAGE_KEY, '{"email":"test@example.com"}');

    expect(readSessionPersonalData(STORAGE_KEY)).toBe('{"email":"test@example.com"}');
    expect(window.localStorage.getItem(STORAGE_KEY)).toBeNull();
    expect(window.sessionStorage.getItem(STORAGE_KEY)).toBe('{"email":"test@example.com"}');
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
