import {
  clearAllSessionPersonalData,
  clearSessionPersonalData,
  readSessionPersonalData,
  reconcileSessionPersonalData,
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

  it('preserves an anonymous draft when the user authenticates in the same tab', () => {
    writeSessionPersonalData('tdf-marketplace-buyer', 'anonymous checkout');

    expect(reconcileSessionPersonalData(null, 42)).toBe(false);
    expect(window.sessionStorage.getItem('tdf-marketplace-buyer')).toBe('anonymous checkout');
  });

  it('preserves drafts while the same authenticated identity is refreshed', () => {
    writeSessionPersonalData('tdf-public-booking-profile', 'current booking');

    expect(reconcileSessionPersonalData(42, 42)).toBe(false);
    expect(window.sessionStorage.getItem('tdf-public-booking-profile')).toBe('current booking');
  });

  it('clears every registered and legacy-known draft on logout', () => {
    writeSessionPersonalData(STORAGE_KEY, 'registered value');
    window.sessionStorage.setItem('tdf-marketplace-buyer', 'buyer details');
    window.localStorage.setItem('tdf-public-booking-profile', 'legacy booking details');

    expect(reconcileSessionPersonalData(42, null)).toBe(true);
    expect(window.sessionStorage.getItem(STORAGE_KEY)).toBeNull();
    expect(window.sessionStorage.getItem('tdf-marketplace-buyer')).toBeNull();
    expect(window.localStorage.getItem('tdf-public-booking-profile')).toBeNull();
  });

  it('clears current-tab personal data before switching accounts', () => {
    writeSessionPersonalData('tdf-marketplace-buyer', 'first account');

    expect(reconcileSessionPersonalData(42, 84)).toBe(true);
    expect(window.sessionStorage.getItem('tdf-marketplace-buyer')).toBeNull();
  });

  it('can explicitly clear the complete personal-data registry', () => {
    writeSessionPersonalData(STORAGE_KEY, 'registered value');
    writeSessionPersonalData('tdf-marketplace-buyer', 'buyer details');

    clearAllSessionPersonalData();
    expect(window.sessionStorage.getItem(STORAGE_KEY)).toBeNull();
    expect(window.sessionStorage.getItem('tdf-marketplace-buyer')).toBeNull();
  });
});
