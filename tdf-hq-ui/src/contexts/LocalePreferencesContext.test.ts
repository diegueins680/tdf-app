import type { LocalePreferences } from '../api/preferences';
import { normalizePreferences, rememberChosenTimezone, wasTimezoneChosenOnThisDevice } from './LocalePreferencesContext';

const fallback: LocalePreferences = {
  localeId: '',
  locale: 'en',
  currencyId: '',
  currency: 'USD',
  timezone: 'UTC',
  countryId: null,
  countryCode: null,
  showEventRsvpsOnProfile: true,
};

describe('normalizePreferences', () => {
  it('accepts the legacy production response without canonical catalog ids', () => {
    expect(normalizePreferences({
      locale: ' es ',
      currency: ' usd ',
      timezone: ' America/Guayaquil ',
      countryCode: ' ec ',
    }, fallback)).toEqual({
      localeId: '',
      locale: 'es',
      currencyId: '',
      currency: 'USD',
      timezone: 'America/Guayaquil',
      countryId: null,
      countryCode: 'EC',
      showEventRsvpsOnProfile: true,
    });
  });

  it('falls back safely when a response contains absent or malformed fields', () => {
    expect(normalizePreferences({
      localeId: undefined,
      locale: null,
      currencyId: 42,
      currency: '',
      timezone: undefined,
      countryId: false,
      countryCode: [],
    }, fallback)).toEqual(fallback);
  });
});

describe('time zone chosen on this device', () => {
  beforeEach(() => { window.localStorage.clear(); });

  it('tells a saved zone apart from the server placeholder', () => {
    expect(wasTimezoneChosenOnThisDevice('UTC')).toBe(false);
    rememberChosenTimezone(' UTC ');
    expect(wasTimezoneChosenOnThisDevice('UTC')).toBe(true);
    expect(wasTimezoneChosenOnThisDevice('America/Guayaquil')).toBe(false);
  });

  it('ignores an empty zone', () => {
    rememberChosenTimezone('  ');
    rememberChosenTimezone(null);
    expect(wasTimezoneChosenOnThisDevice('')).toBe(false);
  });
});
