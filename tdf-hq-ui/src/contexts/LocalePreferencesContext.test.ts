import type { LocalePreferences } from '../api/preferences';
import { normalizePreferences } from './LocalePreferencesContext';

const fallback: LocalePreferences = {
  localeId: '',
  locale: 'en',
  currencyId: '',
  currency: 'USD',
  timezone: 'UTC',
  countryId: null,
  countryCode: null,
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
