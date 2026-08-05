import { get, post, put } from './client';

export interface LocalePreferences {
  locale: string;
  currency: string;
  timezone: string;
  countryCode: string | null;
  supportedLocales: string[];
  supportedCurrencies: string[];
}

export interface LocalePreferencesUpdate {
  locale: string;
  currency: string;
  timezone: string;
  countryCode: string | null;
}

export const Preferences = {
  get: () => get<LocalePreferences>('/session/preferences'),
  update: (input: LocalePreferencesUpdate) =>
    put<LocalePreferences>('/session/preferences', input),
  auditConversion: (input: {
    sourceCurrency: string;
    targetCurrency: string;
    sourceMinorUnits: number;
    targetMinorUnits: number;
    exchangeRate: number;
    rateSource: string;
  }) => post<void>('/session/currency-conversions', input),
};
