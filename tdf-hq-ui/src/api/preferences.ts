import type { components } from './generated/types';
import { get, post, put } from './client';

export type LocalePreferences = components['schemas']['LocalePreferences'];
export type LocalePreferencesUpdate = components['schemas']['LocalePreferencesUpdate'];

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
