import type { ReactNode } from 'react';
import { createContext, useCallback, useContext, useEffect, useMemo, useState } from 'react';
import i18n, { LOCALE_STORAGE_KEY, normalizeLocale, SUPPORTED_LOCALES } from '../i18n';
import { Preferences, type LocalePreferences, type LocalePreferencesUpdate } from '../api/preferences';
import { useSession } from '../session/SessionContext';

const STORAGE_KEY = 'tdf-hq-ui/locale-preferences';
const DEFAULT_CURRENCIES = ['USD', 'EUR', 'GBP', 'CAD', 'AUD', 'JPY', 'BRL'];

function envList(raw: string | undefined, fallback: string[]): string[] {
  const values = raw?.split(',').map((value) => value.trim()).filter(Boolean) ?? [];
  return values.length > 0 ? [...new Set(values)] : fallback;
}

function defaults(): LocalePreferences {
  const supportedLocales = envList(import.meta.env?.VITE_SUPPORTED_LOCALES, [...SUPPORTED_LOCALES])
    .map((value) => normalizeLocale(value))
    .filter((value): value is NonNullable<typeof value> => value !== null);
  const supportedCurrencies = envList(import.meta.env?.VITE_SUPPORTED_CURRENCIES, DEFAULT_CURRENCIES)
    .map((value) => value.toUpperCase());
  const locale = normalizeLocale(i18n.language) ?? supportedLocales[0] ?? 'en';
  const requestedCurrency = (import.meta.env.VITE_DEFAULT_CURRENCY ?? 'USD').toUpperCase();
  return {
    locale,
    currency: supportedCurrencies.includes(requestedCurrency) ? requestedCurrency : supportedCurrencies[0] ?? 'USD',
    timezone: import.meta.env.VITE_DEFAULT_TIMEZONE ?? 'America/Guayaquil',
    countryCode: null,
    supportedLocales,
    supportedCurrencies,
  };
}

function readStoredPreferences(): LocalePreferences {
  const fallback = defaults();
  if (typeof window === 'undefined') return fallback;
  try {
    const raw = window.localStorage.getItem(STORAGE_KEY);
    if (!raw) return fallback;
    const stored = JSON.parse(raw) as Partial<LocalePreferences>;
    const locale = normalizeLocale(stored.locale) ?? fallback.locale;
    const currency = typeof stored.currency === 'string' ? stored.currency.toUpperCase() : fallback.currency;
    return {
      ...fallback,
      locale,
      currency: fallback.supportedCurrencies.includes(currency) ? currency : fallback.currency,
      timezone: typeof stored.timezone === 'string' && stored.timezone.trim() ? stored.timezone : fallback.timezone,
      countryCode: typeof stored.countryCode === 'string' ? stored.countryCode.toUpperCase() : null,
    };
  } catch {
    return fallback;
  }
}

function normalizePreferences(value: LocalePreferences, fallback: LocalePreferences): LocalePreferences {
  const supportedLocales = value.supportedLocales
    .map((entry) => normalizeLocale(entry))
    .filter((entry): entry is NonNullable<typeof entry> => entry !== null);
  const supportedCurrencies = value.supportedCurrencies.map((entry) => entry.toUpperCase());
  const locale = normalizeLocale(value.locale) ?? normalizeLocale(fallback.locale) ?? 'en';
  const currency = value.currency.toUpperCase();
  return {
    locale: supportedLocales.includes(locale) ? locale : supportedLocales[0] ?? fallback.locale,
    currency: supportedCurrencies.includes(currency) ? currency : supportedCurrencies[0] ?? fallback.currency,
    timezone: value.timezone.trim() || fallback.timezone,
    countryCode: value.countryCode?.trim().toUpperCase() || null,
    supportedLocales: supportedLocales.length > 0 ? supportedLocales : fallback.supportedLocales,
    supportedCurrencies: supportedCurrencies.length > 0 ? supportedCurrencies : fallback.supportedCurrencies,
  };
}

export interface LocalePreferencesContextValue extends LocalePreferences {
  savePreferences: (input: LocalePreferencesUpdate) => Promise<void>;
  saving: boolean;
}

const LocalePreferencesContext = createContext<LocalePreferencesContextValue | undefined>(undefined);

export function LocalePreferencesProvider({ children }: { children: ReactNode }) {
  const { session } = useSession();
  const [preferences, setPreferences] = useState<LocalePreferences>(readStoredPreferences);
  const [saving, setSaving] = useState(false);

  const apply = useCallback((next: LocalePreferences) => {
    const normalized = normalizePreferences(next, defaults());
    setPreferences(normalized);
    if (typeof window !== 'undefined') {
      window.localStorage.setItem(STORAGE_KEY, JSON.stringify(normalized));
      window.localStorage.setItem(LOCALE_STORAGE_KEY, normalized.locale);
    }
    void i18n.changeLanguage(normalized.locale);
  }, []);

  useEffect(() => {
    if (!session) return;
    if (session.preferences) {
      apply(session.preferences);
      return;
    }

    let cancelled = false;
    void Preferences.get()
      .then((remotePreferences) => {
        if (!cancelled) apply(remotePreferences);
      })
      .catch(() => {
        // A stored/browser preference remains usable when the profile request fails.
      });
    return () => {
      cancelled = true;
    };
  }, [apply, session?.preferences, session?.username]);

  const savePreferences = useCallback(async (input: LocalePreferencesUpdate) => {
    setSaving(true);
    try {
      if (session) {
        apply(await Preferences.update(input));
      } else {
        apply({ ...preferences, ...input });
      }
    } finally {
      setSaving(false);
    }
  }, [apply, preferences, session]);

  const value = useMemo(() => ({ ...preferences, savePreferences, saving }), [preferences, savePreferences, saving]);
  return <LocalePreferencesContext.Provider value={value}>{children}</LocalePreferencesContext.Provider>;
}

export function useLocalePreferences(): LocalePreferencesContextValue {
  const context = useContext(LocalePreferencesContext);
  if (!context) throw new Error('useLocalePreferences must be used within LocalePreferencesProvider');
  return context;
}
