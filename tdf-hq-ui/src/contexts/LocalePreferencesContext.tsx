import type { ReactNode } from 'react';
import { createContext, useCallback, useContext, useEffect, useMemo, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import i18n, { LOCALE_STORAGE_KEY, normalizeLocale } from '../i18n';
import { Preferences, type LocalePreferences, type LocalePreferencesUpdate } from '../api/preferences';
import { Catalogs } from '../api/catalogs';
import { useSession } from '../session/SessionContext';

const STORAGE_KEY = 'tdf-hq-ui/locale-preferences';

function browserTimezone(): string {
  try {
    const timezone = Intl.DateTimeFormat().resolvedOptions().timeZone;
    return timezone.length > 0 ? timezone : 'UTC';
  } catch {
    return 'UTC';
  }
}

function defaults(): LocalePreferences {
  const configuredTimezone = import.meta.env.VITE_DEFAULT_TIMEZONE?.trim();
  return {
    localeId: '',
    locale: normalizeLocale(i18n.language) ?? 'en',
    currencyId: '',
    currency: (import.meta.env.VITE_DEFAULT_CURRENCY ?? 'USD').toUpperCase(),
    timezone: configuredTimezone && configuredTimezone.length > 0 ? configuredTimezone : browserTimezone(),
    countryId: null,
    countryCode: null,
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
    return {
      ...fallback,
      localeId: typeof stored.localeId === 'string' ? stored.localeId.trim() : '',
      locale,
      currencyId: typeof stored.currencyId === 'string' ? stored.currencyId.trim() : '',
      currency: typeof stored.currency === 'string' ? stored.currency.toUpperCase() : fallback.currency,
      timezone: typeof stored.timezone === 'string' && stored.timezone.trim() ? stored.timezone : fallback.timezone,
      countryId: typeof stored.countryId === 'string' && stored.countryId.trim() ? stored.countryId.trim() : null,
      countryCode: typeof stored.countryCode === 'string' ? stored.countryCode.toUpperCase() : null,
    };
  } catch {
    return fallback;
  }
}

export function normalizePreferences(value: unknown, fallback: LocalePreferences): LocalePreferences {
  // During a coordinated rollout the previous backend can still return the legacy
  // code-only shape, without localeId, currencyId, or countryId.
  const source = value && typeof value === 'object' ? value as Record<string, unknown> : {};
  const localeId = typeof source['localeId'] === 'string' ? source['localeId'].trim() : fallback.localeId;
  const locale = normalizeLocale(typeof source['locale'] === 'string' ? source['locale'] : undefined)
    ?? normalizeLocale(fallback.locale)
    ?? 'en';
  const currencyId = typeof source['currencyId'] === 'string'
    ? source['currencyId'].trim()
    : fallback.currencyId;
  const currency = typeof source['currency'] === 'string' && source['currency'].trim()
    ? source['currency'].trim().toUpperCase()
    : fallback.currency;
  const timezone = typeof source['timezone'] === 'string' && source['timezone'].trim()
    ? source['timezone'].trim()
    : fallback.timezone;
  const countryId = typeof source['countryId'] === 'string' ? source['countryId'].trim() : fallback.countryId;
  const countryCode = typeof source['countryCode'] === 'string'
    ? source['countryCode'].trim().toUpperCase()
    : fallback.countryCode;
  return {
    localeId,
    locale,
    currencyId,
    currency,
    timezone,
    countryId: countryId && countryId.length > 0 ? countryId : null,
    countryCode: countryCode && countryCode.length > 0 ? countryCode : null,
  };
}

export interface LocalePreferencesContextValue extends LocalePreferences {
  supportedLocales: string[];
  supportedCurrencies: string[];
  savePreferences: (input: LocalePreferencesUpdate) => Promise<void>;
  saving: boolean;
}

const LocalePreferencesContext = createContext<LocalePreferencesContextValue | undefined>(undefined);

export function LocalePreferencesProvider({ children }: { children: ReactNode }) {
  const { session } = useSession();
  const [preferences, setPreferences] = useState<LocalePreferences>(readStoredPreferences);
  const [saving, setSaving] = useState(false);
  const regionalCatalogsQuery = useQuery({
    queryKey: ['catalogs', 'locale-preferences', preferences.locale],
    queryFn: () => Catalogs.listPublicBatch(
      ['locales', 'currencies'],
      { locale: preferences.locale, page: 1, pageSize: 500 },
    ),
  });
  const localeCatalog = useMemo(
    () => regionalCatalogsQuery.data?.catalogs.find((page) => page.catalog.code === 'locales'),
    [regionalCatalogsQuery.data?.catalogs],
  );
  const currencyCatalog = useMemo(
    () => regionalCatalogsQuery.data?.catalogs.find((page) => page.catalog.code === 'currencies'),
    [regionalCatalogsQuery.data?.catalogs],
  );
  const localeOptions = useMemo(() => localeCatalog?.items ?? [], [localeCatalog]);
  const currencyOptions = useMemo(() => currencyCatalog?.items ?? [], [currencyCatalog]);
  const supportedLocales = useMemo(() => localeOptions.map((item) => item.code), [localeOptions]);
  const supportedCurrencies = useMemo(() => currencyOptions.map((item) => item.code), [currencyOptions]);

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
    if (localeOptions.length === 0 || currencyOptions.length === 0) return;
    const selectedLocale = localeOptions.find((item) => item.id === preferences.localeId)
      ?? localeOptions.find((item) => item.code === preferences.locale)
      ?? localeOptions.find((item) => item.id === localeCatalog?.defaults.find(
        (entry) => entry.scopeKind === 'deployment' && entry.scopeId === 'default',
      )?.entityId)
      ?? localeOptions[0];
    const selectedCurrency = currencyOptions.find((item) => item.id === preferences.currencyId)
      ?? currencyOptions.find((item) => item.code === preferences.currency)
      ?? currencyOptions.find((item) => item.id === currencyCatalog?.defaults.find(
        (entry) => entry.scopeKind === 'deployment' && entry.scopeId === 'default',
      )?.entityId)
      ?? currencyOptions[0];
    if (!selectedLocale || !selectedCurrency) return;
    if (
      selectedLocale.id === preferences.localeId
      && selectedLocale.code === preferences.locale
      && selectedCurrency.id === preferences.currencyId
      && selectedCurrency.code === preferences.currency
    ) return;
    apply({
      ...preferences,
      localeId: selectedLocale.id,
      locale: selectedLocale.code,
      currencyId: selectedCurrency.id,
      currency: selectedCurrency.code,
    });
  }, [apply, currencyCatalog, currencyOptions, localeCatalog, localeOptions, preferences]);

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
  }, [apply, session]);

  const savePreferences = useCallback(async (input: LocalePreferencesUpdate) => {
    setSaving(true);
    try {
      if (session) {
        apply(await Preferences.update(input));
      } else {
        const selectedLocale = localeOptions.find((item) => item.id === input.localeId);
        const selectedCurrency = currencyOptions.find((item) => item.id === input.currencyId);
        if (!selectedLocale || !selectedCurrency) {
          throw new Error('Select active locale and currency catalog items.');
        }
        apply({
          ...preferences,
          ...input,
          locale: selectedLocale.code,
          currency: selectedCurrency.code,
          countryCode: preferences.countryCode ?? null,
        });
      }
    } finally {
      setSaving(false);
    }
  }, [apply, currencyOptions, localeOptions, preferences, session]);

  const value = useMemo(
    () => ({ ...preferences, supportedLocales, supportedCurrencies, savePreferences, saving }),
    [preferences, savePreferences, saving, supportedCurrencies, supportedLocales],
  );
  return <LocalePreferencesContext.Provider value={value}>{children}</LocalePreferencesContext.Provider>;
}

export function useLocalePreferences(): LocalePreferencesContextValue {
  const context = useContext(LocalePreferencesContext);
  if (!context) throw new Error('useLocalePreferences must be used within LocalePreferencesProvider');
  return context;
}
