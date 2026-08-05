export interface FormatOptions {
  locale: string;
  timeZone: string;
}

const LOCALE_PREFERENCES_STORAGE_KEY = 'tdf-hq-ui/locale-preferences';

interface StoredLocalePreferences {
  locale?: unknown;
  timezone?: unknown;
  currency?: unknown;
}

function readStoredLocalePreferences(): StoredLocalePreferences | null {
  if (typeof window === 'undefined') return null;

  let rawPreferences: string | null;
  try {
    rawPreferences = window.localStorage.getItem(LOCALE_PREFERENCES_STORAGE_KEY);
  } catch {
    return null;
  }
  if (rawPreferences === null) return null;

  let parsedPreferences: unknown;
  try {
    parsedPreferences = JSON.parse(rawPreferences);
  } catch {
    return null;
  }
  if (parsedPreferences === null || typeof parsedPreferences !== 'object' || Array.isArray(parsedPreferences)) {
    return null;
  }
  return parsedPreferences as StoredLocalePreferences;
}

function browserFormatOptions(): FormatOptions {
  const resolved = Intl.DateTimeFormat().resolvedOptions();
  const documentLocale = typeof document === 'undefined' ? '' : document.documentElement.lang.trim();
  return {
    locale: documentLocale || resolved.locale || (typeof navigator === 'undefined' ? 'en' : navigator.language) || 'en',
    timeZone: resolved.timeZone || 'UTC',
  };
}

export function resolveRuntimeFormatOptions(): FormatOptions {
  const fallbackFormatOptions = browserFormatOptions();
  const storedFormatPreferences = readStoredLocalePreferences();
  if (!storedFormatPreferences) return fallbackFormatOptions;

  try {
    const locale = typeof storedFormatPreferences.locale === 'string' && storedFormatPreferences.locale.trim()
      ? storedFormatPreferences.locale.trim()
      : fallbackFormatOptions.locale;
    const timeZone = typeof storedFormatPreferences.timezone === 'string' && storedFormatPreferences.timezone.trim()
      ? storedFormatPreferences.timezone.trim()
      : fallbackFormatOptions.timeZone;
    new Intl.DateTimeFormat(locale, { timeZone }).format(0);
    return { locale, timeZone };
  } catch {
    return fallbackFormatOptions;
  }
}

export function resolveRuntimeCurrency(): string {
  const runtimeEnv = (import.meta.env ?? {}) as Record<string, string | undefined>;
  const fallbackCurrency = (runtimeEnv['VITE_DEFAULT_CURRENCY'] ?? 'USD').trim().toUpperCase() || 'USD';
  const storedCurrencyPreferences = readStoredLocalePreferences();
  return typeof storedCurrencyPreferences?.currency === 'string'
      && /^[A-Za-z]{3}$/.test(storedCurrencyPreferences.currency.trim())
    ? storedCurrencyPreferences.currency.trim().toUpperCase()
    : fallbackCurrency;
}

export function formatCurrency(
  amount: number,
  currency: string,
  locale: string,
  options: Omit<Intl.NumberFormatOptions, 'style' | 'currency'> = {},
): string {
  return new Intl.NumberFormat(locale, {
    style: 'currency',
    currency: currency.toUpperCase(),
    ...options,
  }).format(amount);
}

export function formatNumber(
  value: number,
  locale: string,
  options: Intl.NumberFormatOptions = {},
): string {
  return new Intl.NumberFormat(locale, options).format(value);
}

export function formatDate(
  value: Date | string | number,
  { locale, timeZone }: FormatOptions,
  options: Intl.DateTimeFormatOptions = { dateStyle: 'medium' },
): string {
  const date = value instanceof Date ? value : new Date(value);
  if (Number.isNaN(date.getTime())) return String(value);
  return new Intl.DateTimeFormat(locale, { ...options, timeZone }).format(date);
}

export function formatDateTime(
  value: Date | string | number,
  options: FormatOptions,
): string {
  return formatDate(value, options, { dateStyle: 'medium', timeStyle: 'short' });
}

export function formatCurrencyForUser(
  amount: number,
  currency: string,
  options: Omit<Intl.NumberFormatOptions, 'style' | 'currency'> = {},
): string {
  return formatCurrency(amount, currency, resolveRuntimeFormatOptions().locale, options);
}

export function formatNumberForUser(value: number, options: Intl.NumberFormatOptions = {}): string {
  return formatNumber(value, resolveRuntimeFormatOptions().locale, options);
}

export function formatDateForUser(
  value: Date | string | number,
  options: Intl.DateTimeFormatOptions = { dateStyle: 'medium' },
): string {
  return formatDate(value, resolveRuntimeFormatOptions(), options);
}

export function formatDateTimeForUser(value: Date | string | number): string {
  return formatDateTime(value, resolveRuntimeFormatOptions());
}
