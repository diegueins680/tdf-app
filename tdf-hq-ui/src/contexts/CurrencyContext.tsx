import type { ReactNode } from 'react';
import { createContext, useCallback, useContext, useEffect, useMemo, useState } from 'react';
import { useLocalePreferences } from './LocalePreferencesContext';
import { formatCurrency } from '../utils/formatters';
import { Preferences } from '../api/preferences';
import { useSession } from '../session/SessionContext';

interface CachedRates {
  base: string;
  fetchedAt: number;
  rates: Record<string, number>;
}

export interface CurrencyContextValue {
  currency: string;
  rates: Record<string, number>;
  ratesLoading: boolean;
  convert: (amount: number, fromCurrency: string, toCurrency?: string) => number | null;
  formatMoney: (amount: number, sourceCurrency?: string) => string;
}

const CACHE_KEY = 'tdf-hq-ui/exchange-rates';
const CACHE_TTL_MS = 24 * 60 * 60 * 1000;
const CurrencyContext = createContext<CurrencyContextValue | undefined>(undefined);

export function convertCurrency(
  amount: number,
  fromCurrency: string,
  toCurrency: string,
  rates: Record<string, number>,
): number | null {
  const from = fromCurrency.toUpperCase();
  const to = toCurrency.toUpperCase();
  if (from === to) return amount;
  const fromRate = rates[from];
  const toRate = rates[to];
  if (!fromRate || !toRate) return null;
  return (amount / fromRate) * toRate;
}

function readCachedRates(): CachedRates | null {
  if (typeof window === 'undefined') return null;
  try {
    const parsed = JSON.parse(window.localStorage.getItem(CACHE_KEY) ?? '') as CachedRates;
    return parsed.base && Date.now() - parsed.fetchedAt < CACHE_TTL_MS ? parsed : null;
  } catch {
    return null;
  }
}

function toMinorUnits(amount: number, currency: string, locale: string): number {
  const fractionDigits = new Intl.NumberFormat(locale, { style: 'currency', currency })
    .resolvedOptions().maximumFractionDigits ?? 2;
  return Math.round(amount * (10 ** fractionDigits));
}

export function CurrencyProvider({ children }: { children: ReactNode }) {
  const { currency, locale } = useLocalePreferences();
  const { session } = useSession();
  const rateBase = (import.meta.env.VITE_DEFAULT_CURRENCY ?? 'USD').toUpperCase();
  const [rates, setRates] = useState<Record<string, number>>({ [rateBase]: 1 });
  const [ratesLoading, setRatesLoading] = useState(false);

  useEffect(() => {
    const cached = readCachedRates();
    if (cached?.base === rateBase) {
      setRates({ ...cached.rates, [rateBase]: 1 });
      return;
    }
    const controller = new AbortController();
    const endpoint = (import.meta.env.VITE_EXCHANGE_RATE_API_BASE ?? 'https://api.frankfurter.app').replace(/\/$/, '');
    setRatesLoading(true);
    void fetch(`${endpoint}/latest?from=${encodeURIComponent(rateBase)}`, { signal: controller.signal })
      .then(async (response) => {
        if (!response.ok) throw new Error(`Exchange-rate service returned ${response.status}`);
        return response.json() as Promise<{ rates?: Record<string, number> }>;
      })
      .then((payload) => {
        const next = { ...(payload.rates ?? {}), [rateBase]: 1 };
        setRates(next);
        window.localStorage.setItem(CACHE_KEY, JSON.stringify({ base: rateBase, fetchedAt: Date.now(), rates: next }));
      })
      .catch((error: unknown) => {
        if (error instanceof DOMException && error.name === 'AbortError') return;
      })
      .finally(() => setRatesLoading(false));
    return () => controller.abort();
  }, [rateBase]);

  const convert = useCallback((amount: number, fromCurrency: string, toCurrency = currency): number | null => {
    const converted = convertCurrency(amount, fromCurrency, toCurrency, rates);
    const from = fromCurrency.toUpperCase();
    const to = toCurrency.toUpperCase();
    if (converted !== null && from !== to && session) {
      void Preferences.auditConversion({
        sourceCurrency: from,
        targetCurrency: to,
        sourceMinorUnits: toMinorUnits(amount, from, locale),
        targetMinorUnits: toMinorUnits(converted, to, locale),
        exchangeRate: rates[to]! / rates[from]!,
        rateSource: 'frankfurter',
      }).catch(() => undefined);
    }
    return converted;
  }, [currency, locale, rates, session]);

  const formatMoney = useCallback((amount: number, sourceCurrency = currency): string => {
    const converted = convertCurrency(amount, sourceCurrency, currency, rates);
    return converted === null
      ? formatCurrency(amount, sourceCurrency, locale)
      : formatCurrency(converted, currency, locale);
  }, [currency, locale, rates]);

  const value = useMemo(() => ({ currency, rates, ratesLoading, convert, formatMoney }), [currency, rates, ratesLoading, convert, formatMoney]);
  return <CurrencyContext.Provider value={value}>{children}</CurrencyContext.Provider>;
}

export function useCurrency(): CurrencyContextValue {
  const context = useContext(CurrencyContext);
  if (!context) throw new Error('useCurrency must be used within CurrencyProvider');
  return context;
}
