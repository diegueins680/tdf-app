import i18n from 'i18next';
import { initReactI18next } from 'react-i18next';
import de from './locales/de';
import en from './locales/en';
import es from './locales/es';
import fr from './locales/fr';
import pt from './locales/pt';

// These keys describe translation bundles compiled into this build. They are
// a renderer capability boundary, not the selectable locale catalog; the
// backend's persisted deployment enablement remains authoritative for choices.
const resources = {
  en: { translation: en },
  es: { translation: es },
  fr: { translation: fr },
  de: { translation: de },
  pt: { translation: pt },
} as const;
type SupportedLocale = keyof typeof resources;
export const LOCALE_STORAGE_KEY = 'tdf-hq-ui/locale';

export function normalizeLocale(value: string | null | undefined): SupportedLocale | null {
  const base = value?.trim().toLowerCase().split(/[-_]/)[0];
  return base && Object.prototype.hasOwnProperty.call(resources, base) ? base as SupportedLocale : null;
}

function initialLocale(): SupportedLocale {
  if (typeof window !== 'undefined') {
    const stored = normalizeLocale(window.localStorage.getItem(LOCALE_STORAGE_KEY));
    if (stored) return stored;
  }
  const envDefault = normalizeLocale(import.meta.env?.VITE_DEFAULT_LOCALE);
  if (envDefault) return envDefault;
  return 'es';
}

const detectedLocale = initialLocale();
if (typeof document !== 'undefined') {
  document.documentElement.lang = detectedLocale;
  document.documentElement.dir = 'ltr';
}

void i18n.use(initReactI18next).init({
  lng: detectedLocale,
  fallbackLng: 'en',
  supportedLngs: Object.keys(resources),
  load: 'languageOnly',
  interpolation: { escapeValue: false },
  resources,
});

i18n.on('languageChanged', (language) => {
  const normalized = normalizeLocale(language) ?? 'en';
  if (typeof window !== 'undefined') window.localStorage.setItem(LOCALE_STORAGE_KEY, normalized);
  if (typeof document !== 'undefined') {
    document.documentElement.lang = normalized;
    document.documentElement.dir = 'ltr';
  }
});

export default i18n;
