import { logger } from './logger';

// Keep this list explicit. Reading import.meta.env as a dictionary makes Vite
// serialize every VITE_* variable into the browser bundle, including variables
// that were mistakenly configured with server-side secrets.
const BAKED_ENV: Record<string, string | undefined> = {
  VITE_API_BASE: import.meta.env?.VITE_API_BASE,
  VITE_CHATKIT_WORKFLOW_ID: import.meta.env?.VITE_CHATKIT_WORKFLOW_ID,
  VITE_DEFAULT_TIMEZONE: import.meta.env?.VITE_DEFAULT_TIMEZONE,
  VITE_FACEBOOK_APP_ID: import.meta.env?.VITE_FACEBOOK_APP_ID,
  VITE_INSTAGRAM_APP_ID: import.meta.env?.VITE_INSTAGRAM_APP_ID,
  VITE_INSTAGRAM_CLIENT_ID: import.meta.env?.VITE_INSTAGRAM_CLIENT_ID,
  VITE_INSTAGRAM_OAUTH_PROVIDER: import.meta.env?.VITE_INSTAGRAM_OAUTH_PROVIDER,
  VITE_INSTAGRAM_REDIRECT_URI: import.meta.env?.VITE_INSTAGRAM_REDIRECT_URI,
  VITE_INSTAGRAM_SCOPES: import.meta.env?.VITE_INSTAGRAM_SCOPES,
  VITE_META_APP_ID: import.meta.env?.VITE_META_APP_ID,
  VITE_PAYPAL_CLIENT_ID: import.meta.env?.VITE_PAYPAL_CLIENT_ID,
  VITE_POSTHOG_HOST: import.meta.env?.VITE_POSTHOG_HOST,
  VITE_POSTHOG_KEY: import.meta.env?.VITE_POSTHOG_KEY,
  VITE_TIDAL_AGENT_MODEL: import.meta.env?.VITE_TIDAL_AGENT_MODEL,
};

const readEnvValue = (key: string): string | undefined => {
  const baked = BAKED_ENV[key];
  if (typeof baked === 'string' && baked.trim()) return baked.trim();
  if (typeof window !== 'undefined') {
    const win = window as EnvWindow;
    const runtimeVal = win.__ENV__?.[key];
    if (typeof runtimeVal === 'string' && runtimeVal.trim()) return runtimeVal.trim();
  }
  return undefined;
};

type EnvWindow = typeof window & {
  __ENV__?: Record<string, string | undefined>;
  __MISSING_ENV__?: string[];
  __MISSING_ENV_KEYS__?: string[];
  __MISSING_ENV_REPORTED__?: string[];
};

const uniqueList = (values: readonly string[]): string[] => {
  return Array.from(new Set(values));
};

/**
 * Log missing env vars once and store them on window for admin-only diagnostics.
 */
export const reportMissingEnv = (keys: string[]): void => {
  if (typeof window === 'undefined') return;
  const win = window as EnvWindow;
  const checkedKeys = uniqueList([...(win.__MISSING_ENV_KEYS__ ?? []), ...keys]);
  const missing = checkedKeys.filter((key) => !readEnvValue(key));
  const reported = new Set(win.__MISSING_ENV_REPORTED__ ?? []);
  const newlyMissing = missing.filter((key) => !reported.has(key));

  // Expose for admin dashboards without surfacing in customer UI.
  win.__MISSING_ENV_KEYS__ = checkedKeys;
  win.__MISSING_ENV__ = missing;

  if (newlyMissing.length > 0) {
    win.__MISSING_ENV_REPORTED__ = uniqueList([...(win.__MISSING_ENV_REPORTED__ ?? []), ...newlyMissing]);
    logger.warn('Faltan variables de entorno críticas', { keys: newlyMissing });
  }
};

export const env = {
  read: readEnvValue,
};
