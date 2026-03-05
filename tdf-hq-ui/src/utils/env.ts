const readEnvValue = (key: string): string | undefined => {
  const baked = (import.meta as unknown as { env?: Record<string, unknown> }).env?.[key];
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
    console.warn('Faltan variables de entorno críticas:', newlyMissing.join(', '));
  }
};

export const env = {
  read: readEnvValue,
};
