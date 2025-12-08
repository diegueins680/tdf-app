const readEnvValue = (key: string): string | undefined => {
  const baked = import.meta.env[key];
  if (typeof baked === 'string' && baked.trim()) return baked.trim();
  if (typeof window !== 'undefined') {
    const win = window as typeof window & { __ENV__?: Record<string, string | undefined> };
    const runtimeVal = win.__ENV__?.[key];
    if (typeof runtimeVal === 'string' && runtimeVal.trim()) return runtimeVal.trim();
  }
  return undefined;
};

/**
 * Log missing env vars once and store them on window for admin-only diagnostics.
 */
export const reportMissingEnv = (keys: string[]): void => {
  if (typeof window === 'undefined') return;
  const missing = keys.filter((key) => !readEnvValue(key));
  if (missing.length === 0) return;
  // Expose for admin dashboards without surfacing in customer UI
  (window as typeof window & { __MISSING_ENV__?: string[] }).__MISSING_ENV__ = missing;
  console.warn('Faltan variables de entorno críticas:', missing.join(', '));
};

export const env = {
  read: readEnvValue,
};
