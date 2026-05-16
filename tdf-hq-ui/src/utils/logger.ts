/**
 * Minimal logger that only emits in development.
 * Use this instead of direct console.* calls to avoid leaking
 * internal state in production builds.
 */
const isDev = import.meta.env.DEV;

export const logger = {
  log: (...args: unknown[]) => {
    if (isDev) console.log(...args);
  },
  warn: (...args: unknown[]) => {
    if (isDev) console.warn(...args);
  },
  error: (...args: unknown[]) => {
    // Errors always log — they indicate real problems
    console.error(...args);
  },
};
