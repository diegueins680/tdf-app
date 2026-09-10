/**
 * Minimal logger that only emits in development.
 * Use this instead of direct console.* calls to avoid leaking
 * internal state in production builds.
 */
const isDev = Boolean(import.meta.env?.DEV);

export const logger = {
  log: (...args: unknown[]) => {
    // This module is the controlled exception to the app-wide no-console rule.
    // eslint-disable-next-line no-console
    if (isDev) console.log(...args);
  },
  warn: (...args: unknown[]) => {
    // This module is the controlled exception to the app-wide no-console rule.
    // eslint-disable-next-line no-console
    if (isDev) console.warn(...args);
  },
  error: (...args: unknown[]) => {
    // Errors always log — they indicate real problems
    console.error(...args);
  },
};
