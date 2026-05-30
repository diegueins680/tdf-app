/**
 * Minimal logger that only emits in development.
 * Use this instead of direct console.* calls to avoid leaking
 * internal state in production builds.
 */
// Guard the access: import.meta.env is defined by Vite at build time but is
// absent under some runners (e.g. jest), where reading `.DEV` directly throws.
const isDev = Boolean((import.meta.env as { DEV?: boolean } | undefined)?.DEV);

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
