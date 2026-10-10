/**
 * Client failure telemetry.
 *
 * Every failure that can leave a visitor without usable UI (render crashes,
 * unhandled rejections, chunk preload failures, a blank root, a stalled
 * session bootstrap, auth hand-off failures) is reported here as a
 * `client_error` analytics event so production incidents such as the Android
 * "white screen after Google/signup" reports can be diagnosed from telemetry.
 *
 * Messages are redacted before leaving the browser: emails, JWT/bearer-like
 * tokens and long opaque identifiers are masked, and the event passes through
 * the analytics client's own property sanitizer. Never pass passwords, tokens
 * or form values in `context`.
 */

export type ClientErrorKind =
  | 'app_render'
  | 'boot_render'
  | 'window_error'
  | 'unhandled_rejection'
  | 'chunk_preload'
  | 'blank_screen'
  | 'session_bootstrap'
  | 'auth_flow';

export type ClientErrorContext = Record<string, string | number | boolean | null | undefined>;

const MAX_REPORTS_PER_PAGE = 25;
const MAX_MESSAGE_LENGTH = 300;
const reported = new Set<string>();
let reportCount = 0;

const EMAIL_PATTERN = /[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}/gi;
const JWT_PATTERN = /\beyJ[\w-]+\.[\w-]+\.[\w-]+/g;
const BEARER_PATTERN = /\b(bearer|token|password|secret|credential)\b(\s*[:=]\s*|\s+)\S+/gi;
const OPAQUE_PATTERN = /\b[A-Za-z0-9_-]{32,}\b/g;

export function redactErrorText(value: string): string {
  return value
    .replace(JWT_PATTERN, '[jwt]')
    .replace(BEARER_PATTERN, '$1$2[redacted]')
    .replace(EMAIL_PATTERN, '[email]')
    .replace(OPAQUE_PATTERN, '[id]')
    .slice(0, MAX_MESSAGE_LENGTH);
}

function describeError(error: unknown): { name: string; message: string } {
  if (error instanceof Error) {
    return { name: error.name || 'Error', message: redactErrorText(error.message || '') };
  }
  if (typeof error === 'string') return { name: 'string', message: redactErrorText(error) };
  if (error && typeof error === 'object' && 'message' in error) {
    return { name: 'object', message: redactErrorText(String((error as { message: unknown }).message)) };
  }
  return { name: typeof error, message: '' };
}

function currentRoute(): string {
  if (typeof window === 'undefined') return '';
  // Path only: query strings and hashes can carry private identifiers.
  return window.location.pathname.slice(0, 120);
}

/** Test-only. */
export function __resetClientErrorReportsForTests(): void {
  reported.clear();
  reportCount = 0;
}

export function reportClientError(kind: ClientErrorKind, error: unknown, context: ClientErrorContext = {}): void {
  const { name, message } = describeError(error);
  const route = currentRoute();
  const dedupeKey = `${kind}|${name}|${message}|${route}`;
  if (reported.has(dedupeKey) || reportCount >= MAX_REPORTS_PER_PAGE) return;
  reported.add(dedupeKey);
  reportCount += 1;

  // Keep the failure visible in devtools/remote debugging too.
  console.error(`[tdf] ${kind}`, error);

  const properties = {
    kind,
    error_name: name,
    error_message: message,
    route,
    commit: typeof __APP_COMMIT__ === 'string' ? __APP_COMMIT__ : 'dev',
    online: typeof navigator === 'undefined' ? null : navigator.onLine,
    visibility: typeof document === 'undefined' ? null : document.visibilityState,
    ...context,
  };

  // Analytics stays out of the initial bundle; failures to report are ignored
  // so error reporting can never become a second failure.
  void import('./posthog')
    .then(({ getAnalyticsClient }) => getAnalyticsClient().capture('client_error', properties))
    .catch(() => undefined);
}
