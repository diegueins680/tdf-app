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
 * tokens, embedded URLs and long opaque identifiers are masked, and the event
 * passes through the analytics client's own property sanitizer. Only selected
 * diagnostic context fields are retained. Never pass passwords, tokens or form
 * values in `context`; pattern redaction cannot recognize arbitrary personal text.
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
// Error strings can embed URLs rather than start with one; SDK URL-property
// sanitizers cannot identify short query values inside those free-text fields.
const DIAGNOSTIC_URL_PATTERN = /(?:\b(?:https?|tdf):\/\/|\/)[^\s<>"']+/gi;
const OPAQUE_PATTERN = /\b[A-Za-z0-9_-]{32,}\b/g;

export function redactErrorText(value: string): string {
  return value
    .replace(JWT_PATTERN, '[jwt]')
    .replace(DIAGNOSTIC_URL_PATTERN, '[url]')
    .replace(BEARER_PATTERN, '$1$2[redacted]')
    .replace(EMAIL_PATTERN, '[email]')
    .replace(OPAQUE_PATTERN, '[id]')
    .slice(0, MAX_MESSAGE_LENGTH);
}

function describeError(error: unknown): { name: string; message: string } {
  try {
    if (error instanceof Error) {
      const name = error.name;
      const message = error.message;
      return {
        name: typeof name === 'string' ? redactErrorText(name || 'Error').slice(0, 80) : 'Error',
        message: typeof message === 'string' ? redactErrorText(message) : '',
      };
    }
    if (typeof error === 'string') return { name: 'string', message: redactErrorText(error) };
    if (error && typeof error === 'object' && 'message' in error) {
      return { name: 'object', message: redactErrorText(String((error as { message: unknown }).message)) };
    }
    return { name: typeof error, message: '' };
  } catch {
    // Unknown errors can expose throwing getters. Reporting must stay usable.
    return { name: 'Error', message: '[unavailable]' };
  }
}

function diagnosticContext(context: ClientErrorContext): ClientErrorContext {
  const text = (value: unknown) => typeof value === 'string' ? redactErrorText(value) : undefined;
  const flag = (value: unknown) => typeof value === 'boolean' ? value : undefined;
  try {
    // Deliberately select the diagnostic fields used by our call sites. A new
    // caller cannot export arbitrary account data or replace the core envelope.
    return {
      boundary: text(context['boundary']),
      component_stack: text(context['component_stack']),
      source: text(context['source']),
      watchdog_route: text(context['watchdog_route']),
      method: text(context['method']),
      timed_out: flag(context['timed_out']),
      had_stored_session: flag(context['had_stored_session']),
      creating_account: flag(context['creating_account']),
      linking: flag(context['linking']),
    };
  } catch {
    return {};
  }
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

  // Do not give console collectors the original Error, stack, cause or custom
  // fields. Third-party/browser-owned logging remains outside this boundary.
  try { console.error(`[tdf] ${kind}`, { name, message }); }
  catch { /* Diagnostics must never turn a recoverable failure into a crash. */ }

  const properties = {
    ...diagnosticContext(context),
    kind,
    error_name: name,
    error_message: message,
    route,
    commit: typeof __APP_COMMIT__ === 'string' ? __APP_COMMIT__ : 'dev',
    online: typeof navigator === 'undefined' ? null : navigator.onLine,
    visibility: typeof document === 'undefined' ? null : document.visibilityState,
  };

  // Analytics stays out of the initial bundle; failures to report are ignored
  // so error reporting can never become a second failure.
  void import('./posthog')
    .then(({ getAnalyticsClient }) => getAnalyticsClient().capture('client_error', properties))
    .catch(() => undefined);
}
