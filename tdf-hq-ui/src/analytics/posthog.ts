/**
 * posthog.ts
 *
 * PostHog client singleton for tdf-hq-ui (web).
 *
 * - Reads config from VITE_POSTHOG_KEY / VITE_POSTHOG_HOST.
 *   Defaults to EU cloud (https://eu.i.posthog.com).
 * - If no key is configured, exposes a no-op client so the rest of the
 *   app never crashes on missing env (preview deploys, local dev, etc).
 * - Session recording is disabled by default (privacy-first).
 *
 * See: docs/analytics.md
 */
import posthog from 'posthog-js';
import { env } from '../utils/env';
import { logger } from '../utils/logger';

function readConfig() {
  return {
    key: env.read('VITE_POSTHOG_KEY'),
    host: env.read('VITE_POSTHOG_HOST') ?? 'https://eu.i.posthog.com',
  };
}

export interface AnalyticsClient {
  ready: boolean;
  capture: (event: string, properties?: Record<string, unknown>) => void;
  identify: (distinctId: string, properties?: Record<string, unknown>) => void;
  reset: () => void;
  page: (name?: string, properties?: Record<string, unknown>) => void;
}

let cachedClient: AnalyticsClient | null = null;

const REDACTED_QUERY_VALUE = '[REDACTED]';
// Only acquisition metadata is needed from URL queries. New private navigation
// parameters must be masked by default, without waiting for a route-specific rule.
const PUBLIC_ATTRIBUTION_QUERY = /^(?:utm_(?:source|medium|campaign|content|term)|ref|referral|referral_code)$/i;
const SENSITIVE_PROPERTY_NAMES = new Set([
  'authorization',
  'cookie',
  'email',
  'emailaddress',
  'phone',
  'phonenumber',
  'username',
  'displayname',
  'role',
  'roles',
  'partyid',
  'password',
  'currentpassword',
  'newpassword',
  'token',
  'accesstoken',
  'refreshtoken',
  'idtoken',
  'code',
  'state',
  'secret',
  'clientsecret',
  'key',
  'apikey',
  'lookuptoken',
  'orderlookuptoken',
  'xorderlookuptoken',
  'ticketcode',
  'ticketcodes',
  'qrcode',
  'qrpayload',
  'transfercode',
  'reservationcode',
  'buyername',
  'buyeremail',
  'buyerphone',
  'holdername',
  'holderemail',
  'recipientemail',
  'orderid',
  'ordernumber',
  'ticketid',
  'paypalorderid',
  'providerorderid',
  'paymentintentid',
  'quoteid',
  'resourcepath',
  'buyer',
  'holder',
  'tickets',
]);

const normalizePropertyName = (key: string): string => key.replace(/[^a-z\d]/gi, '').toLowerCase();

const isSensitivePropertyName = (key: string): boolean =>
  SENSITIVE_PROPERTY_NAMES.has(normalizePropertyName(key));

// Private order/credential paths also reach SDK-generated URL properties. Keep
// public event IDs for funnel analysis, but never export private resource IDs.
const privatePath = (pathname: string): string => pathname.replace(
  /(\/(?:orden|pedido|orders|ticket-orders|ticket-transfers|tickets|checkouts|cotizaciones|scan|notificaciones|perfil|members|tareas|auditorias|interno|documents)\/)[^/]+/gi,
  '$1[REDACTED]',
).replace(/(\/conversacion\/[^/]+\/)[^/]+/gi, '$1[REDACTED]');

export function redactSensitiveQueryValues(value: string, depth = 0): string {
  const isAbsolute = /^[a-z][a-z\d+.-]*:/i.test(value);
  if (!isAbsolute && !value.startsWith('/') && !value.includes('?')) return value;

  try {
    const parsed = new URL(value, 'https://analytics.invalid');
    const decodedPath = decodeURIComponent(parsed.pathname);
    const pathname = parsed.protocol === 'tdf:' && parsed.hostname === 'tickets'
      ? '/[REDACTED]'
      : privatePath(decodedPath);
    const privateResource = pathname !== decodedPath
      || /\/(?:pagos\/retorno|pago-datafast|live-sessions\/registro)\/?$/i.test(decodedPath)
      || /^\/inscripcion\//i.test(decodedPath);
    let changed = privateResource || Boolean(parsed.hash) || Boolean(parsed.username || parsed.password);
    if (pathname !== decodedPath) parsed.pathname = pathname;
    parsed.hash = '';
    parsed.username = '';
    parsed.password = '';
    for (const key of Array.from(parsed.searchParams.keys())) {
      // Return providers may add opaque keys (e.g. `id`) to private pages.
      if (privateResource || !PUBLIC_ATTRIBUTION_QUERY.test(key)) {
        parsed.searchParams.set(key, REDACTED_QUERY_VALUE);
        changed = true;
        continue;
      }
      const currentValue = parsed.searchParams.get(key);
      if (currentValue == null) continue;
      // Bound work without allowing deeply nested redirect URLs to evade masking.
      const sanitizedValue = depth >= 2 && /[?#]/.test(currentValue)
        ? REDACTED_QUERY_VALUE
        : redactSensitiveQueryValues(currentValue, depth + 1);
      if (sanitizedValue !== currentValue) {
        parsed.searchParams.set(key, sanitizedValue);
        changed = true;
      }
    }
    if (!changed) return value;
    return isAbsolute
      ? parsed.toString()
      : `${parsed.pathname}${parsed.search}`;
  } catch {
    return REDACTED_QUERY_VALUE;
  }
}

const sanitizeAnalyticsValue = (value: unknown): unknown => {
  if (typeof value === 'string') return redactSensitiveQueryValues(value);
  if (Array.isArray(value)) return value.map(sanitizeAnalyticsValue);
  if (typeof value !== 'object' || value === null) return value;
  return Object.fromEntries(
    Object.entries(value)
      .filter(([key]) => !isSensitivePropertyName(key))
      .map(([key, nestedValue]) => [key, sanitizeAnalyticsValue(nestedValue)]),
  );
};

export function sanitizeAnalyticsProperties<T extends Record<string, unknown>>(
  properties: T,
): T {
  return Object.fromEntries(
    Object.entries(properties)
      .filter(([key]) => !isSensitivePropertyName(key))
      .map(([key, value]) => [key, sanitizeAnalyticsValue(value)]),
  ) as T;
}

const sanitizedOptionalProperties = (
  properties?: Record<string, unknown>,
): Record<string, unknown> | undefined => {
  if (!properties) return undefined;
  const sanitized = sanitizeAnalyticsProperties(properties);
  return Object.keys(sanitized).length > 0 ? sanitized : undefined;
};

function logAnalyticsFailure(operation: string): void {
  // SDK exceptions can echo the rejected payload or URL.
  logger.warn(`[analytics] ${operation} failed`);
}

function buildNoopClient(reason: string): AnalyticsClient {
  logger.log(`[analytics] PostHog disabled: ${reason}. Events will not be sent.`);
  return {
    ready: false,
    capture: () => undefined,
    identify: () => undefined,
    reset: () => undefined,
    page: () => undefined,
  };
}

export function getAnalyticsClient(): AnalyticsClient {
  if (cachedClient) return cachedClient;

  const { key, host } = readConfig();
  if (!key) {
    cachedClient = buildNoopClient('VITE_POSTHOG_KEY is unset');
    return cachedClient;
  }

  if (typeof window === 'undefined') {
    cachedClient = buildNoopClient('no window (SSR)');
    return cachedClient;
  }

  posthog.init(key, {
    api_host: host,
    autocapture: false,
    capture_pageview: true,
    capture_pageleave: true,
    disable_session_recording: true,
    persistence: 'localStorage+cookie',
    mask_personal_data_properties: true,
    before_send: (event) => {
      if (event === null) return null;
      const projectToken = event.properties?.['token'];
      event.properties = sanitizeAnalyticsProperties(event.properties ?? {});
      // The SDK attaches initial URLs/referrers outside event.properties, and
      // identify also uses these top-level person-property envelopes.
      if (event.$set) event.$set = sanitizeAnalyticsProperties(event.$set);
      if (event.$set_once) event.$set_once = sanitizeAnalyticsProperties(event.$set_once);
      // PostHog injects its public project token at the root; application tokens stay stripped.
      if (projectToken === key) event.properties['token'] = projectToken;
      return event;
    },
  });

  cachedClient = {
    ready: true,
    capture: (event, properties) => {
      try {
        posthog.capture(event, sanitizedOptionalProperties(properties));
      } catch {
        logAnalyticsFailure('capture');
      }
    },
    identify: (distinctId, properties) => {
      try {
        posthog.identify(distinctId, sanitizedOptionalProperties(properties));
      } catch {
        logAnalyticsFailure('identify');
      }
    },
    reset: () => {
      try {
        posthog.reset();
      } catch {
        logAnalyticsFailure('reset');
      }
    },
    page: (name, properties) => {
      try {
        posthog.capture('$pageview', sanitizeAnalyticsProperties({ ...properties, name }));
      } catch {
        logAnalyticsFailure('page');
      }
    },
  };

  return cachedClient;
}

/** Test-only. */
export function __resetAnalyticsForTests(): void {
  cachedClient = null;
}
