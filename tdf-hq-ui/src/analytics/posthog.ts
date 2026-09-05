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

const SENSITIVE_QUERY_PARAMETER = /(^|[_-])(token|code|state|password|secret|key)($|[_-])/i;
const REDACTED_QUERY_VALUE = '[REDACTED]';

export function redactSensitiveQueryValues(value: string): string {
  if (!value.includes('?')) return value;

  try {
    const isAbsolute = /^[a-z][a-z\d+.-]*:/i.test(value);
    const parsed = new URL(value, 'https://analytics.invalid');
    let changed = false;
    for (const key of Array.from(parsed.searchParams.keys())) {
      if (!SENSITIVE_QUERY_PARAMETER.test(key)) continue;
      parsed.searchParams.set(key, REDACTED_QUERY_VALUE);
      changed = true;
    }
    if (!changed) return value;
    return isAbsolute
      ? parsed.toString()
      : `${parsed.pathname}${parsed.search}${parsed.hash}`;
  } catch {
    return value;
  }
}

export function sanitizeAnalyticsProperties<T extends Record<string, unknown>>(
  properties: T,
): T {
  return Object.fromEntries(
    Object.entries(properties).map(([key, value]) => [
      key,
      typeof value === 'string' ? redactSensitiveQueryValues(value) : value,
    ]),
  ) as T;
}

function logAnalyticsFailure(operation: string, error: unknown): void {
  logger.warn(`[analytics] ${operation} failed`, { error });
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
      event.properties = sanitizeAnalyticsProperties(event.properties);
      return event;
    },
  });

  cachedClient = {
    ready: true,
    capture: (event, properties) => {
      try {
        posthog.capture(event, properties);
      } catch (err) {
        logAnalyticsFailure('capture', err);
      }
    },
    identify: (distinctId, properties) => {
      try {
        posthog.identify(distinctId, properties);
      } catch (err) {
        logAnalyticsFailure('identify', err);
      }
    },
    reset: () => {
      try {
        posthog.reset();
      } catch (err) {
        logAnalyticsFailure('reset', err);
      }
    },
    page: (name, properties) => {
      try {
        posthog.capture('$pageview', { ...properties, name });
      } catch (err) {
        logAnalyticsFailure('page', err);
      }
    },
  };

  return cachedClient;
}

/** Test-only. */
export function __resetAnalyticsForTests(): void {
  cachedClient = null;
}
