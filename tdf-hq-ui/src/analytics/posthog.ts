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

function buildNoopClient(reason: string): AnalyticsClient {
  // eslint-disable-next-line no-console
  console.info(`[analytics] PostHog disabled: ${reason}. Events will not be sent.`);
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
    capture_pageview: true,
    capture_pageleave: true,
    disable_session_recording: true,
    persistence: 'localStorage+cookie',
  });

  cachedClient = {
    ready: true,
    capture: (event, properties) => {
      try {
        posthog.capture(event, properties);
      } catch (err) {
        // eslint-disable-next-line no-console
        console.warn('[analytics] capture failed', err);
      }
    },
    identify: (distinctId, properties) => {
      try {
        posthog.identify(distinctId, properties);
      } catch (err) {
        // eslint-disable-next-line no-console
        console.warn('[analytics] identify failed', err);
      }
    },
    reset: () => {
      try {
        posthog.reset();
      } catch (err) {
        // eslint-disable-next-line no-console
        console.warn('[analytics] reset failed', err);
      }
    },
    page: (name, properties) => {
      try {
        posthog.capture('$pageview', { ...properties, name });
      } catch (err) {
        // eslint-disable-next-line no-console
        console.warn('[analytics] page failed', err);
      }
    },
  };

  return cachedClient;
}

/** Test-only. */
export function __resetAnalyticsForTests(): void {
  cachedClient = null;
}
