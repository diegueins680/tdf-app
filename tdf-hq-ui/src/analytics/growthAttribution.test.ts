import { describe, it, expect, beforeEach, jest } from '@jest/globals';
import {
  __growthAttributionTestUtils,
  captureGrowthAttribution,
  captureGrowthEvent,
  getGrowthAttribution,
  growthAttributionProperties,
} from './growthAttribution';
import type { AnalyticsClient } from './posthog';

const makeStorage = () => {
  const values = new Map<string, string>();
  return {
    getItem: (key: string) => values.get(key) ?? null,
    setItem: (key: string, value: string) => values.set(key, value),
    values,
  };
};

describe('growth attribution', () => {
  beforeEach(() => {
    window.localStorage.clear();
  });

  it('captures campaign and referral data without storing unrelated query parameters', () => {
    const storage = makeStorage();
    const attribution = captureGrowthAttribution({
      search: '?utm_source=instagram&utm_medium=organic&utm_campaign=fundadores&ref=TDF-DIEGO&email=private@example.com',
      pathname: '/tdf',
      storage,
      now: new Date('2026-08-07T12:00:00.000Z'),
    });

    expect(attribution).toEqual({
      source: 'instagram',
      medium: 'organic',
      campaign: 'fundadores',
      referralCode: 'TDF-DIEGO',
      landingPath: '/tdf',
      capturedAt: '2026-08-07T12:00:00.000Z',
    });
    expect(storage.values.get(__growthAttributionTestUtils.storageKey)).not.toContain('private@example.com');
  });

  it('keeps attribution while the visitor navigates without new campaign parameters', () => {
    const storage = makeStorage();
    captureGrowthAttribution({
      search: '?utm_source=tiktok&utm_campaign=busco_musico',
      pathname: '/tdf',
      storage,
      now: new Date('2026-08-07T12:00:00.000Z'),
    });

    const next = captureGrowthAttribution({
      search: '?signup=1&roles=Fan',
      pathname: '/login',
      storage,
      now: new Date('2026-08-07T13:00:00.000Z'),
    });

    expect(next.source).toBe('tiktok');
    expect(next.campaign).toBe('busco_musico');
    expect(next.landingPath).toBe('/tdf');
    expect(next.capturedAt).toBe('2026-08-07T12:00:00.000Z');
  });

  it('refreshes attribution when a later explicit campaign is received', () => {
    const storage = makeStorage();
    captureGrowthAttribution({
      search: '?utm_source=instagram&utm_campaign=first',
      pathname: '/tdf',
      storage,
      now: new Date('2026-08-07T12:00:00.000Z'),
    });
    captureGrowthAttribution({
      search: '?utm_source=whatsapp&utm_campaign=ambassadors',
      pathname: '/login',
      storage,
      now: new Date('2026-08-08T12:00:00.000Z'),
    });

    expect(getGrowthAttribution(storage)).toMatchObject({
      source: 'whatsapp',
      campaign: 'ambassadors',
      landingPath: '/login',
      capturedAt: '2026-08-08T12:00:00.000Z',
    });
  });

  it('flattens attribution into stable analytics properties', () => {
    expect(growthAttributionProperties({
      source: 'instagram',
      referralCode: 'TDF-01',
      landingPath: '/tdf',
      capturedAt: '2026-08-07T12:00:00.000Z',
    })).toEqual({
      attribution_source: 'instagram',
      referral_code: 'TDF-01',
      attribution_landing_path: '/tdf',
      attribution_captured_at: '2026-08-07T12:00:00.000Z',
    });
  });

  it('adds attribution to growth events and lets event-specific properties win', () => {
    const storage = makeStorage();
    captureGrowthAttribution({
      search: '?utm_source=instagram',
      pathname: '/tdf',
      storage,
    });
    const capture = jest.fn();
    const analytics = {
      ready: true,
      capture,
      identify: jest.fn(),
      reset: jest.fn(),
      page: jest.fn(),
    } satisfies AnalyticsClient;

    // Use an explicit browser-like storage entry because captureGrowthEvent reads browser storage.
    window.localStorage.setItem(
      __growthAttributionTestUtils.storageKey,
      storage.values.get(__growthAttributionTestUtils.storageKey) ?? '',
    );
    captureGrowthEvent(analytics, 'signup_started', { route: '/login' });

    expect(capture).toHaveBeenCalledWith('signup_started', expect.objectContaining({
      attribution_source: 'instagram',
      route: '/login',
    }));
  });
});
