import { jest } from '@jest/globals';
import type { AnalyticsClient } from './posthog';
import {
  classifyPartySelectorError,
  observePartySelectorSearch,
  recordPartySelectorAvatarFailure,
  recordPartySelectorSelection,
} from './partySelectorTelemetry';

const buildAnalytics = () => {
  const capture = jest.fn<AnalyticsClient['capture']>();
  const analytics: AnalyticsClient = {
    ready: true,
    capture,
    identify: jest.fn(),
    reset: jest.fn(),
    page: jest.fn(),
  };
  return { analytics, capture };
};

describe('Party selector privacy-safe telemetry', () => {
  it('records latency and result cardinality without search text or identity data', async () => {
    const { analytics, capture } = buildAnalytics();
    const times = [1_000, 1_087];
    const page = await observePartySelectorSearch({
      analytics,
      platform: 'web',
      context: 'crm_assignment',
      pageKind: 'initial',
      now: () => times.shift() ?? 1_087,
      request: async () => ({ items: [{ partyId: 17 }], nextCursor: 15 }),
    });

    expect(page.items).toHaveLength(1);
    expect(capture).toHaveBeenCalledWith('party_selector_search_completed', {
      platform: 'web',
      context: 'crm_assignment',
      page_kind: 'initial',
      latency_ms: 87,
      result_count: 1,
      has_more: true,
    });
    const serialized = JSON.stringify(capture.mock.calls);
    expect(serialized).not.toContain('partyId');
    expect(serialized).not.toContain('query');
  });

  it('emits a separate aggregate event for an empty first page', async () => {
    const { analytics, capture } = buildAnalytics();
    await observePartySelectorSearch({
      analytics,
      platform: 'web',
      context: 'event_invitation',
      pageKind: 'initial',
      request: async () => ({ items: [], nextCursor: null }),
    });

    expect(capture).toHaveBeenCalledWith('party_selector_search_no_results', {
      platform: 'web', context: 'event_invitation',
    });
  });

  it('separates cancellation, timeout, authorization, and server failures', async () => {
    expect(classifyPartySelectorError({ name: 'AbortError' })).toBe('cancelled');
    expect(classifyPartySelectorError({ status: 408 })).toBe('timeout');
    expect(classifyPartySelectorError({ status: 403 })).toBe('authorization');
    expect(classifyPartySelectorError({ status: 503 })).toBe('server');

    const { analytics, capture } = buildAnalytics();
    const error = Object.assign(new Error('private server detail'), { status: 503 });
    await expect(observePartySelectorSearch({
      analytics,
      platform: 'web',
      context: 'billing_contact',
      pageKind: 'load_more',
      request: async () => { throw error; },
    })).rejects.toBe(error);

    expect(capture).toHaveBeenCalledWith('party_selector_search_failed', expect.objectContaining({
      context: 'billing_contact', error_kind: 'server',
    }));
    expect(JSON.stringify(capture.mock.calls)).not.toContain('private server detail');
  });

  it('records avatar and selection outcomes without Party identifiers', () => {
    const { analytics, capture } = buildAnalytics();
    recordPartySelectorAvatarFailure(analytics, {
      platform: 'web', context: 'artist_link', partyType: 'person',
    });
    recordPartySelectorSelection(analytics, {
      platform: 'web', context: 'artist_link', mode: 'single', action: 'selected',
    });

    expect(capture).toHaveBeenNthCalledWith(1, 'party_selector_avatar_failed', {
      platform: 'web', context: 'artist_link', party_type: 'person',
    });
    expect(capture).toHaveBeenNthCalledWith(2, 'party_selector_selection_changed', {
      platform: 'web', context: 'artist_link', mode: 'single', action: 'selected',
    });
  });
});
