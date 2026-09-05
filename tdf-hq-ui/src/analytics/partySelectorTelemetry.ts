import type { AnalyticsClient } from './posthog';

export type PartySelectorPlatform = 'web' | 'mobile';
export type PartySelectorPageKind = 'initial' | 'load_more';
export type PartySelectorSelectionAction = 'selected' | 'removed' | 'replaced' | 'duplicate_rejected';

interface SelectorPage {
  items: unknown[];
  nextCursor?: unknown;
}

interface TelemetryError {
  code?: unknown;
  name?: unknown;
  status?: unknown;
}

export function classifyPartySelectorError(error: unknown): string {
  const candidate = error && typeof error === 'object' ? error as TelemetryError : {};
  if (candidate.name === 'AbortError' || candidate.code === 'ERR_CANCELED') return 'cancelled';
  if (candidate.status === 408 || candidate.code === 'ECONNABORTED') return 'timeout';
  if (candidate.status === 401 || candidate.status === 403) return 'authorization';
  if (typeof candidate.status === 'number' && candidate.status >= 500) return 'server';
  if (typeof candidate.status === 'number' && candidate.status >= 400) return 'request';
  return 'network';
}

/**
 * Emits an aggregate-friendly selector metric without the query, Party ID,
 * display name, username, or any other user-provided value.
 */
export async function observePartySelectorSearch<T extends SelectorPage>({
  analytics,
  platform,
  context,
  pageKind,
  request,
  now = Date.now,
}: {
  analytics: AnalyticsClient;
  platform: PartySelectorPlatform;
  context: string;
  pageKind: PartySelectorPageKind;
  request: () => Promise<T>;
  now?: () => number;
}): Promise<T> {
  const startedAt = now();
  try {
    const page = await request();
    analytics.capture('party_selector_search_completed', {
      platform,
      context,
      page_kind: pageKind,
      latency_ms: Math.max(0, now() - startedAt),
      result_count: page.items.length,
      has_more: page.nextCursor != null,
    });
    if (pageKind === 'initial' && page.items.length === 0) {
      analytics.capture('party_selector_search_no_results', { platform, context });
    }
    return page;
  } catch (error) {
    const errorKind = classifyPartySelectorError(error);
    analytics.capture(
      errorKind === 'cancelled' ? 'party_selector_search_cancelled' : 'party_selector_search_failed',
      {
        platform,
        context,
        page_kind: pageKind,
        latency_ms: Math.max(0, now() - startedAt),
        ...(errorKind === 'cancelled' ? {} : { error_kind: errorKind }),
      },
    );
    throw error;
  }
}

export function recordPartySelectorAvatarFailure(
  analytics: AnalyticsClient,
  properties: { platform: PartySelectorPlatform; context: string; partyType: string },
): void {
  analytics.capture('party_selector_avatar_failed', {
    platform: properties.platform,
    context: properties.context,
    party_type: properties.partyType,
  });
}

export function recordPartySelectorSelection(
  analytics: AnalyticsClient,
  properties: {
    platform: PartySelectorPlatform;
    context: string;
    mode: 'single' | 'multiple';
    action: PartySelectorSelectionAction;
  },
): void {
  analytics.capture('party_selector_selection_changed', properties);
}

export function recordPartySelectorSelectionFailure(
  analytics: AnalyticsClient,
  properties: { platform: PartySelectorPlatform; context: string; mode: 'single' | 'multiple' },
): void {
  analytics.capture('party_selector_selection_failed', properties);
}
