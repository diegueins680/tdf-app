/**
 * useAnalytics.ts
 *
 * Hook accessor for the shared PostHog client (web).
 * Initializes on first call via getAnalyticsClient().
 */
import { useMemo } from 'react';

import { getAnalyticsClient, type AnalyticsClient } from './posthog';

export function useAnalytics(): AnalyticsClient {
  return useMemo(() => getAnalyticsClient(), []);
}
