import { useMemo } from 'react';
import { useAnalytics } from './useAnalytics';
import { getGrowthAttribution } from './growthAttribution';
import { createTicketFunnelTracker } from './ticketFunnel';

export function useTicketFunnel() {
  const analytics = useAnalytics();
  return useMemo(() => {
    let storage: Storage | null = null;
    try { storage = window.sessionStorage; } catch { /* SSR/private browsing. */ }
    return createTicketFunnelTracker(analytics, storage, getGrowthAttribution);
  }, [analytics]);
}
