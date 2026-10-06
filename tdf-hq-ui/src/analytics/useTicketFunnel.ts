import { useMemo } from 'react';
import { useLocation } from 'react-router-dom';
import { useAnalytics } from './useAnalytics';
import { captureGrowthAttribution } from './growthAttribution';
import { createTicketFunnelTracker } from './ticketFunnel';

export function useTicketFunnel() {
  const analytics = useAnalytics();
  const { pathname, search } = useLocation();
  return useMemo(() => {
    let storage: Storage | null = null;
    try { storage = window.sessionStorage; } catch { /* SSR/private browsing. */ }
    return createTicketFunnelTracker(analytics, storage, () => captureGrowthAttribution({
      search,
      // Never persist a private order path or its lookup capability as attribution.
      pathname: /^\/eventos\/[1-9][0-9]*(?=\/|$)/.exec(pathname)?.[0] ?? '/eventos',
    }));
  }, [analytics, pathname, search]);
}
