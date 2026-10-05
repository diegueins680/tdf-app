import { useTranslation } from 'react-i18next';
import { useLocation } from 'react-router-dom';
import { useAnalytics } from '../analytics/useAnalytics';
import { getGrowthAttribution } from '../analytics/growthAttribution';
import { useSession } from '../session/SessionContext';
import { detectPlatform } from './distribution';

// Campaign tags are labels, never arbitrary URLs, emails, identifiers or search terms.
export function campaignTags(search: string): Record<string, string> {
  const params = new URLSearchParams(search);
  const tags: Record<string, string> = {};
  for (const key of ['source', 'medium', 'campaign'] as const) {
    const value = params.get(`utm_${key}`);
    if (value && /^[a-zA-Z0-9_-]{1,80}$/.test(value)) tags[key] = value;
  }
  return tags;
}
export function appLink(search: string, surface: string): string {
  const params = new URLSearchParams({ surface });
  Object.entries(campaignTags(search)).forEach(([key, value]) => params.set(`utm_${key}`, value));
  return `/app?${params}`;
}
export function useMobileTelemetry(surface: string) {
  const analytics = useAnalytics();
  const { session } = useSession();
  const { i18n } = useTranslation();
  const { search } = useLocation();
  const stored = getGrowthAttribution();
  const storedParams = new URLSearchParams();
  for (const key of ['source', 'medium', 'campaign'] as const) if (stored?.[key]) storedParams.set(`utm_${key}`, stored[key]);
  return (event: string, properties: Record<string, unknown> = {}) => analytics.capture(event, {
    ...campaignTags(storedParams.toString()), ...campaignTags(search),
    surface, entry_surface: ['tdf_landing', 'homepage', 'footer', 'signup_complete', 'authenticated_home', 'profile', 'settings', 'mobile_banner', 'campaign', 'community'].includes(new URLSearchParams(search).get('surface') ?? '') ? new URLSearchParams(search).get('surface') : undefined, locale: i18n?.resolvedLanguage ?? 'es', authenticated: Boolean(session),
    platform: detectPlatform(navigator.userAgent, navigator.maxTouchPoints), ...properties,
  });
}
