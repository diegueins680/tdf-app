import { useTranslation } from 'react-i18next';
import { useLocation } from 'react-router-dom';
import { useAnalytics } from '../analytics/useAnalytics';
import { getGrowthAttribution } from '../analytics/growthAttribution';
import { useSession } from '../session/SessionContext';
import { MOBILE_CANONICAL_URL, detectPlatform } from './distribution';

// Where a visitor entered the funnel from. Only these labels are reported as entry_surface; anything else in
// ?surface= is dropped so the property can never carry free text.
export const ENTRY_SURFACES = ['tdf_landing', 'homepage', 'footer', 'signup_complete', 'authenticated_home', 'profile', 'settings', 'nav', 'mobile_banner', 'campaign', 'community', 'ticket_confirmation', 'qr', 'share'] as const;
export type EntrySurface = typeof ENTRY_SURFACES[number];
export function entrySurface(search: string): EntrySurface | undefined {
  const value = new URLSearchParams(search).get('surface') ?? '';
  return (ENTRY_SURFACES as readonly string[]).includes(value) ? value as EntrySurface : undefined;
}
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
function linkParams(search: string, surface: string): URLSearchParams {
  const params = new URLSearchParams({ surface });
  Object.entries(campaignTags(search)).forEach(([key, value]) => params.set(`utm_${key}`, value));
  return params;
}
export function appLink(search: string, surface: string): string {
  return `/app?${linkParams(search, surface)}`;
}
// The absolute smart link TDF shares (QR, copy button, messages). It keeps the allowlisted campaign tags of the
// current visit and nothing else, so a shared link never carries another visitor's identifiers.
export function smartLink(search: string, surface: string): string {
  return `${MOBILE_CANONICAL_URL}?${linkParams(search, surface)}`;
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
    surface, entry_surface: entrySurface(search), locale: i18n?.resolvedLanguage ?? 'es', authenticated: Boolean(session),
    platform: detectPlatform(navigator.userAgent, navigator.maxTouchPoints), ...properties,
  });
}
