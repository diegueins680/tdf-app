import { useTranslation } from 'react-i18next';
import { useLocation } from 'react-router-dom';
import { useAnalytics } from '../analytics/useAnalytics';
import { getGrowthAttribution, type GrowthAttribution } from '../analytics/growthAttribution';
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
const LABEL = /^[a-zA-Z0-9_-]{1,80}$/;
export const CAMPAIGN_TAGS = ['source', 'medium', 'campaign', 'content', 'term'] as const;
export function campaignTags(search: string): Record<string, string> {
  const params = new URLSearchParams(search);
  const tags: Record<string, string> = {};
  for (const key of CAMPAIGN_TAGS) {
    const value = params.get(`utm_${key}`);
    if (value && LABEL.test(value)) tags[key] = value;
  }
  return tags;
}
// The campaign this visitor already carries first-party (see growthAttribution), re-validated as labels.
export function storedCampaignTags(stored: GrowthAttribution | null = getGrowthAttribution()): Record<string, string> {
  const tags: Record<string, string> = {};
  for (const key of CAMPAIGN_TAGS) {
    const value = stored?.[key];
    if (value && LABEL.test(value)) tags[key] = value;
  }
  return tags;
}
// Tags of the current query win as a whole; a visit without any tag inherits the stored campaign so a link copied
// after navigating inside TDF still carries the attribution that /app persisted.
function effectiveCampaignTags(search: string, stored: GrowthAttribution | null): Record<string, string> {
  const current = campaignTags(search);
  return Object.keys(current).length > 0 ? current : storedCampaignTags(stored);
}
function linkParams(search: string, surface: string, stored: GrowthAttribution | null): URLSearchParams {
  const params = new URLSearchParams({ surface });
  Object.entries(effectiveCampaignTags(search, stored)).forEach(([key, value]) => params.set(`utm_${key}`, value));
  return params;
}
export function appLink(search: string, surface: string, stored: GrowthAttribution | null = getGrowthAttribution()): string {
  return `/app?${linkParams(search, surface, stored)}`;
}
// The absolute smart link TDF shares (QR, copy button, messages). It keeps the allowlisted campaign tags of this
// visitor (current query, else the stored campaign) and nothing else, so a shared link never carries identifiers.
export function smartLink(search: string, surface: string, stored: GrowthAttribution | null = getGrowthAttribution()): string {
  return `${MOBILE_CANONICAL_URL}?${linkParams(search, surface, stored)}`;
}
export function useMobileTelemetry(surface: string) {
  const analytics = useAnalytics();
  const { session } = useSession();
  const { i18n } = useTranslation();
  const { search } = useLocation();
  // Storage is read when each event is emitted, so an exposure reported right after /app persisted a new campaign
  // never mixes the new source with a previous visit's medium or campaign.
  return (event: string, properties: Record<string, unknown> = {}) => analytics.capture(event, {
    ...effectiveCampaignTags(search, getGrowthAttribution()),
    surface, entry_surface: entrySurface(search), locale: i18n?.resolvedLanguage ?? 'es', authenticated: Boolean(session),
    platform: detectPlatform(navigator.userAgent, navigator.maxTouchPoints), ...properties,
  });
}
