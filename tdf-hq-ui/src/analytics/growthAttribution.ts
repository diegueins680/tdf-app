import type { AnalyticsClient } from './posthog';

const STORAGE_KEY = 'tdf:growth-attribution:v1';
const MAX_VALUE_LENGTH = 160;

const ATTRIBUTION_PARAM_MAP = {
  utm_source: 'source',
  utm_medium: 'medium',
  utm_campaign: 'campaign',
  utm_content: 'content',
  utm_term: 'term',
  ref: 'referralCode',
  referral: 'referralCode',
  referral_code: 'referralCode',
} as const;

export interface GrowthAttribution {
  source?: string;
  medium?: string;
  campaign?: string;
  content?: string;
  term?: string;
  referralCode?: string;
  landingPath: string;
  capturedAt: string;
}

interface CaptureGrowthAttributionOptions {
  search: string;
  pathname: string;
  storage?: Pick<Storage, 'getItem' | 'setItem'> | null;
  now?: Date;
}

const cleanValue = (value: string | null): string | undefined => {
  const cleaned = value?.trim().replace(/[\u0000-\u001f\u007f]/g, '').slice(0, MAX_VALUE_LENGTH);
  return cleaned ? cleaned : undefined;
};

const safePath = (pathname: string): string => {
  const cleaned = cleanValue(pathname);
  return cleaned?.startsWith('/') ? cleaned : '/';
};

const browserStorage = (): Pick<Storage, 'getItem' | 'setItem'> | null => {
  if (typeof window === 'undefined') return null;
  try {
    return window.localStorage;
  } catch {
    return null;
  }
};

const parseStoredAttribution = (
  storage: Pick<Storage, 'getItem'> | null,
): GrowthAttribution | null => {
  if (!storage) return null;
  try {
    const raw = storage.getItem(STORAGE_KEY);
    if (!raw) return null;
    const parsed = JSON.parse(raw) as Partial<GrowthAttribution>;
    if (typeof parsed.landingPath !== 'string' || typeof parsed.capturedAt !== 'string') return null;
    return {
      source: cleanValue(parsed.source ?? null),
      medium: cleanValue(parsed.medium ?? null),
      campaign: cleanValue(parsed.campaign ?? null),
      content: cleanValue(parsed.content ?? null),
      term: cleanValue(parsed.term ?? null),
      referralCode: cleanValue(parsed.referralCode ?? null),
      landingPath: safePath(parsed.landingPath),
      capturedAt: parsed.capturedAt,
    };
  } catch {
    return null;
  }
};

export function captureGrowthAttribution({
  search,
  pathname,
  storage = browserStorage(),
  now = new Date(),
}: CaptureGrowthAttributionOptions): GrowthAttribution {
  const previous = parseStoredAttribution(storage);
  const params = new URLSearchParams(search);
  const incoming: Partial<GrowthAttribution> = {};

  Object.entries(ATTRIBUTION_PARAM_MAP).forEach(([queryParam, property]) => {
    const value = cleanValue(params.get(queryParam));
    if (value) (incoming as Record<string, string>)[property] = value;
  });

  const hasIncomingCampaign = Object.keys(incoming).length > 0;
  const attribution: GrowthAttribution = {
    ...(hasIncomingCampaign ? {} : previous ?? {}),
    ...incoming,
    landingPath: hasIncomingCampaign || !previous ? safePath(pathname) : previous.landingPath,
    capturedAt: hasIncomingCampaign || !previous ? now.toISOString() : previous.capturedAt,
  };

  try {
    storage?.setItem(STORAGE_KEY, JSON.stringify(attribution));
  } catch {
    // Analytics attribution must never block navigation or signup.
  }
  return attribution;
}

export function getGrowthAttribution(
  storage: Pick<Storage, 'getItem'> | null = browserStorage(),
): GrowthAttribution | null {
  return parseStoredAttribution(storage);
}

export function growthAttributionProperties(
  attribution: GrowthAttribution | null = getGrowthAttribution(),
): Record<string, string> {
  if (!attribution) return {};
  return {
    ...(attribution.source ? { attribution_source: attribution.source } : {}),
    ...(attribution.medium ? { attribution_medium: attribution.medium } : {}),
    ...(attribution.campaign ? { attribution_campaign: attribution.campaign } : {}),
    ...(attribution.content ? { attribution_content: attribution.content } : {}),
    ...(attribution.term ? { attribution_term: attribution.term } : {}),
    ...(attribution.referralCode ? { referral_code: attribution.referralCode } : {}),
    attribution_landing_path: attribution.landingPath,
    attribution_captured_at: attribution.capturedAt,
  };
}

export function captureGrowthEvent(
  analytics: AnalyticsClient,
  event: string,
  properties: Record<string, unknown> = {},
): void {
  analytics.capture(event, {
    ...growthAttributionProperties(),
    ...properties,
  });
}

export const __growthAttributionTestUtils = {
  storageKey: STORAGE_KEY,
};
