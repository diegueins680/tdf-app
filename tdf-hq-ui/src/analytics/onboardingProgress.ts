import { captureGrowthEvent } from './growthAttribution';
import type { AnalyticsClient } from './posthog';

const FIRST_VALUE_PREFIX = 'tdf:first-value:';
const SIGNUP_COMPLETED_PREFIX = 'tdf:signup-completed-at:';
const NEW_USER_WINDOW_MS = 24 * 60 * 60 * 1000;

export function markWebSignupCompleted(
  partyId: number | string | null | undefined,
  storage: Pick<Storage, 'setItem'> | null = typeof window === 'undefined' ? null : window.localStorage,
  now = Date.now(),
): boolean {
  if (!partyId || !storage) return false;
  try {
    storage.setItem(`${SIGNUP_COMPLETED_PREFIX}${partyId}`, String(now));
    return true;
  } catch {
    return false;
  }
}

export function captureFirstValueOnce(
  analytics: AnalyticsClient,
  partyId: number | string | null | undefined,
  value: string,
  storage: Pick<Storage, 'getItem' | 'setItem'> | null = typeof window === 'undefined' ? null : window.localStorage,
  now = Date.now(),
): boolean {
  if (!partyId || !value || !storage) return false;
  const key = `${FIRST_VALUE_PREFIX}${partyId}`;
  try {
    const signupAt = Number(storage.getItem(`${SIGNUP_COMPLETED_PREFIX}${partyId}`));
    if (!Number.isFinite(signupAt) || signupAt <= 0 || signupAt > now || now - signupAt > NEW_USER_WINDOW_MS) return false;
    if (storage.getItem(key)) return false;
    storage.setItem(key, JSON.stringify({ value, completedAt: now }));
  } catch {
    return false;
  }
  captureGrowthEvent(analytics, 'first_value_completed', { platform: 'web', value });
  captureGrowthEvent(analytics, 'onboarding_completed', { platform: 'web', reason: 'first_value', value });
  return true;
}

export const __onboardingProgressTestUtils = { FIRST_VALUE_PREFIX, SIGNUP_COMPLETED_PREFIX, NEW_USER_WINDOW_MS };
