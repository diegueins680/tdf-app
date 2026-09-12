import { captureGrowthEvent } from './growthAttribution';
import type { AnalyticsClient } from './posthog';
import {
  completeOnboardingProgress,
  type OnboardingCompletionResultDTO,
  type OnboardingFirstValue,
} from '../api/session';
import { getActiveSession } from '../session/SessionContext';

const FIRST_VALUES = new Set<OnboardingFirstValue>([
  'artist_followed',
  'access_requested',
  'event_saved',
  'moment_reaction',
]);

export const PENDING_FIRST_VALUE_PREFIX = 'tdf-onboarding-first-value:party:';

const normalizePartyId = (partyId: number | string | null | undefined): string | null => {
  const normalized = String(partyId ?? '').trim();
  if (!/^\d+$/.test(normalized)) return null;
  const parsed = Number(normalized);
  return Number.isSafeInteger(parsed) && parsed > 0 ? String(parsed) : null;
};

const pendingFirstValueKey = (partyId: string): string =>
  `${PENDING_FIRST_VALUE_PREFIX}${encodeURIComponent(partyId)}`;

const isOnboardingFirstValue = (value: string): value is OnboardingFirstValue =>
  FIRST_VALUES.has(value as OnboardingFirstValue);

const storePendingFirstValue = (partyId: string, value: OnboardingFirstValue): void => {
  try {
    window.localStorage.setItem(pendingFirstValueKey(partyId), value);
  } catch {
    // Still attempt the authoritative request when browser storage is unavailable.
  }
};

const readPendingFirstValue = (partyId: string): OnboardingFirstValue | null => {
  try {
    const key = pendingFirstValueKey(partyId);
    const stored = window.localStorage.getItem(key);
    if (!stored) return null;
    if (isOnboardingFirstValue(stored)) return stored;
    window.localStorage.removeItem(key);
  } catch {
    // Blocked storage must not interrupt authenticated navigation.
  }
  return null;
};

const clearPendingFirstValueIfCurrent = (
  partyId: string,
  value: OnboardingFirstValue,
): void => {
  try {
    const key = pendingFirstValueKey(partyId);
    if (window.localStorage.getItem(key) === value) window.localStorage.removeItem(key);
  } catch {
    // A later replay is safe because server completion is idempotent.
  }
};

const authoritativeFirstValue = (
  result: OnboardingCompletionResultDTO,
): OnboardingFirstValue | null => {
  const value = result.progress.firstValue;
  return typeof value === 'string' && FIRST_VALUES.has(value as OnboardingFirstValue)
    ? value as OnboardingFirstValue
    : null;
};

export function captureReconciledFirstValue(
  analytics: AnalyticsClient,
  partyId: number | string | null | undefined,
  result: OnboardingCompletionResultDTO,
): boolean {
  const value = authoritativeFirstValue(result);
  if (!partyId || !result.newlyCompleted || !value) return false;
  captureGrowthEvent(analytics, 'first_value_completed', { platform: 'web', value });
  captureGrowthEvent(analytics, 'onboarding_completed', { platform: 'web', reason: 'first_value', value });
  return true;
}

type CompleteOnboarding = (
  firstValue: OnboardingFirstValue,
) => Promise<OnboardingCompletionResultDTO>;

export async function captureFirstValueOnce(
  analytics: AnalyticsClient,
  partyId: number | string | null | undefined,
  value: OnboardingFirstValue,
  complete: CompleteOnboarding = completeOnboardingProgress,
  stillOwnsParty?: () => boolean,
): Promise<boolean> {
  const normalizedPartyId = normalizePartyId(partyId);
  if (!normalizedPartyId) return false;
  const ownsParty = stillOwnsParty
    ?? (() => normalizePartyId(getActiveSession()?.partyId) === normalizedPartyId);
  storePendingFirstValue(normalizedPartyId, value);
  if (!ownsParty()) return false;
  let result: OnboardingCompletionResultDTO;
  try {
    result = await complete(value);
  } catch {
    return false;
  }
  if (!ownsParty()) return false;
  clearPendingFirstValueIfCurrent(normalizedPartyId, value);
  return captureReconciledFirstValue(analytics, normalizedPartyId, result);
}

export async function retryPendingFirstValueCompletion(
  analytics: AnalyticsClient,
  partyId: number | string | null | undefined,
  complete: CompleteOnboarding = completeOnboardingProgress,
  stillOwnsParty?: () => boolean,
): Promise<boolean> {
  const normalizedPartyId = normalizePartyId(partyId);
  if (!normalizedPartyId) return false;
  const ownsParty = stillOwnsParty
    ?? (() => normalizePartyId(getActiveSession()?.partyId) === normalizedPartyId);
  if (!ownsParty()) return false;
  const value = readPendingFirstValue(normalizedPartyId);
  if (!value || !ownsParty()) return false;
  let result: OnboardingCompletionResultDTO;
  try {
    result = await complete(value);
  } catch {
    return false;
  }
  if (!ownsParty()) return false;
  clearPendingFirstValueIfCurrent(normalizedPartyId, value);
  return captureReconciledFirstValue(analytics, normalizedPartyId, result);
}
