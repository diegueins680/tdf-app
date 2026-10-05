import type { AnalyticsClient } from './posthog';
import {
  completeOnboardingProgress,
  type OnboardingCompletionResultDTO,
  type OnboardingFirstValue,
} from '../api/session';
import { getActiveSession } from '../session/SessionContext';
import {
  captureReconciledFirstValue,
  isOnboardingFirstValue,
} from './onboardingCompletionAnalytics';

export { captureReconciledFirstValue } from './onboardingCompletionAnalytics';

export const PENDING_FIRST_VALUE_PREFIX = 'tdf-onboarding-first-value:party:';

const normalizePartyId = (partyId: number | string | null | undefined): string | null => {
  const normalized = String(partyId ?? '').trim();
  if (!/^\d+$/.test(normalized)) return null;
  const parsed = Number(normalized);
  return Number.isSafeInteger(parsed) && parsed > 0 ? String(parsed) : null;
};

const pendingFirstValueKey = (partyId: string): string =>
  `${PENDING_FIRST_VALUE_PREFIX}${encodeURIComponent(partyId)}`;

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
