import {
  persistOnboardingIntent,
  type OnboardingIntent,
} from '../api/session';
import { getActiveSession } from './SessionContext';

type PersistOnboardingIntent = (
  onboardingIntent: OnboardingIntent,
  apiToken?: string,
) => Promise<unknown>;

export const PENDING_ONBOARDING_INTENT_PREFIX = 'tdf-onboarding-intent:party:';

const normalizePartyId = (partyId: number | string | null | undefined): string | null => {
  const normalized = String(partyId ?? '').trim();
  if (!/^\d+$/.test(normalized)) return null;
  const parsed = Number(normalized);
  return Number.isSafeInteger(parsed) && parsed > 0 ? String(parsed) : null;
};

const pendingIntentKey = (partyId: string): string =>
  `${PENDING_ONBOARDING_INTENT_PREFIX}${encodeURIComponent(partyId)}`;

const isOnboardingIntent = (value: string): value is OnboardingIntent =>
  value === 'events'
  || value === 'follow_artists'
  || value === 'artist_profile'
  || value === 'internships'
  || value === 'learning'
  || value === 'professional_tools';

const storePendingIntent = (partyId: string, intent: OnboardingIntent): void => {
  try {
    window.localStorage.setItem(pendingIntentKey(partyId), intent);
  } catch {
    // Intent improves continuity but must never block authentication or navigation.
  }
};

const readPendingIntent = (partyId: string): OnboardingIntent | null => {
  try {
    const key = pendingIntentKey(partyId);
    const stored = window.localStorage.getItem(key);
    if (!stored) return null;
    if (isOnboardingIntent(stored)) return stored;
    window.localStorage.removeItem(key);
  } catch {
    // A blocked storage API must not disrupt the authenticated shell.
  }
  return null;
};

const clearPendingIntentIfCurrent = (partyId: string, intent: OnboardingIntent): void => {
  try {
    const key = pendingIntentKey(partyId);
    if (window.localStorage.getItem(key) === intent) {
      window.localStorage.removeItem(key);
    }
  } catch {
    // A later retry is safe because the server upserts intent for the authenticated Party.
  }
};

const activeSessionOwnsParty = (partyId: string): boolean =>
  normalizePartyId(getActiveSession()?.partyId) === partyId;

export async function persistOnboardingIntentWithRetry(
  partyId: number | string | null | undefined,
  intent: OnboardingIntent,
  apiToken?: string,
  persist: PersistOnboardingIntent = persistOnboardingIntent,
  stillOwnsParty?: () => boolean,
): Promise<boolean> {
  const normalizedPartyId = normalizePartyId(partyId);
  if (!normalizedPartyId) return false;
  const ownsParty = stillOwnsParty ?? (() => activeSessionOwnsParty(normalizedPartyId));
  storePendingIntent(normalizedPartyId, intent);
  if (!ownsParty()) return false;
  try {
    await persist(intent, apiToken);
  } catch {
    return false;
  }
  if (!ownsParty()) return false;
  clearPendingIntentIfCurrent(normalizedPartyId, intent);
  return true;
}

export async function retryPendingOnboardingIntent(
  partyId: number | string | null | undefined,
  persist: PersistOnboardingIntent = persistOnboardingIntent,
  stillOwnsParty?: () => boolean,
): Promise<boolean> {
  const normalizedPartyId = normalizePartyId(partyId);
  if (!normalizedPartyId) return false;
  const ownsParty = stillOwnsParty ?? (() => activeSessionOwnsParty(normalizedPartyId));
  if (!ownsParty()) return false;
  const intent = readPendingIntent(normalizedPartyId);
  if (!intent || !ownsParty()) return false;
  try {
    await persist(intent);
  } catch {
    return false;
  }
  if (!ownsParty()) return false;
  clearPendingIntentIfCurrent(normalizedPartyId, intent);
  return true;
}
