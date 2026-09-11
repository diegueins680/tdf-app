import { captureGrowthEvent } from './growthAttribution';
import type { AnalyticsClient } from './posthog';
import {
  completeOnboardingProgress,
  type OnboardingCompletionResultDTO,
  type OnboardingFirstValue,
} from '../api/session';

const FIRST_VALUES = new Set<OnboardingFirstValue>([
  'artist_followed',
  'access_requested',
  'event_saved',
  'moment_reaction',
]);

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
): Promise<boolean> {
  if (!partyId) return false;
  let result: OnboardingCompletionResultDTO;
  try {
    result = await complete(value);
  } catch {
    return false;
  }
  return captureReconciledFirstValue(analytics, partyId, result);
}
