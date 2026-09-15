import type { OnboardingCompletionResultDTO, OnboardingFirstValue } from '../api/session';
import { captureGrowthEvent } from './growthAttribution';
import type { AnalyticsClient } from './posthog';

const FIRST_VALUES = new Set<OnboardingFirstValue>([
  'artist_followed',
  'access_requested',
  'event_saved',
  'moment_reaction',
]);

export const isOnboardingFirstValue = (value: string): value is OnboardingFirstValue =>
  FIRST_VALUES.has(value as OnboardingFirstValue);

const authoritativeFirstValue = (
  result: OnboardingCompletionResultDTO,
): OnboardingFirstValue | null => {
  const value = result.progress.firstValue;
  return typeof value === 'string' && isOnboardingFirstValue(value) ? value : null;
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
