import { captureGrowthEvent } from './growthAttribution';
import type { AnalyticsClient } from './posthog';
import {
  completeOnboardingProgress,
  type OnboardingCompletionResultDTO,
  type OnboardingFirstValue,
} from '../api/session';

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
  if (!result.newlyCompleted) return false;
  captureGrowthEvent(analytics, 'first_value_completed', { platform: 'web', value });
  captureGrowthEvent(analytics, 'onboarding_completed', { platform: 'web', reason: 'first_value', value });
  return true;
}
