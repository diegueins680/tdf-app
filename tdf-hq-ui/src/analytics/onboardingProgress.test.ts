import { jest } from '@jest/globals';

import { captureFirstValueOnce, captureReconciledFirstValue } from './onboardingProgress';
import type { OnboardingCompletionResultDTO, OnboardingFirstValue } from '../api/session';

const completionResult = (
  firstValue: OnboardingFirstValue | null,
  newlyCompleted: boolean,
): OnboardingCompletionResultDTO => ({
  newlyCompleted,
  progress: {
    eligible: false,
    signupCompletedAt: '2026-09-09T09:00:00Z',
    onboardingIntent: 'events',
    completedAt: newlyCompleted ? '2026-09-09T10:00:00Z' : null,
    firstValue,
    firstValueCompletedAt: firstValue ? '2026-09-09T09:30:00Z' : null,
    updatedAt: '2026-09-09T10:00:00Z',
  },
});

describe('captureFirstValueOnce', () => {
  it('emits first value and completion only after the server claims the first completion', async () => {
    const analytics = { capture: jest.fn() };
    const complete = jest.fn()
      .mockResolvedValueOnce({ progress: { eligible: false, firstValue: 'artist_followed' }, newlyCompleted: true })
      .mockResolvedValueOnce({ progress: { eligible: false, firstValue: 'artist_followed' }, newlyCompleted: false });

    await expect(captureFirstValueOnce(analytics, 42, 'artist_followed', complete)).resolves.toBe(true);
    await expect(captureFirstValueOnce(analytics, 42, 'event_saved', complete)).resolves.toBe(false);
    expect(analytics.capture).toHaveBeenCalledTimes(2);
    expect(analytics.capture).toHaveBeenCalledWith('first_value_completed', expect.objectContaining({
      platform: 'web',
      value: 'artist_followed',
    }));
  });

  it('does not emit when the party is missing or the durable request fails', async () => {
    const analytics = { capture: jest.fn() };
    const complete = jest.fn().mockRejectedValue(new Error('offline'));

    await expect(captureFirstValueOnce(analytics, null, 'artist_followed', complete)).resolves.toBe(false);
    await expect(captureFirstValueOnce(analytics, 7, 'artist_followed', complete)).resolves.toBe(false);
    expect(complete).toHaveBeenCalledTimes(1);
    expect(analytics.capture).not.toHaveBeenCalled();
  });

  it('uses only the canonical server-inferred value and never a missing value', () => {
    const analytics = { capture: jest.fn() };

    expect(captureReconciledFirstValue(
      analytics,
      42,
      completionResult('event_saved', true),
    )).toBe(true);
    expect(captureReconciledFirstValue(
      analytics,
      42,
      completionResult(null, true),
    )).toBe(false);
    expect(captureReconciledFirstValue(
      analytics,
      42,
      completionResult('moment_reaction', false),
    )).toBe(false);

    expect(analytics.capture).toHaveBeenCalledTimes(2);
    expect(analytics.capture).toHaveBeenCalledWith('first_value_completed', expect.objectContaining({
      platform: 'web',
      value: 'event_saved',
    }));
  });
});
