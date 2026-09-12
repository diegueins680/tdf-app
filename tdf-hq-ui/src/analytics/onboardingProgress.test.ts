import { jest } from '@jest/globals';

import {
  captureFirstValueOnce,
  captureReconciledFirstValue,
  PENDING_FIRST_VALUE_PREFIX,
  retryPendingFirstValueCompletion,
} from './onboardingProgress';
import type { OnboardingCompletionResultDTO, OnboardingFirstValue } from '../api/session';

const pendingKey = (partyId: number): string => `${PENDING_FIRST_VALUE_PREFIX}${partyId}`;

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
  beforeEach(() => {
    window.localStorage.clear();
  });

  it('emits first value and completion only after the server claims the first completion', async () => {
    const analytics = { capture: jest.fn() };
    const complete = jest.fn()
      .mockResolvedValueOnce({ progress: { eligible: false, firstValue: 'artist_followed' }, newlyCompleted: true })
      .mockResolvedValueOnce({ progress: { eligible: false, firstValue: 'artist_followed' }, newlyCompleted: false });

    await expect(captureFirstValueOnce(analytics, 42, 'artist_followed', complete, () => true)).resolves.toBe(true);
    await expect(captureFirstValueOnce(analytics, 42, 'event_saved', complete, () => true)).resolves.toBe(false);
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
    await expect(captureFirstValueOnce(analytics, 7, 'artist_followed', complete, () => true)).resolves.toBe(false);
    expect(complete).toHaveBeenCalledTimes(1);
    expect(analytics.capture).not.toHaveBeenCalled();
    expect(window.localStorage.getItem(pendingKey(7))).toBe('artist_followed');
  });

  it('replays a failed handshake for the same Party and clears it after an authoritative response', async () => {
    const analytics = { capture: jest.fn() };
    const offlineComplete = jest.fn().mockRejectedValue(new Error('offline'));

    await expect(captureFirstValueOnce(
      analytics,
      7,
      'access_requested',
      offlineComplete,
      () => true,
    )).resolves.toBe(false);

    const retryComplete = jest.fn().mockResolvedValue({
      progress: { eligible: false, firstValue: 'access_requested' },
      newlyCompleted: true,
    });
    await expect(retryPendingFirstValueCompletion(
      analytics,
      7,
      retryComplete,
      () => true,
    )).resolves.toBe(true);

    expect(retryComplete).toHaveBeenCalledWith('access_requested');
    expect(window.localStorage.getItem(pendingKey(7))).toBeNull();
    expect(analytics.capture).toHaveBeenCalledTimes(2);
  });

  it('clears an idempotent replay without duplicating completion analytics', async () => {
    const analytics = { capture: jest.fn() };
    window.localStorage.setItem(pendingKey(7), 'artist_followed');
    const complete = jest.fn().mockResolvedValue({
      progress: { eligible: false, firstValue: 'artist_followed' },
      newlyCompleted: false,
    });

    await expect(retryPendingFirstValueCompletion(
      analytics,
      7,
      complete,
      () => true,
    )).resolves.toBe(false);

    expect(window.localStorage.getItem(pendingKey(7))).toBeNull();
    expect(analytics.capture).not.toHaveBeenCalled();
  });

  it('retains the initiating Party retry and suppresses analytics after an account switch', async () => {
    const analytics = { capture: jest.fn() };
    let resolveCompletion: ((value: OnboardingCompletionResultDTO) => void) | undefined;
    const complete = jest.fn(() => new Promise<OnboardingCompletionResultDTO>((resolve) => {
      resolveCompletion = resolve;
    }));
    let ownsParty = true;

    const pending = captureFirstValueOnce(analytics, 7, 'artist_followed', complete, () => ownsParty);
    ownsParty = false;
    resolveCompletion?.(completionResult('artist_followed', true));

    await expect(pending).resolves.toBe(false);
    expect(window.localStorage.getItem(pendingKey(7))).toBe('artist_followed');
    expect(analytics.capture).not.toHaveBeenCalled();
  });

  it('keeps Party queues isolated and removes invalid stored values without an API call', async () => {
    const analytics = { capture: jest.fn() };
    const complete = jest.fn();
    window.localStorage.setItem(pendingKey(7), 'not-supported');
    window.localStorage.setItem(pendingKey(8), 'event_saved');

    await expect(retryPendingFirstValueCompletion(
      analytics,
      7,
      complete,
      () => true,
    )).resolves.toBe(false);

    expect(window.localStorage.getItem(pendingKey(7))).toBeNull();
    expect(window.localStorage.getItem(pendingKey(8))).toBe('event_saved');
    expect(complete).not.toHaveBeenCalled();
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
