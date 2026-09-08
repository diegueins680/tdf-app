import { jest } from '@jest/globals';

import {
  captureFirstValueOnce,
  PENDING_FIRST_VALUE_PREFIX,
  retryPendingFirstValueCompletion,
} from './onboardingProgress';

const pendingKey = (partyId: number): string => `${PENDING_FIRST_VALUE_PREFIX}${partyId}`;

describe('captureFirstValueOnce', () => {
  beforeEach(() => {
    window.localStorage.clear();
  });

  it('emits first value and completion only after the server claims the first completion', async () => {
    const analytics = { capture: jest.fn() };
    const complete = jest.fn()
      .mockResolvedValueOnce({ progress: { eligible: false }, newlyCompleted: true })
      .mockResolvedValueOnce({ progress: { eligible: false }, newlyCompleted: false });

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

    await expect(captureFirstValueOnce(analytics, null, 'artist_followed', complete, () => true)).resolves.toBe(false);
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
      progress: { eligible: false },
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
      progress: { eligible: false },
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
    let resolveCompletion: ((value: { progress: { eligible: boolean }; newlyCompleted: boolean }) => void) | undefined;
    const complete = jest.fn(() => new Promise<{ progress: { eligible: boolean }; newlyCompleted: boolean }>((resolve) => {
      resolveCompletion = resolve;
    }));
    let ownsParty = true;

    const pending = captureFirstValueOnce(analytics, 7, 'artist_followed', complete, () => ownsParty);
    ownsParty = false;
    resolveCompletion?.({ progress: { eligible: false }, newlyCompleted: true });

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
});
