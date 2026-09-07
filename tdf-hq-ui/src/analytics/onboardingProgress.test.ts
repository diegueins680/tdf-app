import { jest } from '@jest/globals';

import { captureFirstValueOnce } from './onboardingProgress';

describe('captureFirstValueOnce', () => {
  it('emits first value and completion only after the server claims the first completion', async () => {
    const analytics = { capture: jest.fn() };
    const complete = jest.fn()
      .mockResolvedValueOnce({ progress: { eligible: false }, newlyCompleted: true })
      .mockResolvedValueOnce({ progress: { eligible: false }, newlyCompleted: false });

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
});
