import { jest } from '@jest/globals';

import { captureFirstValueOnce, markWebSignupCompleted } from './onboardingProgress';

describe('captureFirstValueOnce', () => {
  it('emits first value and completion only once per party', () => {
    const values = new Map<string, string>();
    const storage = {
      getItem: (key: string) => values.get(key) ?? null,
      setItem: (key: string, value: string) => { values.set(key, value); },
    };
    const analytics = { capture: jest.fn() };

    expect(markWebSignupCompleted(42, storage, 1_000)).toBe(true);
    expect(captureFirstValueOnce(analytics, 42, 'artist_followed', storage, 1_100)).toBe(true);
    expect(captureFirstValueOnce(analytics, 42, 'event_saved', storage, 1_200)).toBe(false);
    expect(analytics.capture).toHaveBeenCalledTimes(2);
    expect(analytics.capture).toHaveBeenCalledWith('first_value_completed', expect.objectContaining({
      platform: 'web',
      value: 'artist_followed',
    }));
  });

  it('does not label existing or expired accounts as onboarding conversions', () => {
    const values = new Map<string, string>();
    const storage = {
      getItem: (key: string) => values.get(key) ?? null,
      setItem: (key: string, value: string) => { values.set(key, value); },
    };
    const analytics = { capture: jest.fn() };

    expect(captureFirstValueOnce(analytics, 7, 'artist_followed', storage, 1_000)).toBe(false);
    markWebSignupCompleted(7, storage, 1_000);
    expect(captureFirstValueOnce(analytics, 7, 'artist_followed', storage, 1_000 + 24 * 60 * 60 * 1000 + 1)).toBe(false);
    expect(analytics.capture).not.toHaveBeenCalled();
  });
});
