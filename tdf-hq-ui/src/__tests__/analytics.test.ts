/**
 * Smoke tests for the web analytics module.
 *
 * Mocks posthog-js so we never reach the network from a unit test.
 */
jest.mock('posthog-js', () => {
  const init = jest.fn();
  const capture = jest.fn();
  const identify = jest.fn();
  const reset = jest.fn();
  return {
    __esModule: true,
    default: { init, capture, identify, reset },
  };
});

import posthog from 'posthog-js';

import { __resetAnalyticsForTests, getAnalyticsClient } from '../analytics/posthog';

const initMock = (posthog as unknown as { init: jest.Mock }).init;
const captureMock = (posthog as unknown as { capture: jest.Mock }).capture;
const identifyMock = (posthog as unknown as { identify: jest.Mock }).identify;
const resetMock = (posthog as unknown as { reset: jest.Mock }).reset;

describe('analytics/posthog (web)', () => {
  const originalKey = (import.meta as unknown as { env: Record<string, string | undefined> }).env.VITE_POSTHOG_KEY;
  const env = (import.meta as unknown as { env: Record<string, string | undefined> }).env;

  beforeEach(() => {
    __resetAnalyticsForTests();
    initMock.mockClear();
    captureMock.mockClear();
    identifyMock.mockClear();
    resetMock.mockClear();
  });

  afterAll(() => {
    env.VITE_POSTHOG_KEY = originalKey;
    __resetAnalyticsForTests();
  });

  test('returns a no-op client when no key is configured', () => {
    delete env.VITE_POSTHOG_KEY;
    const client = getAnalyticsClient();
    expect(client.ready).toBe(false);
    client.capture('rsvp_created', { eventId: '1' });
    client.identify('42');
    client.reset();
    client.page('Home');
    expect(initMock).not.toHaveBeenCalled();
    expect(captureMock).not.toHaveBeenCalled();
    expect(identifyMock).not.toHaveBeenCalled();
    expect(resetMock).not.toHaveBeenCalled();
  });

  test('forwards calls to PostHog when a key is configured', () => {
    env.VITE_POSTHOG_KEY = 'phc_unit_test';
    const client = getAnalyticsClient();
    expect(client.ready).toBe(true);
    expect(initMock).toHaveBeenCalledTimes(1);

    client.capture('rsvp_created', { eventId: '7', artistId: '99' });
    client.identify('42', { username: 'aria' });
    client.page('Home');
    client.reset();

    expect(captureMock).toHaveBeenCalledWith('rsvp_created', { eventId: '7', artistId: '99' });
    expect(identifyMock).toHaveBeenCalledWith('42', { username: 'aria' });
    expect(captureMock).toHaveBeenCalledWith('$pageview', { name: 'Home' });
    expect(resetMock).toHaveBeenCalled();
  });

  test('memoizes the client across calls', () => {
    env.VITE_POSTHOG_KEY = 'phc_unit_test';
    const c1 = getAnalyticsClient();
    const c2 = getAnalyticsClient();
    expect(c1).toBe(c2);
    expect(initMock).toHaveBeenCalledTimes(1);
  });
});
