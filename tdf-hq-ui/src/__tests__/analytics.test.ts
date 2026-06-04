/**
 * Smoke tests for the web analytics module.
 *
 * Mocks posthog-js so we never reach the network from a unit test.
 */
import { jest } from '@jest/globals';

const initMock = jest.fn();
const captureMock = jest.fn();
const identifyMock = jest.fn();
const resetMock = jest.fn();

jest.unstable_mockModule('posthog-js', () => ({
  __esModule: true,
  default: {
    init: initMock,
    capture: captureMock,
    identify: identifyMock,
    reset: resetMock,
  },
}));

const { __resetAnalyticsForTests, getAnalyticsClient } = await import('../analytics/posthog');

type AnalyticsFixtureIds = Readonly<{
  noKeyEventId: string;
  forwardedEventId: string;
  forwardedArtistId: string;
  identifiedUserId: string;
}>;

// Invariant: analytics IDs are opaque strings; these tests assert passthrough behavior only.
const analyticsFixtureIds: AnalyticsFixtureIds = Object.freeze({
  noKeyEventId: 'event-rsvp-no-key',
  forwardedEventId: 'event-rsvp-forwarded',
  forwardedArtistId: 'artist-aria',
  identifiedUserId: 'user-aria',
});

type AnalyticsTestWindow = Window & {
  __ENV__?: Record<string, string | undefined>;
};

describe('analytics/posthog (web)', () => {
  const testWindow = window as AnalyticsTestWindow;

  beforeEach(() => {
    delete testWindow.__ENV__;
    __resetAnalyticsForTests();
    initMock.mockReset();
    captureMock.mockReset();
    identifyMock.mockReset();
    resetMock.mockReset();
  });

  afterAll(() => {
    delete testWindow.__ENV__;
    __resetAnalyticsForTests();
  });

  test('returns a no-op client when no key is configured', () => {
    const client = getAnalyticsClient();
    expect(client.ready).toBe(false);
    client.capture('rsvp_created', { eventId: analyticsFixtureIds.noKeyEventId });
    client.identify(analyticsFixtureIds.identifiedUserId);
    client.reset();
    client.page('Home');
    expect(initMock).not.toHaveBeenCalled();
    expect(captureMock).not.toHaveBeenCalled();
    expect(identifyMock).not.toHaveBeenCalled();
    expect(resetMock).not.toHaveBeenCalled();
  });

  test('forwards calls to PostHog when a key is configured', () => {
    testWindow.__ENV__ = { VITE_POSTHOG_KEY: 'phc_unit_test' };
    const client = getAnalyticsClient();
    expect(client.ready).toBe(true);
    expect(initMock).toHaveBeenCalledTimes(1);

    client.capture('rsvp_created', {
      eventId: analyticsFixtureIds.forwardedEventId,
      artistId: analyticsFixtureIds.forwardedArtistId,
    });
    client.identify(analyticsFixtureIds.identifiedUserId, { username: 'aria' });
    client.page('Home');
    client.reset();

    expect(captureMock).toHaveBeenCalledWith('rsvp_created', {
      eventId: analyticsFixtureIds.forwardedEventId,
      artistId: analyticsFixtureIds.forwardedArtistId,
    });
    expect(identifyMock).toHaveBeenCalledWith(analyticsFixtureIds.identifiedUserId, { username: 'aria' });
    expect(captureMock).toHaveBeenCalledWith('$pageview', { name: 'Home' });
    expect(resetMock).toHaveBeenCalled();
  });

  test('memoizes the client across calls', () => {
    testWindow.__ENV__ = { VITE_POSTHOG_KEY: 'phc_unit_test' };
    const c1 = getAnalyticsClient();
    const c2 = getAnalyticsClient();
    expect(c1).toBe(c2);
    expect(initMock).toHaveBeenCalledTimes(1);
  });
});
