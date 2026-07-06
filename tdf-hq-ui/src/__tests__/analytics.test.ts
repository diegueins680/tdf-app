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
const loggerLogMock = jest.fn();
const loggerWarnMock = jest.fn();
const loggerErrorMock = jest.fn();

jest.unstable_mockModule('posthog-js', () => ({
  __esModule: true,
  default: {
    init: initMock,
    capture: captureMock,
    identify: identifyMock,
    reset: resetMock,
  },
}));

jest.unstable_mockModule('../utils/logger', () => ({
  logger: {
    error: loggerErrorMock,
    log: loggerLogMock,
    warn: loggerWarnMock,
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
    loggerLogMock.mockReset();
    loggerWarnMock.mockReset();
    loggerErrorMock.mockReset();
  });

  afterAll(() => {
    delete testWindow.__ENV__;
    __resetAnalyticsForTests();
  });

  test('returns a no-op client when no key is configured', () => {
    const noopAnalyticsClient = getAnalyticsClient();
    expect(noopAnalyticsClient.ready).toBe(false);
    noopAnalyticsClient.capture('rsvp_created', { eventId: analyticsFixtureIds.noKeyEventId });
    noopAnalyticsClient.identify(analyticsFixtureIds.identifiedUserId);
    noopAnalyticsClient.reset();
    noopAnalyticsClient.page('Home');
    expect(initMock).not.toHaveBeenCalled();
    expect(captureMock).not.toHaveBeenCalled();
    expect(identifyMock).not.toHaveBeenCalled();
    expect(resetMock).not.toHaveBeenCalled();
    expect(loggerLogMock).toHaveBeenCalledWith(
      '[analytics] PostHog disabled: VITE_POSTHOG_KEY is unset. Events will not be sent.',
    );
  });

  test('forwards calls to PostHog when a key is configured', () => {
    testWindow.__ENV__ = { VITE_POSTHOG_KEY: 'phc_unit_test' };
    const configuredAnalyticsClient = getAnalyticsClient();
    expect(configuredAnalyticsClient.ready).toBe(true);
    expect(initMock).toHaveBeenCalledTimes(1);

    configuredAnalyticsClient.capture('rsvp_created', {
      eventId: analyticsFixtureIds.forwardedEventId,
      artistId: analyticsFixtureIds.forwardedArtistId,
    });
    configuredAnalyticsClient.identify(analyticsFixtureIds.identifiedUserId, { username: 'aria' });
    configuredAnalyticsClient.page('Home');
    configuredAnalyticsClient.reset();

    expect(captureMock).toHaveBeenCalledWith('rsvp_created', {
      eventId: analyticsFixtureIds.forwardedEventId,
      artistId: analyticsFixtureIds.forwardedArtistId,
    });
    expect(identifyMock).toHaveBeenCalledWith(analyticsFixtureIds.identifiedUserId, { username: 'aria' });
    expect(captureMock).toHaveBeenCalledWith('$pageview', { name: 'Home' });
    expect(resetMock).toHaveBeenCalled();
  });

  test('logs PostHog failures through the app logger', () => {
    testWindow.__ENV__ = { VITE_POSTHOG_KEY: 'phc_unit_test' };
    const resilientAnalyticsClient = getAnalyticsClient();
    const captureError = new Error('capture failed');
    const identifyError = new Error('identify failed');
    const resetError = new Error('reset failed');
    const pageError = new Error('page failed');
    const consoleWarnSpy = jest.spyOn(console, 'warn').mockImplementation(() => undefined);

    captureMock
      .mockImplementationOnce(() => {
        throw captureError;
      })
      .mockImplementationOnce(() => {
        throw pageError;
      });
    identifyMock.mockImplementationOnce(() => {
      throw identifyError;
    });
    resetMock.mockImplementationOnce(() => {
      throw resetError;
    });

    try {
      resilientAnalyticsClient.capture('rsvp_created', { eventId: analyticsFixtureIds.forwardedEventId });
      resilientAnalyticsClient.identify(analyticsFixtureIds.identifiedUserId);
      resilientAnalyticsClient.reset();
      resilientAnalyticsClient.page('Home');

      expect(loggerWarnMock).toHaveBeenCalledWith('[analytics] capture failed', { error: captureError });
      expect(loggerWarnMock).toHaveBeenCalledWith('[analytics] identify failed', { error: identifyError });
      expect(loggerWarnMock).toHaveBeenCalledWith('[analytics] reset failed', { error: resetError });
      expect(loggerWarnMock).toHaveBeenCalledWith('[analytics] page failed', { error: pageError });
      expect(consoleWarnSpy).not.toHaveBeenCalled();
    } finally {
      consoleWarnSpy.mockRestore();
    }
  });

  test('memoizes the client across calls', () => {
    testWindow.__ENV__ = { VITE_POSTHOG_KEY: 'phc_unit_test' };
    const firstAnalyticsClient = getAnalyticsClient();
    const secondAnalyticsClient = getAnalyticsClient();
    expect(firstAnalyticsClient).toBe(secondAnalyticsClient);
    expect(initMock).toHaveBeenCalledTimes(1);
  });
});
