/**
 * Smoke tests for the web analytics module.
 *
 * Mocks posthog-js so we never reach the network from a unit test.
 */
import { jest } from '@jest/globals';
import { readFileSync } from 'node:fs';

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

const {
  __resetAnalyticsForTests,
  getAnalyticsClient,
  redactSensitiveQueryValues,
  sanitizeAnalyticsProperties,
} = await import('../analytics/posthog');

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
    expect(identifyMock).toHaveBeenCalledWith(analyticsFixtureIds.identifiedUserId, undefined);
    expect(captureMock).toHaveBeenCalledWith('$pageview', { name: 'Home' });
    expect(resetMock).toHaveBeenCalled();
  });

  test('redacts credentials while preserving PostHog\'s required project token', () => {
    const secret = 'reset-secret-sentinel';
    const source = `https://tdf.test/reset?token=${secret}&redirect=%2Ffans`;
    const sanitizedUrl = redactSensitiveQueryValues(source);
    const sanitizedProperties = sanitizeAnalyticsProperties({
      $current_url: source,
      $referrer: `https://tdf.test/oauth?redirect=${encodeURIComponent(`/reset?token=${secret}`)}`,
      route: '/reset',
      token: secret,
      nested: {
        email: 'private@example.com',
        password: secret,
        returnUrl: source,
      },
    });

    expect(sanitizedUrl).not.toContain(secret);
    expect(JSON.stringify(sanitizedProperties)).not.toContain(secret);
    expect(JSON.stringify(sanitizedProperties)).not.toContain('private@example.com');
    expect(sanitizedProperties).toMatchObject({ route: '/reset' });

    testWindow.__ENV__ = { VITE_POSTHOG_KEY: 'phc_unit_test' };
    getAnalyticsClient();
    const initOptions = initMock.mock.calls[0]?.[1] as {
      autocapture?: boolean;
      before_send?: (event: { properties: Record<string, unknown> }) => {
        properties: Record<string, unknown>;
      } | null;
    };
    expect(initOptions.autocapture).toBe(false);
    const outgoing = initOptions.before_send?.({
      properties: {
        $current_url: source,
        token: 'phc_unit_test',
        nested: { token: secret },
      },
    });
    expect(JSON.stringify(outgoing)).not.toContain(secret);
    expect(outgoing?.properties.token).toBe('phc_unit_test');

    const outgoingWithApplicationToken = initOptions.before_send?.({
      properties: { token: secret },
    });
    expect(outgoingWithApplicationToken?.properties).not.toHaveProperty('token');
  });

  test('masks ticket credentials and buyer data in SDK and explicit properties', () => {
    const sentinel = 'PRIVATE-TICKET-SENTINEL';
    const properties = sanitizeAnalyticsProperties({
      event_id: 141,
      tier_id: 2,
      quantity: 2,
      lookupToken: sentinel,
      buyer_email: sentinel,
      buyerName: sentinel,
      holder: { name: sentinel },
      ticketCodes: [sentinel],
      qr_payload: sentinel,
      nested: { transferCode: sentinel, resourcePath: sentinel, order_id: sentinel },
      $current_url: `https://www.tdfrecords.net/eventos/141/orden/${sentinel}?id=${sentinel}`,
      $pathname: `/eventos/141/orden/${sentinel}`,
      $referrer: `https://tdf.test/pagos/retorno?reference=${sentinel}#${sentinel}`,
      landingPath: `/social-events/ticket-transfers/${sentinel}/accept`,
    });
    expect(JSON.stringify(properties)).not.toContain(sentinel);
    expect(properties).toMatchObject({ event_id: 141, tier_id: 2, quantity: 2 });
    expect(properties.$current_url).toContain('/eventos/141/orden/');
    expect(redactSensitiveQueryValues('https://tdf.test/eventos/141?utm_source=artist'))
      .toBe('https://tdf.test/eventos/141?utm_source=artist');
  });

  test('sanitizes SDK initial and identify person-property envelopes before sending', () => {
    testWindow.__ENV__ = { VITE_POSTHOG_KEY: 'phc_unit_test' };
    getAnalyticsClient();
    interface Event {
      event: string;
      properties: Record<string, unknown>;
      $set?: Record<string, unknown>;
      $set_once?: Record<string, unknown>;
    }
    const initOptions = initMock.mock.calls[0]?.[1] as {
      before_send: (event: Event | null) => Event | null;
    };
    const sentinel = 'PRIVATE-INITIAL-PERSON-URL';
    const initialUrl = `https://tdf.test/eventos/141/orden/${sentinel}?lookupToken=${sentinel}`;
    for (const event of ['$pageview', '$identify']) {
      const outgoing = initOptions.before_send({
        event,
        properties: { token: 'phc_unit_test', $current_url: initialUrl },
        $set: { email: sentinel, landing_url: initialUrl, campaign: 'patch-culture' },
        $set_once: { $initial_current_url: initialUrl, $initial_referrer: initialUrl,
          $initial_pathname: `/eventos/141/orden/${sentinel}`, $initial_utm_source: 'artist' },
      });
      expect(JSON.stringify(outgoing)).not.toContain(sentinel);
      expect(outgoing?.properties.token).toBe('phc_unit_test');
      expect(outgoing?.$set).toMatchObject({ campaign: 'patch-culture' });
      expect(outgoing?.$set_once).toMatchObject({ $initial_utm_source: 'artist' });
    }
    expect(initOptions.before_send(null)).toBeNull();
  });

  test('masks camel-case credentials, fragments and nested redirect URLs', () => {
    const sentinel = 'PRIVATE-LOOKUP-SENTINEL';
    let redirect = `/eventos/141?lookupToken=${sentinel}&buyerEmail=${sentinel}`;
    for (let index = 0; index < 5; index += 1) redirect = `/return?next=${encodeURIComponent(redirect)}`;
    for (const url of [redirect, `/eventos/141#access_token=${sentinel}`,
      `/eventos/141?resourcePath=${sentinel}`, `https://${sentinel}@tdf.test/eventos/141`,
      `/social-events/ticket-transfers/${sentinel}/accept`, `tdf://tickets/${sentinel}`]) {
      expect(redactSensitiveQueryValues(url)).not.toContain(sentinel);
    }
  });

  test('redacts every payment return route including encoded and case variants', () => {
    const sentinel = 'PRIVATE-PROVIDER-RESOURCE';
    for (const path of ['/marketplace/pago-datafast', '/mezcla-mastering/pago-datafast',
      '/pagos/retorno/', '/PAGOS/RETORNO', '/marketplace/%70ago-datafast',
      '/eventos/141/%6Frden/92', '/domo-del-pululahua/cotizaciones/92',
      '/curso/produccion/orden/92', '/live-sessions/registro',
      '/mezcla-mastering/pedido/PRIVATE-PROVIDER-RESOURCE']) {
      expect(redactSensitiveQueryValues(`https://tdf.test${path}?id=${sentinel}&reference=${sentinel}&t=${sentinel}`))
        .not.toContain(sentinel);
    }
  });

  test('keeps configured private navigation identifiers out of automatic pageviews', () => {
    const sentinel = 'PRIVATE-ROUTE-CAPABILITY';
    const privateParam = /^(?:orderId|orderNumber|registrationId|bookingId|quoteId|token|notificationId|partyId|reportId|taskId|planId|destinationId|id)$/;
    let checked = 0;
    for (const file of ['publicRoutes.tsx', 'protectedRoutes.tsx']) {
      const source = readFileSync(new URL(`../routes/${file}`, import.meta.url), 'utf8');
      for (const match of source.matchAll(/path="([^"]*:[^"]+)"/g)) {
        const route = match[1]!;
        let sensitive = false;
        const path = route.replace(/:([A-Za-z0-9_]+)/g, (_value, name: string) => {
          if (privateParam.test(name)) { sensitive = true; return sentinel; }
          return 'public';
        });
        if (!sensitive) continue;
        checked += 1;
        const url = `https://tdf.test/${path.replace(/^\//, '')}`;
        expect(redactSensitiveQueryValues(url)).not.toContain(sentinel);
      }
    }
    expect(checked).toBeGreaterThan(15);
    expect(redactSensitiveQueryValues(`/inscripcion/produccion?lead=${sentinel}&t=${sentinel}`))
      .not.toContain(sentinel);
  });

  test('masks arbitrary navigation queries while retaining reviewed attribution parameters', () => {
    const sentinel = 'PRIVATE-NOTIFICATION-IDENTIFIER';
    for (const key of ['application', 'invitation', 'review', 'alert', 'request', 'futurePrivateKey']) {
      const value = redactSensitiveQueryValues(`https://tdf.test/mis-clasificados?${key}=${sentinel}&utm_source=artist&ref=partner&referral_code=artist-code`);
      expect(value).not.toContain(sentinel);
      expect(new URL(value).searchParams.get('utm_source')).toBe('artist');
      expect(new URL(value).searchParams.get('ref')).toBe('partner');
      expect(new URL(value).searchParams.get('referral_code')).toBe('artist-code');
    }
    const campaign = redactSensitiveQueryValues('https://tdf.test/eventos/141?utm_campaign=%23release');
    expect(new URL(campaign).searchParams.get('utm_campaign')).toBe('#release');
  });

  test('preserves ordinary fragment-like campaign names outside URL values', () => {
    expect(sanitizeAnalyticsProperties({ attribution_campaign: '#release' }))
      .toEqual({ attribution_campaign: '#release' });
    expect(redactSensitiveQueryValues('https://tdf.test/eventos/141#private-token'))
      .not.toContain('private-token');
  });

  test('preserves question-mark campaign labels while masking relative URL properties', () => {
    const properties = sanitizeAnalyticsProperties({
      attribution_campaign: 'fall?sale',
      $initial_utm_campaign: 'fall?sale',
      $current_url: 'https://tdf.test/eventos/141?utm_campaign=fall%3Fsale',
      nested: { returnUrl: 'receipt?lookupToken=PRIVATE-RELATIVE-TOKEN' },
    });
    expect(properties.attribution_campaign).toBe('fall?sale');
    expect(properties.$initial_utm_campaign).toBe('fall?sale');
    expect(new URL(properties.$current_url).searchParams.get('utm_campaign')).toBe('fall?sale');
    expect(JSON.stringify(properties)).not.toContain('PRIVATE-RELATIVE-TOKEN');
  });

  test('does not reinterpret scheme-like scalar labels as custom URL schemes', () => {
    for (const campaign of ['launch:fall?sale', 'instagram:stories#launch']) {
      const properties = sanitizeAnalyticsProperties({
        attribution_campaign: campaign,
        $initial_utm_campaign: campaign,
        $current_url: `https://tdf.test/eventos/141?utm_campaign=${encodeURIComponent(campaign)}`,
      });
      expect(properties.attribution_campaign).toBe(campaign);
      expect(properties.$initial_utm_campaign).toBe(campaign);
      expect(new URL(properties.$current_url).searchParams.get('utm_campaign')).toBe(campaign);
    }
    const privateLinks = sanitizeAnalyticsProperties({
      returnUrl: 'custom:receipt?lookupToken=PRIVATE-CUSTOM-URI',
      sharedLink: 'tdf://tickets/PRIVATE-CUSTOM-URI',
    });
    expect(JSON.stringify(privateLinks)).not.toContain('PRIVATE-CUSTOM-URI');
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

      expect(loggerWarnMock).toHaveBeenCalledWith('[analytics] capture failed');
      expect(loggerWarnMock).toHaveBeenCalledWith('[analytics] identify failed');
      expect(loggerWarnMock).toHaveBeenCalledWith('[analytics] reset failed');
      expect(loggerWarnMock).toHaveBeenCalledWith('[analytics] page failed');
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
