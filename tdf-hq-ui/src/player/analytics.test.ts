import { jest } from '@jest/globals';
import type { sendPlayerAnalytics as SendPlayerAnalytics } from './analytics';

const buildAuthorizationHeaderMock = jest.fn<() => string | undefined>(() => undefined);

jest.unstable_mockModule('../api/authHeader', () => ({
  buildAuthorizationHeader: buildAuthorizationHeaderMock,
}));

jest.unstable_mockModule('../config/apiBase', () => ({
  resolveApiBase: () => 'https://api.tdf.test/',
}));

let sendPlayerAnalytics: typeof SendPlayerAnalytics;

const successfulResponse = { ok: true, status: 200 } as Response;

describe('player analytics delivery', () => {
  const fetchMock = jest.fn<typeof fetch>();

  beforeEach(async () => {
    jest.resetModules();
    ({ sendPlayerAnalytics } = await import('./analytics'));
    fetchMock.mockReset();
    fetchMock.mockResolvedValue(successfulResponse);
    buildAuthorizationHeaderMock.mockReset();
    buildAuthorizationHeaderMock.mockReturnValue(undefined);
    window.localStorage.clear();
    (globalThis as unknown as { fetch: typeof fetch }).fetch = fetchMock;
  });

  afterEach(() => jest.restoreAllMocks());

  const event = (eventType = 'progress') => ({
    schemaVersion: 1, eventId: crypto.randomUUID(), eventType,
    recordingId: '20000000-0000-4000-8000-000000000009',
    releaseVersionId: '30000000-0000-4000-8000-000000000009',
    positionMs: 1000, listenedDeltaMs: 1000, occurredAt: '2026-09-16T12:00:00.000Z',
  });
  const payloads = () => fetchMock.mock.calls.map(([, init]) =>
    JSON.parse(init?.body as string) as Record<string, unknown>);

  it('rotates sessions across login, account changes and logout, without carrying sequence or tokens', async () => {
    await sendPlayerAnalytics(event());
    buildAuthorizationHeaderMock.mockReturnValue('Bearer account-a');
    await sendPlayerAnalytics(event());
    await sendPlayerAnalytics(event());
    buildAuthorizationHeaderMock.mockReturnValue('Bearer account-b');
    await sendPlayerAnalytics(event());
    buildAuthorizationHeaderMock.mockReturnValue(undefined);
    await sendPlayerAnalytics(event());
    const rows = payloads();
    expect(rows.map(row => row['sequenceNumber'])).toEqual([0, 0, 1, 0, 0]);
    expect(rows[1]?.['sessionId']).toBe(rows[2]?.['sessionId']);
    expect(new Set(rows.map(row => row['sessionId'])).size).toBe(4);
    expect(rows[4]?.['anonymousId']).toBe(rows[0]?.['anonymousId']);
    expect(JSON.stringify(rows)).not.toMatch(/account-a|account-b|Bearer/);
  });

  it.each(['getItem', 'setItem'] as const)('keeps one page-local anonymous identity when storage %s fails', async method => {
    jest.spyOn(Storage.prototype, method).mockImplementation(() => { throw new DOMException('Denied', 'SecurityError'); });
    await sendPlayerAnalytics(event());
    await sendPlayerAnalytics(event());
    const [first, second] = payloads();
    expect(first?.['anonymousId']).toBe(second?.['anonymousId']);
    expect(first?.['sessionId']).toBe(second?.['sessionId']);
    expect(second?.['sequenceNumber']).toBe(1);
  });

  it('rotates session and visitor identity after accessible storage is cleared', async () => {
    await sendPlayerAnalytics(event());
    window.localStorage.clear();
    await sendPlayerAnalytics(event());
    const [first, second] = payloads();
    expect(first?.['anonymousId']).not.toBe(second?.['anonymousId']);
    expect(first?.['sessionId']).not.toBe(second?.['sessionId']);
    expect(second?.['sequenceNumber']).toBe(0);
  });

  it('creates a new session on replay after completion for the same identity', async () => {
    await sendPlayerAnalytics(event('play_start'));
    await sendPlayerAnalytics(event('complete'));
    await sendPlayerAnalytics(event('play_start'));
    const rows = payloads();
    expect(rows[0]?.['sessionId']).toBe(rows[1]?.['sessionId']);
    expect(rows[2]?.['sessionId']).not.toBe(rows[0]?.['sessionId']);
    expect(rows[2]?.['sequenceNumber']).toBe(0);
  });

  it('sends an opaque anonymous identifier without an authorization header', async () => {
    await sendPlayerAnalytics({
      schemaVersion: 1,
      eventId: '10000000-0000-4000-8000-000000000001',
      eventType: 'progress',
      recordingId: '20000000-0000-4000-8000-000000000001',
      releaseVersionId: '30000000-0000-4000-8000-000000000001',
      positionMs: 5000,
      listenedDeltaMs: 5000,
      occurredAt: '2026-09-12T12:00:00.000Z',
    });

    const [url, init] = fetchMock.mock.calls[0] ?? [];
    expect(url).toBe('https://api.tdf.test/music/playback-events');
    expect(init).toMatchObject({ method: 'POST', credentials: 'include', keepalive: true });
    expect(typeof init?.body).toBe('string');
    const payload = JSON.parse(init?.body as string) as Record<string, unknown>;
    expect(payload['anonymousId']).toEqual(expect.any(String));
    expect(String(payload['anonymousId'])).toHaveLength(72);
    expect(payload['sequenceNumber']).toBe(0);
    expect(payload['listenedDeltaMs']).toBe(5000);
    expect(new Headers(init?.headers).has('Authorization')).toBe(false);
  });

  it('uses the authenticated route and never sends the anonymous identifier', async () => {
    buildAuthorizationHeaderMock.mockReturnValue('Bearer session-token');
    await sendPlayerAnalytics({
      schemaVersion: 1,
      eventId: '10000000-0000-4000-8000-000000000002',
      eventType: 'play_start',
      recordingId: '20000000-0000-4000-8000-000000000002',
      releaseVersionId: '30000000-0000-4000-8000-000000000002',
      positionMs: 0,
      occurredAt: '2026-09-12T12:00:00.000Z',
    });

    const [url, init] = fetchMock.mock.calls[0] ?? [];
    expect(url).toBe('https://api.tdf.test/music/me/playback-events');
    expect(new Headers(init?.headers).get('Authorization')).toBe('Bearer session-token');
    expect(typeof init?.body).toBe('string');
    const payload = JSON.parse(init?.body as string) as Record<string, unknown>;
    expect(payload).not.toHaveProperty('anonymousId');
  });

  it('does not send legacy non-UUID release sources to the canonical analytics API', async () => {
    await sendPlayerAnalytics({
      schemaVersion: 1,
      eventId: '10000000-0000-4000-8000-000000000003',
      eventType: 'play_start',
      recordingId: 'legacy-track-1',
      releaseVersionId: 'legacy-release-1',
      positionMs: 0,
      occurredAt: '2026-09-12T12:00:00.000Z',
    });
    expect(fetchMock).not.toHaveBeenCalled();
  });
});
