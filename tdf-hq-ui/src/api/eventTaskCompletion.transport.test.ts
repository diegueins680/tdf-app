import { jest } from '@jest/globals';

const ambientToken = jest.fn<() => string | undefined>(() => 'Bearer ambient-test');
jest.unstable_mockModule('./authHeader', () => ({ buildAuthorizationHeader: ambientToken }));
jest.unstable_mockModule('../utils/env', () => ({ env: {
  read: (key: string) => key === 'VITE_API_BASE' ? 'https://api.tdf.test' : undefined,
} }));
jest.unstable_mockModule('../utils/logger', () => ({ logger: {
  error: jest.fn(), log: jest.fn(), warn: jest.fn(),
} }));
const { EventOperations } = await import('./eventOperations');
const { ApiError, getPendingApiRequestCount, subscribeToApiActivity } = await import('./client');
const key = '60000000-0000-4000-8000-000000000400';
const command = { expectedRevision: '4', reason: ' Preparación lista ', correlationId: 'test' };
const outcome = { eventId: 83, activityId: 8300, commandId: key, status: 'completed',
  activityVersion: 2, aggregateRevision: '5', replayed: false };
const response = (body: unknown, status = 200): Response => ({
  ok: status === 200, status, statusText: status === 200 ? 'OK' : 'Conflict',
  headers: new Headers({ 'Content-Type': 'application/json' }),
  text: async () => JSON.stringify(body),
} as Response);

describe('completion through real shared HTTP transport with synthetic fetch', () => {
  const originalFetch = globalThis.fetch;
  const fetchMock = jest.fn<typeof fetch>();
  beforeEach(() => {
    fetchMock.mockReset(); ambientToken.mockReturnValue('Bearer ambient-test');
    globalThis.fetch = fetchMock;
    expect(getPendingApiRequestCount()).toBe(0);
  });
  afterEach(() => {
    globalThis.fetch = originalFetch;
    expect(getPendingApiRequestCount()).toBe(0);
  });

  it('serializes the original body once and pins explicit credentials despite synchronous caller edits', async () => {
    const mutable = { ...command };
    const context = { apiToken: 'captured-test' };
    const unsubscribe = subscribeToApiActivity(() => {
      mutable.expectedRevision = '10'; mutable.reason = 'changed'; context.apiToken = 'changed';
    });
    fetchMock.mockResolvedValue(response(outcome));
    try {
      await expect(EventOperations.completeTask(83, 8300, key, mutable, context)).resolves.toEqual(outcome);
      expect(fetchMock).toHaveBeenCalledTimes(1);
      const [url, options] = fetchMock.mock.calls[0]!;
      expect(url).toBe('https://api.tdf.test/event-operations/events/83/tasks/8300/complete');
      expect(options).toMatchObject({ method: 'POST', cache: 'no-store', credentials: 'include',
        body: JSON.stringify(command) });
      const headers = new Headers(options?.headers);
      expect(headers.get('Authorization')).toBe('Bearer captured-test');
      expect(headers.get('Idempotency-Key')).toBe(key);
      expect(headers.get('Content-Type')).toBe('application/json');
    } finally { unsubscribe(); }
  });

  it('propagates a server conflict without a second fetch or fallback', async () => {
    fetchMock.mockResolvedValue(response({ code: 'dependencies_not_ready' }, 409));
    await expect(EventOperations.completeTask(83, 8300, key, command)).rejects.toMatchObject({
      name: ApiError.name, status: 409, message: 'dependencies_not_ready',
    });
    expect(fetchMock).toHaveBeenCalledTimes(1);
  });

  it('rejects a successful but foreign receipt without compensating or claiming rollback', async () => {
    fetchMock.mockResolvedValue(response({ ...outcome, activityId: 999 }));
    await expect(EventOperations.completeTask(83, 8300, key, command)).rejects.toThrow('Conserva la solicitud original');
    expect(fetchMock).toHaveBeenCalledTimes(1);
  });

  it('relays caller cancellation and cleans up transport activity without retry', async () => {
    const controller = new AbortController();
    fetchMock.mockImplementation((_url, options) => new Promise<Response>((_resolve, reject) => {
      options?.signal?.addEventListener('abort', () => reject(new DOMException('Cancelled', 'AbortError')), { once: true });
    }));
    const request = EventOperations.completeTask(83, 8300, key, command, { signal: controller.signal });
    expect(getPendingApiRequestCount()).toBe(1);
    controller.abort();
    await expect(request).rejects.toMatchObject({ name: 'AbortError' });
    expect(fetchMock).toHaveBeenCalledTimes(1);
  });
});
