import { jest } from '@jest/globals';

const buildAuthorizationHeaderMock = jest.fn<() => string | undefined>(() => 'Bearer test-token');
const envReadMock = jest.fn<(key: string) => string | undefined>((key) =>
  key === 'VITE_API_BASE' ? 'https://api.tdf.test' : undefined
);

jest.unstable_mockModule('./authHeader', () => ({
  buildAuthorizationHeader: buildAuthorizationHeaderMock,
}));

jest.unstable_mockModule('../utils/env', () => ({
  env: {
    read: envReadMock,
  },
}));

jest.unstable_mockModule('../utils/logger', () => ({
  logger: {
    error: jest.fn(),
    log: jest.fn(),
    warn: jest.fn(),
  },
}));

const {
  ApiError,
  get,
  getPendingApiRequestCount,
  HTTP_STATUS_REQUEST_TIMEOUT,
  post,
  postForm,
  subscribeToApiActivity,
} = await import('./client');
const { advanceAuthSessionEpoch, AUTH_SESSION_EXPIRED_EVENT } = await import('../session/authEvents');

interface MockResponseOptions {
  ok?: boolean;
  status?: number;
  statusText?: string;
  contentType?: string;
  body?: string;
}

const HTTP_STATUS_SERVER_ERROR_BASE = 500;
const HTTP_STATUS_BAD_GATEWAY_OFFSET = 2;
const HTTP_STATUS_BAD_GATEWAY: NonNullable<MockResponseOptions['status']> =
  HTTP_STATUS_SERVER_ERROR_BASE + HTTP_STATUS_BAD_GATEWAY_OFFSET;

const buildResponse = (opts: MockResponseOptions = {}): Response => {
  const {
    ok = true,
    status = 200,
    statusText = 'OK',
    contentType = 'application/json',
    body = '',
  } = opts;

  return {
    ok,
    status,
    statusText,
    headers: {
      get: (key: string) => (key.toLowerCase() === 'content-type' ? contentType : null),
    } as Headers,
    text: jest.fn<() => Promise<string>>().mockResolvedValue(body),
  } as unknown as Response;
};

describe('api client', () => {
  const fetchMock = jest.fn<typeof fetch>();

  beforeEach(() => {
    fetchMock.mockReset();
    buildAuthorizationHeaderMock.mockReset();
    buildAuthorizationHeaderMock.mockReturnValue('Bearer test-token');
    (globalThis as unknown as { fetch: typeof fetch }).fetch = fetchMock;
    expect(getPendingApiRequestCount()).toBe(0);
  });

  it('returns undefined for successful responses with empty body', async () => {
    fetchMock.mockResolvedValueOnce(buildResponse({ body: '' }));

    const result = await get<undefined>('/health');

    expect(result).toBeUndefined();
  });

  it('tracks pending requests for global loading indicators', async () => {
    let resolveFetch: ((response: Response) => void) | undefined;
    fetchMock.mockImplementationOnce(
      () =>
        new Promise<Response>((resolve) => {
          resolveFetch = resolve;
        }),
    );
    const activityCounts: number[] = [];
    const unsubscribe = subscribeToApiActivity(() => {
      activityCounts.push(getPendingApiRequestCount());
    });

    try {
      const request = get<{ ok: boolean }>('/slow-report');

      expect(fetchMock).toHaveBeenCalledTimes(1);
      expect(getPendingApiRequestCount()).toBe(1);
      expect(activityCounts).toEqual([1]);

      expect(resolveFetch).toBeDefined();
      resolveFetch?.(buildResponse({ body: '{"ok":true}' }));

      await expect(request).resolves.toEqual({ ok: true });
      expect(getPendingApiRequestCount()).toBe(0);
      expect(activityCounts).toEqual([1, 0]);
    } finally {
      unsubscribe();
    }
  });

  it('represents an aborted request with the request-timeout status contract', async () => {
    fetchMock.mockRejectedValueOnce(new DOMException('Request aborted', 'AbortError'));

    const request = get('/slow-report');

    await expect(request).rejects.toMatchObject({
      name: ApiError.name,
      status: HTTP_STATUS_REQUEST_TIMEOUT,
    });
    expect(getPendingApiRequestCount()).toBe(0);
  });

  it('propagates caller cancellation without converting it into a timeout', async () => {
    fetchMock.mockImplementationOnce((_path, init) => new Promise<Response>((_resolve, reject) => {
      init?.signal?.addEventListener('abort', () => {
        reject(new DOMException('Request cancelled', 'AbortError'));
      }, { once: true });
    }));
    const controller = new AbortController();

    const cancelledRequest = get('/superseded-search', { signal: controller.signal });
    controller.abort();

    await expect(cancelledRequest).rejects.toMatchObject({ name: 'AbortError' });
    expect(getPendingApiRequestCount()).toBe(0);
  });

  it('parses JSON when successful responses include a payload', async () => {
    fetchMock.mockResolvedValueOnce(buildResponse({ body: '{"ok":true,"version":"1.0"}' }));

    const result = await get<{ ok: boolean; version: string }>('/version');

    expect(result).toEqual({ ok: true, version: '1.0' });
  });

  it('returns raw text when a successful payload is not JSON', async () => {
    fetchMock.mockResolvedValueOnce(buildResponse({ contentType: 'text/plain', body: 'pong' }));

    const result = await get<string>('/ping');

    expect(result).toBe('pong');
  });

  it('sends the authorization header when present', async () => {
    fetchMock.mockResolvedValueOnce(buildResponse({ body: '{"ok":true}' }));

    await get<{ ok: boolean }>('/auth-check');

    const call = fetchMock.mock.calls[0];
    expect(call?.[0]).toBe('https://api.tdf.test/auth-check');
    const headers = new Headers(call?.[1]?.headers);
    expect(headers.get('Authorization')).toBe('Bearer test-token');
  });

  it('adds the JSON content-type only when a request body is present', async () => {
    fetchMock.mockResolvedValueOnce(buildResponse({ body: '{"ok":true}' }));
    await get<{ ok: boolean }>('/no-body');

    const firstCall = fetchMock.mock.calls[0];
    expect(firstCall).toBeDefined();
    const firstHeaders = new Headers(firstCall?.[1]?.headers);
    expect(firstHeaders.has('Content-Type')).toBe(false);

    fetchMock.mockResolvedValueOnce(buildResponse({ body: '{"ok":true}' }));
    await post<{ ok: boolean }>('/with-body', { ok: true });

    const secondCall = fetchMock.mock.calls[1];
    expect(secondCall).toBeDefined();
    const secondHeaders = new Headers(secondCall?.[1]?.headers);
    expect(secondHeaders.get('Content-Type')).toBe('application/json');
  });

  it('keeps FormData requests as multipart bodies without forcing JSON content-type', async () => {
    fetchMock.mockResolvedValueOnce(buildResponse({ body: '{"ok":true}' }));
    const form = new FormData();
    form.append('file', new Blob(['image-bytes'], { type: 'image/png' }), 'cover.png');

    await postForm<{ ok: boolean }>('/upload', form, { headers: { 'Idempotency-Key': 'upload-key-123' } });

    const call = fetchMock.mock.calls[0];
    expect(call).toBeDefined();
    expect(call?.[1]?.body).toBe(form);
    const headers = new Headers(call?.[1]?.headers);
    expect(headers.get('Content-Type')).toBeNull();
    expect(headers.get('Idempotency-Key')).toBe('upload-key-123');
  });

  it('joins API base and paths that omit the leading slash', async () => {
    fetchMock.mockResolvedValueOnce(buildResponse({ body: '{"ok":true}' }));

    await get<{ ok: boolean }>('rooms');

    expect(fetchMock).toHaveBeenCalledWith(
      'https://api.tdf.test/rooms',
      expect.anything()
    );
  });

  it('keeps plain text numeric payloads as text when content-type is not JSON', async () => {
    fetchMock.mockResolvedValueOnce(buildResponse({ contentType: 'text/plain', body: '123' }));

    const result = await get<string>('/text-id');

    expect(result).toBe('123');
  });

  it('extracts message from application/problem+json errors', async () => {
    fetchMock.mockResolvedValueOnce(
      buildResponse({
        ok: false,
        status: 400,
        statusText: 'Bad Request',
        contentType: 'application/problem+json',
        body: '{"title":"Validation failed","detail":"Campo inválido"}',
      }),
    );

    await expect(get('/problem-json')).rejects.toThrow('Campo inválido');
  });

  it('extracts nested error messages from JSON error objects', async () => {
    fetchMock.mockResolvedValueOnce(
      buildResponse({
        ok: false,
        status: 400,
        statusText: 'Bad Request',
        contentType: 'application/json',
        body: '{"error":{"message":"Token inválido"}}',
      }),
    );

    await expect(get('/nested-error')).rejects.toThrow('Token inválido');
  });

  it('extracts the first readable message from JSON errors arrays', async () => {
    fetchMock.mockResolvedValueOnce(
      buildResponse({
        ok: false,
        status: 422,
        statusText: 'Unprocessable Entity',
        contentType: 'application/json',
        body: '{"errors":["",{"detail":"Campo requerido"}]}',
      }),
    );

    await expect(get('/errors-array')).rejects.toThrow('Campo requerido');
  });

  it('falls back to the raw body when JSON error payloads are malformed', async () => {
    const malformedBody = '{"message":"truncated"';
    fetchMock.mockResolvedValueOnce(
      buildResponse({
        ok: false,
        status: HTTP_STATUS_BAD_GATEWAY,
        statusText: 'Bad Gateway',
        contentType: 'application/json',
        body: malformedBody,
      }),
    );

    await expect(get('/malformed-error')).rejects.toThrow(malformedBody);
  });

  it('notifies the session layer when protected API auth has expired', async () => {
    const listener = jest.fn();
    window.addEventListener(AUTH_SESSION_EXPIRED_EVENT, listener);
    fetchMock.mockResolvedValueOnce(
      buildResponse({
        ok: false,
        status: 401,
        statusText: 'Unauthorized',
        contentType: 'text/plain',
        body: 'Missing or invalid auth token',
      }),
    );

    try {
      await expect(get('/protected')).rejects.toThrow('Missing or invalid auth token');

      expect(listener).toHaveBeenCalledTimes(1);
    } finally {
      window.removeEventListener(AUTH_SESSION_EXPIRED_EVENT, listener);
    }
  });
  it('does not expire a later session when an earlier request returns an auth failure', async () => {
    const listener = jest.fn();
    window.addEventListener(AUTH_SESSION_EXPIRED_EVENT, listener);
    let finish: ((response: Response) => void) | undefined;
    fetchMock.mockImplementationOnce(() => new Promise(resolve => { finish = resolve; }));
    const pending = get('/old-protected');
    const denied = expect(pending).rejects.toThrow('Missing or invalid auth token');
    // A -> B -> A: the same bearer value is deliberately retained. Occurrence,
    // rather than identity/token equality, must govern expiration authority.
    advanceAuthSessionEpoch();
    advanceAuthSessionEpoch();
    try {
      finish?.(buildResponse({ ok: false, status: 401, body: 'Missing or invalid auth token' }));
      await denied;
      expect(listener).not.toHaveBeenCalled();
    } finally {
      window.removeEventListener(AUTH_SESSION_EXPIRED_EVENT, listener);
    }
  });


  it.each([true, false])('keeps the deadline while reading the body (ok=%s)', async (ok) => {
    jest.useFakeTimers();
    try {
      fetchMock.mockImplementation(async (_path, init) => ({
        ...buildResponse({ ok, status: ok ? 200 : 500 }),
        text: () => new Promise((_resolve, reject) => {
          init?.signal?.addEventListener('abort', () => reject(new DOMException('Aborted', 'AbortError')), { once: true });
        }),
      }) as Response);
      let outcome: unknown = 'pending';
      void get('/stalled-body').then(() => { outcome = 'success'; }, (error: unknown) => { outcome = error; });
      await jest.advanceTimersByTimeAsync(29_999);
      expect(outcome).toBe('pending');
      expect(getPendingApiRequestCount()).toBe(1);
      await jest.advanceTimersByTimeAsync(1);
      expect(outcome).toMatchObject({ status: HTTP_STATUS_REQUEST_TIMEOUT });
      expect(getPendingApiRequestCount()).toBe(0);
      expect(fetchMock).toHaveBeenCalledTimes(1);
      expect(jest.getTimerCount()).toBe(0);
    } finally { jest.useRealTimers(); }
  });

  it('preserves cancellation after headers and releases the loading indicator', async () => {
    const controller = new AbortController();
    let readingBody = false;
    fetchMock.mockImplementation(async (_path, init) => ({
      ...buildResponse(),
      text: () => new Promise((_resolve, reject) => {
        readingBody = true;
        init?.signal?.addEventListener('abort', () => reject(new DOMException('Cancelled', 'AbortError')), { once: true });
      }),
    }) as Response);
    let outcome: unknown = 'pending';
    void get('/cancelled-body', { signal: controller.signal })
      .then(() => { outcome = 'success'; }, (error: unknown) => { outcome = error; });
    await Promise.resolve();
    await Promise.resolve();
    expect(readingBody).toBe(true);
    controller.abort();
    // Drain the async request/reader cleanup, without a real-time wait.
    for (let turn = 0; turn < 12; turn += 1) await Promise.resolve();
    expect(outcome).toMatchObject({ name: 'AbortError' });
    expect(getPendingApiRequestCount()).toBe(0);
  });

  it('reports a disconnected body instead of treating a failed read as empty success', async () => {
    fetchMock.mockResolvedValueOnce({
      ...buildResponse(),
      text: async () => { throw new TypeError('Connection lost'); },
    } as Response);
    await expect(get('/disconnected-body')).rejects.toThrow('No se pudo conectar con el servicio.');
    expect(getPendingApiRequestCount()).toBe(0);
  });


  it('uses one total deadline for headers and body rather than restarting the clock', async () => {
    jest.useFakeTimers();
    try {
      fetchMock.mockImplementation((_path, init) => new Promise((resolve) => {
        setTimeout(() => resolve({
          ...buildResponse(),
          text: () => new Promise((_resolve, reject) => {
            init?.signal?.addEventListener('abort', () => reject(new DOMException('Aborted', 'AbortError')), { once: true });
          }),
        } as Response), 20_000);
      }));
      let outcome: unknown = 'pending';
      void get('/slow-headers-and-body').catch((error: unknown) => { outcome = error; });
      await jest.advanceTimersByTimeAsync(29_999);
      expect(outcome).toBe('pending');
      await jest.advanceTimersByTimeAsync(1);
      expect(outcome).toMatchObject({ status: HTTP_STATUS_REQUEST_TIMEOUT });
      expect(getPendingApiRequestCount()).toBe(0);
    } finally { jest.useRealTimers(); }
  });

  it('does not start an already cancelled request', async () => {
    const controller = new AbortController();
    controller.abort();
    await expect(get('/already-cancelled', { signal: controller.signal })).rejects.toMatchObject({ name: 'AbortError' });
    expect(fetchMock).not.toHaveBeenCalled();
    expect(getPendingApiRequestCount()).toBe(0);
  });

});
