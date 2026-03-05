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

const { get, post, postForm } = await import('./client');

interface MockResponseOptions {
  ok?: boolean;
  status?: number;
  statusText?: string;
  contentType?: string;
  body?: string;
}

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
  });

  it('returns undefined for successful responses with empty body', async () => {
    fetchMock.mockResolvedValueOnce(buildResponse({ body: '' }));

    const result = await get<undefined>('/health');

    expect(result).toBeUndefined();
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

    await postForm<{ ok: boolean }>('/upload', form);

    const call = fetchMock.mock.calls[0];
    expect(call).toBeDefined();
    expect(call?.[1]?.body).toBe(form);
    const headers = new Headers(call?.[1]?.headers);
    expect(headers.get('Content-Type')).toBeNull();
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
});
