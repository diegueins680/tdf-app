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

const { get } = await import('./client');

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

    expect(fetchMock).toHaveBeenCalledWith(
      'https://api.tdf.test/auth-check',
      expect.objectContaining({
        headers: expect.objectContaining({
          Authorization: 'Bearer test-token',
          'Content-Type': 'application/json',
        }),
      })
    );
  });
});
