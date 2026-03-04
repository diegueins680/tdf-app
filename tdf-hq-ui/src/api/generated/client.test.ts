import { jest } from '@jest/globals';

const buildAuthorizationHeaderMock = jest.fn<() => string | undefined>(() => 'Bearer generated-token');

jest.unstable_mockModule('../authHeader', () => ({
  buildAuthorizationHeader: buildAuthorizationHeaderMock,
}));

const { ApiClient } = await import('./client');

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

describe('generated ApiClient', () => {
  const fetchMock = jest.fn<typeof fetch>();

  beforeEach(() => {
    fetchMock.mockReset();
    buildAuthorizationHeaderMock.mockReset();
    buildAuthorizationHeaderMock.mockReturnValue('Bearer generated-token');
    (globalThis as unknown as { fetch: typeof fetch }).fetch = fetchMock;
  });

  it('returns raw text for successful non-JSON responses', async () => {
    fetchMock.mockResolvedValueOnce(buildResponse({ contentType: 'text/plain', body: 'pong' }));

    const client = new ApiClient('https://api.tdf.test');
    const result = await client.getUsers();

    expect(result as unknown).toBe('pong');
  });

  it('includes nested JSON error message in thrown errors', async () => {
    fetchMock.mockResolvedValueOnce(
      buildResponse({ ok: false, status: 400, statusText: 'Bad Request', body: '{"message":"payload inválido"}' })
    );

    const client = new ApiClient('https://api.tdf.test');

    await expect(client.getUsers()).rejects.toThrow('API error: 400 payload inválido');
  });

  it('wraps network failures with a stable API error message', async () => {
    fetchMock.mockRejectedValueOnce(new TypeError('network down'));

    const client = new ApiClient('https://api.tdf.test');

    await expect(client.getUsers()).rejects.toThrow('No se pudo contactar la API.');
  });
});
