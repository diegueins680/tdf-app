import { jest } from '@jest/globals';

const envReadMock = jest.fn<(key: string) => string | undefined>(() => undefined);

jest.unstable_mockModule('../utils/env', () => ({
  env: {
    read: envReadMock,
  },
}));

const { loadSessionSnapshot } = await import('./session');

describe('session api', () => {
  const fetchMock = jest.fn<typeof fetch>();

  beforeEach(() => {
    fetchMock.mockReset();
    (globalThis as unknown as { fetch: typeof fetch }).fetch = fetchMock;
  });

  it('returns null when the backend reports no authenticated session', async () => {
    fetchMock.mockResolvedValueOnce({
      ok: false,
      status: 401,
      text: jest.fn<() => Promise<string>>().mockResolvedValue('Invalid or inactive token'),
    } as unknown as Response);

    await expect(loadSessionSnapshot()).resolves.toBeNull();
  });

  it('accepts a public null session payload without treating it as an error', async () => {
    fetchMock.mockResolvedValueOnce({
      ok: true,
      status: 200,
      json: jest.fn<() => Promise<null>>().mockResolvedValue(null),
    } as unknown as Response);

    await expect(loadSessionSnapshot()).resolves.toBeNull();
  });

  it('returns the authenticated session snapshot when present', async () => {
    fetchMock.mockResolvedValueOnce({
      ok: true,
      status: 200,
      json: jest.fn<() => Promise<unknown>>().mockResolvedValue({
        username: 'alice',
        displayName: 'Alice',
        roles: ['Admin'],
        modules: ['CRM'],
        partyId: 42,
      }),
    } as unknown as Response);

    await expect(loadSessionSnapshot()).resolves.toEqual({
      username: 'alice',
      displayName: 'Alice',
      roles: ['Admin'],
      modules: ['CRM'],
      partyId: 42,
    });
  });
});
