import { jest } from '@jest/globals';

const envReadMock = jest.fn<(key: string) => string | undefined>(() => undefined);

jest.unstable_mockModule('../utils/env', () => ({
  env: {
    read: envReadMock,
  },
}));

const { confirmPasswordReset } = await import('./auth');
const { loginRequest } = await import('./auth');

const createHeaders = (contentType?: string) => ({
  get: jest.fn((name: string) => (name.toLowerCase() === 'content-type' ? contentType ?? null : null)),
}) as unknown as Headers;

describe('auth api', () => {
  const fetchMock = jest.fn<typeof fetch>();

  beforeEach(() => {
    fetchMock.mockReset();
    (globalThis as unknown as { fetch: typeof fetch }).fetch = fetchMock;
    jest.useRealTimers();
  });

  afterEach(() => {
    jest.useRealTimers();
  });

  it('posts password reset confirmations to the v1 confirm endpoint', async () => {
    fetchMock.mockResolvedValueOnce({
      ok: true,
      json: jest.fn<() => Promise<unknown>>().mockResolvedValue({
        token: 'session-token',
        partyId: 45,
        roles: ['Fan'],
        modules: ['CRM'],
      }),
    } as unknown as Response);

    const result = await confirmPasswordReset({
      token: 'reset-token',
      newPassword: 'Password123',
    });

    expect(fetchMock).toHaveBeenCalledWith('/v1/password-reset/confirm', {
      method: 'POST',
      credentials: 'include',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        token: 'reset-token',
        newPassword: 'Password123',
      }),
    });
    expect(result).toEqual({
      token: 'session-token',
      partyId: 45,
      roles: ['Fan'],
      modules: ['CRM'],
    });
  });

  it('surfaces the backend error message when reset confirmation fails', async () => {
    fetchMock.mockResolvedValueOnce({
      ok: false,
      status: 400,
      headers: createHeaders('text/plain'),
      text: jest.fn<() => Promise<string>>().mockResolvedValue('Invalid or expired token'),
    } as unknown as Response);

    await expect(
      confirmPasswordReset({ token: 'expired-token', newPassword: 'Password123' }),
    ).rejects.toThrow('Invalid or expired token');
  });

  it('retries transient startup responses for login and succeeds once the backend is ready', async () => {
    jest.useFakeTimers();

    fetchMock
      .mockResolvedValueOnce({
        ok: false,
        status: 503,
        headers: createHeaders('application/json'),
        text: jest.fn<() => Promise<string>>().mockResolvedValue('{"error":"starting"}'),
      } as unknown as Response)
      .mockResolvedValueOnce({
        ok: true,
        json: jest.fn<() => Promise<unknown>>().mockResolvedValue({
          token: 'session-token',
          partyId: 45,
          roles: ['Fan'],
          modules: ['CRM'],
        }),
      } as unknown as Response);

    const promise = loginRequest({
      username: 'admin',
      password: 'Password123',
    });

    expect(fetchMock).toHaveBeenCalledTimes(1);
    await jest.advanceTimersByTimeAsync(1000);

    await expect(promise).resolves.toEqual({
      token: 'session-token',
      partyId: 45,
      roles: ['Fan'],
      modules: ['CRM'],
    });
    expect(fetchMock).toHaveBeenCalledTimes(2);
  });

  it('shows a friendly startup message after login retries are exhausted', async () => {
    jest.useFakeTimers();

    fetchMock.mockImplementation(() =>
      Promise.resolve({
        ok: false,
        status: 503,
        headers: createHeaders('application/json'),
        text: jest.fn<() => Promise<string>>().mockResolvedValue('{"error":"starting"}'),
      } as unknown as Response),
    );

    const promise = loginRequest({
      username: 'admin',
      password: 'Password123',
    });
    const rejection = expect(promise).rejects.toThrow('El servicio está arrancando. Intenta de nuevo en unos segundos.');

    await jest.advanceTimersByTimeAsync(1000);
    await jest.advanceTimersByTimeAsync(2000);

    await rejection;
    expect(fetchMock).toHaveBeenCalledTimes(3);
  });

  it('extracts structured JSON error messages for login failures', async () => {
    fetchMock.mockResolvedValueOnce({
      ok: false,
      status: 401,
      headers: createHeaders('application/json'),
      text: jest.fn<() => Promise<string>>().mockResolvedValue('{"message":"Usuario o contraseña inválidos"}'),
    } as unknown as Response);

    await expect(
      loginRequest({
        username: 'admin',
        password: 'Password123',
      }),
    ).rejects.toThrow('Usuario o contraseña inválidos');
  });
});
