import { jest } from '@jest/globals';

const envReadMock = jest.fn<(key: string) => string | undefined>(() => undefined);

jest.unstable_mockModule('../utils/env', () => ({
  env: {
    read: envReadMock,
  },
}));

const { confirmPasswordReset, googleLoginRequest, loginRequest, signupRequest, requestPasswordReset } = await import('./auth');

const createHeaders = (contentType?: string, extra: Record<string, string | null> = {}) => ({
  get: jest.fn((name: string) => {
    const normalized = name.toLowerCase();
    if (normalized === 'content-type') return contentType ?? null;
    return extra[normalized] ?? null;
  }),
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

  it.each([undefined, null, 'https://evil.example/', '//evil.example/', '/\\evil.example/'])('omits unsafe reset destinations: %s', async (redirect) => {
    fetchMock.mockResolvedValueOnce({ ok: true } as Response);
    await requestPasswordReset('ana@example.com', redirect);
    expect(fetchMock).toHaveBeenCalledWith('/v1/password-reset', expect.objectContaining({
      body: JSON.stringify({ email: 'ana@example.com' }),
    }));
  });

  it('carries a safe destination separately from the compatible email-only body', async () => {
    fetchMock.mockResolvedValueOnce({ ok: true } as Response);
    const redirect = '/fans?artist=42&tab=eventos#próximo';
    await requestPasswordReset('ana@example.com', redirect);
    const [url, init] = fetchMock.mock.calls[0]!;
    if (typeof url !== 'string' || typeof init?.body !== 'string') {
      throw new Error('Expected a URL string and the compatible JSON body');
    }
    expect(new URL(url, 'https://tdf.local').searchParams.get('redirect')).toBe('/fans?artist=42&tab=eventos#pr%C3%B3ximo');
    expect(JSON.parse(init.body)).toEqual({ email: 'ana@example.com' });
  });

  it('carries the requested recovery language without changing the email body', async () => {
    fetchMock.mockResolvedValueOnce({ ok: true } as Response);
    await requestPasswordReset('ana@example.com', '/fans', 'en-US');
    expect(fetchMock).toHaveBeenCalledWith('/v1/password-reset?redirect=%2Ffans&locale=en', expect.objectContaining({ body: JSON.stringify({ email: 'ana@example.com' }) }));
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
      signal: expect.any(AbortSignal),
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

  it('retries transient startup responses for login using the backend retry-after hint', async () => {
    jest.useFakeTimers();

    fetchMock
      .mockResolvedValueOnce({
        ok: false,
        status: 503,
        headers: createHeaders('application/json', { 'retry-after': '5' }),
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
    await jest.advanceTimersByTimeAsync(4999);
    expect(fetchMock).toHaveBeenCalledTimes(1);
    await jest.advanceTimersByTimeAsync(1);

    await expect(promise).resolves.toEqual({
      token: 'session-token',
      partyId: 45,
      roles: ['Fan'],
      modules: ['CRM'],
    });
    expect(fetchMock).toHaveBeenCalledTimes(2);
  });

  it('uses each configured login retry once before showing a startup message', async () => {
    jest.useFakeTimers();
    const loginPayload = {
      username: 'admin',
      password: 'Password123',
    };

    fetchMock.mockImplementation(() =>
      Promise.resolve({
        ok: false,
        status: 503,
        headers: createHeaders('application/json'),
        text: jest.fn<() => Promise<string>>().mockResolvedValue('{"error":"starting"}'),
      } as unknown as Response),
    );

    const promise = loginRequest(loginPayload);
    const rejection = expect(promise).rejects.toThrow('El servicio está arrancando. Intenta de nuevo en unos segundos.');

    expect(fetchMock).toHaveBeenCalledTimes(1);
    await jest.advanceTimersByTimeAsync(999);
    expect(fetchMock).toHaveBeenCalledTimes(1);
    await jest.advanceTimersByTimeAsync(1);
    expect(fetchMock).toHaveBeenCalledTimes(2);

    await jest.advanceTimersByTimeAsync(1999);
    expect(fetchMock).toHaveBeenCalledTimes(2);
    await jest.advanceTimersByTimeAsync(1);
    expect(fetchMock).toHaveBeenCalledTimes(3);

    await jest.advanceTimersByTimeAsync(4999);
    expect(fetchMock).toHaveBeenCalledTimes(3);
    await jest.advanceTimersByTimeAsync(1);

    await rejection;
    expect(fetchMock).toHaveBeenCalledTimes(4);
    expect(fetchMock.mock.calls).toEqual(
      Array.from({ length: 4 }, () => [
        '/login',
        {
          method: 'POST',
          credentials: 'include',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(loginPayload),
          signal: expect.any(AbortSignal),
        },
      ]),
    );
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
    expect(fetchMock).toHaveBeenCalledTimes(1);
  });

  it('wraps login network failures with a stable service message', async () => {
    jest.useFakeTimers();
    fetchMock.mockRejectedValue(new TypeError('Failed to fetch'));

    const promise = loginRequest({
      username: 'admin',
      password: 'Password123',
    });
    const rejection = expect(promise).rejects.toThrow(
      'No se pudo conectar con el servicio. Revisa tu conexión e inténtalo de nuevo.',
    );

    expect(fetchMock).toHaveBeenCalledTimes(1);
    await jest.advanceTimersByTimeAsync(1000);
    expect(fetchMock).toHaveBeenCalledTimes(2);
    await jest.advanceTimersByTimeAsync(2000);
    expect(fetchMock).toHaveBeenCalledTimes(3);
    await jest.advanceTimersByTimeAsync(5000);

    await rejection;
    expect(fetchMock).toHaveBeenCalledTimes(4);
  });

  it('tags an empty unauthorized response for locale-aware presentation', async () => {
    fetchMock.mockResolvedValueOnce({ ok: false, status: 401, headers: createHeaders('text/plain'), text: async () => '' } as Response);
    await expect(loginRequest({ username: 'synthetic', password: 'fictional' })).rejects.toMatchObject({ code: 'credentials', message: 'Credenciales inválidas' });
  });

  it('aborts a hanging signup request with a stable timeout message', async () => {
    jest.useFakeTimers();
    fetchMock.mockImplementation((_input, init) => new Promise((_resolve, reject) => {
      init?.signal?.addEventListener('abort', () => {
        reject(new DOMException('Aborted', 'AbortError'));
      });
    }));

    const pending = signupRequest({
      firstName: 'Elena',
      lastName: 'Paredes',
      email: 'elena@persona.test',
      password: 'fictional-password',
      termsAccepted: true,
      termsVersion: 'test-terms',
    });
    const rejection = expect(pending).rejects.toMatchObject({ code: 'timeout', message: 'La solicitud tardó demasiado. Revisa tu conexión e inténtalo de nuevo.' });

    await jest.advanceTimersByTimeAsync(30_000);
    await rejection;
  });

  it.each(['success', 'error'] as const)('bounds a signup %s body after headers arrive', async (kind) => {
    jest.useFakeTimers();
    fetchMock.mockImplementation(async (_input, init) => ({
      ok: kind === 'success',
      status: kind === 'success' ? 200 : 400,
      headers: createHeaders('application/json'),
      [kind === 'success' ? 'json' : 'text']: () => new Promise((_resolve, reject) => {
        init?.signal?.addEventListener('abort', () => reject(new DOMException('Aborted', 'AbortError')), { once: true });
      }),
    }) as unknown as Response);
    let outcome: unknown = 'pending';
    void signupRequest({
      firstName: 'Elena', lastName: 'Paredes', email: 'elena@persona.test',
      password: 'fictional-password', termsAccepted: true, termsVersion: 'test-terms',
    }).then(() => { outcome = 'success'; }, (error: unknown) => { outcome = error; });

    await jest.advanceTimersByTimeAsync(29_999);
    expect(outcome).toBe('pending');
    await jest.advanceTimersByTimeAsync(1);
    expect(outcome).toMatchObject({ code: 'timeout' });
    expect(fetchMock).toHaveBeenCalledTimes(1);
    expect(jest.getTimerCount()).toBe(0);
  });

  it.each([loginRequest, googleLoginRequest])('does not replay a login after its response body stalls', async (request) => {
    jest.useFakeTimers();
    fetchMock.mockImplementation(async (_input, init) => ({
      ok: true,
      json: () => new Promise((_resolve, reject) => {
        init?.signal?.addEventListener('abort', () => reject(new DOMException('Aborted', 'AbortError')), { once: true });
      }),
    }) as unknown as Response);
    let outcome: unknown = 'pending';
    // Both requests use the same transport; this union needs their compatible intersection.
    void request({ username: 'synthetic', password: 'fictional', idToken: 'synthetic' })
      .then(() => { outcome = 'success'; }, (error: unknown) => { outcome = error; });
    await jest.advanceTimersByTimeAsync(30_000);
    expect(outcome).toMatchObject({ code: 'timeout' });
    expect(fetchMock).toHaveBeenCalledTimes(1);
    expect(jest.getTimerCount()).toBe(0);
  });

  it('bounds reset confirmation body reads without replaying the single-use token', async () => {
    jest.useFakeTimers();
    fetchMock.mockImplementation(async (_input, init) => ({
      ok: true,
      json: () => new Promise((_resolve, reject) => {
        init?.signal?.addEventListener('abort', () => reject(new DOMException('Aborted', 'AbortError')), { once: true });
      }),
    }) as unknown as Response);
    let outcome: unknown = 'pending';
    void confirmPasswordReset({ token: 'synthetic', newPassword: 'fictional' })
      .then(() => { outcome = 'success'; }, (error: unknown) => { outcome = error; });
    await jest.advanceTimersByTimeAsync(30_000);
    expect(outcome).toMatchObject({ code: 'timeout' });
    expect(fetchMock).toHaveBeenCalledTimes(1);
  });


  it('keeps reset-token errors distinct from login credential errors', async () => {
    fetchMock.mockResolvedValueOnce({
      ok: false, status: 401, headers: createHeaders('text/plain'),
      text: async () => 'Invalid or expired token',
    } as Response);
    await expect(confirmPasswordReset({ token: 'expired', newPassword: 'fictional' }))
      .rejects.toMatchObject({ code: 'response', message: 'Invalid or expired token' });
    expect(fetchMock).toHaveBeenCalledTimes(1);
  });

});
