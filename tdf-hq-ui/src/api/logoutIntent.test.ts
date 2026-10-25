import { jest } from '@jest/globals';
jest.unstable_mockModule('./authHeader', () => ({ buildAuthorizationHeader: () => 'Bearer stale-test-token' }));
jest.unstable_mockModule('../utils/env', () => ({ env: { read: () => undefined } }));
const { beginLogoutIntent, clearLogoutIntent, trackLogoutRequest } = await import('../session/logoutIntent');
const { get } = await import('./client');
const { loadSessionSnapshot, logoutSessionRequest } = await import('./session');
const { loginRequest } = await import('./auth');
const { uploadToDrive } = await import('./drive');
const fetchMock = jest.fn<typeof fetch>();
beforeEach(() => { localStorage.clear(); clearLogoutIntent(); fetchMock.mockReset(); globalThis.fetch = fetchMock; });
afterEach(() => { jest.useRealTimers(); });
it('makes no cookie bootstrap request after explicit logout', async () => {
  beginLogoutIntent(); expect(await loadSessionSnapshot()).toBeNull(); expect(fetchMock).not.toHaveBeenCalled();
});
it('still bootstraps a cookie-only session when no logout was requested', async () => {
  fetchMock.mockResolvedValueOnce({ status: 200, ok: true, json: async () => ({ partyId: 42 }) } as Response);
  expect(await loadSessionSnapshot()).toEqual({ partyId: 42 });
  expect(fetchMock).toHaveBeenCalledWith(expect.stringContaining('/session'), expect.objectContaining({ credentials: 'include' }));
});
it('allows public reads without sending an old cookie or explicit bearer', async () => {
  beginLogoutIntent();
  fetchMock.mockResolvedValueOnce({ ok: true, status: 200, text: async () => '{"public":true}', headers: new Headers({ 'content-type': 'application/json' }) } as Response);
  expect(await get('/artists/public', { headers: { Authorization: 'Bearer old', 'X-Api-Token': 'old' } })).toEqual({ public: true });
  const init = fetchMock.mock.calls[0]![1]!;
  expect(init.credentials).toBe('omit');
  expect(new Headers(init.headers).has('Authorization')).toBe(false);
  expect(new Headers(init.headers).has('X-Api-Token')).toBe(false);
});
it('rejects credentialed XHR before dispatch while signed out', async () => {
  beginLogoutIntent(); const xhr = jest.spyOn(globalThis, 'XMLHttpRequest');
  try { await expect(uploadToDrive(new File(['x'], 'fictional.png'))).rejects.toThrow('Inicia sesión'); expect(xhr).not.toHaveBeenCalled(); }
  finally { xhr.mockRestore(); }
});
it('waits for an outstanding logout before submitting fresh credentials', async () => {
  let finish!: () => void; const pending = new Promise<void>((resolve) => { finish = resolve; }); trackLogoutRequest(pending);
  fetchMock.mockResolvedValueOnce({ ok: true, status: 200, headers: new Headers(), json: async () => ({ token: 'new' }), text: async () => '{"token":"new"}' } as Response);
  const login = loginRequest({ username: 'fictional', password: 'fictional-password' }); expect(fetchMock).not.toHaveBeenCalled();
  finish(); await expect(login).resolves.toEqual({ token: 'new' }); expect(fetchMock).toHaveBeenCalledTimes(1);
});
it('does not confirm logout on authorization failure', async () => {
  fetchMock.mockResolvedValueOnce({ ok: false, status: 403 } as Response);
  await expect(logoutSessionRequest()).rejects.toThrow('unconfirmed');
});
it('bounds a stalled logout so explicit login can recover', async () => {
  jest.useFakeTimers();
  fetchMock.mockImplementationOnce((_, init) => new Promise((_, reject) => { init?.signal?.addEventListener('abort', () => reject(new DOMException('aborted', 'AbortError'))); }));
  const result = expect(logoutSessionRequest()).rejects.toThrow('deadline');
  await jest.advanceTimersByTimeAsync(30000); await result;
});
