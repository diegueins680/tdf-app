import { jest } from '@jest/globals';

let owner = 10;
let epoch = 1;
const post = jest.fn<(...args: unknown[]) => Promise<unknown>>();
jest.unstable_mockModule('../api/client', () => ({ post }));
jest.unstable_mockModule('../session/SessionContext', () => ({ getActiveSession: () => ({ partyId: owner }) }));
jest.unstable_mockModule('../session/authEvents', () => ({ getAuthSessionEpoch: () => epoch }));
const drive = await import('./googleDrive');
beforeEach(() => { owner = 10; epoch = 1; localStorage.clear(); post.mockReset(); });

it('never reuses an unbound legacy credential or another account credential', () => {
  localStorage.setItem('tdf-google-drive-token', JSON.stringify({ accessToken: 'legacy', expiresAt: Date.now() + 90_000 }));
  expect(drive.getStoredToken()).toBeNull();
  drive.storeToken({ accessToken: 'owner-ten', expiresAt: Date.now() + 90_000 });
  owner = 11;
  expect(drive.getStoredToken()).toBeNull();
});

it('coalesces refreshes and preserves the rotated refresh token', async () => {
  drive.storeToken({ accessToken: 'expired', refreshToken: 'old-refresh', expiresAt: 1 });
  let release: ((value: unknown) => void) | undefined;
  post.mockImplementation(() => new Promise((resolve) => { release = resolve; }));
  const first = drive.ensureAccessToken();
  const second = drive.ensureAccessToken();
  expect(post).toHaveBeenCalledTimes(1);
  release?.({ accessToken: 'new-access', refreshToken: 'rotated-refresh', expiresIn: 3600 });
  const results = await Promise.all([first, second]);
  expect(results[0]?.accessToken).toBe('new-access');
  expect(drive.getStoredToken()?.refreshToken).toBe('rotated-refresh');
});

it('cannot overwrite a newly signed-in account with an old pending refresh', async () => {
  drive.storeToken({ accessToken: 'expired', refreshToken: 'old-refresh', expiresAt: 1 });
  let release: ((value: unknown) => void) | undefined;
  post.mockImplementation(() => new Promise((resolve) => { release = resolve; }));
  const oldRequest = drive.ensureAccessToken();
  owner = 11; epoch += 1;
  drive.storeToken({ accessToken: 'new-account', expiresAt: Date.now() + 90_000 });
  release?.({ accessToken: 'stale-account', expiresIn: 3600 });
  await expect(oldRequest).rejects.toMatchObject({ code: 'storage_reconnect_required' });
  expect(drive.getStoredToken()?.accessToken).toBe('new-account');
});
