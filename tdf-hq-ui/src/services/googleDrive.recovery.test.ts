import { jest } from '@jest/globals';

let owner = 10;
let epoch = 1;
const post = jest.fn<(...args: unknown[]) => Promise<unknown>>();
jest.unstable_mockModule('../api/client', () => ({ post, del: jest.fn() }));
jest.unstable_mockModule('../session/SessionContext', () => ({ getActiveSession: () => ({ partyId: owner }) }));
jest.unstable_mockModule('../session/authEvents', () => ({ getAuthSessionEpoch: () => epoch }));
const drive = await import('./googleDrive');
const firstHandle = 'tdf-vault-v1_00000000-0000-4000-8000-000000000001';
const secondHandle = 'tdf-vault-v1_00000000-0000-4000-8000-000000000002';
beforeEach(() => { owner = 10; epoch = 1; localStorage.clear(); post.mockReset(); });

it('never reuses an unbound legacy credential or another account credential', () => {
  localStorage.setItem('tdf-google-drive-token', JSON.stringify({ accessToken: 'legacy', expiresAt: Date.now() + 90_000 }));
  expect(drive.getStoredToken()).toBeNull();
  drive.storeToken({ accessToken: firstHandle, expiresAt: Date.now() + 90_000 });
  owner = 11;
  expect(drive.getStoredToken()).toBeNull();
});

it('coalesces refreshes and preserves the opaque owner-bound connection handle', async () => {
  drive.storeToken({ accessToken: firstHandle, refreshToken: firstHandle, expiresAt: 1 });
  let release: ((value: unknown) => void) | undefined;
  post.mockImplementation(() => new Promise((resolve) => { release = resolve; }));
  const first = drive.ensureAccessToken();
  const second = drive.ensureAccessToken();
  expect(post).toHaveBeenCalledTimes(1);
  release?.({ accessToken: firstHandle, refreshToken: firstHandle, expiresIn: 3600 });
  const results = await Promise.all([first, second]);
  expect(results[0]?.accessToken).toBe(firstHandle);
  expect(drive.getStoredToken()?.refreshToken).toBe(firstHandle);
});

it('cannot overwrite a newly signed-in account with an old pending refresh', async () => {
  drive.storeToken({ accessToken: firstHandle, refreshToken: firstHandle, expiresAt: 1 });
  let release: ((value: unknown) => void) | undefined;
  post.mockImplementation(() => new Promise((resolve) => { release = resolve; }));
  const oldRequest = drive.ensureAccessToken();
  owner = 11; epoch += 1;
  drive.storeToken({ accessToken: secondHandle, expiresAt: Date.now() + 90_000 });
  release?.({ accessToken: secondHandle, expiresIn: 3600 });
  await expect(oldRequest).rejects.toMatchObject({ code: 'storage_reconnect_required' });
  expect(drive.getStoredToken()?.accessToken).toBe(secondHandle);
});

it('never persists raw delegated credentials, even if an old server returns them', () => {
  expect(() => drive.storeToken({ accessToken: 'raw-google-access-token', refreshToken: 'raw-google-refresh-token', expiresAt: 123 })).toThrow();
  expect(localStorage.length).toBe(0);
  localStorage.setItem('tdf-google-drive-token:10', JSON.stringify({ ownerPartyId: 10, accessToken: 'old-scoped-provider-token', expiresAt: Date.now() + 90_000 }));
  expect(drive.getStoredToken()).toBeNull();
  expect(localStorage.getItem('tdf-google-drive-token:10')).toBeNull();
});
