import { jest } from '@jest/globals';
let owner = 10;
let epoch = 1;
let strictMode: string | undefined;
const post = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const del = jest.fn<(...args: unknown[]) => Promise<void>>();
jest.unstable_mockModule('../utils/env', () => ({ env: { read: (key: string) => key === 'VITE_DRIVE_VAULT_REQUIRED' ? strictMode : undefined } }));
jest.unstable_mockModule('../api/client', () => ({ post, del }));
jest.unstable_mockModule('../session/SessionContext', () => ({ getActiveSession: () => ({ partyId: owner }) }));
jest.unstable_mockModule('../session/authEvents', () => ({ getAuthSessionEpoch: () => epoch }));
jest.unstable_mockModule('../api/authHeader', () => ({ buildAuthorizationHeader: () => 'Bearer synthetic-tdf-session' }));
jest.unstable_mockModule('../config/apiBase', () => ({ resolveApiBase: () => 'https://api.example.test' }));
jest.unstable_mockModule('../session/logoutIntent', () => ({ assertNoLogoutIntent: () => undefined }));
const drive = await import('./googleDrive');
const { retireLegacyDriveCredentials } = await import('./storageCredentials');
const { uploadToDrive } = await import('../api/drive');
const handle = 'tdf-vault-v1_00000000-0000-4000-8000-000000000001';
const legacy = () => ({ ownerPartyId: owner, ownerEpoch: epoch, accessToken: 'ya29.synthetic-access', refreshToken: '1//synthetic-refresh', expiresAt: Date.now() + 90_000 });
let requests: { url: string; form: FormData }[];
let responseStatus: number;
const OriginalXHR = globalThis.XMLHttpRequest;
class SyntheticXHR {
  static DONE = 4;
  readyState = 0; status = 0; responseText = ''; withCredentials = false; timeout = 0;
  upload = { onprogress: null }; onreadystatechange: (() => void) | null = null;
  private url = '';
  open(_method: string, url: string) { this.url = url; }
  setRequestHeader() { /* synthetic current-session header only */ }
  send(form: FormData) {
    requests.push({ url: this.url, form }); this.readyState = 4; this.status = responseStatus;
    this.responseText = responseStatus === 200 ? JSON.stringify({ duFileId: 'synthetic-file', duWebViewLink: 'https://drive.example.test/file' }) : JSON.stringify({ code: 'storage_resource_access_denied' });
    this.onreadystatechange?.();
  }
  abort() { /* no network exists in this test */ }
}
beforeEach(() => { strictMode = undefined; owner = 10; epoch = 1; localStorage.clear(); post.mockReset(); del.mockReset(); del.mockResolvedValue(); requests = []; responseStatus = 200; globalThis.XMLHttpRequest = SyntheticXHR as unknown as typeof XMLHttpRequest; });
afterAll(() => { globalThis.XMLHttpRequest = OriginalXHR; });

it.each([undefined, 'false', 'TRUE', '1'])('preserves an exact owner-bound old-server connection in staged mode %s', async (mode) => {
  strictMode = mode;
  drive.storeToken(legacy()); retireLegacyDriveCredentials();
  const token = await drive.ensureAccessToken();
  await uploadToDrive(new File(['data'], 'synthetic.txt'), { authMode: 'user', accessToken: token.accessToken });
  expect(token.accessToken).toBe('ya29.synthetic-access');
  expect(drive.getStoredToken()?.refreshToken).toBe('1//synthetic-refresh');
  expect(requests).toHaveLength(1); expect(requests[0]?.url).toBe('https://api.example.test/drive/upload');
  expect(requests[0]?.form.get('accessToken')).toBe('ya29.synthetic-access');
});
it.each([undefined, 'true'])('keeps new-backend opaque connections and the same authenticated proxy in mode %s', async (mode) => {
  strictMode = mode;
  drive.storeToken({ ...legacy(), accessToken: handle, refreshToken: handle }); retireLegacyDriveCredentials();
  const token = await drive.ensureAccessToken();
  await uploadToDrive(new File(['data'], 'synthetic.txt'), { authMode: 'user', accessToken: token.accessToken });
  expect(requests).toHaveLength(1); expect(requests[0]?.url).toBe('https://api.example.test/drive/upload');
  expect(requests[0]?.form.get('accessToken')).toBe(handle);
});
it('strict mode rejects an old-server response and retires its descriptor without sending an upload', () => {
  strictMode = 'true';
  expect(() => drive.storeToken(legacy())).toThrow();
  localStorage.setItem('tdf-google-drive-token:10', JSON.stringify(legacy()));
  expect(drive.getStoredToken()).toBeNull(); expect(localStorage.length).toBe(0); expect(requests).toHaveLength(0);
});
it.each(['tdf-vault-v1_invalid', 'tdf-vault-v2_unknown', 'TDF-VAULT-v1_invalid'])('never reinterprets declared malformed vault identity %s as an old-provider credential', (accessToken) => {
  expect(() => drive.storeToken({ ...legacy(), accessToken, refreshToken: undefined })).toThrow();
  expect(localStorage.length).toBe(0);
});
it('rejects mixed opaque and provider credential pairs in staged mode', () => {
  expect(() => drive.storeToken({ ...legacy(), accessToken: handle })).toThrow(); expect(localStorage.length).toBe(0);
});
it('preserves owner and in-flight epoch fences for old-server compatibility', () => {
  expect(() => drive.storeToken({ ...legacy(), ownerPartyId: 11 })).toThrow();
  expect(() => drive.storeToken({ ...legacy(), ownerEpoch: 0 })).toThrow();
  drive.storeToken(legacy()); owner = 11; epoch = 2;
  expect(drive.getStoredToken()).toBeNull(); expect(requests).toHaveLength(0);
});
it('never retries a denied legacy upload through a different destination', async () => {
  drive.storeToken(legacy()); responseStatus = 403;
  await expect(uploadToDrive(new File(['data'], 'synthetic.txt'), { authMode: 'user', accessToken: drive.getStoredToken()?.accessToken })).rejects.toBeDefined();
  expect(requests).toHaveLength(1); expect(requests[0]?.url).toBe('https://api.example.test/drive/upload');
});
it('retires unbound, malformed and mismatched-owner descriptors while preserving the exact owned legacy format', () => {
  localStorage.setItem('tdf-google-drive-token', JSON.stringify(legacy()));
  localStorage.setItem('tdf-google-drive-token:11', JSON.stringify(legacy()));
  localStorage.setItem('tdf-google-drive-token:12', JSON.stringify({ ...legacy(), ownerPartyId: 12, expiresAt: 'never' }));
  const descriptor = legacy();
  localStorage.setItem('tdf-google-drive-token:10', JSON.stringify(descriptor));
  retireLegacyDriveCredentials();
  expect(localStorage.getItem('tdf-google-drive-token')).toBeNull();
  expect(localStorage.getItem('tdf-google-drive-token:11')).toBeNull();
  expect(localStorage.getItem('tdf-google-drive-token:12')).toBeNull();
  expect(JSON.parse(localStorage.getItem('tdf-google-drive-token:10') ?? 'null')).toEqual(descriptor);
});

it('disconnects a staged legacy descriptor locally without requiring a new backend route', async () => {
  drive.storeToken(legacy());
  await drive.disconnectDrive();
  expect(del).not.toHaveBeenCalled(); expect(drive.getStoredToken()).toBeNull();
});
it.each([401, 403, 404])('does not downgrade an opaque disconnect denied with %s to a local success', async (status) => {
  drive.storeToken({ ...legacy(), accessToken: handle, refreshToken: handle });
  const denied = Object.assign(new Error('synthetic denied'), { status }); del.mockRejectedValue(denied);
  await expect(drive.disconnectDrive()).rejects.toBe(denied);
  expect(del).toHaveBeenCalledTimes(1); expect(del).toHaveBeenCalledWith('/drive/token');
  expect(drive.getStoredToken()?.accessToken).toBe(handle);
});
it.each([false, true])('cannot clear a replacement connection while an opaque revocation is pending (new epoch %s)', async (newEpoch) => {
  drive.storeToken({ ...legacy(), accessToken: handle, refreshToken: handle });
  let release: (() => void) | undefined;
  del.mockImplementation(() => new Promise<void>((resolve) => { release = resolve; }));
  const pending = drive.disconnectDrive();
  if (newEpoch) epoch += 1;
  const replacement = 'tdf-vault-v1_00000000-0000-4000-8000-000000000002';
  drive.storeToken({ ...legacy(), accessToken: replacement, refreshToken: replacement });
  release?.(); await pending;
  expect(drive.getStoredToken()?.accessToken).toBe(replacement);
});
