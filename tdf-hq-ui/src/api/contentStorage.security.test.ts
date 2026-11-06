import { jest } from '@jest/globals';

let epoch = 1;
const get = jest.fn(); const put = jest.fn(); const del = jest.fn();
jest.unstable_mockModule('./client', () => ({ get, put, del }));
jest.unstable_mockModule('./authHeader', () => ({ buildAuthorizationHeader: () => 'Bearer tdf-session' }));
jest.unstable_mockModule('../config/apiBase', () => ({ resolveApiBase: () => 'https://api.tdf.test' }));
jest.unstable_mockModule('../session/authEvents', () => ({ getAuthSessionEpoch: () => epoch }));
const storage = await import('./contentStorage');
const identifier = '00000000-0000-4000-8000-000000000001';
const url = `https://api.tdf.test/storage/files/${identifier}`;
const fetchMock = jest.fn<typeof fetch>();
beforeEach(() => { epoch = 1; fetchMock.mockReset(); globalThis.fetch = fetchMock; });

it('only sends TDF authentication to exact canonical storage URLs', async () => {
  for (const foreign of [`https://evil.test/storage/files/${identifier}`, `${url}?download=1`, `${url}#public`, 'https://api.tdf.test@evil.test/storage/files/' + identifier]) {
    await expect(storage.downloadStorageUrl(foreign, 'private.pdf')).rejects.toThrow('Invalid protected storage URL');
  }
  expect(fetchMock).not.toHaveBeenCalled();
  expect(storage.storageFileIdFromUrl(url)).toBe(identifier);
});

it('does not deliver bytes after an account or session change', async () => {
  fetchMock.mockResolvedValue({ ok: true, blob: async () => { epoch += 1; return new Blob(['private']); } } as Response);
  await expect(storage.downloadStorageUrl(url, 'private.pdf')).rejects.toThrow('Session changed');
  expect(fetchMock).toHaveBeenCalledWith(url, expect.objectContaining({ cache: 'no-store', credentials: 'include', headers: { Authorization: 'Bearer tdf-session' } }));
});

it('never downloads an unauthorized response body', async () => {
  const blob = jest.fn();
  fetchMock.mockResolvedValue({ ok: false, status: 404, blob } as unknown as Response);
  await expect(storage.downloadStorageUrl(url, 'private.pdf')).rejects.toThrow('Storage file unavailable');
  expect(blob).not.toHaveBeenCalled();
});
