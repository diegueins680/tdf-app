import { jest } from '@jest/globals';

const authorization = jest.fn<() => string | undefined>(() => undefined);
jest.unstable_mockModule('./authHeader', () => ({ buildAuthorizationHeader: authorization }));
jest.unstable_mockModule('../config/apiBase', () => ({ resolveApiBase: () => 'https://api.tdf.test' }));
const { uploadToDrive } = await import('./drive');

class UploadRequest {
  static DONE = 4;
  static current: UploadRequest;
  withCredentials = false;
  readyState = 0;
  status = 0;
  responseText = '';
  onreadystatechange?: () => void;
  upload = { onprogress: undefined };
  headers = new Map<string, string>();
  url = '';
  body?: FormData;
  constructor() { UploadRequest.current = this; }
  open(_method: string, url: string) { this.url = url; }
  setRequestHeader(name: string, value: string) { this.headers.set(name, value); }
  send(body: FormData) {
    this.body = body;
    this.status = this.withCredentials || this.headers.has('Authorization') ? 200 : 401;
    this.responseText = this.status === 200
      ? JSON.stringify({ duFileId: 'synthetic-cover', duPublicUrl: 'https://assets.test/smoke.png' })
      : 'Missing or invalid auth token';
    this.readyState = UploadRequest.DONE;
    this.onreadystatechange?.();
  }
}

const original = globalThis.XMLHttpRequest;
beforeEach(() => {
  authorization.mockReset();
  globalThis.XMLHttpRequest = UploadRequest as unknown as typeof XMLHttpRequest;
});
afterEach(() => { globalThis.XMLHttpRequest = original; });

it('uploads with a cookie-only Google session when no readable bearer token exists', async () => {
  const file = new File(['smoke'], 'smoke.png', { type: 'image/png' });
  await expect(uploadToDrive(file)).resolves.toMatchObject({ publicUrl: 'https://assets.test/smoke.png' });
  expect(UploadRequest.current.withCredentials).toBe(true);
  expect(UploadRequest.current.headers.has('Authorization')).toBe(false);
  expect(UploadRequest.current.body?.get('file')).toBe(file);
});

it('retains bearer authentication for existing token sessions', async () => {
  authorization.mockReturnValue('Bearer existing-token');
  await uploadToDrive(new File(['smoke'], 'smoke.png'));
  expect(UploadRequest.current.headers.get('Authorization')).toBe('Bearer existing-token');
});

it('uses the shared API resolver and preserves selected upload fields', async () => {
  const file = new File(['smoke'], 'smoke.png');
  await uploadToDrive(file, { folderId: 'selected-folder', name: 'public-cover', accessToken: 'provider-token' });
  expect(UploadRequest.current.url).toBe('https://api.tdf.test/drive/upload');
  expect(UploadRequest.current.body?.get('file')).toBe(file);
  expect(UploadRequest.current.body?.get('folderId')).toBe('selected-folder');
  expect(UploadRequest.current.body?.get('name')).toBe('public-cover');
  expect(UploadRequest.current.body?.get('accessToken')).toBe('provider-token');
});
