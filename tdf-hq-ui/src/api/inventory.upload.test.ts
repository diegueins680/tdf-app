import { jest } from '@jest/globals';

const authorization = jest.fn<() => string | undefined>(() => undefined);
jest.unstable_mockModule('./authHeader', () => ({ buildAuthorizationHeader: authorization }));
jest.unstable_mockModule('./client', () => ({ get: jest.fn(), post: jest.fn(), patch: jest.fn(), del: jest.fn() }));
jest.unstable_mockModule('../config/apiBase', () => ({ resolveApiBase: () => 'https://api.tdf.test' }));
const { uploadAssetPhoto, uploadAssetPhotoByQrToken } = await import('./inventory');

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
      ? JSON.stringify({ auPath: '/uploads/smoke.png', auFileName: 'smoke.png', auPublicUrl: 'https://assets.test/smoke.png' })
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
  await expect(uploadAssetPhoto(file)).resolves.toMatchObject({ publicUrl: 'https://assets.test/smoke.png' });
  expect(UploadRequest.current.withCredentials).toBe(true);
  expect(UploadRequest.current.headers.has('Authorization')).toBe(false);
  expect(UploadRequest.current.body?.get('file')).toBe(file);
});

it('retains bearer authentication for existing token sessions', async () => {
  authorization.mockReturnValue('Bearer existing-token');
  await uploadAssetPhoto(new File(['smoke'], 'smoke.png'));
  expect(UploadRequest.current.headers.get('Authorization')).toBe('Bearer existing-token');
});

it('preserves encoded QR-token upload routing', async () => {
  await uploadAssetPhotoByQrToken('opaque/token', new File(['smoke'], 'smoke.png'));
  expect(UploadRequest.current.url).toContain('/public/assets/qr/opaque%2Ftoken/upload');
});
