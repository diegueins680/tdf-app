import { jest } from '@jest/globals';

const postMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const getMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const putMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const delMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
jest.unstable_mockModule('./client', () => ({
  post: postMock, get: getMock, put: putMock, del: delMock,
}));
const { uploadMusicAsset, MusicUploadSessionUnusableError, resetMusicUploadEvidenceForTests } = await import('./musicReleases');
const originalFetch = globalThis.fetch;
const fetchMock = jest.fn<typeof fetch>();
const options = {
  releaseId: 'release', versionId: 'version', recordingId: 'recording',
  assetRole: 'master_audio' as const, idempotencyKey: 'resume-synthetic',
  file: { size: 1, name: 'synthetic.wav', type: 'audio/wav',
    slice: () => ({ arrayBuffer: async () => new Uint8Array([1]).buffer }),
  } as unknown as File,
};
function response(body: string, etag: string | null) {
  return { ok: true, status: 200, text: async () => body,
    headers: { get: () => etag },
  } as unknown as Response;
}

describe('music upload completion gate', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    globalThis.fetch = fetchMock;
    postMock.mockResolvedValueOnce({ id: 'session', providerUploadIdBound: true,
      partSizeBytes: 5 * 1024 * 1024, parts: [{ partNumber: 1, byteSize: 1 }],
    });
    getMock.mockResolvedValueOnce({ url: 'https://storage.test.invalid/complete',
      method: 'POST', contentType: 'application/xml', body: '<CompleteMultipartUpload/>',
    });
  });
  afterEach(() => { globalThis.fetch = originalFetch; });

  it('does not confirm or cancel resumable evidence when S3 returns HTTP 200 with Error', async () => {
    fetchMock.mockResolvedValueOnce(response('<Error><Code>InvalidPart</Code></Error>', '"etag"'));
    await expect(uploadMusicAsset(options)).rejects.toThrow('InvalidPart');
    expect(postMock).toHaveBeenCalledTimes(1);
    expect(delMock).not.toHaveBeenCalled();
    expect(putMock).not.toHaveBeenCalled();
  });

  it('confirms once after a valid multipart response and omits browser credentials', async () => {
    fetchMock.mockResolvedValueOnce(response(
      '<CompleteMultipartUploadResult><ETag>"etag"</ETag></CompleteMultipartUploadResult>', null,
    ));
    postMock.mockResolvedValueOnce({ status: 'completed' });
    await expect(uploadMusicAsset(options)).resolves.toEqual({ status: 'completed' });
    expect(postMock).toHaveBeenCalledTimes(2);
    expect(postMock).toHaveBeenLastCalledWith('/music/uploads/session/confirm',
      { etag: '"etag"' }, { signal: undefined });
    expect(fetchMock).toHaveBeenCalledWith('https://storage.test.invalid/complete',
      expect.objectContaining({ credentials: 'omit', method: 'POST' }));
  });
});

describe('music upload replayed sessions', () => {
  beforeEach(() => { jest.clearAllMocks(); globalThis.fetch = fetchMock; });
  afterEach(() => { globalThis.fetch = originalFetch; });

  it('returns the already-confirmed upload without repeating multipart completion', async () => {
    postMock.mockResolvedValueOnce({ id: 'session', status: 'completed', providerUploadIdBound: true,
      partSizeBytes: 5 * 1024 * 1024, parts: [{ partNumber: 1, byteSize: 1 }] });
    await expect(uploadMusicAsset(options)).resolves.toEqual({ status: 'completed', sessionId: 'session', replayed: true });
    expect(postMock).toHaveBeenCalledTimes(1);
    expect(getMock).not.toHaveBeenCalled();
    expect(putMock).not.toHaveBeenCalled();
    expect(delMock).not.toHaveBeenCalled();
    expect(fetchMock).not.toHaveBeenCalled();
  });

  it.each(['cancelled', 'failed', 'expired'] as const)('refuses to resume a %s session so the caller allocates a new key', async (status) => {
    postMock.mockResolvedValueOnce({ id: 'session', status, providerUploadIdBound: true, partSizeBytes: 5 * 1024 * 1024, parts: [] });
    const outcome = uploadMusicAsset(options);
    await expect(outcome).rejects.toBeInstanceOf(MusicUploadSessionUnusableError);
    await expect(outcome).rejects.toMatchObject({ name: 'MusicUploadSessionUnusableError', status, sessionId: 'session' });
    expect(getMock).not.toHaveBeenCalled();
    expect(delMock).not.toHaveBeenCalled();
    expect(fetchMock).not.toHaveBeenCalled();
  });
});

describe('music upload expired sessions', () => {
  beforeEach(() => { jest.clearAllMocks(); globalThis.fetch = fetchMock; });
  afterEach(() => { globalThis.fetch = originalFetch; });

  it('treats a replayed session past its expiresAt as unusable before touching storage', async () => {
    postMock.mockResolvedValueOnce({ id: 'session', status: 'uploading', expiresAt: '2000-01-01T00:00:00Z',
      providerUploadIdBound: true, partSizeBytes: 5 * 1024 * 1024, parts: [] });
    await expect(uploadMusicAsset(options)).rejects.toMatchObject({ name: 'MusicUploadSessionUnusableError', status: 'expired', sessionId: 'session' });
    expect(getMock).not.toHaveBeenCalled();
    expect(fetchMock).not.toHaveBeenCalled();
    expect(delMock).not.toHaveBeenCalled();
  });

  it('translates HTTP 410 from a resume operation into the unusable-session error', async () => {
    postMock.mockResolvedValueOnce({ id: 'session', status: 'uploading', expiresAt: '2999-01-01T00:00:00Z',
      providerUploadIdBound: true, partSizeBytes: 5 * 1024 * 1024, parts: [] });
    getMock.mockRejectedValueOnce(Object.assign(new Error('Gone'), { status: 410 }));
    await expect(uploadMusicAsset(options)).rejects.toMatchObject({ name: 'MusicUploadSessionUnusableError', status: 'expired', sessionId: 'session' });
    expect(getMock).toHaveBeenCalledWith('/music/uploads/session/parts/1', { signal: undefined });
    expect(delMock).not.toHaveBeenCalled();
    expect(putMock).not.toHaveBeenCalled();
  });

  it('keeps other resume failures retryable with the same session', async () => {
    postMock.mockResolvedValueOnce({ id: 'session', status: 'uploading', expiresAt: '2999-01-01T00:00:00Z',
      providerUploadIdBound: true, partSizeBytes: 5 * 1024 * 1024, parts: [] });
    getMock.mockRejectedValueOnce(Object.assign(new Error('Service unavailable'), { status: 503 }));
    await expect(uploadMusicAsset(options)).rejects.toMatchObject({ message: 'Service unavailable' });
    expect(delMock).not.toHaveBeenCalled();
  });
});

describe('music upload confirmation evidence', () => {
  const resumable = () => ({ id: 'session', status: 'uploading', expiresAt: '2999-01-01T00:00:00Z',
    providerUploadIdBound: true, partSizeBytes: 5 * 1024 * 1024, parts: [{ partNumber: 1, byteSize: 1 }] });
  const completion = () => ({ url: 'https://storage.test.invalid/complete', method: 'POST', contentType: 'application/xml', body: '<CompleteMultipartUpload/>' });
  beforeEach(() => { jest.clearAllMocks(); resetMusicUploadEvidenceForTests(); globalThis.fetch = fetchMock; });
  afterEach(() => { globalThis.fetch = originalFetch; resetMusicUploadEvidenceForTests(); });

  it('retries confirmation with the retained final ETag instead of completing the multipart upload again', async () => {
    postMock.mockResolvedValueOnce(resumable());
    getMock.mockResolvedValueOnce(completion());
    fetchMock.mockResolvedValueOnce(response('<CompleteMultipartUploadResult><ETag>"final"</ETag></CompleteMultipartUploadResult>', null));
    postMock.mockRejectedValueOnce(new Error('No se pudo conectar con el servicio.'));
    await expect(uploadMusicAsset(options)).rejects.toThrow('No se pudo conectar');
    expect(fetchMock).toHaveBeenCalledTimes(1);

    postMock.mockResolvedValueOnce(resumable());
    postMock.mockResolvedValueOnce({ status: 'completed' });
    await expect(uploadMusicAsset(options)).resolves.toEqual({ status: 'completed' });
    expect(getMock).toHaveBeenCalledTimes(1);
    expect(fetchMock).toHaveBeenCalledTimes(1);
    expect(postMock).toHaveBeenLastCalledWith('/music/uploads/session/confirm', { etag: '"final"' }, { signal: undefined });
  });

  it('treats a consumed multipart upload without local evidence as unusable so a new session is created', async () => {
    postMock.mockResolvedValueOnce(resumable());
    getMock.mockResolvedValueOnce(completion());
    fetchMock.mockResolvedValueOnce({ ok: false, status: 404, text: async () => '<Error><Code>NoSuchUpload</Code></Error>', headers: { get: () => null } } as unknown as Response);
    await expect(uploadMusicAsset(options)).rejects.toMatchObject({ name: 'MusicUploadSessionUnusableError', status: 'failed', sessionId: 'session' });
    expect(delMock).not.toHaveBeenCalled();
    expect(postMock).toHaveBeenCalledTimes(1);
  });
});
