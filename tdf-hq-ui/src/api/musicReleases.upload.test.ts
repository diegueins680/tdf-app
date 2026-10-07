import { jest } from '@jest/globals';

const postMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const getMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const putMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const delMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
jest.unstable_mockModule('./client', () => ({
  post: postMock, get: getMock, put: putMock, del: delMock,
}));
const { uploadMusicAsset } = await import('./musicReleases');
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
