import { jest } from '@jest/globals';

const getMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
}));

const { Records } = await import('./records');

describe('Records API', () => {
  beforeEach(() => {
    getMock.mockReset();
  });

  it('loads the typed public Records feed for the requested locale', async () => {
    const feed = {
      locale: 'es',
      revision: 243,
      collections: [],
      releases: [],
      recordings: [],
      sessions: [],
    };
    getMock.mockResolvedValueOnce(feed);

    await expect(Records.getFeed('es')).resolves.toEqual(feed);
    expect(getMock).toHaveBeenCalledWith('/records/feed?locale=es');
  });

  it('encodes a locale before placing it in the query string', async () => {
    getMock.mockResolvedValueOnce({});

    await Records.getFeed('es EC');

    expect(getMock).toHaveBeenCalledWith('/records/feed?locale=es%20EC');
  });

  it('forwards the original abort signal without replacing it', async () => {
    const controller = new AbortController();
    getMock.mockResolvedValueOnce({});

    await Records.getFeed('en', controller.signal);

    expect(getMock).toHaveBeenCalledWith('/records/feed?locale=en', { signal: controller.signal });
    const options = getMock.mock.calls[0]?.[1] as RequestInit;
    expect(options.signal).toBe(controller.signal);
    expect(options.signal?.aborted).toBe(false);
    controller.abort();
    expect(options.signal?.aborted).toBe(true);
  });
});
