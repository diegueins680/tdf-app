import { jest } from '@jest/globals';

const getMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const putMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({ get: getMock, put: putMock }));

const { Reputation } = await import('./reputation');

describe('Reputation API', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('gets only the authenticated viewer preference context', async () => {
    getMock.mockResolvedValueOnce({ contextKind: 'general', categories: [] });

    await Reputation.getMyPreferences('service booking');

    expect(getMock).toHaveBeenCalledWith('/reputation/preferences?contextKind=service%20booking');
  });

  it('saves private preferences with an idempotency key', async () => {
    const payload = {
      contextKind: 'general', expectedRevision: 0, activate: false,
      categories: [],
    };
    putMock.mockResolvedValueOnce({ contextKind: 'general', categories: [] });

    await Reputation.saveMyPreferences(payload, 'preference-write-123');

    expect(putMock).toHaveBeenCalledWith(
      '/reputation/preferences',
      payload,
      { headers: { 'Idempotency-Key': 'preference-write-123' } },
    );
  });
});
