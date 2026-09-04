import { jest } from '@jest/globals';

const getMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({ get: getMock }));

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
});
