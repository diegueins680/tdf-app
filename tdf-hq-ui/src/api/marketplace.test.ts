import { jest } from '@jest/globals';

const getMock = jest.fn<(path: string) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
  post: jest.fn(),
  put: jest.fn(),
}));

const { Marketplace } = await import('./marketplace');

describe('Marketplace.listOrders', () => {
  beforeEach(() => {
    getMock.mockReset();
  });

  it('keeps offset=0 when building query params', async () => {
    getMock.mockResolvedValueOnce([]);

    await Marketplace.listOrders({ status: 'pending', limit: 25, offset: 0 });

    expect(getMock).toHaveBeenCalledWith('/marketplace/orders?status=pending&limit=25&offset=0');
  });

  it('drops invalid numeric filters', async () => {
    getMock.mockResolvedValueOnce([]);

    await Marketplace.listOrders({ limit: 0, offset: -2 });

    expect(getMock).toHaveBeenCalledWith('/marketplace/orders');
  });
});
