import { jest } from '@jest/globals';

const getMock = jest.fn<(path: string) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
}));

const { loadAvailableCheckoutMethods } = await import('./paymentCapabilities');

describe('canonical checkout capability client', () => {
  beforeEach(() => {
    getMock.mockReset();
  });

  it('requires compliant marketplace settlement capabilities and maps exact providers', async () => {
    getMock
      .mockResolvedValueOnce({ routes: [{ provider: 'datafast' }] })
      .mockResolvedValueOnce({ routes: [{ provider: 'paypal' }] })
      .mockResolvedValueOnce({ routes: [] });

    await expect(loadAvailableCheckoutMethods({
      currency: 'usd',
      amountMinor: 1250,
      productFlow: 'marketplace',
      marketplace: true,
    })).resolves.toEqual({
      datafast: true,
      paypal: true,
      bankTransfer: false,
    });

    expect(getMock).toHaveBeenCalledTimes(3);
    for (const [path] of getMock.mock.calls) {
      const query = new URLSearchParams(path.split('?')[1]);
      expect(query.get('buyerCountry')).toBe('ZZ');
      expect(query.get('currency')).toBe('USD');
      expect(query.get('amountMinor')).toBe('1250');
      expect(query.get('productFlow')).toBe('marketplace');
      expect(query.getAll('requires')).toEqual([
        'one_time',
        'connected_accounts',
        'split_settlement',
        'seller_payouts',
      ]);
    }
  });

  it('rejects the aggregate check when any route lookup cannot be verified', async () => {
    getMock.mockRejectedValueOnce(new Error('capability service unavailable'));
    getMock.mockResolvedValue({ routes: [] });

    await expect(loadAvailableCheckoutMethods({
      currency: 'USD',
      amountMinor: 500,
      productFlow: 'professional_service',
    })).rejects.toThrow('capability service unavailable');
  });
});
