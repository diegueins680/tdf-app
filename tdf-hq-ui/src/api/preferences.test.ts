import { jest } from '@jest/globals';

const getMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const postMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const putMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
  post: postMock,
  put: putMock,
}));

const { Preferences } = await import('./preferences');

describe('Locale preference canonical regional-reference contract', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('writes persisted locale, currency, and country UUIDs with no copied codes', async () => {
    putMock.mockResolvedValueOnce({});
    const payload = {
      localeId: '11111111-1111-4111-8111-111111111111',
      currencyId: '33333333-3333-4333-8333-333333333333',
      timezone: 'America/Guayaquil',
      countryId: '22222222-2222-4222-8222-222222222222',
    };

    await Preferences.update(payload);

    expect(putMock).toHaveBeenCalledWith('/session/preferences', payload);
    expect(putMock.mock.calls[0]?.[1]).not.toHaveProperty('locale');
    expect(putMock.mock.calls[0]?.[1]).not.toHaveProperty('currency');
    expect(putMock.mock.calls[0]?.[1]).not.toHaveProperty('countryCode');
  });

  it('uses a null UUID as the explicit country clear operation', async () => {
    putMock.mockResolvedValueOnce({});

    await Preferences.update({
      localeId: '11111111-1111-4111-8111-111111111111',
      currencyId: '33333333-3333-4333-8333-333333333333',
      timezone: 'UTC',
      countryId: null,
    });

    expect(putMock.mock.calls[0]?.[1]).toMatchObject({ countryId: null });
  });
});
