import { jest } from '@jest/globals';

const getMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const postMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const delMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
  post: postMock,
  del: delMock,
}));

const { RadioAPI } = await import('./radio');

describe('Radio API canonical catalog contracts', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('loads typed auto-stop policy from the persisted Radio catalog', async () => {
    getMock.mockResolvedValueOnce([]);

    await RadioAPI.listAutoStopOptions('es');

    expect(getMock).toHaveBeenCalledWith('/radio/auto-stop-options?locale=es');
  });

  it('filters streams by the persisted genre UUID query parameter', async () => {
    getMock.mockResolvedValueOnce([]);

    await RadioAPI.search({
      countryId: '22222222-2222-4222-8222-222222222222',
      genreId: '11111111-1111-4111-8111-111111111111',
    });

    expect(getMock).toHaveBeenCalledWith(
      '/radio/streams?countryId=22222222-2222-4222-8222-222222222222&genreId=11111111-1111-4111-8111-111111111111',
    );
  });

  it('sends only the canonical genre UUID when creating a transmission', async () => {
    postMock.mockResolvedValueOnce({});

    await RadioAPI.createTransmission({
      name: 'TDF Live',
      genreId: '11111111-1111-4111-8111-111111111111',
      countryId: '22222222-2222-4222-8222-222222222222',
    });

    expect(postMock).toHaveBeenCalledWith('/radio/transmissions', {
      rtrName: 'TDF Live',
      rtrGenreId: '11111111-1111-4111-8111-111111111111',
      rtrCountryId: '22222222-2222-4222-8222-222222222222',
    });
    expect(postMock.mock.calls[0]?.[1]).not.toHaveProperty('rtrGenre');
    expect(postMock.mock.calls[0]?.[1]).not.toHaveProperty('rtrCountry');
  });

  it('passes canonical stream upserts without adding copied genre labels', async () => {
    postMock.mockResolvedValueOnce({});
    const payload = {
      rsuStreamUrl: 'https://radio.example.com/live',
      rsuGenreId: '11111111-1111-4111-8111-111111111111',
    };

    await RadioAPI.upsertActive(payload);

    expect(postMock).toHaveBeenCalledWith('/radio/streams/active', payload);
    expect(postMock.mock.calls[0]?.[1]).not.toHaveProperty('rsuGenre');
  });

  it('uses an explicit command when removing an existing genre relation', async () => {
    postMock.mockResolvedValueOnce({});
    const payload = {
      rsuStreamUrl: 'https://radio.example.com/live',
      rsuClearGenre: true,
    };

    await RadioAPI.upsertActive(payload);

    expect(postMock).toHaveBeenCalledWith('/radio/streams/active', payload);
    expect(postMock.mock.calls[0]?.[1]).not.toHaveProperty('rsuGenreId');
    expect(postMock.mock.calls[0]?.[1]).not.toHaveProperty('rsuGenre');
  });

  it('uses an explicit command when removing an existing country relation', async () => {
    postMock.mockResolvedValueOnce({});
    const payload = {
      rsuStreamUrl: 'https://radio.example.com/live',
      rsuClearCountry: true,
    };

    await RadioAPI.upsertActive(payload);

    expect(postMock).toHaveBeenCalledWith('/radio/streams/active', payload);
    expect(postMock.mock.calls[0]?.[1]).not.toHaveProperty('rsuCountryId');
    expect(postMock.mock.calls[0]?.[1]).not.toHaveProperty('rsuCountry');
  });
});
