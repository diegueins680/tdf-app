import { jest } from '@jest/globals';

const buildAuthorizationHeaderMock = jest.fn<() => string | undefined>(() => undefined);

jest.unstable_mockModule('./authHeader', () => ({
  buildAuthorizationHeader: buildAuthorizationHeaderMock,
}));

jest.unstable_mockModule('../config/apiBase', () => ({
  resolveApiBase: () => '',
}));

const { submitLiveSessionIntake } = await import('./liveSessions');

const successfulResponse = {
  ok: true,
  status: 204,
} as Response;

describe('Live Session canonical catalog contracts', () => {
  const fetchMock = jest.fn<typeof fetch>();

  beforeEach(() => {
    fetchMock.mockReset();
    buildAuthorizationHeaderMock.mockReset();
    buildAuthorizationHeaderMock.mockReturnValue(undefined);
    (globalThis as unknown as { fetch: typeof fetch }).fetch = fetchMock;
  });

  it('submits only persisted genre and instrument IDs without copied labels or roles', async () => {
    fetchMock.mockResolvedValueOnce(successfulResponse);

    await submitLiveSessionIntake({
      bandName: 'The House Band',
      primaryGenreId: '11111111-1111-4111-8111-111111111111',
      acceptedTerms: true,
      termsVersion: 'TDF Live Sessions v2',
      musicians: [{
        partyId: 42,
        name: 'Ana',
        instrumentId: '22222222-2222-4222-8222-222222222222',
        isExisting: true,
      }],
    });

    expect(fetchMock).toHaveBeenCalledTimes(1);
    const [url, request] = fetchMock.mock.calls[0] ?? [];
    expect(url).toBe('/live-sessions/intake');
    expect(request).toEqual(expect.objectContaining({ method: 'POST' }));

    const form = request?.body as FormData;
    expect(form.get('primaryGenreId')).toBe('11111111-1111-4111-8111-111111111111');
    expect(form.get('acceptedTerms')).toBe('true');
    expect(form.get('termsVersion')).toBe('TDF Live Sessions v2');
    expect(form.has('primaryGenre')).toBe(false);
    const musiciansPart = form.get('musicians');
    expect(typeof musiciansPart).toBe('string');
    if (typeof musiciansPart !== 'string') throw new Error('musicians must be a JSON string');
    expect(JSON.parse(musiciansPart)).toEqual([{
      partyId: 42,
      name: 'Ana',
      instrumentId: '22222222-2222-4222-8222-222222222222',
      isExisting: true,
    }]);
    expect(musiciansPart).not.toContain('"instrument"');
    expect(musiciansPart).not.toContain('"role"');
  });
});
