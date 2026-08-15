import { jest } from '@jest/globals';

const mockGet = jest.fn();
const mockPost = jest.fn();
const mockPut = jest.fn();
const mockPatch = jest.fn();
const mockDelete = jest.fn();

jest.unstable_mockModule('./client', () => ({
  get: (...args: unknown[]) => mockGet(...args),
  post: (...args: unknown[]) => mockPost(...args),
  put: (...args: unknown[]) => mockPut(...args),
  patch: (...args: unknown[]) => mockPatch(...args),
  del: (...args: unknown[]) => mockDelete(...args),
}));

const { Directory } = await import('./directory');

describe('web music directory canonical API', () => {
  beforeEach(() => jest.clearAllMocks());

  it('encodes anonymous universal-search and public-detail requests', async () => {
    mockGet.mockResolvedValue({});
    await Directory.search({ q: 'bajista & productor', cityId: '11111111-1111-4111-8111-111111111111', remote: true });
    expect(mockGet).toHaveBeenCalledWith('/directory/search?q=bajista+%26+productor&cityId=11111111-1111-4111-8111-111111111111&remote=true');

    await Directory.classified('audición/quito');
    expect(mockGet).toHaveBeenLastCalledWith('/directory/classifieds/audici%C3%B3n%2Fquito');
  });

  it('keeps retry keys stable for applications and saved searches', async () => {
    mockPost.mockResolvedValue({});
    await Directory.apply('22222222-2222-4222-8222-222222222222', {
      applicantProfileId: '33333333-3333-4333-8333-333333333333',
      message: 'Tengo experiencia y disponibilidad para esta oportunidad.',
      portfolio: [],
    }, 'web-application-retry-1');
    expect(mockPost).toHaveBeenCalledWith(
      '/directory/classifieds/22222222-2222-4222-8222-222222222222/applications',
      expect.any(Object),
      { headers: { 'Idempotency-Key': 'web-application-retry-1' } },
    );

    await Directory.saveSearch({ name: 'Productores en Quito', canonicalQuery: { q: 'productor' }, alertsEnabled: true, alertFrequency: 'instant' }, 'web-saved-search-1');
    expect(mockPost).toHaveBeenLastCalledWith('/directory/saved-searches', expect.any(Object), { headers: { 'Idempotency-Key': 'web-saved-search-1' } });
  });

  it('exposes the explicit non-destructive admin merge contract', async () => {
    mockPost.mockResolvedValue({});
    const request = {
      sourceProfileId: '44444444-4444-4444-8444-444444444444',
      targetProfileId: '55555555-5555-4555-8555-555555555555',
      reason: 'Duplicate confirmed by an authorized reviewer.',
    };
    await Directory.mergeProfiles(request, 'profile-merge-retry-1');
    expect(mockPost).toHaveBeenCalledWith('/directory/admin/merges', request, { headers: { 'Idempotency-Key': 'profile-merge-retry-1' } });
  });
});
