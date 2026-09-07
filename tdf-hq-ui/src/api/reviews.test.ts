import { jest } from '@jest/globals';

const mockGet = jest.fn();
const mockPost = jest.fn();

jest.unstable_mockModule('./client', () => ({
  get: (...args: unknown[]) => mockGet(...args),
  post: (...args: unknown[]) => mockPost(...args),
}));

const { Reviews } = await import('./reviews');

describe('Reviews API', () => {
  beforeEach(() => jest.clearAllMocks());

  it('encodes public review targets and cursors', async () => {
    mockGet.mockResolvedValue({
      summary: { targetKind: 'marketplace_listing', targetId: 'listing / uno', average: null, count: 0 },
      items: [],
      nextCursor: null,
    });
    await Reviews.list('marketplace_listing', 'listing / uno', '11111111-1111-4111-8111-111111111111', 10);
    expect(mockGet).toHaveBeenCalledWith(
      '/reviews/marketplace_listing/listing%20%2F%20uno?limit=10&cursor=11111111-1111-4111-8111-111111111111',
    );
  });

  it('rejects malformed public review responses instead of passing them to the UI', async () => {
    mockGet.mockResolvedValue('<!doctype html>');

    await expect(Reviews.list('event', '42')).rejects.toThrow(
      'La respuesta de reseñas no tiene el formato esperado.',
    );
  });

  it('keeps eligibility evidence protected and sends an idempotency key on create', async () => {
    mockGet.mockResolvedValue([]);
    mockPost.mockResolvedValue({});
    await Reviews.eligibility('event', '42');
    expect(mockGet).toHaveBeenCalledWith('/reviews/eligibility?targetKind=event&targetId=42');

    const request = {
      targetKind: 'event' as const,
      targetId: '42',
      sourceKind: 'event_ticket_order' as const,
      sourceId: '7',
      rating: 5,
      body: 'Una experiencia excelente.',
    };
    await Reviews.create(request, 'review-create-1');
    expect(mockPost).toHaveBeenCalledWith('/reviews', request, {
      headers: { 'Idempotency-Key': 'review-create-1' },
    });
  });
});
