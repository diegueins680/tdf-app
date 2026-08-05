import type {
  ServiceStorefrontPackageDTO,
  ServiceStorefrontOrderCreate,
  ServiceStorefrontOrderDTO,
} from './serviceStorefront';

describe('ServiceStorefront types', () => {
  it('should define valid package DTO shape', () => {
    const pkg: ServiceStorefrontPackageDTO = {
      sspId: 'test-id',
      sspServiceKind: 'Mixing',
      sspTier: 'Pro',
      sspName: 'Mezcla Profesional',
      sspDescription: 'Mezcla de hasta 24 pistas',
      sspPriceUsdCents: 15000,
      sspCurrency: 'USD',
      sspTurnaroundDays: 7,
      sspRevisionCount: 2,
      sspDeliverables: ['WAV 48kHz/24-bit', 'Stems'],
      sspFeatures: ['Hasta 24 pistas', '2 revisiones'],
      sspActive: true,
      sspSortOrder: 1,
    };
    expect(pkg.sspPriceUsdCents).toBe(15000);
    expect(pkg.sspServiceKind).toBe('Mixing');
    expect(pkg.sspTier).toBe('Pro');
  });

  it('should define valid order create shape', () => {
    const order: ServiceStorefrontOrderCreate = {
      ssocPackageId: 'pkg-123',
      ssocBuyerName: 'Juan Pérez',
      ssocBuyerEmail: 'juan@example.com',
      ssocBuyerPhone: '+593999999999',
      ssocArtistName: 'Los Testigos',
      ssocGenre: 'Rock',
      ssocSongCount: 3,
      ssocNotes: 'Necesitamos un sonido agresivo',
      ssocReferenceTrackUrl: 'https://youtube.com/watch?v=test',
    };
    expect(order.ssocBuyerEmail).toBe('juan@example.com');
    expect(order.ssocSongCount).toBe(3);
  });

  it('should allow optional fields in order create', () => {
    const minimalOrder: ServiceStorefrontOrderCreate = {
      ssocPackageId: 'pkg-123',
      ssocBuyerName: 'María',
      ssocBuyerEmail: 'maria@example.com',
    };
    expect(minimalOrder.ssocBuyerPhone).toBeUndefined();
    expect(minimalOrder.ssocArtistName).toBeUndefined();
  });

  it('should define valid order DTO shape', () => {
    const order: ServiceStorefrontOrderDTO = {
      ssoId: 'order-uuid',
      ssoOrderNumber: 'TDF-ABC123',
      ssoBuyerName: 'Juan Pérez',
      ssoBuyerEmail: 'juan@example.com',
      ssoPackageId: 'pkg-123',
      ssoServiceKind: 'Mixing',
      ssoTier: 'Pro',
      ssoPriceUsdCents: 15000,
      ssoCurrency: 'USD',
      ssoStatus: 'pending_payment',
      ssoSongCount: 1,
      ssoCreatedAt: '2026-08-04T12:00:00Z',
      ssoUpdatedAt: '2026-08-04T12:00:00Z',
    };
    expect(order.ssoStatus).toBe('pending_payment');
    expect(order.ssoPriceUsdCents).toBe(15000);
  });

  it('should cover all service kinds', () => {
    const kinds: ServiceStorefrontPackageDTO['sspServiceKind'][] = ['Mixing', 'Mastering', 'Bundle'];
    expect(kinds).toHaveLength(3);
  });

  it('should cover all tiers', () => {
    const tiers: ServiceStorefrontPackageDTO['sspTier'][] = ['Basic', 'Pro', 'Premium'];
    expect(tiers).toHaveLength(3);
  });

  it('should cover all order statuses', () => {
    const statuses: ServiceStorefrontOrderDTO['ssoStatus'][] = [
      'pending_payment',
      'paid',
      'in_progress',
      'v1_delivered',
      'revisions',
      'approved',
      'delivered',
      'completed',
      'payment_failed',
      'cancelled',
    ];
    expect(statuses.length).toBeGreaterThanOrEqual(8);
  });
});
