import {
  decodeLegacyServiceOfferingId,
  normalizePublicServiceCatalogResponse,
} from './services';

describe('public service catalog compatibility', () => {
  it('preserves the current versioned catalog envelope', () => {
    const response = {
      sceSchemaVersion: 1,
      sceRevision: 42,
      sceLocale: 'es',
      sceItems: [],
    };

    expect(normalizePublicServiceCatalogResponse(response)).toBe(response);
  });

  it('normalizes the deployed legacy array and keeps its booking service type recoverable', () => {
    const response = normalizePublicServiceCatalogResponse([
      {
        scId: 13,
        scName: 'Producción de eventos',
        scKind: 'EventProduction',
        scPricingModel: 'Quote',
        scRateCents: null,
        scCurrency: 'USD',
        scBillingUnit: null,
        scTaxBps: 1200,
        scActive: true,
      },
    ]);

    expect(response).toMatchObject({
      sceSchemaVersion: 0,
      sceRevision: 0,
      sceLocale: 'es',
    });
    expect(response.sceItems).toHaveLength(1);
    expect(response.sceItems[0]).toMatchObject({
      scCode: 'event-production',
      scName: 'Producción de eventos',
      scKind: 'EventProduction',
      scPricingModel: 'Quote',
      scCurrency: 'USD',
      scActive: true,
    });
    expect(decodeLegacyServiceOfferingId(response.sceItems[0].scId)).toBe('Producción de eventos');
  });

  it('rejects unrecognized responses instead of silently hiding every service', () => {
    expect(() => normalizePublicServiceCatalogResponse({ items: [] })).toThrow(
      'El catálogo público de servicios devolvió un formato no reconocido.',
    );
  });
});
