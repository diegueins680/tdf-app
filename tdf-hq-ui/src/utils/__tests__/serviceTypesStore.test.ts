import { mapServiceCatalogDto, mergeServiceTypes } from '../serviceTypesStore';
import type { ServiceCatalogDTO } from '../../api/types';

const serviceDto = (
  overrides: Partial<ServiceCatalogDTO> = {},
): ServiceCatalogDTO => ({
  scId: '11111111-1111-4111-8111-111111111111',
  scCode: 'studio-recording',
  scName: 'Grabación de estudio',
  scNameEs: 'Grabación de estudio',
  scNameEn: 'Studio recording',
  scCategoryId: '22222222-2222-4222-8222-222222222222',
  scKind: 'recording',
  scPricingModelId: '33333333-3333-4333-8333-333333333333',
  scPricingModel: 'hourly',
  scRateCents: 12500,
  scCurrency: 'USD',
  scCurrencyId: '44444444-4444-4444-8444-444444444444',
  scBillingUnit: 'hour',
  scTaxRateCode: 'ec-iva-standard',
  scDefaultDurationMinutes: 120,
  scRequiresEngineer: true,
  scDefaultResources: [
    {
      sdrResourceId: '12',
      sdrResourceName: 'Control Room',
      sdrSelectionModeId: '55555555-5555-4555-8555-555555555555',
      sdrSelectionMode: 'all',
      sdrSortOrder: 10,
    },
  ],
  scSortOrder: 20,
  scActive: true,
  ...overrides,
});

describe('serviceTypesStore', () => {
  it('does not invent fallback services when the canonical API has no data', () => {
    expect(mergeServiceTypes()).toEqual([]);
    expect(mergeServiceTypes([])).toEqual([]);
  });

  it('maps canonical service metadata without inferring behavior from its name', () => {
    const mapped = mapServiceCatalogDto(serviceDto());
    expect(mapped).toMatchObject({
      id: '11111111-1111-4111-8111-111111111111',
      code: 'studio-recording',
      name: 'Grabación de estudio',
      priceCents: 12500,
      currency: 'USD',
      billingUnit: 'hour',
      kind: 'recording',
      pricingModel: 'hourly',
      taxRateCode: 'ec-iva-standard',
      defaultDurationMinutes: 120,
      requiresEngineer: true,
      sortOrder: 20,
      active: true,
    });
    expect(mapped.defaultResources).toHaveLength(1);
  });

  it('filters inactive services unless historical administration explicitly includes them', () => {
    const items = [
      serviceDto({ scName: 'Activo', scSortOrder: 20 }),
      serviceDto({
        scId: '33333333-3333-4333-8333-333333333333',
        scCode: 'retired-service',
        scName: 'Inactivo',
        scSortOrder: 10,
        scActive: false,
      }),
    ];

    expect(mergeServiceTypes(items).map((service) => service.name)).toEqual(['Activo']);
    expect(mergeServiceTypes(items, { includeInactive: true }).map((service) => service.name)).toEqual([
      'Inactivo',
      'Activo',
    ]);
  });

  it('uses persisted order with the localized name as the deterministic tie breaker', () => {
    const items = [
      serviceDto({ scName: 'Zeta', scSortOrder: 10 }),
      serviceDto({
        scId: '44444444-4444-4444-8444-444444444444',
        scCode: 'alpha-service',
        scName: 'Alfa',
        scSortOrder: 10,
      }),
    ];

    expect(mergeServiceTypes(items).map((service) => service.name)).toEqual(['Alfa', 'Zeta']);
    expect(mergeServiceTypes(items, { sort: false }).map((service) => service.name)).toEqual(['Zeta', 'Alfa']);
  });
});
