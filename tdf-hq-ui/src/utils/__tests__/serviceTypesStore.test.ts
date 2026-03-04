import { defaultServiceTypes, mapServiceCatalogDto, mergeServiceTypes } from '../serviceTypesStore';
import type { ServiceCatalogDTO } from '../../api/types';

describe('serviceTypesStore', () => {
  it('returns defaults when there is no API data', () => {
    expect(mergeServiceTypes()).toEqual(defaultServiceTypes);
    expect(mergeServiceTypes([])).toEqual(defaultServiceTypes);
  });

  it('returns cloned defaults to avoid accidental global mutation', () => {
    const first = mergeServiceTypes();
    expect(first).not.toBe(defaultServiceTypes);
    const firstItem = first[0];
    const defaultFirstItem = defaultServiceTypes[0];
    expect(firstItem).toBeDefined();
    expect(defaultFirstItem).toBeDefined();
    if (!firstItem || !defaultFirstItem) return;
    firstItem.name = 'Mutado accidentalmente';
    const second = mergeServiceTypes();
    expect(second[0]?.name).toBe(defaultFirstItem.name);
  });

  it('maps service catalog DTOs to service types', () => {
    const dto: ServiceCatalogDTO = {
      scId: 1,
      scName: 'Grabación Deluxe',
      scKind: 'Recording',
      scPricingModel: 'Hourly',
      scRateCents: 12500,
      scCurrency: 'USD',
      scBillingUnit: 'hora',
      scTaxBps: 1000,
      scActive: true,
    };
    const mapped = mapServiceCatalogDto(dto);
    expect(mapped).toMatchObject({
      id: '1',
      name: 'Grabación Deluxe',
      priceCents: 12500,
      currency: 'USD',
      billingUnit: 'hora',
      kind: 'Recording',
      pricingModel: 'Hourly',
      taxBps: 1000,
      active: true,
    });
  });

  it('filters inactive services unless includeInactive is true', () => {
    const items: ServiceCatalogDTO[] = [
      {
        scId: 1,
        scName: 'Activo',
        scKind: 'Recording',
        scPricingModel: 'Hourly',
        scRateCents: 1000,
        scCurrency: 'USD',
        scBillingUnit: 'hora',
        scTaxBps: 1200,
        scActive: true,
      },
      {
        scId: 2,
        scName: 'Inactivo',
        scKind: 'Mixing',
        scPricingModel: 'PerSong',
        scRateCents: 2000,
        scCurrency: 'USD',
        scBillingUnit: 'canción',
        scTaxBps: 1200,
        scActive: false,
      },
    ];
    const activeOnly = mergeServiceTypes(items);
    expect(activeOnly.some((svc) => svc.name === 'Inactivo')).toBe(false);

    const withInactive = mergeServiceTypes(items, { includeInactive: true });
    expect(withInactive.some((svc) => svc.name === 'Inactivo')).toBe(true);
  });

  it('returns an empty list when API data exists but every service is inactive', () => {
    const items: ServiceCatalogDTO[] = [
      {
        scId: 1,
        scName: 'Inactivo A',
        scKind: 'Recording',
        scPricingModel: 'Hourly',
        scRateCents: 1000,
        scCurrency: 'USD',
        scBillingUnit: 'hora',
        scTaxBps: 1200,
        scActive: false,
      },
      {
        scId: 2,
        scName: 'Inactivo B',
        scKind: 'Mixing',
        scPricingModel: 'PerSong',
        scRateCents: 2000,
        scCurrency: 'USD',
        scBillingUnit: 'canción',
        scTaxBps: 1200,
        scActive: false,
      },
    ];

    expect(mergeServiceTypes(items)).toEqual([]);
    expect(mergeServiceTypes(items, { includeInactive: true }).map((svc) => svc.name)).toEqual([
      'Inactivo A',
      'Inactivo B',
    ]);
  });
});
