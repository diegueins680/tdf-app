import { bookingBillingUnitLabel } from './bookingBillingUnitLabel';
import { mapServiceCatalogDto } from './serviceTypesStore';
import type { ServiceCatalogDTO } from '../api/types';

describe('bookingBillingUnitLabel', () => {
  it('translates seeded units for display without changing the catalog unit', () => {
    expect(bookingBillingUnitLabel('hour')).toBe('hora');
    expect(bookingBillingUnitLabel('episode')).toBe('episodio');
    expect(bookingBillingUnitLabel('song')).toBe('canción');
    expect(bookingBillingUnitLabel(' PACKAGE ')).toBe('paquete');
    const service = mapServiceCatalogDto({ scBillingUnit: 'hour' } as ServiceCatalogDTO);
    bookingBillingUnitLabel(service.billingUnit);
    expect(service.billingUnit).toBe('hour');
  });

  it('preserves unknown and already localized units and handles missing values', () => {
    expect(bookingBillingUnitLabel('por jornada extendida')).toBe('por jornada extendida');
    expect(bookingBillingUnitLabel('hora')).toBe('hora');
    expect(bookingBillingUnitLabel('toString')).toBe('toString');
    expect(bookingBillingUnitLabel(null)).toBe('');
    expect(bookingBillingUnitLabel(undefined)).toBe('');
    expect(bookingBillingUnitLabel('  ')).toBe('');
  });
});
