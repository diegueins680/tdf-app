import { defaultServiceTypes, loadServiceTypes, saveServiceTypes, type ServiceType } from '../serviceTypesStore';

describe('serviceTypesStore', () => {
  beforeEach(() => {
    localStorage.clear();
  });

  it('returns defaults when storage is empty', () => {
    const result = loadServiceTypes();
    expect(result).toEqual(defaultServiceTypes);
  });

  it('returns defaults when storage has invalid JSON', () => {
    localStorage.setItem('tdf-service-types', '{not json');
    const result = loadServiceTypes();
    expect(result).toEqual(defaultServiceTypes);
  });

  it('returns stored values when JSON is valid', () => {
    const custom: ServiceType[] = [
      { id: 'custom', name: 'Custom', price: 10, currency: 'USD', billingUnit: 'unit' },
    ];
    localStorage.setItem('tdf-service-types', JSON.stringify(custom));

    const result = loadServiceTypes();
    expect(result).toEqual(custom);
  });

  it('saves service types to localStorage', () => {
    const custom: ServiceType[] = [
      { id: 'save', name: 'Save Me', price: 20, currency: 'USD' },
    ];
    saveServiceTypes(custom);
    const stored = localStorage.getItem('tdf-service-types');
    expect(stored).toBe(JSON.stringify(custom));
  });

  it('is SSR-safe and falls back to defaults when window is undefined', () => {
    const realWindow = (globalThis as { window?: unknown }).window;
    (globalThis as { window?: unknown }).window = undefined;
    try {
      const result = loadServiceTypes();
      expect(result).toEqual(defaultServiceTypes);
    } finally {
      (globalThis as { window?: unknown }).window = realWindow;
    }
  });
});
