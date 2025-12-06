export interface ServiceType {
  id: string;
  name: string;
  price: number;
  currency: string;
  billingUnit?: string;
}

const STORAGE_KEY = 'tdf-service-types';

export const defaultServiceTypes: ServiceType[] = [
  { id: 'rec', name: 'Grabación', price: 50, currency: 'USD', billingUnit: 'hora' },
  { id: 'mix', name: 'Mezcla', price: 120, currency: 'USD', billingUnit: 'canción' },
  { id: 'master', name: 'Mastering', price: 70, currency: 'USD', billingUnit: 'canción' },
  { id: 'ensayo', name: 'Ensayo', price: 30, currency: 'USD', billingUnit: 'hora' },
  { id: 'podcast', name: 'Podcast', price: 80, currency: 'USD', billingUnit: 'episodio' },
];

export function loadServiceTypes(): ServiceType[] {
  if (typeof window === 'undefined') return defaultServiceTypes;
  const raw = window.localStorage.getItem(STORAGE_KEY);
  if (!raw) return defaultServiceTypes;
  try {
    const parsed = JSON.parse(raw) as ServiceType[];
    if (!Array.isArray(parsed)) return defaultServiceTypes;
    return parsed;
  } catch {
    return defaultServiceTypes;
  }
}

export function saveServiceTypes(list: ServiceType[]) {
  if (typeof window === 'undefined') return;
  window.localStorage.setItem(STORAGE_KEY, JSON.stringify(list));
}
