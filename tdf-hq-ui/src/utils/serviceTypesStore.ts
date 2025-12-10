export interface ServiceType {
  id: string;
  name: string;
  price: number;
  currency: string;
  billingUnit?: string;
}

const STORAGE_KEY = 'tdf-service-types';

const normalize = (value: string) =>
  value
    .normalize('NFD')
    .replace(/\p{Diacritic}/gu, '')
    .toLowerCase()
    .trim();

const BAND_SERVICE: ServiceType = { id: 'band-rec', name: 'Grabación de Banda', price: 50, currency: 'USD', billingUnit: 'hora' };
const VOCAL_SERVICE: ServiceType = { id: 'vocal-rec', name: 'Grabación de Voz', price: 35, currency: 'USD', billingUnit: 'hora' };

export const defaultServiceTypes: ServiceType[] = [
  BAND_SERVICE,
  VOCAL_SERVICE,
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
    const filtered = parsed.filter((p) => normalize(p.name) !== 'grabacion');
    const hasBand = filtered.some((p) => normalize(p.name) === normalize(BAND_SERVICE.name));
    const hasVocal = filtered.some((p) => normalize(p.name) === normalize(VOCAL_SERVICE.name));
    const withBand = hasBand ? filtered : [BAND_SERVICE, ...filtered];
    const withVocal = hasVocal ? withBand : [VOCAL_SERVICE, ...withBand];
    return withVocal;
  } catch {
    return defaultServiceTypes;
  }
}

export function saveServiceTypes(list: ServiceType[]) {
  if (typeof window === 'undefined') return;
  window.localStorage.setItem(STORAGE_KEY, JSON.stringify(list));
}
