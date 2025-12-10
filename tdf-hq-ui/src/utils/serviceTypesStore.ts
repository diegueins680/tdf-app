import type { ServiceCatalogDTO } from '../api/types';

export interface ServiceType {
  id: string;
  name: string;
  priceCents: number | null;
  currency: string;
  billingUnit?: string | null;
  kind?: string;
  pricingModel?: string;
  taxBps?: number | null;
  active: boolean;
}

export const defaultServiceTypes: ServiceType[] = [
  {
    id: 'band-rec',
    name: 'Grabación de Banda',
    priceCents: 50 * 100,
    currency: 'USD',
    billingUnit: 'hora',
    kind: 'Recording',
    pricingModel: 'Hourly',
    taxBps: 1200,
    active: true,
  },
  {
    id: 'vocal-rec',
    name: 'Grabación de Voz',
    priceCents: 35 * 100,
    currency: 'USD',
    billingUnit: 'hora',
    kind: 'Recording',
    pricingModel: 'Hourly',
    taxBps: 1200,
    active: true,
  },
  {
    id: 'mix',
    name: 'Mezcla',
    priceCents: 120 * 100,
    currency: 'USD',
    billingUnit: 'canción',
    kind: 'Mixing',
    pricingModel: 'PerSong',
    taxBps: 1200,
    active: true,
  },
  {
    id: 'master',
    name: 'Mastering',
    priceCents: 70 * 100,
    currency: 'USD',
    billingUnit: 'canción',
    kind: 'Mastering',
    pricingModel: 'PerSong',
    taxBps: 1200,
    active: true,
  },
  {
    id: 'ensayo',
    name: 'Ensayo',
    priceCents: 30 * 100,
    currency: 'USD',
    billingUnit: 'hora',
    kind: 'Rehearsal',
    pricingModel: 'Hourly',
    taxBps: 1200,
    active: true,
  },
  {
    id: 'podcast',
    name: 'Podcast',
    priceCents: 80 * 100,
    currency: 'USD',
    billingUnit: 'episodio',
    kind: 'EventProduction',
    pricingModel: 'PerSong',
    taxBps: 1200,
    active: true,
  },
];

export const mapServiceCatalogDto = (dto: ServiceCatalogDTO): ServiceType => ({
  id: String(dto.scId),
  name: dto.scName,
  priceCents: dto.scRateCents ?? null,
  currency: dto.scCurrency,
  billingUnit: dto.scBillingUnit ?? null,
  kind: dto.scKind,
  pricingModel: dto.scPricingModel,
  taxBps: dto.scTaxBps ?? null,
  active: dto.scActive,
});

export const mergeServiceTypes = (
  items?: ServiceCatalogDTO[] | null,
  opts: { includeInactive?: boolean; sort?: boolean } = {},
): ServiceType[] => {
  if (!items || items.length === 0) return defaultServiceTypes;
  const filtered = opts.includeInactive ? items : items.filter((svc) => svc.scActive);
  if (filtered.length === 0) return defaultServiceTypes;
  const mapped = filtered.map(mapServiceCatalogDto);
  if (opts.sort === false) return mapped;
  return mapped.sort((a, b) => a.name.localeCompare(b.name));
};
