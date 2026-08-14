import type { ServiceCatalogDTO } from '../api/types';

export interface ServiceType {
  id: string;
  code: string;
  name: string;
  categoryId: string;
  priceCents: number | null;
  currency: string;
  billingUnit?: string | null;
  kind?: string;
  pricingModel?: string;
  pricingModelId: string;
  taxBps?: number | null;
  taxRateCode?: string | null;
  taxRateId?: string | null;
  currencyId: string;
  defaultDurationMinutes?: number | null;
  requiresEngineer: boolean;
  defaultResources: ServiceCatalogDTO['scDefaultResources'];
  sortOrder: number;
  active: boolean;
}

export const mapServiceCatalogDto = (dto: ServiceCatalogDTO): ServiceType => ({
  id: String(dto.scId),
  code: dto.scCode,
  name: dto.scName,
  categoryId: dto.scCategoryId,
  priceCents: dto.scRateCents ?? null,
  currency: dto.scCurrency,
  billingUnit: dto.scBillingUnit ?? null,
  kind: dto.scKind,
  pricingModel: dto.scPricingModel,
  pricingModelId: dto.scPricingModelId,
  taxBps: null,
  taxRateCode: dto.scTaxRateCode ?? null,
  taxRateId: dto.scTaxRateId ?? null,
  currencyId: dto.scCurrencyId,
  defaultDurationMinutes: dto.scDefaultDurationMinutes ?? null,
  requiresEngineer: dto.scRequiresEngineer,
  defaultResources: dto.scDefaultResources,
  sortOrder: dto.scSortOrder,
  active: dto.scActive,
});

export const mergeServiceTypes = (
  items?: ServiceCatalogDTO[] | null,
  opts: { includeInactive?: boolean; sort?: boolean } = {},
): ServiceType[] => {
  if (!items || items.length === 0) return [];
  const filtered = opts.includeInactive ? items : items.filter((svc) => svc.scActive);
  if (filtered.length === 0) return [];
  const mapped = filtered.map(mapServiceCatalogDto);
  if (opts.sort === false) return mapped;
  return mapped.sort((a, b) => a.sortOrder - b.sortOrder || a.name.localeCompare(b.name));
};
