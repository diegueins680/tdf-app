import { get } from './client';
import type { ServiceCatalogDTO } from './types';
import type { components } from './generated/types';

export type ServiceCatalogEnvelope = components['schemas']['ServiceCatalogEnvelope'];

interface LegacyServiceCatalogItem {
  scId: number;
  scName: string;
  scKind: string;
  scPricingModel: string;
  scRateCents?: number | null;
  scCurrency: string;
  scBillingUnit?: string | null;
  scTaxBps?: number | null;
  scActive: boolean;
}

const LEGACY_SERVICE_TYPE_PREFIX = 'legacy-service-type:';

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === 'object' && value !== null && !Array.isArray(value);

const isLegacyServiceCatalogItem = (value: unknown): value is LegacyServiceCatalogItem =>
  isRecord(value)
  && typeof value['scId'] === 'number'
  && typeof value['scName'] === 'string'
  && typeof value['scKind'] === 'string'
  && typeof value['scPricingModel'] === 'string'
  && typeof value['scCurrency'] === 'string'
  && typeof value['scActive'] === 'boolean';

const serviceCode = (name: string) => {
  const normalized = name
    .trim()
    .toLowerCase()
    .normalize('NFD')
    .replace(/\p{Diacritic}/gu, '')
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-|-$/g, '');
  return normalized === 'produccion-de-eventos' ? 'event-production' : `legacy-${normalized || 'service'}`;
};

export const encodeLegacyServiceOfferingId = (serviceType: string): string =>
  `${LEGACY_SERVICE_TYPE_PREFIX}${encodeURIComponent(serviceType)}`;

export const decodeLegacyServiceOfferingId = (serviceOfferingId: string): string | null => {
  if (!serviceOfferingId.startsWith(LEGACY_SERVICE_TYPE_PREFIX)) return null;
  try {
    return decodeURIComponent(serviceOfferingId.slice(LEGACY_SERVICE_TYPE_PREFIX.length)).trim() || null;
  } catch {
    return null;
  }
};

const normalizeLegacyService = (service: LegacyServiceCatalogItem, index: number): ServiceCatalogDTO => ({
  scId: encodeLegacyServiceOfferingId(service.scName),
  scCode: serviceCode(service.scName),
  scName: service.scName,
  scNameEs: service.scName,
  scNameEn: service.scName,
  scCategoryId: `legacy-category:${service.scKind}`,
  scKind: service.scKind,
  scPricingModel: service.scPricingModel,
  scPricingModelId: `legacy-pricing-model:${service.scPricingModel}`,
  scRateCents: service.scRateCents ?? null,
  scCurrency: service.scCurrency,
  scCurrencyId: `legacy-currency:${service.scCurrency}`,
  scBillingUnit: service.scBillingUnit ?? null,
  scTaxRateCode: service.scTaxBps == null ? null : `legacy-tax-bps:${service.scTaxBps}`,
  scTaxRateId: null,
  scDefaultDurationMinutes: null,
  scRequiresEngineer: false,
  scDefaultResources: [],
  scSortOrder: index,
  scActive: service.scActive,
});

export const normalizePublicServiceCatalogResponse = (
  response: unknown,
  locale = 'es',
): ServiceCatalogEnvelope => {
  if (isRecord(response) && Array.isArray(response['sceItems'])) {
    return response as ServiceCatalogEnvelope;
  }
  if (Array.isArray(response)) {
    return {
      sceSchemaVersion: 0,
      sceRevision: 0,
      sceLocale: locale,
      sceItems: response.filter(isLegacyServiceCatalogItem).map(normalizeLegacyService),
    };
  }
  throw new Error('El catálogo público de servicios devolvió un formato no reconocido.');
};

const localeQuery = (locale?: string): string => locale ? `locale=${encodeURIComponent(locale)}` : '';

export const Services = {
  getPublicEnvelope: (locale?: string) =>
    get<unknown>(`/services/catalog/public${locale ? `?${localeQuery(locale)}` : ''}`)
      .then((response) => normalizePublicServiceCatalogResponse(response, locale)),
  listPublic: (locale?: string): Promise<ServiceCatalogDTO[]> =>
    Services.getPublicEnvelope(locale).then((envelope) => envelope.sceItems),
  getEnvelope: (includeInactive = false, locale?: string) => {
    const params = new URLSearchParams();
    if (includeInactive) params.set('includeInactive', 'true');
    if (locale) params.set('locale', locale);
    const query = params.toString();
    return get<ServiceCatalogEnvelope>(`/services/catalog${query ? `?${query}` : ''}`);
  },
  list: (includeInactive?: boolean, locale?: string): Promise<ServiceCatalogDTO[]> =>
    Services.getEnvelope(includeInactive, locale).then((envelope) => envelope.sceItems),
};
