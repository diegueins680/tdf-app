import { get } from './client';
import type { ServiceCatalogDTO } from './types';
import type { components } from './generated/types';

export type ServiceCatalogEnvelope = components['schemas']['ServiceCatalogEnvelope'];

const localeQuery = (locale?: string): string => locale ? `locale=${encodeURIComponent(locale)}` : '';

export const Services = {
  getPublicEnvelope: (locale?: string) =>
    get<ServiceCatalogEnvelope>(`/services/catalog/public${locale ? `?${localeQuery(locale)}` : ''}`),
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
