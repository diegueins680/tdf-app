import { del, get, patch, post } from './client';
import type { ServiceCatalogCreate, ServiceCatalogDTO, ServiceCatalogUpdate } from './types';

export const Services = {
  listPublic: () => get<ServiceCatalogDTO[]>('/services/catalog/public'),
  list: (includeInactive?: boolean) =>
    get<ServiceCatalogDTO[]>(`/services/catalog${includeInactive ? '?includeInactive=true' : ''}`),
  create: (body: ServiceCatalogCreate) => post<ServiceCatalogDTO>('/services/catalog', body),
  update: (serviceId: number | string, body: ServiceCatalogUpdate) =>
    patch<ServiceCatalogDTO>(`/services/catalog/${serviceId}`, body),
  remove: (serviceId: number | string) => del<void>(`/services/catalog/${serviceId}`),
};
