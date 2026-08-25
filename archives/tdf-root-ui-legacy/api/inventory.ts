import { del, get, post, patch } from './client';
import type { AssetCreate, AssetDTO, AssetUpdate, Page } from './types';

export const Inventory = {
  list: (params?: { q?: string; page?: number; pageSize?: number }) => {
    const search = new URLSearchParams();
    if (params?.q) search.set('q', params.q);
    if (params?.page) search.set('page', String(params.page));
    if (params?.pageSize) search.set('pageSize', String(params.pageSize));
    const suffix = search.toString() ? `?${search.toString()}` : '';
    return get<Page<AssetDTO>>(`/assets${suffix}`);
  },
  create: (body: AssetCreate) => post<AssetDTO>('/assets', body),
  detail: (id: string) => get<AssetDTO>(`/assets/${id}`),
  update: (id: string, body: AssetUpdate) => patch<AssetDTO>(`/assets/${id}`, body),
  remove: (id: string) => del<void>(`/assets/${id}`),
};
