import { get, post } from './client';
import type { BandCreate, BandDTO, Page } from './types';

export const Bands = {
  list: (params?: { page?: number; pageSize?: number }) => {
    const search = new URLSearchParams();
    if (params?.page) search.set('page', String(params.page));
    if (params?.pageSize) search.set('pageSize', String(params.pageSize));
    const suffix = search.toString() ? `?${search.toString()}` : '';
    return get<Page<BandDTO>>(`/bands${suffix}`);
  },
  create: (body: BandCreate) => post<BandDTO>('/bands', body),
  detail: (id: string) => get<BandDTO>(`/bands/${id}`),
};
