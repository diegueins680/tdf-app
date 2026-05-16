import { get, post, patch } from './client';
import type { Page, SessionCreate, SessionDTO, SessionOptionsDTO, SessionUpdate } from './types';

export const Sessions = {
  list: (params?: { page?: number; pageSize?: number }) => {
    const search = new URLSearchParams();
    if (params?.page) search.set('page', String(params.page));
    if (params?.pageSize) search.set('pageSize', String(params.pageSize));
    const suffix = search.toString() ? `?${search.toString()}` : '';
    return get<Page<SessionDTO>>(`/sessions${suffix}`);
  },
  create: (body: SessionCreate) => post<SessionDTO>('/sessions', body),
  detail: (id: string) => get<SessionDTO>(`/sessions/${id}`),
  update: (id: string, body: SessionUpdate) => patch<SessionDTO>(`/sessions/${id}`, body),
  options: () => get<SessionOptionsDTO>('/sessions/options'),
};
