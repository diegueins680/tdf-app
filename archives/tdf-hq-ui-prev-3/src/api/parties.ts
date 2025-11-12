import { get, post, put } from './client';
import type { PartyDTO, PartyCreate, PartyUpdate } from './types';

export const Parties = {
  list: () => get<PartyDTO[]>('/parties'),
  create: (body: PartyCreate) => post<PartyDTO>('/parties', body),
  getOne: (id: number) => get<PartyDTO>(`/parties/${id}`),
  update: (id: number, body: PartyUpdate) => put<PartyDTO>(`/parties/${id}`, body),
  addRole: (id: number, role: string) =>
    post<void>(`/parties/${id}/roles`, role),
};
