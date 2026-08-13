import { get, post, put } from './client';
import type { PartyDTO, PartyCreate, PartyRelatedDTO, PartyUpdate } from './types';

const requirePositiveInteger = (value: number, field: string): number => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${field} debe ser un entero positivo.`);
  }
  return value;
};

const omitNullPartyUpdateFields = (body: PartyUpdate): PartyUpdate =>
  Object.fromEntries(
    Object.entries(body).filter(([, value]) => value !== null),
  ) as PartyUpdate;

export const Parties = {
  list: () => get<PartyDTO[]>('/parties'),
  create: (body: PartyCreate) => post<PartyDTO>('/parties', body),
  getOne: (id: number) => get<PartyDTO>(`/parties/${requirePositiveInteger(id, 'id')}`),
  update: (id: number, body: PartyUpdate) =>
    put<PartyDTO>(`/parties/${requirePositiveInteger(id, 'id')}`, omitNullPartyUpdateFields(body)),
  related: (id: number) => get<PartyRelatedDTO>(`/parties/${requirePositiveInteger(id, 'id')}/related`),
};
