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

// Server caps each page at 500 and offsets at 10000; without explicit paging it
// silently returns only the first 200 contacts, hiding newer ones from the CRM.
const PARTY_PAGE_SIZE = 500;
const PARTY_MAX_OFFSET = 10000;

const listAllParties = async (): Promise<PartyDTO[]> => {
  const parties: PartyDTO[] = [];
  for (let offset = 0; offset <= PARTY_MAX_OFFSET; offset += PARTY_PAGE_SIZE) {
    const page = await get<PartyDTO[]>(`/parties?limit=${PARTY_PAGE_SIZE}&offset=${offset}`);
    parties.push(...page);
    if (page.length < PARTY_PAGE_SIZE) break;
  }
  return parties;
};

export const Parties = {
  list: () => listAllParties(),
  create: (body: PartyCreate, requestKey: string) => post<PartyDTO>('/parties', body,
    requestKey ? { headers: { 'Idempotency-Key': requestKey } } : {}),
  getOne: (id: number) => get<PartyDTO>(`/parties/${requirePositiveInteger(id, 'id')}`),
  update: (id: number, body: PartyUpdate) =>
    put<PartyDTO>(`/parties/${requirePositiveInteger(id, 'id')}`, omitNullPartyUpdateFields(body)),
  related: (id: number) => get<PartyRelatedDTO>(`/parties/${requirePositiveInteger(id, 'id')}/related`),
};
