import { get, post } from './client';
import type { PaymentCreate, PaymentDTO } from './types';

const requirePositiveInteger = (value: number, field: string): number => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${field} debe ser un entero positivo.`);
  }
  return value;
};

export const Payments = {
  list: (partyId?: number) => {
    const query = partyId == null ? '' : `?partyId=${requirePositiveInteger(partyId, 'partyId')}`;
    return get<PaymentDTO[]>(`/payments${query}`);
  },
  create: (body: PaymentCreate) => post<PaymentDTO>('/payments', body),
  getOne: (id: number) => get<PaymentDTO>(`/payments/${id}`),
};
