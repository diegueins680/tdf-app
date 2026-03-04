import { get, post } from './client';
import type { PaymentCreate, PaymentDTO } from './types';

export const Payments = {
  list: (partyId?: number) => {
    const query = partyId ? `?partyId=${partyId}` : '';
    return get<PaymentDTO[]>(`/payments${query}`);
  },
  create: (body: PaymentCreate) => post<PaymentDTO>('/payments', body),
  getOne: (id: number) => get<PaymentDTO>(`/payments/${id}`),
};
