import { get, post } from './client';
import type { PaymentCreate, PaymentDTO } from './types';

export const Payments = {
  list: async (partyId?: number) => {
    const query = partyId ? `?partyId=${partyId}` : '';
    try {
      return await get<PaymentDTO[]>(`/payments${query}`);
    } catch (error) {
      const msg = (error as Error).message.toLowerCase();
      if (msg.includes('404') || msg.includes('could not parse') || msg.includes('not found')) {
        return get<PaymentDTO[]>(`/${query.startsWith('?') ? query.slice(1) : query}`);
      }
      throw error;
    }
  },
  create: async (body: PaymentCreate) => {
    try {
      return await post<PaymentDTO>('/payments', body);
    } catch (error) {
      const msg = (error as Error).message.toLowerCase();
      if (msg.includes('404') || msg.includes('could not parse') || msg.includes('not found')) {
        return post<PaymentDTO>('/', body);
      }
      throw error;
    }
  },
  getOne: async (id: number) => {
    try {
      return await get<PaymentDTO>(`/payments/${id}`);
    } catch (error) {
      const msg = (error as Error).message.toLowerCase();
      if (msg.includes('404') || msg.includes('could not parse') || msg.includes('not found')) {
        return get<PaymentDTO>(`/${id}`);
      }
      throw error;
    }
  },
};
