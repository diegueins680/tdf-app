import { get, post } from './client';
import type { CreateInvoiceReq, InvoiceDTO } from './types';

export const Invoices = {
  list: () => get<InvoiceDTO[]>('/invoices'),
  listByParty: async (partyId: number) => {
    const params = new URLSearchParams({ customerId: String(partyId) });
    try {
      return await get<InvoiceDTO[]>(`/invoices?${params.toString()}`);
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error ?? '');
      if (message.includes('404')) {
        return [];
      }
      throw error;
    }
  },
  create: (body: CreateInvoiceReq) => post<InvoiceDTO>('/invoices', body),
};
