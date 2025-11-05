import { get, post } from './client';
import type { InvoiceDTO, InvoiceCreate, PaymentDTO, PaymentCreate } from './types';

export const Invoices = {
  // Invoice management
  list: (partyId?: number) => 
    get<InvoiceDTO[]>(partyId ? `/invoices?partyId=${partyId}` : '/invoices'),
  create: (body: InvoiceCreate) => post<InvoiceDTO>('/invoices', body),
  getOne: (id: number) => get<InvoiceDTO>(`/invoices/${id}`),
  generatePDF: (id: number) => get<Blob>(`/invoices/${id}/pdf`),
  
  // Payment management
  listPayments: (invoiceId?: number) => 
    get<PaymentDTO[]>(invoiceId ? `/payments?invoiceId=${invoiceId}` : '/payments'),
  createPayment: (body: PaymentCreate) => post<PaymentDTO>('/payments', body),
  getPayment: (id: number) => get<PaymentDTO>(`/payments/${id}`),
};
