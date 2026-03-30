import { get, post } from './client';

export interface InvoiceLineDTO {
  lineId: number;
  description: string;
  quantity: number;
  unitCents: number;
  taxBps?: number | null;
  totalCents: number;
  serviceOrderId?: number | null;
  packagePurchaseId?: number | null;
}

export interface InvoiceDTO {
  invId: number;
  number?: string | null;
  statusI: string;
  subtotalC: number;
  taxC: number;
  totalC: number;
  currency: string;
  customerId?: number | null;
  sriDocumentId?: string | null;
  notes?: string | null;
  receiptId?: number | null;
  lineItems: InvoiceLineDTO[];
}

export interface GenerateSessionInvoiceLineInput {
  description: string;
  quantity: number;
  unitCents: number;
  taxBps?: number;
  sriCode?: string;
  sriAuxiliaryCode?: string;
  sriAdditionalInfo?: string;
  sriIvaCode?: string;
}

export interface GenerateSessionInvoiceInput {
  customerId?: number;
  currency?: string;
  number?: string;
  notes?: string;
  lineItems: GenerateSessionInvoiceLineInput[];
  generateReceipt?: boolean;
  issueSri?: boolean;
  certificatePassword?: string;
}

export interface SriIssueBuyerDTO {
  ruc: string;
  legalName: string;
  email?: string | null;
  phone?: string | null;
}

export interface SriIssueResultDTO {
  ok: boolean;
  status: string;
  targetId?: string | null;
  buyer?: SriIssueBuyerDTO | null;
  total?: number | null;
  authorizationNumber?: string | null;
  invoiceNumber?: string | null;
  buyerEmail?: string | null;
}

export interface SriIssueErrorDTO {
  ok: false;
  error: string;
}

export interface GenerateSessionInvoiceResponse {
  ok: boolean;
  sessionId: string;
  invoice: InvoiceDTO;
  sri?: SriIssueResultDTO | SriIssueErrorDTO | null;
}

const requirePositiveInteger = (value: number, field: string): number => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${field} debe ser un entero positivo.`);
  }
  return value;
};

const requireSessionId = (value: string): string => {
  const trimmed = value.trim();
  if (!trimmed) {
    throw new Error('sessionId es obligatorio.');
  }
  return trimmed;
};

export const Invoices = {
  listBySession: (sessionId: string) => get<InvoiceDTO[]>(`/invoices/by-session/${encodeURIComponent(requireSessionId(sessionId))}`),
  getOne: (invoiceId: number) => get<InvoiceDTO>(`/invoices/${requirePositiveInteger(invoiceId, 'invoiceId')}`),
  generateForSession: (sessionId: string, payload: GenerateSessionInvoiceInput) =>
    post<GenerateSessionInvoiceResponse>(`/invoices/${encodeURIComponent(requireSessionId(sessionId))}/generate`, payload),
};
