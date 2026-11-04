import { get, post } from './client';

import type { components } from './generated/types';

export type InvoiceLineDTO = components['schemas']['InvoiceLineDTO'];
export type InvoiceDTO = components['schemas']['InvoiceDTO'];

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
  /** Required when issueSri is not false: a retry with the same key and request replays instead of issuing again. */
  idempotencyKey?: string;
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

export interface InvoiceIssueAttempt {
  fingerprint: string;
  key: string;
}

/**
 * PAY-INVOICE-ISSUE-001: keep the idempotency key while the request is
 * unchanged, so a resubmission replays instead of issuing a second SRI
 * invoice, and use a new key for any edited request. The certificate password
 * is excluded because it is never stored and a retry may re-enter it.
 */
export function nextInvoiceIssueAttempt(
  previous: InvoiceIssueAttempt | null,
  sessionId: string,
  payload: GenerateSessionInvoiceInput,
  newKey: () => string,
): InvoiceIssueAttempt {
  const { certificatePassword: _password, idempotencyKey: _key, ...rest } = payload;
  const fingerprint = JSON.stringify([sessionId, rest]);
  return previous?.fingerprint === fingerprint ? previous : { fingerprint, key: newKey() };
}
