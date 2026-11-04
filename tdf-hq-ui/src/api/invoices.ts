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

type IssueStorage = Pick<Storage, 'getItem' | 'setItem'>;

/** Storage key for unresolved session invoice attempts (fingerprint to key). */
export const PENDING_INVOICE_ISSUE_STORAGE_KEY = 'tdf.sessionInvoiceIssueAttempts.v1';
const MAX_PENDING_INVOICE_ISSUES = 50;

const defaultIssueStorage = (): IssueStorage | null => {
  try {
    return typeof window === 'undefined' ? null : window.localStorage;
  } catch {
    return null;
  }
};

const readPendingIssues = (storage: IssueStorage | null): Record<string, string> => {
  if (!storage) return {};
  try {
    const parsed: unknown = JSON.parse(storage.getItem(PENDING_INVOICE_ISSUE_STORAGE_KEY) ?? '{}');
    if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) return {};
    return Object.fromEntries(
      Object.entries(parsed as Record<string, unknown>).filter(
        (entry): entry is [string, string] => typeof entry[1] === 'string',
      ),
    );
  } catch {
    return {};
  }
};

const writePendingIssues = (storage: IssueStorage | null, pending: Record<string, string>) => {
  if (!storage) return;
  try {
    const entries = Object.entries(pending).slice(-MAX_PENDING_INVOICE_ISSUES);
    storage.setItem(PENDING_INVOICE_ISSUE_STORAGE_KEY, JSON.stringify(Object.fromEntries(entries)));
  } catch {
    // Storage may be full or blocked; the in-memory attempt still applies.
  }
};

/** Session plus request, without the certificate password and the key itself. */
export function invoiceIssueFingerprint(sessionId: string, payload: GenerateSessionInvoiceInput): string {
  const { certificatePassword: _password, idempotencyKey: _key, ...rest } = payload;
  return JSON.stringify([sessionId, rest]);
}

/**
 * PAY-INVOICE-ISSUE-001: keep the idempotency key while the request is
 * unchanged, so a resubmission replays instead of issuing a second SRI
 * invoice, and use a new key for any edited request. The certificate password
 * is excluded because it is never stored and a retry may re-enter it.
 *
 * The key is also kept in browser storage until the attempt is settled, so
 * reloading or leaving the page and submitting the same invoice again still
 * replays instead of issuing a second legal document.
 */
export function nextInvoiceIssueAttempt(
  previous: InvoiceIssueAttempt | null,
  sessionId: string,
  payload: GenerateSessionInvoiceInput,
  newKey: () => string,
  storage: IssueStorage | null = defaultIssueStorage(),
): InvoiceIssueAttempt {
  const fingerprint = invoiceIssueFingerprint(sessionId, payload);
  if (previous?.fingerprint === fingerprint) return previous;
  const pending = readPendingIssues(storage);
  const key = pending[fingerprint] ?? newKey();
  if (pending[fingerprint] !== key) {
    pending[fingerprint] = key;
    writePendingIssues(storage, pending);
  }
  return { fingerprint, key };
}

/** Forget a stored attempt once it is settled or staff start a new attempt. */
export function forgetInvoiceIssueAttempt(
  attempt: InvoiceIssueAttempt | null,
  storage: IssueStorage | null = defaultIssueStorage(),
): void {
  if (!attempt) return;
  const pending = readPendingIssues(storage);
  if (attempt.fingerprint in pending) {
    delete pending[attempt.fingerprint];
    writePendingIssues(storage, pending);
  }
}

/**
 * Whether a session invoice response settles the attempt, so its idempotency
 * key may be dropped: SRI was not requested, or SRI reported the document as
 * issued. Any other SRI outcome (an error, or a status other than issued) is
 * uncertain: the portal may have issued the document, so the key is kept and
 * resubmitting the same request replays the stored answer instead of issuing
 * again.
 */
export function invoiceIssueSettled(response: GenerateSessionInvoiceResponse): boolean {
  const sri = response.sri;
  if (sri == null) return true;
  if ('error' in sri) return false;
  return sri.ok === true && sri.status === 'issued';
}
