import { post, API_BASE_URL } from './client';
import { buildAuthorizationHeader } from './authHeader';

export interface ContractCreateRequest {
  kind: string;
  eventId?: string | null;
  counterparty?: string | null;
  concept: string;
  amountCents: number;
  currency: string;
  notes?: string | null;
  metadata?: Record<string, unknown> | null;
}

export interface ContractCreateResponse {
  status: string;
  id: string;
  kind: string;
  payload: unknown;
}

export interface ContractSendResponse {
  status: string;
  id: string;
}

const normalizeNetworkError = (err: unknown) => {
  const wrapped = new Error('No se pudo contactar la API de contratos.');
  (wrapped as Error & { cause?: unknown }).cause = err;
  return wrapped;
};

async function getPdfBlob(contractId: string): Promise<Blob> {
  const authHeader = buildAuthorizationHeader();
  let res: Response;
  try {
    res = await fetch(`${API_BASE_URL}/contracts/${encodeURIComponent(contractId)}/pdf`, {
      method: 'GET',
      headers: {
        ...(authHeader ? { Authorization: authHeader } : {}),
      },
    });
  } catch (err) {
    throw normalizeNetworkError(err);
  }
  if (!res.ok) {
    const body = await res.text().catch(() => '');
    throw new Error(body.trim() || `No se pudo generar PDF de contrato (${res.status})`);
  }
  return res.blob();
}

export const ContractsAPI = {
  create: (payload: ContractCreateRequest) =>
    post<ContractCreateResponse>('/contracts', payload),
  send: (contractId: string, email?: string | null) =>
    post<ContractSendResponse>(`/contracts/${encodeURIComponent(contractId)}/send`, {
      email: email?.trim() ?? null,
    }),
  downloadPdf: (contractId: string) => getPdfBlob(contractId),
};
