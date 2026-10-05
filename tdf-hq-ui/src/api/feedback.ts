import { buildAuthorizationHeader } from './authHeader';
import { resolveApiBase } from '../config/apiBase';
import type { components } from './generated/types';

type FeedbackWirePayload = components['schemas']['FeedbackMultipart'];

export interface FeedbackPayload extends Omit<FeedbackWirePayload, 'attachment'> {
  attachment?: File | null;
}

export async function submitFeedback(payload: FeedbackPayload, options?: { sessionCookieOnly?: boolean; accountDeletionPartyId?: number }): Promise<void> {
  const base = resolveApiBase();
  const authHeader = options?.sessionCookieOnly ? undefined : buildAuthorizationHeader();

  const form = new FormData();
  form.append('title', payload.title);
  form.append('description', payload.description);
  form.append('categoryId', payload.categoryId);
  form.append('severityId', payload.severityId);
  if (payload.contactEmail) form.append('contactEmail', payload.contactEmail);
  form.append('consent', String(payload.consent));
  if (payload.attachment) {
    form.append('attachment', payload.attachment);
  }

  const accountId = options?.accountDeletionPartyId;
  if (accountId !== undefined && (!Number.isSafeInteger(accountId) || accountId <= 0)) throw new Error('Invalid account');
  const endpoint = accountId === undefined ? '/feedback' : `/feedback/account-deletion?accountId=${accountId}`;
  const res = await fetch(`${base}${endpoint}`, {
    method: 'POST',
    body: form,
    headers: accountId !== undefined ? {
      ...(authHeader ? { Authorization: authHeader } : {}),
      ...(accountId !== undefined ? { 'X-Requested-With': 'TDF-Account-Deletion' } : {}),
    } : authHeader ? { Authorization: authHeader } : undefined,
    credentials: 'include',
  });

  if (!res.ok) {
    const text = await res.text();
    throw new Error(text || 'No se pudo enviar tu feedback.');
  }
  if (accountId !== undefined) {
    const receipt: unknown = await res.json();
    if (!receipt || typeof receipt !== 'object' || !('adrCreatedBy' in receipt) || receipt.adrCreatedBy !== accountId
      || !('adrRequestId' in receipt) || typeof receipt.adrRequestId !== 'string' || !receipt.adrRequestId.trim()) {
      throw new Error('The server did not confirm an authenticated deletion request');
    }
  }
}
