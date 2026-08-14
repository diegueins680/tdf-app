import { buildAuthorizationHeader } from './authHeader';
import { resolveApiBase } from '../config/apiBase';
import type { components } from './generated/types';

type FeedbackWirePayload = components['schemas']['FeedbackMultipart'];

export interface FeedbackPayload extends Omit<FeedbackWirePayload, 'attachment'> {
  attachment?: File | null;
}

export async function submitFeedback(payload: FeedbackPayload): Promise<void> {
  const base = resolveApiBase();
  const authHeader = buildAuthorizationHeader();

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

  const res = await fetch(`${base}/feedback`, {
    method: 'POST',
    body: form,
    headers: authHeader ? { Authorization: authHeader } : undefined,
    credentials: 'include',
  });

  if (!res.ok) {
    const text = await res.text();
    throw new Error(text || 'No se pudo enviar tu feedback.');
  }
}
