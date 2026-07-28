import { buildAuthorizationHeader } from './authHeader';

export interface FeedbackPayload {
  title: string;
  description: string;
  category?: string;
  severity?: string;
  contactEmail?: string;
  consent: boolean;
  attachment?: File | null;
}

export async function submitFeedback(payload: FeedbackPayload): Promise<void> {
  const base = import.meta.env.VITE_API_BASE ?? '';
  const authHeader = buildAuthorizationHeader();

  const form = new FormData();
  form.append('title', payload.title);
  form.append('description', payload.description);
  if (payload.category) form.append('category', payload.category);
  if (payload.severity) form.append('severity', payload.severity);
  if (payload.contactEmail) form.append('contactEmail', payload.contactEmail);
  form.append('consent', String(payload.consent));
  if (payload.attachment) {
    form.append('attachment', payload.attachment);
  }

  const res = await fetch(`${base}/feedback`, {
    method: 'POST',
    body: form,
    headers: authHeader ? { Authorization: authHeader } : undefined,
  });

  if (!res.ok) {
    const text = await res.text();
    throw new Error(text || 'No se pudo enviar tu feedback.');
  }
}
