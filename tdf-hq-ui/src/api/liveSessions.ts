import { getStoredSessionToken } from '../session/SessionContext';

export interface LiveSessionMusicianInput {
  partyId: number;
  name: string;
  email?: string | null;
  instrument?: string | null;
  role?: string | null;
  notes?: string | null;
  isExisting: boolean;
}

export interface LiveSessionIntakePayload {
  bandName: string;
  contactEmail?: string | null;
  contactPhone?: string | null;
  sessionDate?: string | null;
  musicians: LiveSessionMusicianInput[];
  riderFile?: File | null;
}

export async function submitLiveSessionIntake(payload: LiveSessionIntakePayload): Promise<void> {
  const base = import.meta.env.VITE_API_BASE ?? '';
  const token = getStoredSessionToken();

  const form = new FormData();
  form.append('bandName', payload.bandName);
  if (payload.contactEmail) form.append('contactEmail', payload.contactEmail);
  if (payload.contactPhone) form.append('contactPhone', payload.contactPhone);
  if (payload.sessionDate) form.append('sessionDate', payload.sessionDate);
  form.append('musicians', JSON.stringify(payload.musicians));
  if (payload.riderFile) {
    form.append('rider', payload.riderFile);
  }

  const res = await fetch(`${base}/live-sessions/intake`, {
    method: 'POST',
    body: form,
    headers: token ? { Authorization: `Bearer ${token}` } : undefined,
  });

  if (!res.ok) {
    const text = await res.text();
    throw new Error(text || 'No se pudo guardar la inscripción de Live Session.');
  }
}
