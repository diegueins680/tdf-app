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

export interface LiveSessionSongInput {
  title: string;
  bpm?: number | null;
  songKey?: string | null;
  lyrics?: string | null;
  sortOrder?: number;
}

export interface LiveSessionIntakePayload {
  bandName: string;
  bandDescription?: string | null;
  primaryGenre?: string | null;
  inputList?: string | null;
  contactEmail?: string | null;
  contactPhone?: string | null;
  sessionDate?: string | null;
  availabilityNotes?: string | null;
  acceptedTerms?: boolean;
  termsVersion?: string | null;
  musicians: LiveSessionMusicianInput[];
  setlist?: LiveSessionSongInput[];
  riderFile?: File | null;
}

export interface InputInventoryItem {
  id: string;
  name: string;
  category: string;
  brand?: string | null;
  model?: string | null;
  status?: string | null;
}

export async function submitLiveSessionIntake(payload: LiveSessionIntakePayload): Promise<void> {
  const base = import.meta.env.VITE_API_BASE ?? '';
  const token = getStoredSessionToken();

  const form = new FormData();
  form.append('bandName', payload.bandName);
  if (payload.bandDescription) form.append('bandDescription', payload.bandDescription);
  if (payload.primaryGenre) form.append('primaryGenre', payload.primaryGenre);
  if (payload.inputList) form.append('inputList', payload.inputList);
  if (payload.contactEmail) form.append('contactEmail', payload.contactEmail);
  if (payload.contactPhone) form.append('contactPhone', payload.contactPhone);
  if (payload.sessionDate) form.append('sessionDate', payload.sessionDate);
  if (payload.availabilityNotes) form.append('availability', payload.availabilityNotes);
  if (typeof payload.acceptedTerms === 'boolean') form.append('acceptedTerms', String(payload.acceptedTerms));
  if (payload.termsVersion) form.append('termsVersion', payload.termsVersion);
  form.append('musicians', JSON.stringify(payload.musicians));
  if (payload.setlist) {
    form.append('setlist', JSON.stringify(payload.setlist));
  }
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

export async function listInputInventory(field?: 'mic' | 'preamp' | 'interface'): Promise<InputInventoryItem[]> {
  const base = import.meta.env.VITE_API_BASE ?? '';
  const params = new URLSearchParams();
  if (field) params.set('field', field);
  const res = await fetch(`${base}/input-list/inventory?${params.toString()}`);
  if (!res.ok) {
    throw new Error('No se pudo cargar el inventario para input list.');
  }
  return res.json();
}
