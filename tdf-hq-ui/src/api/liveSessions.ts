import { buildAuthorizationHeader } from './authHeader';
import { resolveApiBase } from '../config/apiBase';
import type { components } from './generated/types';

export type LiveSessionMusicianInput = components['schemas']['LiveSessionMusician'];
export type LiveSessionSongInput = components['schemas']['LiveSessionSong'];
type LiveSessionIntakeMultipart = components['schemas']['LiveSessionIntakeMultipart'];

export interface LiveSessionIntakePayload {
  bandName: string;
  bandDescription?: string | null;
  primaryGenreId?: string | null;
  inputList?: string | null;
  contactEmail?: string | null;
  contactPhone?: string | null;
  sessionDate?: string | null;
  availabilityNotes?: string | null;
  acceptedTerms: LiveSessionIntakeMultipart['acceptedTerms'];
  termsVersion: LiveSessionIntakeMultipart['termsVersion'];
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
  const base = resolveApiBase();
  const authHeader = buildAuthorizationHeader();
  const wireFields: Omit<LiveSessionIntakeMultipart, 'rider'> = {
    bandName: payload.bandName,
    bandDescription: payload.bandDescription,
    primaryGenreId: payload.primaryGenreId,
    inputList: payload.inputList,
    contactEmail: payload.contactEmail,
    contactPhone: payload.contactPhone,
    sessionDate: payload.sessionDate,
    availability: payload.availabilityNotes,
    acceptedTerms: payload.acceptedTerms,
    termsVersion: payload.termsVersion,
    musicians: JSON.stringify(payload.musicians),
    setlist: payload.setlist ? JSON.stringify(payload.setlist) : undefined,
  };

  const form = new FormData();
  form.append('bandName', wireFields.bandName);
  if (wireFields.bandDescription) form.append('bandDescription', wireFields.bandDescription);
  if (wireFields.primaryGenreId) form.append('primaryGenreId', wireFields.primaryGenreId);
  if (wireFields.inputList) form.append('inputList', wireFields.inputList);
  if (wireFields.contactEmail) form.append('contactEmail', wireFields.contactEmail);
  if (wireFields.contactPhone) form.append('contactPhone', wireFields.contactPhone);
  if (wireFields.sessionDate) form.append('sessionDate', wireFields.sessionDate);
  if (wireFields.availability) form.append('availability', wireFields.availability);
  form.append('acceptedTerms', String(wireFields.acceptedTerms));
  form.append('termsVersion', wireFields.termsVersion);
  form.append('musicians', wireFields.musicians);
  if (wireFields.setlist) form.append('setlist', wireFields.setlist);
  if (payload.riderFile) {
    form.append('rider', payload.riderFile);
  }

  const res = await fetch(`${base}/live-sessions/intake`, {
    method: 'POST',
    body: form,
    headers: authHeader ? { Authorization: authHeader } : undefined,
  });

  if (!res.ok) {
    const text = await res.text();
    throw new Error(text || 'No se pudo guardar la inscripción de Live Session.');
  }
}

export async function listInputInventory(field?: 'mic' | 'preamp' | 'interface'): Promise<InputInventoryItem[]> {
  const base = resolveApiBase();
  const params = new URLSearchParams();
  if (field) params.set('field', field);
  const res = await fetch(`${base}/input-list/inventory?${params.toString()}`);
  if (!res.ok) {
    throw new Error('No se pudo cargar el inventario para input list.');
  }
  const json = (await res.json()) as unknown;
  return Array.isArray(json) ? (json as InputInventoryItem[]) : [];
}
