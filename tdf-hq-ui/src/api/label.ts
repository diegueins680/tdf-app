import { get, post, patch, del } from './client';
import type { LabelTrackDTO } from './types';

const requirePositiveInteger = (value: number, field: string): number => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${field} debe ser un entero positivo.`);
  }
  return value;
};

const normalizeTrackId = (id: string): string => {
  const trimmed = id.trim();
  if (!trimmed) {
    throw new Error('id no puede estar vacío.');
  }
  return encodeURIComponent(trimmed);
};

const normalizeOptionalOwnerId = (ownerId?: number | null): number | undefined => {
  if (ownerId == null) return undefined;
  return requirePositiveInteger(ownerId, 'ownerId');
};

export interface LabelTrackCreate {
  ltcTitle: string;
  ltcNote?: string | null;
  ltcOwnerId?: number | null;
}

export interface LabelTrackUpdate {
  ltuTitle?: string;
  ltuNote?: string | null;
  ltuStatus?: string;
}

export const Label = {
  listTracks: (ownerId?: number | null) => {
    const normalizedOwnerId = normalizeOptionalOwnerId(ownerId);
    const ownerParam = normalizedOwnerId ? `?ownerId=${normalizedOwnerId}` : '';
    return get<LabelTrackDTO[]>(`/label/tracks${ownerParam}`);
  },
  createTrack: (payload: LabelTrackCreate, ownerId?: number | null) => {
    const normalizedOwnerId = normalizeOptionalOwnerId(ownerId);
    return post<LabelTrackDTO>('/label/tracks', {
      ...payload,
      ...(normalizedOwnerId ? { ltcOwnerId: normalizedOwnerId } : {}),
    });
  },
  updateTrack: (id: string, payload: LabelTrackUpdate) =>
    patch<LabelTrackDTO>(`/label/tracks/${normalizeTrackId(id)}`, payload),
  deleteTrack: (id: string) => del<void>(`/label/tracks/${normalizeTrackId(id)}`),
};
