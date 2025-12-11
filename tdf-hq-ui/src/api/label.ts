import { get, post, patch, del } from './client';
import type { LabelTrackDTO } from './types';

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
    const ownerParam = ownerId && ownerId > 0 ? `?ownerId=${ownerId}` : '';
    return get<LabelTrackDTO[]>(`/label/tracks${ownerParam}`);
  },
  createTrack: (payload: LabelTrackCreate, ownerId?: number | null) =>
    post<LabelTrackDTO>('/label/tracks', {
      ...payload,
      ...(ownerId && ownerId > 0 ? { ltcOwnerId: ownerId } : {}),
    }),
  updateTrack: (id: string, payload: LabelTrackUpdate) =>
    patch<LabelTrackDTO>(`/label/tracks/${id}`, payload),
  deleteTrack: (id: string) => del<void>(`/label/tracks/${id}`),
};
