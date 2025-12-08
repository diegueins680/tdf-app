import { get, post, patch, del } from './client';
import type { LabelTrackDTO } from './types';

export interface LabelTrackCreate {
  ltcTitle: string;
  ltcNote?: string | null;
}

export interface LabelTrackUpdate {
  ltuTitle?: string;
  ltuNote?: string | null;
  ltuStatus?: string;
}

export const Label = {
  listTracks: () => get<LabelTrackDTO[]>('/label/tracks'),
  createTrack: (payload: LabelTrackCreate) => post<LabelTrackDTO>('/label/tracks', payload),
  updateTrack: (id: string, payload: LabelTrackUpdate) =>
    patch<LabelTrackDTO>(`/label/tracks/${id}`, payload),
  deleteTrack: (id: string) => del<void>(`/label/tracks/${id}`),
};
