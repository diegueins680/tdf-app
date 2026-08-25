import { get, post, put } from './client';

export interface NavigationPreferenceDTO {
  featureId: string;
  favorite: boolean;
  pinned: boolean;
  pinOrder: number | null;
  lastVisitedAt: string | null;
  useCount: number;
  updatedAt: string;
}

export const NavigationPreferences = {
  list: () => get<NavigationPreferenceDTO[]>('/navigation/preferences'),
  update: (featureId: string, input: { favorite: boolean; pinned: boolean; pinOrder: number | null }) =>
    put<NavigationPreferenceDTO>(`/navigation/preferences/${encodeURIComponent(featureId)}`, input),
  visit: (featureId: string) =>
    post<NavigationPreferenceDTO>(`/navigation/preferences/${encodeURIComponent(featureId)}/visit`, {}),
};
