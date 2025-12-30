import { get, patch, post } from './client';

export interface BrainEntryDTO {
  bedId: number;
  bedTitle: string;
  bedBody: string;
  bedCategory?: string | null;
  bedTags: string[];
  bedActive: boolean;
  bedUpdatedAt: string;
}

export interface BrainEntryCreate {
  becTitle: string;
  becBody: string;
  becCategory?: string | null;
  becTags?: string[] | null;
  becActive?: boolean | null;
}

export interface BrainEntryUpdate {
  beuTitle?: string;
  beuBody?: string;
  beuCategory?: string | null;
  beuTags?: string[] | null;
  beuActive?: boolean;
}

export interface RagIndexStatus {
  risCount: number;
  risUpdatedAt?: string | null;
  risStale: boolean;
}

export interface RagRefreshResponse {
  rrrStatus: string;
  rrrChunks: number;
}

export const Brain = {
  listEntries: (includeInactive?: boolean) =>
    get<BrainEntryDTO[]>(`/admin/brain/entries${includeInactive ? '?includeInactive=true' : ''}`),
  createEntry: (payload: BrainEntryCreate) =>
    post<BrainEntryDTO>('/admin/brain/entries', payload),
  updateEntry: (entryId: number, payload: BrainEntryUpdate) =>
    patch<BrainEntryDTO>(`/admin/brain/entries/${entryId}`, payload),
};

export const RagAdmin = {
  status: () => get<RagIndexStatus>('/admin/rag/status'),
  refresh: () => post<RagRefreshResponse>('/admin/rag/refresh', {}),
};
