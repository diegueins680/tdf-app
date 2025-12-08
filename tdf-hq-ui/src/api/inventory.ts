import { del, get, patch, post } from './client';
import type { AssetCheckoutDTO, AssetCreate, AssetDTO, AssetUpdate, PageResponse } from './types';

interface AssetListParams {
  q?: string;
  page?: number;
  pageSize?: number;
}

type AssetListResponse = PageResponse<AssetDTO> | { items: AssetDTO[] } | AssetDTO[];

function buildQuery(params?: AssetListParams): string {
  if (!params) return '';
  const qs = new URLSearchParams();
  if (params.q) qs.set('q', params.q);
  if (params.page) qs.set('page', String(params.page));
  if (params.pageSize) qs.set('pageSize', String(params.pageSize));
  const suffix = qs.toString();
  return suffix ? `?${suffix}` : '';
}

export interface AssetCheckoutRequest {
  coTargetKind?: string;
  coTargetSession?: string | null;
  coTargetParty?: string | null;
  coTargetRoom?: string | null;
  coDueAt?: string | null;
  coConditionOut?: string | null;
  coNotes?: string | null;
}

export interface AssetCheckinRequest {
  ciConditionIn?: string | null;
  ciNotes?: string | null;
}

export interface AssetQrDTO {
  qrToken: string;
  qrUrl: string;
}

export const Inventory = {
  list: (params?: AssetListParams) => get<AssetListResponse>(`/assets${buildQuery(params)}`),
  create: (body: AssetCreate) => post<AssetDTO>('/assets', body),
  update: (assetId: string, body: AssetUpdate) => patch<AssetDTO>(`/assets/${assetId}`, body),
  remove: (assetId: string) => del<void>(`/assets/${assetId}`),
  checkout: (assetId: string, body: AssetCheckoutRequest) =>
    post<AssetCheckoutDTO>(`/assets/${assetId}/checkout`, body),
  checkin: (assetId: string, body: AssetCheckinRequest) =>
    post<AssetCheckoutDTO>(`/assets/${assetId}/checkin`, body),
  history: (assetId: string) => get<AssetCheckoutDTO[]>(`/assets/${assetId}/history`),
  generateQr: (assetId: string) => post<AssetQrDTO>(`/assets/${assetId}/qr`, {}),
  byQrToken: (token: string) => get<AssetDTO>(`/assets/qr/${token}`),
};
