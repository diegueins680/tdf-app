import { get, post } from './client';
import type { AssetDTO, AssetCheckoutDTO } from './types';

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
  list: () => get<{ items: AssetDTO[] } | AssetDTO[]>('/assets'),
  checkout: (assetId: string, body: AssetCheckoutRequest) =>
    post<AssetCheckoutDTO>(`/assets/${assetId}/checkout`, body),
  checkin: (assetId: string, body: AssetCheckinRequest) =>
    post<AssetCheckoutDTO>(`/assets/${assetId}/checkin`, body),
  history: (assetId: string) => get<AssetCheckoutDTO[]>(`/assets/${assetId}/history`),
  generateQr: (assetId: string) => post<AssetQrDTO>(`/assets/${assetId}/qr`, {}),
  byQrToken: (token: string) => get<AssetDTO>(`/assets/qr/${token}`),
};
