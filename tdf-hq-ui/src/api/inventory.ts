import { getStoredSessionToken } from '../session/SessionContext';
import type { DriveFileInfo } from '../services/googleDrive';
import { del, get, patch, post } from './client';
import type {
  AssetCheckoutDTO,
  AssetCreate,
  AssetDTO,
  AssetUpdate,
  AssetUploadDTO,
  PageResponse,
} from './types';

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

const buildAuthHeader = () => {
  const token = getStoredSessionToken();
  if (!token) return undefined;
  return token.toLowerCase().startsWith('bearer ') ? token : `Bearer ${token}`;
};

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

export interface AssetUploadOptions {
  name?: string;
  onProgress?: (pct: number) => void;
}

export async function uploadAssetPhoto(
  file: File,
  options: AssetUploadOptions = {},
): Promise<DriveFileInfo> {
  const base = import.meta.env.VITE_API_BASE ?? '';
  const authHeader = buildAuthHeader();

  const form = new FormData();
  form.append('file', file);
  if (options.name) form.append('name', options.name);

  const dto = await new Promise<AssetUploadDTO>((resolve, reject) => {
    const xhr = new XMLHttpRequest();
    xhr.open('POST', `${base}/assets/upload`);
    if (authHeader) xhr.setRequestHeader('Authorization', authHeader);
    xhr.onreadystatechange = () => {
      if (xhr.readyState !== XMLHttpRequest.DONE) return;
      if (xhr.status >= 200 && xhr.status < 300) {
        try {
          resolve(JSON.parse(xhr.responseText) as AssetUploadDTO);
        } catch (err) {
          reject(err instanceof Error ? err : new Error('No se pudo interpretar la respuesta del servidor.'));
        }
        return;
      }
      const message = (xhr.responseText ?? '').trim();
      reject(new Error(message !== '' ? message : `Upload failed (${xhr.status})`));
    };
    xhr.upload.onprogress = (evt) => {
      if (!evt.lengthComputable) return;
      const pct = Math.round((evt.loaded / evt.total) * 100);
      options.onProgress?.(pct);
    };
    xhr.send(form);
  });

  return {
    id: dto.auPath,
    name: dto.auFileName || file.name,
    publicUrl: dto.auPath,
    webContentLink: dto.auPublicUrl,
  };
}

export const Inventory = {
  list: (params?: AssetListParams) => get<AssetListResponse>(`/assets${buildQuery(params)}`),
  create: (body: AssetCreate) => post<AssetDTO>('/assets', body),
  uploadPhoto: uploadAssetPhoto,
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
