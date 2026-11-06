import { del, get, put } from './client';
import { buildAuthorizationHeader } from './authHeader';
import { resolveApiBase } from '../config/apiBase';
import { getAuthSessionEpoch } from '../session/authEvents';

export type StorageDestination = 'tdf' | 'google_drive';
export interface StorageAccount {
  allowanceBytes: number;
  usedBytes: number;
  reservedBytes: number;
  defaultDestination: StorageDestination | null;
  revision: number;
  legacyUsageVerified: boolean;
}

function parseAccount(value: unknown): StorageAccount {
  if (!value || typeof value !== 'object') throw new Error('Invalid storage account');
  const data = value as Record<string, unknown>;
  for (const field of ['allowanceBytes', 'usedBytes', 'reservedBytes', 'revision']) {
    if (!Number.isSafeInteger(data[field]) || Number(data[field]) < 0) throw new Error('Invalid storage account');
  }
  if (typeof data['legacyUsageVerified'] !== 'boolean') throw new Error('Invalid storage accounting status');
  if (data['defaultDestination'] !== null && data['defaultDestination'] !== 'tdf' && data['defaultDestination'] !== 'google_drive') throw new Error('Invalid storage destination');
  return data as unknown as StorageAccount;
}

export const loadStorageAccount = async (): Promise<StorageAccount> => parseAccount(await get<unknown>('/storage/me'));
export const saveStorageDestination = async (destination: StorageDestination, expectedRevision: number): Promise<StorageAccount> =>
  parseAccount(await put<unknown>('/storage/me/destination', { destination, expectedRevision }));

export const freeStorageBytes = 5_000_000_000;
export const availableStorageBytes = (account: StorageAccount): number =>
  Math.max(0, account.allowanceBytes - account.usedBytes - account.reservedBytes);

export interface StorageFile {
  fileId: string;
  fileName: string;
  byteSize: number;
  status: 'uploading' | 'completed' | 'cancelled';
  visibility: 'private' | 'public';
}
export async function loadStorageFiles(after?: string): Promise<StorageFile[]> {
  const value = await get<unknown>(`/storage/files?limit=20${after ? `&after=${encodeURIComponent(after)}` : ''}`);
  if (!Array.isArray(value) || value.some((file: unknown) => {
    if (!file || typeof file !== 'object') return true;
    const data = file as Record<string, unknown>;
    return typeof data['fileId'] !== 'string' || !/^[a-f0-9-]{36}$/i.test(data['fileId'])
      || typeof data['fileName'] !== 'string' || !Number.isSafeInteger(data['byteSize']) || Number(data['byteSize']) <= 0
      || typeof data['status'] !== 'string' || !['uploading', 'completed', 'cancelled'].includes(data['status'])
      || typeof data['visibility'] !== 'string' || !['private', 'public'].includes(data['visibility']);
  })) throw new Error('Invalid storage files');
  return value as StorageFile[];
}
export const deleteStorageFile = (fileId: string): Promise<void> => del<void>(`/storage/files/${encodeURIComponent(fileId)}`);
export function storageFileIdFromUrl(value: string): string | null {
  try {
    const base = new URL(resolveApiBase(), window.location.origin);
    const url = new URL(value, `${base.origin}/`);
    if (url.origin !== base.origin || url.search || url.hash || url.username || url.password) return null;
    const prefix = `${base.pathname.replace(/\/$/, '')}/storage/files/`;
    const identifier = url.pathname.startsWith(prefix) ? url.pathname.slice(prefix.length) : '';
    return /^[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$/.test(identifier) ? identifier : null;
  } catch { return null; }
}

export const downloadStorageFile = (file: StorageFile): Promise<void> => downloadStorageUrl(
  `${resolveApiBase()}/storage/files/${encodeURIComponent(file.fileId)}`, file.fileName,
);
export async function downloadStorageUrl(url: string, fileName: string): Promise<void> {
  const identifier = storageFileIdFromUrl(url);
  if (!identifier) throw new Error('Invalid protected storage URL');
  const epoch = getAuthSessionEpoch();
  const authorization = buildAuthorizationHeader();
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), 30_000);
  try {
  const response = await fetch(`${resolveApiBase()}/storage/files/${identifier}`, {
    credentials: 'include', cache: 'no-store', redirect: 'error', headers: authorization ? { Authorization: authorization } : {},
    signal: controller.signal,
  });
  if (!response.ok) throw new Error('Storage file unavailable');
  const data = await response.blob();
  if (epoch !== getAuthSessionEpoch()) throw new Error('Session changed');
  const url = URL.createObjectURL(data);
  const anchor = document.createElement('a');
  anchor.href = url; anchor.download = fileName; anchor.click();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
  } finally { clearTimeout(timer); }
}
