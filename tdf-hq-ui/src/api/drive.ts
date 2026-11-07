import { assertNoLogoutIntent } from '../session/logoutIntent';
import type { DriveUploadDTO } from './types';
import { buildPublicContentUrl, type DriveFileInfo } from '../services/googleDrive';
import { buildAuthorizationHeader } from './authHeader';
import { resolveApiBase } from '../config/apiBase';
import { parseUploadFailure, UploadFailure, type UploadAuthMode } from './uploadFailure';

export interface DriveUploadOptions {
  folderId?: string;
  name?: string;
  onProgress?: (pct: number) => void;
  accessToken?: string;
  authMode?: Exclude<UploadAuthMode, 'assets'>;
  destination?: 'tdf' | 'google_drive';
  visibility?: 'private' | 'public';
  idempotencyKey?: string;
  signal?: AbortSignal;
}

export async function uploadToDrive(file: File, options: DriveUploadOptions = {}): Promise<DriveFileInfo> {
  assertNoLogoutIntent();
  const mode = options.authMode ?? (options.accessToken ? 'user' : 'server');
  if (mode === 'user' && !options.accessToken) throw new UploadFailure('storage_reconnect_required', 409, 'reconnect');
  if (options.signal?.aborted) throw new DOMException('Cancelled', 'AbortError');
  const base = resolveApiBase();
  const authHeader = buildAuthorizationHeader();

  const form = new FormData();
  form.append('file', file);
  form.append('authMode', mode);
  if (options.destination) form.append('destination', options.destination);
  if (options.visibility) form.append('visibility', options.visibility);
  if (options.idempotencyKey) form.append('idempotencyKey', options.idempotencyKey);
  if (options.folderId) form.append('folderId', options.folderId);
  if (options.name) form.append('name', options.name);
  if (options.accessToken) form.append('accessToken', options.accessToken);

  const dto = await new Promise<DriveUploadDTO>((resolve, reject) => {
    const xhr = new XMLHttpRequest();
    xhr.open('POST', `${base}/drive/upload`);
    xhr.withCredentials = true;
    xhr.timeout = 120_000;
    const abort = () => xhr.abort();
    options.signal?.addEventListener('abort', abort, { once: true });
    const cleanup = () => options.signal?.removeEventListener('abort', abort);
    xhr.onerror = () => { cleanup(); reject(parseUploadFailure(0, '', mode)); };
    xhr.ontimeout = () => { cleanup(); reject(parseUploadFailure(408, '', mode)); };
    xhr.onabort = () => { cleanup(); reject(new DOMException('Cancelled', 'AbortError')); };
    if (authHeader) xhr.setRequestHeader('Authorization', authHeader);
    xhr.onreadystatechange = () => {
      if (xhr.readyState !== XMLHttpRequest.DONE) return;
      cleanup();
      if (xhr.status >= 200 && xhr.status < 300) {
        try {
          resolve(JSON.parse(xhr.responseText) as DriveUploadDTO);
        } catch (err) {
          reject(err instanceof Error ? err : new Error('No se pudo interpretar la respuesta del servidor.'));
        }
        return;
      }
      reject(parseUploadFailure(xhr.status, xhr.responseText ?? '', mode));
    };
    xhr.upload.onprogress = (evt) => {
      if (!evt.lengthComputable) return;
      const pct = Math.round((evt.loaded / evt.total) * 100);
      options.onProgress?.(pct);
    };
    xhr.send(form);
  });

  const contentLink = dto.duWebContentLink?.startsWith('/storage/files/')
    ? `${base}${dto.duWebContentLink}` : dto.duWebContentLink;
  const publicUrl = dto.duPublicUrl ?? (options.visibility === undefined && options.destination === undefined
    ? dto.duWebContentLink ?? (dto.duFileId ? buildPublicContentUrl(dto.duFileId) : undefined) : undefined);

  return {
    id: dto.duFileId,
    name: file.name,
    webViewLink: dto.duWebViewLink ?? undefined,
    webContentLink: contentLink ?? undefined,
    publicUrl,
  };
}
