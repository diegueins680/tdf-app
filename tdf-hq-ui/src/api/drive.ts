import { assertNoLogoutIntent } from '../session/logoutIntent';
import type { DriveUploadDTO } from './types';
import { buildPublicContentUrl, type DriveFileInfo } from '../services/googleDrive';
import { buildAuthorizationHeader } from './authHeader';
import { resolveApiBase } from '../config/apiBase';

export interface DriveUploadOptions {
  folderId?: string;
  name?: string;
  onProgress?: (pct: number) => void;
  accessToken?: string;
}

export async function uploadToDrive(file: File, options: DriveUploadOptions = {}): Promise<DriveFileInfo> {
  assertNoLogoutIntent();
  const base = resolveApiBase();
  const authHeader = buildAuthorizationHeader();

  const form = new FormData();
  form.append('file', file);
  if (options.folderId) form.append('folderId', options.folderId);
  if (options.name) form.append('name', options.name);
  if (options.accessToken) form.append('accessToken', options.accessToken);

  const dto = await new Promise<DriveUploadDTO>((resolve, reject) => {
    const xhr = new XMLHttpRequest();
    xhr.open('POST', `${base}/drive/upload`);
    xhr.withCredentials = true;
    if (authHeader) xhr.setRequestHeader('Authorization', authHeader);
    xhr.onreadystatechange = () => {
      if (xhr.readyState !== XMLHttpRequest.DONE) return;
      if (xhr.status >= 200 && xhr.status < 300) {
        try {
          resolve(JSON.parse(xhr.responseText) as DriveUploadDTO);
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

  const publicUrl =
    dto.duPublicUrl ??
    dto.duWebContentLink ??
    (dto.duFileId ? buildPublicContentUrl(dto.duFileId) : undefined);

  return {
    id: dto.duFileId,
    name: file.name,
    webViewLink: dto.duWebViewLink ?? undefined,
    webContentLink: dto.duWebContentLink ?? undefined,
    publicUrl,
  };
}
