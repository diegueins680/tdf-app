export type UploadAuthMode = 'user' | 'server' | 'assets';
export type UploadRecovery = 'reconnect' | 'retry' | 'none';

/** A provider failure is distinct from authentication of the TDF session. */
export class UploadFailure extends Error {
  constructor(public readonly code: string, public readonly status: number, public readonly recovery: UploadRecovery) {
    super(code);
    this.name = 'UploadFailure';
  }
}

export function parseUploadFailure(status: number, body: string, mode: UploadAuthMode): UploadFailure {
  let code: unknown;
  try { code = (JSON.parse(body) as { code?: unknown }).code; } catch { /* Legacy text responses. */ }
  if (code === 'storage_reconnect_required' || (mode === 'user' && (status === 401 || /invalid_grant|refresh token|reauthoriz|reautoriza/i.test(body)))) {
    return new UploadFailure('storage_reconnect_required', status, 'reconnect');
  }
  if (code === 'storage_activation_pending') return new UploadFailure(code, status, 'none');
  if (code === 'storage_public_media_invalid') return new UploadFailure(code, status, 'none');
  if (code === 'storage_inventory_required') return new UploadFailure(code, status, 'none');
  if (code === 'storage_quota_exceeded') return new UploadFailure(code, status, 'none');
  if (status === 403 || code === 'storage_permission_denied') return new UploadFailure('storage_permission_denied', status, 'none');
  if (status === 413) return new UploadFailure('storage_file_too_large', status, 'none');
  if (status === 409) return new UploadFailure('storage_upload_conflict', status, 'retry');
  if (status === 0 || status === 408) return new UploadFailure('storage_upload_uncertain', status, 'retry');
  return new UploadFailure(mode === 'user' ? 'storage_provider_unavailable' : 'storage_service_unavailable', status, 'retry');
}

export function normalizeUploadFailure(error: unknown, mode: UploadAuthMode): UploadFailure {
  if (error instanceof UploadFailure) return error;
  // Do not echo exceptions, OAuth responses, or backend configuration to users.
  return parseUploadFailure(502, error instanceof Error ? error.message : '', mode);
}

export function uploadFailureMessage(error: UploadFailure, english = false): string {
  switch (error.code) {
    case 'storage_activation_pending': return english ? 'File uploads are being prepared. Your changes are saved; please try again after activation.' : 'Estamos preparando las cargas de archivos. Tus cambios se conservan; podrás subirlos cuando se habilite el servicio.';
    case 'storage_reconnect_required': return english
      ? 'Reconnect your Google Drive, then retry this file. Your changes are still here.'
      : 'Conecta de nuevo tu Google Drive y reintenta este archivo. Tus cambios se conservan.';
    case 'storage_public_media_invalid': return english ? 'Choose a valid supported image, audio or video file for public access.' : 'Elige una imagen, audio o video válido y compatible para compartir mediante enlace.';
    case 'storage_inventory_required': return english ? 'TDF must verify your existing storage usage before accepting new files. Your changes are still here; you can choose your own storage.' : 'TDF debe verificar el espacio de tus archivos anteriores antes de aceptar nuevas cargas. Tus cambios se conservan; puedes elegir tu propio almacenamiento.';
    case 'storage_quota_exceeded': return english
      ? 'Your TDF storage is full. Free up space or choose your own storage.'
      : 'Tu almacenamiento de TDF está lleno. Libera espacio o elige tu propio almacenamiento.';
    case 'storage_permission_denied': return english
      ? 'You do not have permission to upload to this destination. Check the chosen account and folder.'
      : 'No tienes permiso para subir a este destino. Revisa la cuenta y carpeta elegidas.';
    case 'storage_file_too_large': return english ? 'This file exceeds the upload size limit.' : 'Este archivo supera el tamaño permitido por carga.';
    case 'storage_upload_uncertain': return english
      ? 'The upload could not be confirmed. Retry the same file to check the existing attempt.'
      : 'No pudimos confirmar la carga. Reintenta el mismo archivo para comprobar el intento existente.';
    case 'storage_upload_conflict': return english
      ? 'This upload needs reconciliation before it can continue. Keep this file and retry shortly.'
      : 'Hay que comprobar el intento anterior antes de continuar. Conserva este archivo y reintenta en un momento.';
    case 'storage_provider_unavailable': return english
      ? 'Your storage provider is temporarily unavailable. Keep this file and retry shortly.'
      : 'Tu proveedor de almacenamiento no está disponible. Conserva este archivo y reintenta en un momento.';
    default: return english
      ? 'TDF storage is temporarily unavailable. Your changes are still here; retry shortly.'
      : 'El almacenamiento de TDF no está disponible en este momento. Tus cambios se conservan; reintenta en un momento.';
  }
}

export const newUploadKey = (): string =>
  Array.from(crypto.getRandomValues(new Uint8Array(32)), (byte) => byte.toString(16).padStart(2, '0')).join('');
