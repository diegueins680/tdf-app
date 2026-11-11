/** Spanish names for the directory statuses (profiles, classifieds, applications, invitations) the API reports as codes. */
export const DIRECTORY_STATUS_LABELS = {
  draft: 'Borrador',
  pending_review: 'En revisión',
  pending_moderation: 'En revisión',
  published: 'Publicado',
  paused: 'Pausado',
  archived: 'Archivado',
  suspended: 'Suspendido',
  merged: 'Fusionado',
  filled: 'Cubierto',
  expired: 'Vencido',
  withdrawn: 'Retirado',
  rejected: 'Rechazado',
  moderated: 'Retirado por moderación',
  submitted: 'Enviada',
  viewed: 'Vista',
  shortlisted: 'Preseleccionada',
  pending: 'Pendiente',
  accepted: 'Aceptada',
  declined: 'Rechazada',
  blocked: 'Bloqueada',
  conversation_open: 'Conversación abierta',
  converted: 'Concretada',
} as const;

export type DirectoryStatusValue = keyof typeof DIRECTORY_STATUS_LABELS;

const isDirectoryStatus = (value: string): value is DirectoryStatusValue =>
  Object.prototype.hasOwnProperty.call(DIRECTORY_STATUS_LABELS, value);

/** An unknown status is shown as received rather than hidden. */
export function directoryStatusLabel(status: string | null | undefined): string {
  const value = status?.trim() ?? '';
  return isDirectoryStatus(value) ? DIRECTORY_STATUS_LABELS[value] : value;
}
