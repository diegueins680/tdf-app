import type { NotificationDTO } from '../api/types';

export const positiveNotificationId = (value: unknown): string | null =>
  typeof value === 'number' && Number.isSafeInteger(value) && value > 0 ? String(value) : null;
export const notificationUuid = (value: unknown): string | null =>
  typeof value === 'string' && /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(value) ? value : null;

// Only typed identities form links. Text, display names and arbitrary URLs never do.
export function notificationTargetPath(notification: NotificationDTO): string | null {
  const id = positiveNotificationId(notification.nTargetId);
  const key = notificationUuid(notification.nTargetKey);
  const activity = typeof notification.nTargetKey === 'string' && /^[1-9]\d*$/.test(notification.nTargetKey)
    ? positiveNotificationId(Number(notification.nTargetKey)) : null;
  switch (notification.nTargetType) {
    case 'party_profile': return id ? `/perfil/${id}` : null;
    case 'feature_access_request': return id ? `/solicitudes-acceso?request=${id}` : null;
    case 'event_logistics': return id ? `/social/eventos/${id}/logistica${activity ? `?activity=${activity}` : ''}` : null;
    case 'intern_audit_plan': return key ? `/practicas/auditorias/${key}` : null;
    case 'internal_feedback_report': return key ? `/feedback/interno/${key}` : null;
    case 'directory_application': return key ? `/mis-clasificados?application=${key}` : null;
    case 'directory_invitation': return key ? `/mis-clasificados?invitation=${key}` : null;
    case 'directory_review': return key ? `/mis-clasificados?review=${key}` : null;
    case 'directory_alert': return key ? `/mis-clasificados?alert=${key}` : null;
    // Old artist_liked rows point at the recipient, so they must not be used.
    default: return null;
  }
}

export function notificationFallbackPath(notification: NotificationDTO): string {
  const kind = notification.nTargetType;
  if (kind === 'feature_access_request') return '/solicitudes-acceso';
  if (kind?.startsWith('directory_')) return '/mis-clasificados';
  if (kind === 'internal_feedback_report') return '/feedback/interno';
  if (kind === 'intern_audit_plan' || kind === 'internship_task') return '/practicas';
  if (kind === 'event_logistics') return '/social/eventos';
  return '/inicio';
}

export function notificationLink(notification: NotificationDTO): string {
  return `/notificaciones/${positiveNotificationId(notification.nId) ?? '0'}`;
}
