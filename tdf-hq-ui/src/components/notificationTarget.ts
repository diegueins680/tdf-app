import type { NotificationDTO } from '../api/types';

export function notificationTargetPath(notification: NotificationDTO): string | null {
  if (notification.nTargetType === 'event_logistics' && notification.nTargetId != null) {
    return `/social/eventos/${notification.nTargetId}/logistica`;
  }

  if (notification.nTargetType === 'feature_access_request') {
    return notification.nType === 'access_request_review'
      ? '/solicitudes-acceso/revision'
      : '/solicitudes-acceso';
  }

  return null;
}
