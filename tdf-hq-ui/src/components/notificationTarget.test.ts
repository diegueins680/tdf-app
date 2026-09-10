import type { NotificationDTO } from '../api/types';
import { notificationTargetPath } from './notificationTarget';

const notification = (overrides: Partial<NotificationDTO>): NotificationDTO => ({
  nId: 1,
  nType: 'test',
  nTitle: 'Test',
  nBody: 'Test',
  nTargetType: null,
  nTargetId: null,
  nIsRead: false,
  nCreatedAt: '2026-09-10T00:00:00Z',
  ...overrides,
});

describe('notification target routing', () => {
  it('opens the reviewer queue for a review notification', () => {
    expect(notificationTargetPath(notification({
      nType: 'access_request_review',
      nTargetType: 'feature_access_request',
      nTargetId: 17,
    }))).toBe('/solicitudes-acceso/revision');
  });

  it.each(['access_request_submitted', 'access_request_decided'])(
    'opens the requester history for %s',
    (nType) => {
      expect(notificationTargetPath(notification({
        nType,
        nTargetType: 'feature_access_request',
        nTargetId: 17,
      }))).toBe('/solicitudes-acceso');
    },
  );

  it('preserves event logistics deep links', () => {
    expect(notificationTargetPath(notification({
      nTargetType: 'event_logistics',
      nTargetId: 42,
    }))).toBe('/social/eventos/42/logistica');
  });

  it('does not invent destinations for informational notifications', () => {
    expect(notificationTargetPath(notification({ nTargetType: 'artist' }))).toBeNull();
  });
});
