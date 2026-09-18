import { buildLoginRedirectPath, resolvePostAuthPath } from '../utils/loginRouting';
import type { NotificationDTO } from '../api/types';
import { notificationLink, notificationTargetPath } from './notificationTarget';
const key = '00000000-0000-0000-0000-000000000017';
const notification = (overrides: Partial<NotificationDTO>): NotificationDTO => ({
  nId: 1, nType: 'test', nTitle: 'Test', nBody: 'Test', nTargetType: null,
  nTargetId: null, nIsRead: false, nCreatedAt: '2026-09-10T00:00:00Z', ...overrides,
});
describe('notification destination identities', () => {
  it('uses the stored follower identity, never names or the old recipient target', () => {
    expect(notificationTargetPath(notification({ nType: 'artist_liked', nTargetType: 'party_profile', nTargetId: 7, nBody: 'Galo' }))).toBe('/perfil/7');
    expect(notificationTargetPath(notification({ nType: 'artist_liked', nTargetType: 'artist', nTargetId: 5, nBody: 'Galo' }))).toBeNull();
  });
  it.each(['access_request_review', 'access_request_submitted', 'access_request_decided'])('preserves the specific request for %s', (nType) => {
    expect(notificationTargetPath(notification({ nType, nTargetType: 'feature_access_request', nTargetId: 17 }))).toBe('/solicitudes-acceso?request=17');
  });
  it.each([
    ['internal_feedback_report', `/feedback/interno/${key}`],
    ['intern_audit_plan', `/practicas/auditorias/${key}`],
    ['directory_application', `/mis-clasificados?application=${key}`],
    ['directory_invitation', `/mis-clasificados?invitation=${key}`],
    ['directory_review', `/mis-clasificados?review=${key}`],
    ['directory_alert', `/mis-clasificados?alert=${key}`],
  ])('routes %s with its exact UUID', (nTargetType, path) => {
    expect(notificationTargetPath(notification({ nTargetType, nTargetKey: key }))).toBe(path);
  });
  it('retains logistics activity context', () => {
    expect(notificationTargetPath(notification({ nTargetType: 'event_logistics', nTargetId: 42, nTargetKey: '91' }))).toBe('/social/eventos/42/logistica?activity=91');
  });
  it.each([0, -1, NaN, Infinity, 1.5, Number.MAX_SAFE_INTEGER + 1])('rejects invalid numeric identity %s', (nTargetId) => {
    expect(notificationTargetPath(notification({ nTargetType: 'party_profile', nTargetId }))).toBeNull();
  });
  it.each(['//evil.example', 'https://evil.example', '../secret', key + '?redirect=https://evil.example'])('rejects unsafe UUID identity %s', (nTargetKey) => {
    expect(notificationTargetPath(notification({ nTargetType: 'intern_audit_plan', nTargetKey }))).toBeNull();
  });
  it('keeps unknown and unresolved historical records actionable without inventing a target', () => {
    expect(notificationLink(notification({ nType: 'weekly_top', nTargetType: 'unknown' }))).toBe('/notificaciones/1');
  });
});

it.each(['/perfil/7', '/solicitudes-acceso?request=17', '/notificaciones/3'])('retains notification destination through an expired session: %s', (path) => {
  const login = buildLoginRedirectPath(path);
  const redirect = new URL(login, 'https://tdf.local').searchParams.get('redirect');
  expect(redirect).toBe(path);
  expect(resolvePostAuthPath(null, ['Customer'], [], redirect)).toBe(path);
});
