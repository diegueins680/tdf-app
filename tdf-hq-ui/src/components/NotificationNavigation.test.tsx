import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, Route, Routes, useLocation } from 'react-router-dom';
import type { NotificationDTO } from '../api/types';
import type { FeatureAccessRequestDTO } from '../api/accessRequests';
(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
const mark = jest.fn<(id: number) => Promise<void>>();
const markAll = jest.fn<() => Promise<void>>();
const getDetail = jest.fn<() => Promise<{ request: FeatureAccessRequestDTO; canReview: boolean; canCancel: boolean }>>();
const decide = jest.fn();
const profile = jest.fn<(id: number) => Promise<unknown>>();
const getNotification = jest.fn<(id: number) => Promise<NotificationDTO>>();
const list = jest.fn<() => Promise<NotificationDTO[]>>();
const row: NotificationDTO = { nId: 3, nType: 'artist_liked', nTitle: 'Nuevo fan', nBody: 'Galo empezó a seguir tu perfil', nTargetType: 'party_profile', nTargetId: 7, nIsRead: false, nCreatedAt: '2026-09-16T10:00:00Z' };
jest.unstable_mockModule('../api/fans', () => ({ Fans: { getNotification, listNotifications: list, getNotificationCount: async () => ({ ncUnread: 2 }), markNotificationRead: mark, markAllNotificationsRead: markAll } }));
jest.unstable_mockModule('../api/accessRequests', () => ({ AccessRequests: { get: getDetail, decide, cancel: jest.fn() } }));
jest.unstable_mockModule('../api/social', () => ({ SocialAPI: { getProfile: profile, listFriends: async () => [], listFollowing: async () => [], listFollowers: async () => [] } }));
jest.unstable_mockModule('../api/radio', () => ({ RadioAPI: { getPresence: async () => null } }));
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session: { partyId: 5 } }), getActiveSession: () => null }));
jest.unstable_mockModule('../contexts/LocalePreferencesContext', () => ({ useLocalePreferences: () => ({ locale: 'es-EC' }) }));
jest.unstable_mockModule('../analytics/posthog', () => ({ getAnalyticsClient: () => ({ capture: jest.fn() }) }));
jest.unstable_mockModule('react-i18next', () => ({ initReactI18next: { type: '3rdParty', init: () => {} }, useTranslation: () => ({ t: (key: string) => key, i18n: { language: 'es' } }) }));
jest.unstable_mockModule('./events/EventRsvpFeed', () => ({ default: () => null }));
const { default: NotificationPage } = await import('../pages/NotificationPage');
const { default: Bell } = await import('./NotificationBell');
const { default: Profile } = await import('../pages/PublicProfilePage');
const { default: Requests } = await import('../pages/AccessRequestsPage');
const flush = () => new Promise<void>((resolve) => setTimeout(resolve, 10));
function Location() { const location = useLocation(); return <output data-location>{location.pathname}{location.search}</output>; }
let root: Root;
let container: HTMLDivElement;
let client: QueryClient;
async function render(path = '/') {
  container = document.createElement('div'); document.body.appendChild(container); root = createRoot(container);
  client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  await act(async () => { root.render(<MemoryRouter initialEntries={[path]}><QueryClientProvider client={client}><Bell /><Location /><Routes><Route path="/notificaciones/:notificationId" element={<NotificationPage />} /><Route path="/perfil/:partyId" element={<Profile />} /><Route path="/solicitudes-acceso" element={<Requests />} /><Route path="/" element={null} /></Routes></QueryClientProvider></MemoryRouter>); await flush(); });
  if (path === '/') await act(async () => { (container.querySelector('button')!).click(); await flush(); });
  await act(flush);
}
async function clickLink(options: MouseEventInit = {}) {
  const link = document.querySelector('a[href^="/notificaciones/"]')!;
  expect(link).not.toBeNull();
  await act(async () => { link.dispatchEvent(new MouseEvent('click', { bubbles: true, cancelable: true, button: 0, ...options })); await flush(); });
  await act(flush);
}
beforeEach(() => {
  jest.clearAllMocks(); getNotification.mockResolvedValue(row); list.mockResolvedValue([row]); mark.mockResolvedValue();
  profile.mockResolvedValue({ sppPartyId: 7, sppDisplayName: 'Actual follower', sppBio: 'Public biography' });
  getDetail.mockResolvedValue({ request: { id: 17, requesterPartyId: 7, requesterName: 'Requester', featureId: 'label.ddex.inbox', action: 'view', roleContext: [], moduleContext: [], status: 'pending', reviewerGroup: 'label-reviewers', justification: 'Please review this request', reviewerNotes: null, requestedAt: '2026-09-16T10:00:00Z', updatedAt: '2026-09-16T10:00:00Z', decidedAt: null, cancelledAt: null, expiresAt: null, history: [] }, canReview: true, canCancel: false });
});
afterEach(async () => { if (root) await act(async () => root.unmount()); client?.clear(); container?.remove(); });
it('opening the bell does not read notifications; activation opens the actual follower before read completes', async () => {
  mark.mockImplementation(() => new Promise(() => {}));
  await render(); expect(mark).not.toHaveBeenCalled(); expect(markAll).not.toHaveBeenCalled();
  await clickLink(); expect(container.querySelector('[data-location]')?.textContent).toBe('/perfil/7');
  expect(profile).toHaveBeenCalledWith(7); expect(container.textContent).toContain('Actual follower');
  expect(mark).toHaveBeenCalledWith(3); expect(document.querySelector('h2[tabindex="-1"]')).toBeNull();
});
it('failed read updates do not block navigation or falsely decrement the unread count', async () => {
  mark.mockRejectedValue(new Error('offline')); await render(); await clickLink();
  expect(container.textContent).toContain('Actual follower'); expect(document.body.textContent).toContain('notifications.readError');
  expect(client.getQueryData(['notification-count', 5])).toEqual({ ncUnread: 2 });
});
it('provides a focusable native link and preserves modified-click behavior', async () => {
  await render(); const link = document.querySelector('a[href="/notificaciones/3"]')!;
  link.focus(); expect(document.activeElement).toBe(link); expect(link.getAttribute('role')).not.toBe('button');
  await clickLink({ ctrlKey: true }); expect(container.querySelector('[data-location]')?.textContent).toBe('/');
  expect(mark).not.toHaveBeenCalled();
});
it('opens one request with current details and available review actions without executing them', async () => {
  list.mockResolvedValue([{ ...row, nType: 'access_request_review', nTargetType: 'feature_access_request', nTargetId: 17 }]);
  await render(); await clickLink(); expect(getDetail).toHaveBeenCalledWith(17);
  expect(container.textContent).toContain('Solicitud #17'); expect(container.textContent).toContain('Please review this request');
  expect(container.textContent).toContain('Aprobar para provisión'); expect(decide).not.toHaveBeenCalled();
});
it('keeps an already-handled request visible with its status and no review action', async () => {
  const detail = await getDetail(); getDetail.mockResolvedValue({ ...detail, request: { ...detail.request, status: 'approved', reviewerNotes: 'Already reviewed' } });
  list.mockResolvedValue([{ ...row, nTargetType: 'feature_access_request', nTargetId: 17 }]);
  await render(); await clickLink(); expect(container.textContent).toContain('Aprobada');
  expect(container.textContent).toContain('Already reviewed'); expect(container.textContent).not.toContain('Aprobar para provisión'); expect(decide).not.toHaveBeenCalled();
});
it('withdraws request details after denial and offers an authorized fallback', async () => {
  getDetail.mockRejectedValue(new Error('404'));
  list.mockResolvedValue([{ ...row, nTargetType: 'feature_access_request', nTargetId: 17 }]);
  await render(); await clickLink(); expect(container.textContent).toContain('no está disponible para tu cuenta');
  expect(container.textContent).not.toContain('Please review'); expect(container.querySelector('a[href="/solicitudes-acceso"]')).not.toBeNull();
});

it('direct notification links resolve the owned record and persist read state without delaying navigation', async () => {
  mark.mockRejectedValue(new Error('offline')); await render('/notificaciones/3'); await act(flush);
  expect(getNotification).toHaveBeenCalledWith(3); expect(profile).toHaveBeenCalledWith(7);
  expect(container.textContent).toContain('Actual follower'); expect(document.body.textContent).toContain('notifications.readError');
  expect(mark.mock.calls[0][0]).toBe(3);
});
it('missing or foreign notification links reveal no notification context and do not mark read', async () => {
  getNotification.mockRejectedValue(new Error('404')); await render('/notificaciones/3'); await act(flush);
  expect(container.textContent).toContain('No hay un destino específico'); expect(container.textContent).not.toContain('Galo'); expect(mark).not.toHaveBeenCalled();
});
