import { jest } from '@jest/globals';
import { fireEvent, render, screen, waitFor, cleanup } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import i18n from '../i18n';
import { expectNoSeriousAccessibilityViolations } from '../test/accessibility';
const capture = jest.fn();
const submit = jest.fn<() => Promise<void>>();
const category = '10000000-0000-4000-8000-000000000001';
const severity = '10000000-0000-4000-8000-000000000002';
jest.unstable_mockModule('../analytics/useAnalytics', () => ({ useAnalytics: () => ({ capture }) }));
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session: null }) }));
jest.unstable_mockModule('../api/feedback', () => ({ submitFeedback: submit }));
jest.unstable_mockModule('../api/catalogs', () => ({ Catalogs: { listPublicBatch: async () => ({ catalogs: [['feedback-categories', 'feedback-category', category], ['feedback-severities', 'feedback-severity', severity]].map(([code, scopeKind, id]) => ({ catalog: { code }, items: [{ id, active: true, workflowState: 'published' }], defaults: [{ scopeKind, scopeId: 'global', entityId: id }] })) }) } }));
const { default: Page } = await import('../pages/MobileAppPage');
const { default: Promo, DISMISS_KEY, DISMISS_MS, promoDismissed } = await import('./MobilePromoContent');
const { campaignTags, appLink } = await import('./telemetry');
const config = { ios: { status: 'testflight_external', url: 'https://testflight.apple.com/join/7k3VE2JJ', verifiedAt: new Date(Date.now() - 1000).toISOString(), validUntil: new Date(Date.now() + 86400000).toISOString(), capacity: 'available' }, android: { status: 'unavailable', verifiedAt: '2026-01-01', validUntil: '2027-01-01' } };
function mount(element: React.ReactNode) { return render(<QueryClientProvider client={new QueryClient({ defaultOptions: { queries: { retry: false } } })}><MemoryRouter>{element}</MemoryRouter></QueryClientProvider>); }
beforeEach(async () => { await i18n.changeLanguage('es'); localStorage.clear(); capture.mockClear(); submit.mockReset(); submit.mockResolvedValue(); global.fetch = jest.fn<typeof fetch>().mockResolvedValue({ ok: true, json: async () => config } as Response); });
afterEach(cleanup);
it('shows unavailable Android honestly, keeps iOS selectable and translates', async () => {
  mount(<Page />);
  expect(screen.getByText('Todavía no tenemos acceso abierto confirmado para esta plataforma.')).toBeTruthy();
  fireEvent.click(screen.getByRole('button', { name: 'iPhone / iOS' }));
  expect(await screen.findByRole('link', { name: 'Probar beta en TestFlight' })).toBeTruthy();
  fireEvent.click(screen.getByRole('button', { name: 'English' }));
  expect(await screen.findByRole('link', { name: 'Try the beta on TestFlight' })).toBeTruthy();
});
it('collects only consented access requests and never reports admission as installed', async () => {
  mount(<Page />); fireEvent.click(screen.getByRole('button', { name: 'Solicitar acceso' }));
  const send = await screen.findByRole('button', { name: 'Enviar' }); expect((send as HTMLButtonElement).disabled).toBe(true);
  fireEvent.change(screen.getByLabelText(/Correo para coordinar/), { target: { value: 'tester@example.com' } });
  fireEvent.click(screen.getByRole('checkbox'));
  await waitFor(() => expect((send as HTMLButtonElement).disabled).toBe(false)); fireEvent.click(send);
  expect(await screen.findByText(/Solicitud recibida/)).toBeTruthy();
  expect(submit).toHaveBeenCalledTimes(1);
  expect(capture.mock.calls.some(([event]) => event === 'mobile_testing_request_submitted')).toBe(true);
  expect(JSON.stringify(capture.mock.calls)).not.toContain('tester@example.com');
});
it('submits feedback only on successful response and keeps failed input', async () => {
  submit.mockRejectedValueOnce(new Error('private diagnostic'));
  mount(<Page />); fireEvent.click(screen.getByRole('button', { name: /Ya estoy probando/ }));
  fireEvent.change(screen.getByLabelText(/Cuéntanos qué ocurrió/), { target: { value: 'The play button is difficult to find' } });
  fireEvent.click(screen.getByRole('checkbox'));
  await waitFor(() => expect((screen.getByRole('button', { name: 'Enviar' }) as HTMLButtonElement).disabled).toBe(false));
  fireEvent.click(screen.getByRole('button', { name: 'Enviar' }));
  expect(await screen.findByText(/No se pudo enviar/)).toBeTruthy();
  expect((screen.getByLabelText(/Cuéntanos qué ocurrió/) as HTMLInputElement).value).toContain('play button');
  expect(capture.mock.calls.some(([event]) => event === 'mobile_feedback_submitted')).toBe(false);
  fireEvent.click(screen.getByRole('button', { name: 'Enviar' })); expect(await screen.findByText(/Tu comentario fue recibido/)).toBeTruthy();
});
it('persists banner dismissal for 30 days', () => {
  Object.defineProperty(navigator, 'userAgent', { configurable: true, value: 'Android' });
  const view = mount(<Promo surface="mobile_banner" banner />); fireEvent.click(screen.getByRole('button', { name: 'Ahora no' }));
  expect(promoDismissed()).toBe(true); expect(promoDismissed(Date.now() + DISMISS_MS + 1000)).toBe(false);
  expect(localStorage.getItem(DISMISS_KEY)).toBeTruthy(); view.unmount(); mount(<Promo surface="mobile_banner" banner />);
  expect(screen.queryByText('Quiero participar')).toBeNull();
});
it('allowlists campaign labels and excludes PII and capability parameters', () => {
  expect(campaignTags('?utm_source=instagram&utm_campaign=tu_escena&token=secret&utm_medium=person@example.com')).toEqual({ source: 'instagram', campaign: 'tu_escena' });
  expect(appLink('?utm_source=whatsapp&token=secret', 'footer')).toBe('/app?surface=footer&utm_source=whatsapp');
});
it('has accessible landing and feedback semantics', async () => {
  const view = mount(<Page />); await screen.findByRole('button', { name: 'Solicitar acceso' });
  await expectNoSeriousAccessibilityViolations(view.container);
  fireEvent.click(screen.getByRole('button', { name: /Ya estoy probando/ }));
  await screen.findByLabelText(/Cuéntanos qué ocurrió/); await expectNoSeriousAccessibilityViolations(view.container);
});
