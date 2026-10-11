import { jest } from '@jest/globals';
import { act, fireEvent, render, screen, waitFor, cleanup } from '@testing-library/react';
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
jest.unstable_mockModule('../api/catalogs', () => ({ Catalogs: { listPublicBatch: async () => ({ catalogs: [['feedback-categories', 'feedback-category', category], ['feedback-severities', 'feedback-severity', severity]].map(([code, scopeKind, id]) => ({ catalog: { code }, items: (code === 'feedback-categories' ? ['bug', 'idea', 'ux'] : ['p2', 'p4']).map((itemCode, index) => ({ id: index === 0 ? id : `${id}-${itemCode}`, code: itemCode, active: true, workflowState: 'published' })), defaults: [{ scopeKind, scopeId: 'global', entityId: id }] })) }) } }));
const { default: Page } = await import('../pages/MobileAppPage');
const { default: Promo, DISMISS_KEY, DISMISS_MS, promoDismissed } = await import('./MobilePromoContent');
const { campaignTags, appLink } = await import('./telemetry');
const config = { ios: { status: 'testflight_external', url: 'https://testflight.apple.com/join/7k3VE2JJ', verifiedAt: new Date(Date.now() - 1000).toISOString(), validUntil: new Date(Date.now() + 86400000).toISOString(), capacity: 'available' }, android: { status: 'unavailable', verifiedAt: '2026-01-01', validUntil: '2027-01-01' } };
function mount(element: React.ReactNode) { return render(<QueryClientProvider client={new QueryClient({ defaultOptions: { queries: { retry: false } } })}><MemoryRouter>{element}</MemoryRouter></QueryClientProvider>); }
beforeEach(async () => { await i18n.changeLanguage('es'); localStorage.clear(); capture.mockClear(); submit.mockReset(); submit.mockResolvedValue(); global.fetch = jest.fn<typeof fetch>().mockResolvedValue({ ok: true, json: async () => config } as Response); });
afterEach(cleanup);
it('shows unavailable Android honestly, keeps iOS selectable and translates', async () => {
  mount(<Page />);
  expect(await screen.findByText('Todavía no tenemos acceso abierto confirmado para esta plataforma.')).toBeTruthy();
  fireEvent.click(screen.getByRole('button', { name: 'iPhone / iOS' }));
  expect(await screen.findByRole('link', { name: 'Probar beta en TestFlight' })).toBeTruthy();
  fireEvent.click(screen.getByRole('button', { name: 'English' }));
  expect(await screen.findByRole('link', { name: 'Try the beta on TestFlight' })).toBeTruthy();
});
it('collects only consented access requests and never reports admission as installed', async () => {
  mount(<Page />); fireEvent.click(await screen.findByRole('button', { name: 'Solicitar acceso' }));
  const send = await screen.findByRole('button', { name: 'Enviar' }); expect((send as HTMLButtonElement).disabled).toBe(true);
  fireEvent.change(screen.getByLabelText(/Correo para coordinar/), { target: { value: 'tester@example.com' } });
  fireEvent.click(screen.getByRole('checkbox'));
  await waitFor(() => expect((send as HTMLButtonElement).disabled).toBe(false)); fireEvent.click(send);
  expect(await screen.findByText(/Solicitud recibida/)).toBeTruthy();
  expect(submit).toHaveBeenCalledTimes(1);
  expect(submit).toHaveBeenCalledWith(expect.objectContaining({ categoryId: `${category}-idea`, severityId: `${severity}-p4` }));
  expect(capture.mock.calls.some(([event]) => event === 'mobile_testing_request_submitted')).toBe(true);
  expect(JSON.stringify(capture.mock.calls)).not.toContain('tester@example.com');
});
it('submits feedback only on successful response and keeps failed input', async () => {
  submit.mockRejectedValueOnce(new Error('private diagnostic'));
  mount(<Page />); fireEvent.click(screen.getByRole('button', { name: /Ya estoy probando/ }));
  fireEvent.change(screen.getByLabelText(/Cuéntanos qué ocurrió/), { target: { value: 'The play button is difficult to find' } });
  fireEvent.click(screen.getByRole('checkbox'));
  await waitFor(() => expect(screen.getByRole<HTMLButtonElement>('button', { name: 'Enviar' }).disabled).toBe(false));
  fireEvent.click(screen.getByRole('button', { name: 'Enviar' }));
  expect(await screen.findByText(/No se pudo enviar/)).toBeTruthy();
  expect(screen.getByLabelText<HTMLInputElement>(/Cuéntanos qué ocurrió/).value).toContain('play button');
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

it('waits for the manifest before offering enrollment', async () => {
  let resolve!: (value: Response) => void;
  global.fetch = jest.fn<typeof fetch>().mockReturnValue(new Promise<Response>(done => { resolve = done; }));
  mount(<Page />);
  expect(screen.getByRole('status').textContent).toContain('Comprobando disponibilidad');
  expect(screen.queryByRole('button', { name: 'Solicitar acceso' })).toBeNull();
  fireEvent.click(screen.getByRole('button', { name: 'iPhone / iOS' }));
  resolve({ ok: true, json: async () => config } as Response);
  expect(await screen.findByRole('link', { name: 'Probar beta en TestFlight' })).toBeTruthy();
  expect(screen.queryByRole('textbox', { name: 'Correo para coordinar el acceso' })).toBeNull();
});

it('keeps general comments out of bug triage with the live catalog vocabulary', async () => {
  mount(<Page />);
  fireEvent.click(screen.getByRole('button', { name: /Ya estoy probando/ }));
  fireEvent.mouseDown(screen.getByRole('combobox'));
  fireEvent.click(await screen.findByRole('option', { name: 'Comentario general' }));
  fireEvent.change(screen.getByLabelText(/Cuéntanos qué ocurrió/), { target: { value: 'The first experience felt welcoming' } });
  fireEvent.click(screen.getByRole('checkbox'));
  const send = screen.getByRole<HTMLButtonElement>('button', { name: 'Enviar' });
  await waitFor(() => expect(send.disabled).toBe(false));
  fireEvent.click(send);
  await screen.findByText(/Tu comentario fue recibido/);
  expect(submit).toHaveBeenCalledWith(expect.objectContaining({ categoryId: `${category}-idea`, severityId: `${severity}-p4`, description: expect.stringContaining('kind: general') }));
});

it('uses public store instructions without a beta capacity or opt-in requirement', async () => {
  global.fetch = jest.fn<typeof fetch>().mockResolvedValue({ ok: true, json: async () => ({ ...config, android: { ...config.android, status: 'public', url: 'https://play.google.com/store/apps/details?id=com.tdf.records', verifiedAt: config.ios.verifiedAt, validUntil: config.ios.validUntil } }) } as Response);
  mount(<Page />); fireEvent.click(screen.getByRole('button', { name: 'Android' }));
  expect(await screen.findByRole('link', { name: 'Descargar en Google Play' })).toBeTruthy();
  expect(screen.getByText('Abre Google Play, instala TDF e inicia sesión con tu cuenta de TDF.')).toBeTruthy();
  expect(screen.queryByText(/acepta participar con tu cuenta/)).toBeNull();
});

function closedConfig(overrides = {}) {
  return { ...config, android: { ...config.ios, status: 'closed_testing', admission: 'approval_required', url: 'https://play.google.com/apps/testing/com.tdf.records', ...overrides } };
}
it('lets admitted Android testers open Play without another request, in both languages', async () => {
  global.fetch = jest.fn<typeof fetch>().mockResolvedValue({ ok: true, json: async () => closedConfig() } as Response);
  const view = mount(<Page />);
  const link = await screen.findByRole('link', { name: 'Ya tengo acceso: abrir Google Play' });
  expect(link.getAttribute('href')).toBe('https://play.google.com/apps/testing/com.tdf.records');
  expect(screen.getByText(/Solicita acceso con la cuenta de Google/)).toBeTruthy();
  expect(screen.getByRole('button', { name: 'Solicitar acceso' })).toBeTruthy();
  expect(screen.queryByRole('textbox', { name: /Correo/ })).toBeNull();
  link.addEventListener('click', event => event.preventDefault(), { once: true }); // JSDOM has no external navigation.
  fireEvent.click(link);
  expect(capture).toHaveBeenCalledWith('mobile_testing_join_clicked', expect.objectContaining({ platform: 'android', distribution_status: 'closed_testing', destination: 'testing' }));
  expect(capture.mock.calls.some(([event]) => /installed|request_submitted/.test(String(event)))).toBe(false);
  fireEvent.click(screen.getByRole('button', { name: 'English' }));
  expect(await screen.findByRole('link', { name: 'I already have access: open Google Play' })).toBeTruthy();
  await expectNoSeriousAccessibilityViolations(view.container);
});

it('uses the official store badges only where the store allows them', async () => {
  global.fetch = jest.fn<typeof fetch>().mockResolvedValue({ ok: true, json: async () => closedConfig() } as Response);
  mount(<Page />);
  const play = await screen.findByRole('link', { name: 'Ya tengo acceso: abrir Google Play' });
  expect(play.querySelector('img')?.getAttribute('src')).toBe('/badges/google-play-badge-es.png');
  fireEvent.click(screen.getByRole('button', { name: 'iPhone / iOS' }));
  const testflight = await screen.findByRole('link', { name: 'Probar beta en TestFlight' });
  expect(testflight.querySelector('img')).toBeNull();
});

it('shows the App Store badge once iOS is a public App Store listing, in the page language', async () => {
  global.fetch = jest.fn<typeof fetch>().mockResolvedValue({ ok: true, json: async () => ({ ios: { ...config.ios, status: 'public', url: 'https://apps.apple.com/app/id6779786470' }, android: closedConfig().android }) } as Response);
  mount(<Page />);
  fireEvent.click(screen.getByRole('button', { name: 'iPhone / iOS' }));
  const badgeSrc = (href: string) => document.querySelector(`a[href="${href}"] img`)?.getAttribute('src');
  await waitFor(() => expect(badgeSrc('https://apps.apple.com/app/id6779786470')).toBe('/badges/app-store-badge-es.svg'));
  fireEvent.click(screen.getByRole('button', { name: 'English' }));
  await waitFor(() => expect(badgeSrc('https://apps.apple.com/app/id6779786470')).toBe('/badges/app-store-badge-en.svg'));
  fireEvent.click(screen.getByRole('button', { name: 'Android' }));
  await waitFor(() => expect(document.querySelector('a[href^="https://play.google.com"] img')?.getAttribute('src')).toBe('/badges/google-play-badge-en.png'));
  await act(async () => { await i18n.changeLanguage('fr'); });
  await waitFor(() => expect(document.querySelector('a[href^="https://play.google.com"] img')?.getAttribute('src')).toBe('/badges/google-play-badge-en.png'));
  expect(screen.getByRole('link', { name: 'I already have access: open Google Play' })).toBeTruthy();
});

it.each(['ios', 'android'] as const)('keeps the %s pre-order link as text instead of a download badge', async platform => {
  const url = platform === 'ios' ? 'https://apps.apple.com/app/id6779786470' : 'https://play.google.com/store/apps/details?id=com.tdf.records';
  global.fetch = jest.fn<typeof fetch>().mockResolvedValue({ ok: true, json: async () => ({ ...config, [platform]: { ...config.ios, status: 'store_preorder', url } }) } as Response);
  mount(<Page />);
  fireEvent.click(screen.getByRole('button', { name: platform === 'ios' ? 'iPhone / iOS' : 'Android' }));
  const link = await waitFor(() => { const found = document.querySelector(`a[href="${url}"]`); if (!found) throw new Error('pre-order link missing'); return found; });
  expect(link.querySelector('img')).toBeNull();
  expect(link.textContent).toBe(i18n.t('app.preorder'));
});
it.each([
  { capacity: 'full' }, { capacity: 'unknown' }, { validUntil: new Date(Date.now() - 1).toISOString() },
])('hides admitted tester links when access is not verified: %j', async overrides => {
  global.fetch = jest.fn<typeof fetch>().mockResolvedValue({ ok: true, json: async () => closedConfig(overrides) } as Response);
  mount(<Page />);
  expect(await screen.findByRole('button', { name: 'Solicitar acceso' })).toBeTruthy();
  expect(screen.queryByRole('link', { name: 'Ya tengo acceso: abrir Google Play' })).toBeNull();
});
it('keeps Google Group enrollment distinct from manual email admission', async () => {
  global.fetch = jest.fn<typeof fetch>().mockResolvedValue({ ok: true, json: async () => closedConfig({ enrollmentUrl: 'https://groups.google.com/g/tdf-testers' }) } as Response);
  mount(<Page />);
  expect(await screen.findByRole('link', { name: 'Unirme al grupo de testers' })).toBeTruthy();
  expect(screen.getByRole('link', { name: 'Probar beta en Google Play' })).toBeTruthy();
  expect(screen.queryByRole('link', { name: 'Ya tengo acceso: abrir Google Play' })).toBeNull();
});

it('rechecks expiry at click time before sending an admitted tester away', async () => {
  global.fetch = jest.fn<typeof fetch>().mockResolvedValue({ ok: true, json: async () => closedConfig() } as Response);
  mount(<Page />);
  const link = await screen.findByRole('link', { name: 'Ya tengo acceso: abrir Google Play' });
  const now = jest.spyOn(Date, 'now').mockReturnValue(Date.parse(config.ios.validUntil));
  try {
    expect(fireEvent.click(link)).toBe(false);
    expect(capture.mock.calls.some(([event]) => event === 'mobile_testing_join_clicked')).toBe(false);
    expect(await screen.findByRole('textbox', { name: /Correo para coordinar/ })).toBeTruthy();
  } finally { now.mockRestore(); }
});
