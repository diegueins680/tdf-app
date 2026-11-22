import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, Route, Routes, useNavigate } from 'react-router-dom';
import type { MerchOrder } from '../api/merch';

const report = jest.fn<(...args: unknown[]) => Promise<Record<string, unknown>>>();
const cancel = jest.fn<(...args: unknown[]) => Promise<MerchOrder>>();
const claim = jest.fn<() => Promise<unknown>>();
let language = 'es', hasCapability = true, serial = 0;
let data: MerchOrder;
const read = jest.fn<(id: string, token: string) => Promise<MerchOrder>>();
const initial = (id = 'order-a') => ({ id, orderNumber: id === 'order-a' ? 'SYNTHETIC-A' : 'SYNTHETIC-B', commercialStatus: 'created', paymentStatus: 'pending', fulfillmentStatus: 'pending', refundStatus: 'none', currency: 'USD', totalMinor: 3000, lines: [], timeline: [], issues: [] } as MerchOrder);
jest.unstable_mockModule('../api/merch', () => ({ Merch: { order: read, reportIssue: report, cancelUnpaidOrder: cancel }, createMerchIdempotencyKey: (scope: string) => `${scope}-synthetic-${++serial}`, readStoredMerchOrder: (id: string) => hasCapability ? { id, token: 'synthetic-private-token' } : null }));
jest.unstable_mockModule('../api/merchReputation', () => ({ MerchReputation: { claimBuyer: claim } }));
jest.unstable_mockModule('../session/SessionContext', () => ({ getStoredSessionToken: () => null, getActiveSession: () => null }));
jest.unstable_mockModule('react-i18next', () => ({ useTranslation: () => ({ i18n: { resolvedLanguage: language } }) }));
const { default: Tracking } = await import('./MerchOrderTrackingPage');
let client: QueryClient;
const reason = () => screen.getByRole<HTMLTextAreaElement>('textbox', { name: 'Motivo de cancelación' });
const message = () => screen.getByRole<HTMLTextAreaElement>('textbox', { name: '¿Qué ocurrió?' });
const edit = (field: HTMLElement, value: string) => fireEvent.change(field, { target: { value } });
const submitCancel = () => fireEvent.submit(document.querySelector<HTMLFormElement>('form[aria-label="Cancelar orden sin pagar"]')!);
const submitReport = () => fireEvent.submit(document.querySelector<HTMLFormElement>('form[aria-label="Reportar un problema"]')!);
const confirm = () => fireEvent.click(within(screen.getByRole('dialog')).getByRole('button', { name: 'Cancelar orden sin pagar' }));
function Navigation() { const navigate = useNavigate(); return <button onClick={() => { data = initial('order-b'); navigate('/tienda/orden/order-b'); }}>Other order</button>; }
async function mount() {
  client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  render(<QueryClientProvider client={client}><MemoryRouter initialEntries={['/tienda/orden/order-a']}><Navigation /><Routes><Route path="/tienda/orden/:orderId" element={<Tracking />} /></Routes></MemoryRouter></QueryClientProvider>);
  if (hasCapability) await screen.findByText('SYNTHETIC-A');
  await waitFor(() => expect(client.isFetching()).toBe(0));
}
beforeEach(() => { language = 'es'; hasCapability = true; serial = 0; data = initial(); read.mockReset().mockImplementation(async id => id === data.id ? data : initial(id)); report.mockReset(); cancel.mockReset(); claim.mockReset(); });
afterEach(() => { cleanup(); client.clear(); });

it.each([
  ['cancel', ''], ['cancel', '         '], ['cancel', '123456789'], ['cancel', 'x'.repeat(2001)], ['cancel', 'Una línea\notra línea'], ['cancel', 'Texto\u0085de prueba'],
  ['report', ''], ['report', '123456789'], ['report', 'x'.repeat(5001)], ['report', 'Un párrafo\notro párrafo'],
])('keeps invalid %s text and blocks direct submit without a request or confirmation', async (kind, value) => {
  await mount(); const input = kind === 'cancel' ? reason() : message(); edit(input, value);
  expect(input.getAttribute('aria-invalid')).toBe('false');
  if (kind === 'cancel') submitCancel(); else submitReport();
  expect(input.value).toBe(value); expect(input.getAttribute('aria-invalid')).toBe('true'); expect(document.activeElement).toBe(input);
  const helper = input.getAttribute('aria-describedby'); expect(document.getElementById(helper!)?.textContent).toBeTruthy();
  expect(cancel).not.toHaveBeenCalled(); expect(report).not.toHaveBeenCalled(); expect(screen.queryByRole('dialog')).toBeNull();
});

it('shows help on blur without moving focus and names the order and reason before cancellation', async () => {
  await mount(); act(() => message().focus()); fireEvent.blur(reason()); expect(document.activeElement).toBe(message());
  edit(reason(), '  Motivo sintético de cancelación  '); submitCancel();
  const dialog = screen.getByRole('dialog', { name: '¿Cancelar el pedido SYNTHETIC-A?' });
  expect(dialog.textContent).toContain('Motivo sintético de cancelación'); expect(dialog.textContent).toContain('El stock reservado será liberado. No se puede deshacer.');
  expect(cancel).not.toHaveBeenCalled(); fireEvent.click(within(dialog).getByRole('button', { name: 'Conservar pedido' }));
  await waitFor(() => expect(screen.queryByRole('dialog')).toBeNull()); expect(reason().value).toBe('  Motivo sintético de cancelación  '); expect(cancel).not.toHaveBeenCalled();
});

it('keeps a report and its type/key after rejection, locks concurrent writes and rotates the key only on success', async () => {
  let reject!: (reason: Error) => void;
  report.mockImplementationOnce(() => new Promise((_ok, fail) => { reject = fail; }));
  await mount(); edit(message(), '  Mensaje sintético de soporte  ');
  fireEvent.mouseDown(screen.getByRole('combobox')); fireEvent.click(screen.getByRole('option', { name: 'Reembolso' }));
  submitReport(); await waitFor(() => expect(message().disabled).toBe(true)); expect(reason().disabled).toBe(true); expect(screen.getByRole('combobox').getAttribute('aria-disabled')).toBe('true');
  submitReport(); submitCancel(); expect(report).toHaveBeenCalledTimes(1); expect(cancel).not.toHaveBeenCalled();
  await act(async () => reject(new Error('Synthetic report rejection'))); await screen.findByText(/Conservamos tu mensaje/);
  expect(message().value).toBe('  Mensaje sintético de soporte  '); expect(screen.getByRole('combobox').textContent).toContain('Reembolso');
  report.mockResolvedValueOnce({ id: 'synthetic-issue', status: 'open' }); submitReport(); await screen.findByText('La incidencia quedó registrada.');
  expect(report.mock.calls[1]).toEqual(report.mock.calls[0]); expect(report.mock.calls[0]).toEqual(['order-a', 'synthetic-private-token', 'refund', 'Mensaje sintético de soporte', 'issue-synthetic-1']);
  expect(message().value).toBe(''); edit(message(), 'Otro mensaje sintético'); report.mockResolvedValueOnce({}); submitReport(); await waitFor(() => expect(report).toHaveBeenCalledTimes(3));
  expect(report.mock.calls[2]?.[4]).not.toBe(report.mock.calls[0]?.[4]); expect(data.paymentStatus).toBe('pending');
});

it('requires another deliberate confirmation after a rejected cancellation and keeps its exact request', async () => {
  cancel.mockRejectedValueOnce(new Error('Synthetic conflict'));
  await mount(); edit(reason(), '  Motivo sintético sin pagar  '); submitCancel(); confirm();
  await screen.findByText(/Conservamos tu motivo/); await waitFor(() => expect(screen.queryByRole('dialog')).toBeNull());
  expect(reason().value).toBe('  Motivo sintético sin pagar  '); expect(cancel).toHaveBeenCalledTimes(1);
  cancel.mockImplementationOnce(async () => { data = { ...data, commercialStatus: 'cancelled', fulfillmentStatus: 'cancelled' }; return data; });
  await waitFor(() => expect(client.isFetching()).toBe(0)); submitCancel(); expect(cancel).toHaveBeenCalledTimes(1); confirm();
  await screen.findByText('La orden sin pagar fue cancelada y su stock quedó liberado.');
  expect(cancel.mock.calls[1]).toEqual(cancel.mock.calls[0]); expect(cancel.mock.calls[0]).toEqual(['order-a', 'synthetic-private-token', 'Motivo sintético sin pagar', 'cancel-order-synthetic-2']);
  expect(screen.queryByRole('form', { name: 'Cancelar orden sin pagar' })).toBeNull();
});

it('blocks confirmation when refreshed payment state makes cancellation unavailable', async () => {
  await mount(); edit(reason(), 'Motivo sintético de cancelación'); submitCancel();
  await act(async () => client.setQueryData(['merch-order', 'order-a'], { ...data, paymentStatus: 'paid' }));
  await waitFor(() => expect(within(screen.getByRole('dialog')).getByRole<HTMLButtonElement>('button', { name: 'Cancelar orden sin pagar' }).disabled).toBe(true));
  confirm(); expect(cancel).not.toHaveBeenCalled(); expect(screen.getByRole('dialog').textContent).toContain('Revisa el estado actual');
});

it('locks cancellation while pending, ignores dismiss/duplicate submits and keeps the confirmed request', async () => {
  let accept!: (value: MerchOrder) => void; cancel.mockImplementationOnce(() => new Promise(resolve => { accept = resolve; }));
  await mount(); edit(reason(), 'Motivo sintético de cancelación'); submitCancel(); confirm();
  await waitFor(() => expect(screen.getByRole<HTMLButtonElement>('button', { name: 'Cancelando…' }).disabled).toBe(true));
  fireEvent.keyDown(screen.getByRole('dialog'), { key: 'Escape' }); expect(screen.getByRole('dialog')).toBeTruthy();
  submitCancel(); submitReport(); expect(cancel).toHaveBeenCalledTimes(1); expect(report).not.toHaveBeenCalled();
  data = { ...data, commercialStatus: 'cancelled', fulfillmentStatus: 'cancelled' }; await act(async () => accept(data));
  await screen.findByText('La orden sin pagar fue cancelada y su stock quedó liberado.');
});

it('shows a cancellation failure after the latest server state removes the cancellation form', async () => {
  cancel.mockImplementationOnce(async () => { data = { ...data, paymentStatus: 'paid' }; throw new Error('Payment started'); });
  await mount(); edit(reason(), 'Motivo sintético de cancelación'); submitCancel(); confirm();
  await screen.findByText(/Conservamos tu motivo/); await waitFor(() => expect(screen.queryByRole('form', { name: 'Cancelar orden sin pagar' })).toBeNull());
  await screen.findByRole('form', { name: 'Reportar un problema' }); expect(report).not.toHaveBeenCalled();
});

it('keeps drafts when a background read fails and blocks writes until a successful refresh', async () => {
  await mount(); edit(reason(), 'Motivo sintético de cancelación'); edit(message(), 'Mensaje sintético de soporte'); read.mockRejectedValueOnce(new Error('Synthetic network failure'));
  await act(async () => client.invalidateQueries({ queryKey: ['merch-order', 'order-a'] }));
  await screen.findByText(/Estos son los últimos valores cargados/); expect(message().value).toBe('Mensaje sintético de soporte');
  submitReport(); submitCancel(); expect(report).not.toHaveBeenCalled(); expect(cancel).not.toHaveBeenCalled();
  fireEvent.click(screen.getByRole('button', { name: 'Reintentar' })); await waitFor(() => expect(screen.queryByText(/Estos son los últimos valores cargados/)).toBeNull());
  expect(reason().value).toBe('Motivo sintético de cancelación');
});

it('resets private drafts/confirmation at the order boundary and an old response cannot clear a new draft', async () => {
  let accept!: (value: Record<string, unknown>) => void; report.mockImplementationOnce(() => new Promise(resolve => { accept = resolve; }));
  await mount(); edit(message(), 'Mensaje sintético para A'); submitReport(); await waitFor(() => expect(report).toHaveBeenCalledTimes(1));
  fireEvent.click(screen.getByRole('button', { name: 'Other order' })); await screen.findByText('SYNTHETIC-B');
  expect(reason().value).toBe(''); expect(message().value).toBe(''); edit(message(), 'Mensaje sintético para B');
  await act(async () => accept({ id: 'issue-a' })); expect(message().value).toBe('Mensaje sintético para B');
});

it.each(['report', 'cancel'])('accepts the %s Unicode boundary without truncation', async kind => {
  await mount(); const value = '🎵'.repeat(kind === 'report' ? 5000 : 2000);
  if (kind === 'report') { report.mockResolvedValueOnce({}); edit(message(), value); submitReport(); await waitFor(() => expect(report).toHaveBeenCalledTimes(1)); expect(report.mock.calls[0]?.[3]).toBe(value); }
  else { edit(reason(), value); submitCancel(); expect(screen.getByRole('dialog').textContent).toContain(value); expect(cancel).not.toHaveBeenCalled(); }
});

it('uses readable English recovery and confirmation labels without exposing a private token', async () => {
  language = 'en'; await mount(); expect(screen.getByRole('heading', { name: 'Updates' })).toBeTruthy();
  edit(screen.getByRole('textbox', { name: 'Reason for cancellation' }), 'Synthetic cancellation reason'); fireEvent.submit(screen.getByRole('form', { name: 'Cancel unpaid order' }));
  expect(screen.getByRole('dialog', { name: 'Cancel order SYNTHETIC-A?' })).toBeTruthy(); expect(screen.getByRole('button', { name: 'Keep order' })).toBeTruthy(); expect(screen.queryByText('Cancelar')).toBeNull();
  expect(screen.queryByText(/synthetic-private-token/)).toBeNull();
});

it('does not fetch or show mutation forms without the original private order access', async () => {
  hasCapability = false; await mount(); expect(screen.getByText('Abre este pedido en el navegador donde lo hiciste o contacta a soporte.')).toBeTruthy(); expect(read).not.toHaveBeenCalled(); expect(screen.queryByRole('form')).toBeNull();
});
