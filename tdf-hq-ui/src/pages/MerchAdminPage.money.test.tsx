import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { MerchOperationalIssue, MerchRefundRequest } from '../api/merch';

let language: 'es' | 'en' = 'es';
let issue: MerchOperationalIssue;
const createAdminRefund = jest.fn<(id: string, payload: MerchRefundRequest, key: string) => Promise<unknown>>();
const empty = async () => [];
jest.unstable_mockModule('../api/merch', () => ({
  createMerchIdempotencyKey: () => 'synthetic-refund-key',
  Merch: {
    capabilities: async () => ({ features: { refundOperations: true } }),
    adminStores: empty, adminProducts: empty, adminSettlements: empty, adminRefunds: empty,
    adminIssues: async () => [issue], createAdminRefund,
  },
}));
jest.unstable_mockModule('react-i18next', () => ({ useTranslation: () => ({ i18n: { resolvedLanguage: language } }) }));
const { default: MerchAdminPage } = await import('./MerchAdminPage');
let client: QueryClient;
async function mount() {
  client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  render(<QueryClientProvider client={client}><MerchAdminPage /></QueryClientProvider>);
  return await screen.findByRole('textbox', { name: /^(Monto|Amount) \(USD;/ });
}
const request = () => screen.getByRole<HTMLButtonElement>('button', { name: /^(Registrar solicitud de reembolso|Record refund request)$/ });
beforeEach(() => {
  language = 'es';
  issue = {
    id: 'synthetic-issue', orderId: 'synthetic-order', storeId: 'synthetic-store', orderNumber: 'SYNTHETIC',
    storeName: 'Synthetic store', customerName: 'Synthetic customer', customerEmail: 'synthetic@example.test',
    issueType: 'refund', status: 'staff_review', message: 'Solicitud sintética', currency: 'USD',
    totalMinor: 5000, refundedMinor: 1000, paymentStatus: 'partially_refunded',
    createdAt: '2030-01-01T12:00:00Z', updatedAt: '2030-01-01T12:00:00Z',
  };
  createAdminRefund.mockReset().mockResolvedValue({});
});
afterEach(() => { cleanup(); client.clear(); });

it.each(['es', 'en'] as const)('converts a partial refund in %s and retains request-only guidance', async locale => {
  language = locale;
  const amount = await mount();
  fireEvent.change(amount, { target: { value: '25,50' } });
  fireEvent.click(request());
  await waitFor(() => expect(createAdminRefund).toHaveBeenCalledWith('synthetic-order', expect.objectContaining({ issueId: 'synthetic-issue', amountMinor: 2550 }), 'synthetic-refund-key'));
  expect(screen.getByText(locale === 'en' ? /never contacts the provider or returns money/ : /Nunca contacta al proveedor ni devuelve dinero/)).toBeTruthy();
});

it('keeps a blank amount as the server-resolved full remaining balance', async () => {
  await mount();
  fireEvent.click(request());
  await waitFor(() => expect(createAdminRefund).toHaveBeenCalledWith('synthetic-order', expect.objectContaining({ amountMinor: null }), 'synthetic-refund-key'));
});

it.each(['0', '-1', '40.01', '25.505', '1e2', '90071992547409.92'])('blocks invalid or excessive refund %s without changing the entered amount', async value => {
  const amount = await mount();
  fireEvent.change(amount, { target: { value } });
  expect(amount.getAttribute('aria-invalid')).toBe('true');
  const helper = document.getElementById(amount.getAttribute('aria-describedby')!);
  expect(helper?.textContent).toContain('Saldo verificado restante:');
  expect(helper?.textContent).toContain('40,00');
  expect(request().disabled).toBe(true);
  fireEvent.click(request());
  expect(createAdminRefund).not.toHaveBeenCalled();
  expect((amount as HTMLInputElement).value).toBe(value);
});

it('allows correction at the remaining-balance boundary and retains the retry key after rejection', async () => {
  createAdminRefund.mockRejectedValueOnce(new Error('Synthetic rejection'));
  const amount = await mount();
  fireEvent.change(amount, { target: { value: '40.01' } });
  expect(request().disabled).toBe(true);
  fireEvent.change(amount, { target: { value: '40.00' } });
  fireEvent.click(request());
  await screen.findByText(/La solicitud fue rechazada/);
  fireEvent.click(request());
  await waitFor(() => expect(createAdminRefund).toHaveBeenCalledTimes(2));
  expect(createAdminRefund.mock.calls.every(([, body, key]) => body.amountMinor === 4000 && key === 'synthetic-refund-key')).toBe(true);
});

it('refuses an unsafe stored balance instead of rounding it into refund authority', async () => {
  issue.totalMinor = Number.MAX_SAFE_INTEGER + 1;
  const amount = await mount();
  fireEvent.change(amount, { target: { value: '1.00' } });
  expect(request().disabled).toBe(true);
  expect(screen.getByText('Saldo verificado no disponible.')).toBeTruthy();
  expect(createAdminRefund).not.toHaveBeenCalled();
});
