/** @jest-environment jsdom */
import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';

import type { TicketManualPaymentDTO } from '../../api/socialEvents';

const list = jest.fn<() => Promise<TicketManualPaymentDTO[]>>();
const getReceipt = jest.fn<(...args: string[]) => Promise<{ contentType: string; base64: string }>>();
const review = jest.fn();
jest.unstable_mockModule('../../api/socialEvents', () => ({
  SocialEventsAPI: { listManualPayments: list, getManualPaymentReceipt: getReceipt, reviewManualPayment: review },
}));
const { default: EventManualPaymentsPanel } = await import('./EventManualPaymentsPanel');
const payment: TicketManualPaymentDTO = {
  tmpOrderId: '3', tmpPaymentReference: 'TDF-3', tmpAmountMinor: 2000, tmpQuantity: 1,
  tmpCurrency: 'USD', tmpEvidenceStatus: 'submitted', tmpCustomerReference: 'TEST-RECEIPT-123',
  tmpHasReceipt: true, tmpCheckoutStatus: 'awaiting_payment', tmpHoldExpiresAt: '2026-10-25T12:00:00Z',
};
const createObjectURL = jest.fn(() => 'blob:receipt-preview');
const revokeObjectURL = jest.fn();
const clients: QueryClient[] = [];
const mount = async (overrides: Partial<TicketManualPaymentDTO> = {}) => {
  list.mockResolvedValue([{ ...payment, ...overrides }]);
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  clients.push(client);
  render(<QueryClientProvider client={client}><EventManualPaymentsPanel eventId="141" formatDate={(date) => date} formatMoney={() => '$20.00'} /></QueryClientProvider>);
  fireEvent.click(await screen.findByRole('button', { name: 'Comprobante: TEST-RECEIPT-123' }));
};
beforeEach(() => {
  jest.clearAllMocks();
  Object.defineProperty(URL, 'createObjectURL', { configurable: true, value: createObjectURL });
  Object.defineProperty(URL, 'revokeObjectURL', { configurable: true, value: revokeObjectURL });
  getReceipt.mockResolvedValue({ contentType: 'image/png', base64: 'iVBORw0KGgo=' });
});
afterEach(() => { cleanup(); clients.splice(0).forEach((client) => client.clear()); });

it('opens the attached receipt from its number and releases the preview on close', async () => {
  await mount();
  const image = await screen.findByAltText('Comprobante de transferencia TEST-RECEIPT-123');
  expect(image.getAttribute('src')).toBe('blob:receipt-preview');
  expect(getReceipt).toHaveBeenCalledWith('141', '3');
  expect(screen.getByRole('link', { name: 'Descargar comprobante' }).getAttribute('download')).toBe('comprobante-TDF-3.png');
  fireEvent.click(screen.getByRole('button', { name: 'Cerrar' }));
  await waitFor(() => expect(revokeObjectURL).toHaveBeenCalledWith('blob:receipt-preview'));
  expect(review).not.toHaveBeenCalled();
});
it('previews a PDF in the same dialog and offers a download', async () => {
  getReceipt.mockResolvedValue({ contentType: 'application/pdf', base64: 'JVBERi0xLjc=' });
  await mount();
  expect((await screen.findByTitle('Comprobante de transferencia')).getAttribute('src')).toBe('blob:receipt-preview');
  expect(screen.getByRole('link', { name: 'Descargar comprobante' }).getAttribute('download')).toBe('comprobante-TDF-3.pdf');
});
it('explains legacy references without pretending a receipt file exists', async () => {
  await mount({ tmpHasReceipt: false });
  expect(screen.getByText(/No adjuntó una imagen ni un PDF/)).toBeTruthy();
  expect(getReceipt).not.toHaveBeenCalled();
  expect(screen.queryByRole('link', { name: 'Descargar comprobante' })).toBeNull();
});
it('shows an actionable error when the file cannot be retrieved', async () => {
  getReceipt.mockRejectedValue(new Error('Not found'));
  await mount();
  expect(await screen.findByText(/No se pudo abrir el comprobante/)).toBeTruthy();
  expect(screen.queryByRole('link', { name: 'Descargar comprobante' })).toBeNull();
});
it('refuses active content from an unexpected receipt response', async () => {
  getReceipt.mockResolvedValue({ contentType: 'text/html', base64: 'PGgxPg==' });
  await mount();
  expect(await screen.findByText(/No se pudo abrir el comprobante/)).toBeTruthy();
  expect(createObjectURL).not.toHaveBeenCalled();
});
