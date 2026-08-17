/** @jest-environment jsdom */
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter, Route, Routes } from 'react-router-dom';
import { jest } from '@jest/globals';
import type { MarketplaceOrderDTO } from '../api/types';

const getOrderMock = jest.fn<() => Promise<MarketplaceOrderDTO>>();
const submitManualEvidenceMock = jest.fn<() => Promise<MarketplaceOrderDTO>>();

jest.unstable_mockModule('../api/marketplace', () => ({
  loadMarketplaceLookupToken: (orderId: string) =>
    window.sessionStorage.getItem(`tdf-marketplace-order-lookup:${orderId}`),
  Marketplace: {
    getOrder: getOrderMock,
    submitManualEvidence: submitManualEvidenceMock,
  },
}));

jest.unstable_mockModule('../utils/logger', () => ({
  logger: { warn: jest.fn() },
}));

const { default: MarketplaceOrderTrackingPage } = await import('../pages/MarketplaceOrderTrackingPage');

const orderFixture = (manualStatus: string): MarketplaceOrderDTO => ({
  moOrderId: 'order-1',
  moCartId: null,
  moCurrency: 'USD',
  moTotalUsdCents: 10000,
  moTotalDisplay: 'USD $100.00',
  moStatus: 'bank_transfer_pending',
  moStatusHistory: [],
  moBuyerName: '',
  moBuyerEmail: '',
  moBuyerPhone: null,
  moPaymentProvider: 'bank_transfer',
  moPaypalOrderId: null,
  moPaypalPayerEmail: null,
  moPaidAt: null,
  moLookupToken: null,
  moCheckoutStatus: 'awaiting_payment',
  moManualPaymentStatus: manualStatus,
  moManualPaymentSubmittedAt: manualStatus === 'awaiting_evidence' ? null : '2030-01-01T15:05:00Z',
  moOrderKind: 'sale',
  moFulfillmentMethod: 'pickup',
  moFulfillmentStatus: 'on_hold',
  moHoldExpiresAt: '2030-01-01T16:15:00Z',
  moTrackingReference: null,
  moFulfillmentHistory: [],
  moRentalStartDate: null,
  moRentalEndDate: null,
  moRentalDurationDays: null,
  moRentalChargeUsdCents: null,
  moSecurityDepositUsdCents: null,
  moDepositStatus: null,
  moDepositDeductionUsdCents: null,
  moRentalTermsVersion: null,
  moRentalTimezone: null,
  moConditionOut: null,
  moConditionIn: null,
  moCreatedAt: '2030-01-01T15:00:00Z',
  moUpdatedAt: '2030-01-01T15:00:00Z',
  moItems: [{
    moiListingId: 'listing-1',
    moiTitle: 'Vintage Mic',
    moiQuantity: 1,
    moiUnitPriceUsdCents: 10000,
    moiSubtotalCents: 10000,
    moiUnitPriceDisplay: 'USD $100.00',
    moiSubtotalDisplay: 'USD $100.00',
  }],
});

const flush = async () => {
  await act(async () => {
    await new Promise((resolve) => setTimeout(resolve, 0));
  });
};

const setInputValue = (input: HTMLInputElement, value: string) => {
  const descriptor = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value');
  descriptor?.set?.call(input, value);
  input.dispatchEvent(new Event('input', { bubbles: true }));
  input.dispatchEvent(new Event('change', { bubbles: true }));
};

describe('MarketplaceOrderTrackingPage manual payments', () => {
  let container: HTMLDivElement;
  let root: Root;

  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    window.sessionStorage.clear();
    window.sessionStorage.setItem('tdf-marketplace-order-lookup:order-1', 'lookup-secret');
    getOrderMock.mockReset();
    submitManualEvidenceMock.mockReset();
    container = document.createElement('div');
    document.body.appendChild(container);
    root = createRoot(container);
  });

  afterEach(async () => {
    await act(async () => root.unmount());
    container.remove();
  });

  const renderPage = async () => {
    await act(async () => {
      root.render(
        <MemoryRouter initialEntries={['/marketplace/orden/order-1']}>
          <Routes>
            <Route path="/marketplace/orden/:orderId" element={<MarketplaceOrderTrackingPage />} />
          </Routes>
        </MemoryRouter>,
      );
    });
    await flush();
  };

  it('submits the customer reference with the scoped lookup token and keeps payment pending', async () => {
    getOrderMock.mockResolvedValue(orderFixture('awaiting_evidence'));
    submitManualEvidenceMock.mockResolvedValue(orderFixture('submitted'));
    await renderPage();

    const input = container.querySelector<HTMLInputElement>('input');
    expect(input).not.toBeNull();
    await act(async () => {
      setInputValue(input as HTMLInputElement, 'BANK-REFERENCE-1');
    });
    const submit = Array.from(container.querySelectorAll('button')).find((button) =>
      button.textContent?.includes('Enviar evidencia'),
    );
    expect(submit).toBeDefined();
    await act(async () => {
      submit?.click();
      await new Promise((resolve) => setTimeout(resolve, 0));
    });

    expect(submitManualEvidenceMock).toHaveBeenCalledWith(
      'order-1',
      'BANK-REFERENCE-1',
      'lookup-secret',
    );
    expect(container.textContent).toContain('aún no está pagado');
    expect(container.textContent).not.toContain('Pago verificado');
  });

  it('renders a submitted bank reference as awaiting review, never as payment success', async () => {
    getOrderMock.mockResolvedValue(orderFixture('submitted'));
    await renderPage();

    expect(getOrderMock).toHaveBeenCalledWith('order-1', 'lookup-secret');
    expect(container.textContent).toContain('Sigue pendiente de revisión');
    expect(container.textContent).toContain('aún no está pagado');
    expect(container.textContent).not.toContain('Pago verificado');
  });
});
