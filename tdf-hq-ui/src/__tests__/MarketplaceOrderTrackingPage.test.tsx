/** @jest-environment jsdom */
import { act } from 'react';
import { fireEvent } from '@testing-library/react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter, Route, Routes, useNavigate, type NavigateFunction } from 'react-router-dom';
import { jest } from '@jest/globals';
import type { MarketplaceCustomerRequestDTO, MarketplaceCustomerRequestSubmitPayload, MarketplaceCustomerRequestType, MarketplaceOrderDTO } from '../api/types';

const getOrderMock = jest.fn<(orderId: string, lookupToken: string) => Promise<MarketplaceOrderDTO>>();
const submitManualEvidenceMock = jest.fn<(orderId: string, reference: string, lookupToken: string) => Promise<MarketplaceOrderDTO>>();
const listCustomerRequestsMock = jest.fn<() => Promise<MarketplaceCustomerRequestDTO[]>>();
const submitCustomerRequestMock = jest.fn<(id: string, payload: MarketplaceCustomerRequestSubmitPayload, token: string, key: string) => Promise<MarketplaceCustomerRequestDTO>>();
const clearRequestKey = jest.fn();
const getRequestKey = jest.fn(() => 'marketplace-request-idempotency-0001');

jest.unstable_mockModule('../api/marketplace', () => ({
  loadMarketplaceLookupToken: (orderId: string) =>
    window.sessionStorage.getItem(`tdf-marketplace-order-lookup:${orderId}`),
  getMarketplaceRequestIdempotencyKey: getRequestKey,
  clearMarketplaceRequestIdempotencyKey: clearRequestKey,
  Marketplace: {
    getOrder: getOrderMock,
    submitManualEvidence: submitManualEvidenceMock,
    listCustomerRequests: listCustomerRequestsMock,
    submitCustomerRequest: submitCustomerRequestMock,
  },
}));

jest.unstable_mockModule('../utils/logger', () => ({
  logger: { warn: jest.fn() },
}));

jest.unstable_mockModule('../components/reviews/ExperienceReviews', () => ({
  default: () => null,
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
    listCustomerRequestsMock.mockReset();
    listCustomerRequestsMock.mockResolvedValue([]);
    submitCustomerRequestMock.mockReset();
    clearRequestKey.mockClear(); getRequestKey.mockClear();
    container = document.createElement('div');
    document.body.appendChild(container);
    root = createRoot(container);
  });

  afterEach(async () => {
    await act(async () => root.unmount());
    container.remove();
  });

  const renderPage = async () => {
    let navigate!: NavigateFunction;
    function TrackingRoutes() {
      navigate = useNavigate();
      return <Routes><Route path="/marketplace/orden/:orderId" element={<MarketplaceOrderTrackingPage />} /></Routes>;
    }
    await act(async () => {
      root.render(<MemoryRouter initialEntries={['/marketplace/orden/order-1']}><TrackingRoutes /></MemoryRouter>);
    });
    await flush();
    return { navigate: (path: string) => navigate(path) };
  };

  const referenceInput = () => container.querySelector<HTMLInputElement>('[data-testid="marketplace-reference-form"] input')!;
  const submitReference = () => container.querySelector<HTMLFormElement>('[data-testid="marketplace-reference-form"]')!
    .dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
  const editReference = async (value: string) => {
    await act(async () => { setInputValue(referenceInput(), value); });
  };

  it('associates invalid references with their field, retains the full text and focuses without sending', async () => {
    getOrderMock.mockResolvedValue(orderFixture('awaiting_evidence'));
    await renderPage();
    await act(async () => { submitReference(); });
    const input = referenceInput();
    expect(input).toBe(document.activeElement);
    expect(input.getAttribute('aria-invalid')).toBe('true');
    const help = document.getElementById(input.getAttribute('aria-describedby')!);
    expect(help?.textContent).toContain('3 a 120');
    expect(input.hasAttribute('maxlength')).toBe(false);
    await editReference(' x ');
    expect(input.value).toBe(' x ');
    await act(async () => { submitReference(); });
    await editReference('a'.repeat(121));
    await act(async () => { submitReference(); });
    expect(input.value).toBe('a'.repeat(121));
    await editReference('REF\u202eBANK');
    await act(async () => { submitReference(); });
    expect(help?.textContent).toContain('caracteres no admitidos');
    await editReference('REF\u0001BANK');
    await act(async () => { submitReference(); });
    expect(submitManualEvidenceMock).not.toHaveBeenCalled();
  });

  it('locks a captured Unicode reference, blocks direct duplicate submits and keeps a rejected draft for explicit retry', async () => {
    getOrderMock.mockResolvedValue(orderFixture('rejected'));
    let reject!: (error: Error) => void;
    submitManualEvidenceMock.mockImplementationOnce(() => new Promise((_resolve, rejectRequest) => { reject = rejectRequest; }));
    submitManualEvidenceMock.mockResolvedValueOnce(orderFixture('submitted'));
    await renderPage();
    const raw = '  ' + '𝄞'.repeat(120) + '  ';
    await editReference(raw);
    await act(async () => { submitReference(); submitReference(); });
    expect(submitManualEvidenceMock).toHaveBeenCalledTimes(1);
    expect(submitManualEvidenceMock).toHaveBeenCalledWith('order-1', '𝄞'.repeat(120), 'lookup-secret');
    expect(referenceInput().disabled).toBe(true);
    expect(container.querySelector<HTMLButtonElement>('button[type="submit"]')?.disabled).toBe(true);
    await act(async () => { reject(new Error('synthetic failure')); });
    expect(referenceInput().value).toBe(raw);
    expect(referenceInput().disabled).toBe(false);
    expect(container.textContent).toContain('no fue marcado como pagado');
    expect(submitManualEvidenceMock).toHaveBeenCalledTimes(1);
    await act(async () => { submitReference(); });
    expect(submitManualEvidenceMock).toHaveBeenCalledTimes(2);
    expect(submitManualEvidenceMock.mock.calls[1]).toEqual(submitManualEvidenceMock.mock.calls[0]);
    expect(container.textContent).toContain('Sigue pendiente de revisión');
    expect(container.textContent).toContain('aún no está pagado');
  });

  it('refuses missing access and mismatched responses without losing the reference', async () => {
    getOrderMock.mockResolvedValue(orderFixture('awaiting_evidence'));
    await renderPage();
    await editReference('BANK-REF-VALID');
    window.sessionStorage.removeItem('tdf-marketplace-order-lookup:order-1');
    await act(async () => { submitReference(); });
    expect(submitManualEvidenceMock).not.toHaveBeenCalled();
    expect(container.textContent).toContain('Falta la credencial');
    window.sessionStorage.setItem('tdf-marketplace-order-lookup:order-1', 'lookup-secret');
    submitManualEvidenceMock.mockResolvedValue({ ...orderFixture('submitted'), moOrderId: 'other-order' });
    await act(async () => { submitReference(); });
    expect(referenceInput().value).toBe('BANK-REF-VALID');
    expect(container.textContent).toContain('no fue marcado como pagado');
    expect(container.textContent).not.toContain('Pedido other-order');
  });

  it('gives a different order its own draft while an old evidence response finishes', async () => {
    getOrderMock.mockImplementation(async (id) => ({ ...orderFixture('awaiting_evidence'), moOrderId: id }));
    window.sessionStorage.setItem('tdf-marketplace-order-lookup:order-2', 'second-secret');
    let resolve!: (order: MarketplaceOrderDTO) => void;
    submitManualEvidenceMock.mockImplementationOnce(() => new Promise(done => { resolve = done; }));
    const router = await renderPage();
    await editReference('OLD-BANK-REFERENCE');
    await act(async () => { submitReference(); });
    await act(async () => { router.navigate('/marketplace/orden/order-2'); });
    await flush();
    expect(container.textContent).toContain('Pedido order-2');
    expect(referenceInput().value).toBe('');
    expect(referenceInput().disabled).toBe(false);
    await editReference('NEW-BANK-REFERENCE');
    await act(async () => { resolve(orderFixture('submitted')); });
    expect(container.textContent).toContain('Pedido order-2');
    expect(container.textContent).not.toContain('Pedido order-1');
    expect(referenceInput().value).toBe('NEW-BANK-REFERENCE');
    expect(submitManualEvidenceMock).toHaveBeenCalledTimes(1);
  });

  it('ignores a late lookup from the previous order and never exposes a draft on a paid order', async () => {
    let resolve!: (order: MarketplaceOrderDTO) => void;
    getOrderMock.mockImplementationOnce(() => new Promise(done => { resolve = done; }));
    getOrderMock.mockResolvedValueOnce({ ...orderFixture('approved'), moOrderId: 'order-2', moCheckoutStatus: 'paid', moStatus: 'paid' });
    window.sessionStorage.setItem('tdf-marketplace-order-lookup:order-2', 'second-secret');
    const router = await renderPage();
    await act(async () => { router.navigate('/marketplace/orden/order-2'); });
    await flush();
    await act(async () => { resolve(orderFixture('awaiting_evidence')); });
    expect(container.textContent).toContain('Pedido order-2');
    expect(container.textContent).not.toContain('Pedido order-1');
    expect(container.querySelector('[data-testid="marketplace-reference-form"]')).toBeNull();
    expect(submitManualEvidenceMock).not.toHaveBeenCalled();
  });

  const rentalOrder = () => ({ ...orderFixture('submitted'), moOrderKind: 'rental', moFulfillmentStatus: 'confirmed', moRentalEndDate: '2030-01-31' });
  const requestFormElement = () => container.querySelector<HTMLFormElement>('[data-testid="marketplace-request-form"]')!;
  const reasonInput = () => requestFormElement().querySelector<HTMLTextAreaElement>('textarea')!;
  const dateInput = () => requestFormElement().querySelector<HTMLInputElement>('input[type="date"]')!;
  const submitRequest = () => requestFormElement().dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
  const chooseRequest = async (label: string) => {
    await act(async () => { fireEvent.mouseDown(requestFormElement().querySelector('[role="combobox"]')!); });
    const option = Array.from(document.querySelectorAll<HTMLElement>('[role="option"]')).find(el => el.textContent === label)!;
    await act(async () => { fireEvent.click(option); });
  };
  const editReason = async (value: string) => {
    await act(async () => {
      Object.getOwnPropertyDescriptor(HTMLTextAreaElement.prototype, 'value')?.set?.call(reasonInput(), value);
      reasonInput().dispatchEvent(new Event('input', { bubbles: true }));
    });
  };
  const createdRequest = (type: MarketplaceCustomerRequestType): MarketplaceCustomerRequestDTO => ({
    mcrRequestId: 'request-2', mcrOrderId: 'order-1', mcrOrderKind: 'rental', mcrRequestType: type,
    mcrStatus: 'submitted', mcrReason: '  Motivo original  '.trim(), mcrRequestedEndDate: '2030-02-01',
    mcrEvidenceUrl: null, mcrRequestedAt: '2030-01-01T15:10:00Z', mcrReviewedAt: null, mcrReviewNotes: null,
  });

  it('shows type/reason/date errors without creating a key or request, and keeps overlong text', async () => {
    getOrderMock.mockResolvedValue(rentalOrder()); await renderPage();
    await act(async () => { submitRequest(); });
    expect(requestFormElement().querySelector('[role="combobox"]')).toBe(document.activeElement);
    expect(container.textContent).toContain('Elige un tipo de solicitud');
    await chooseRequest('Solicitar extensión de fechas');
    await editReason('Motivo válido');
    await act(async () => { setInputValue(dateInput(), '2030-01-31'); submitRequest(); });
    expect(dateInput()).toBe(document.activeElement);
    expect(dateInput().getAttribute('aria-invalid')).toBe('true');
    expect(dateInput().getAttribute('min')).toBe('2030-02-01');
    await act(async () => { setInputValue(dateInput(), '2030-02-01'); });
    await editReason('R'.repeat(1001));
    await act(async () => { submitRequest(); });
    expect(reasonInput().value).toBe('R'.repeat(1001));
    expect(reasonInput().hasAttribute('maxlength')).toBe(false);
    expect(reasonInput()).toBe(document.activeElement);
    expect(document.getElementById(reasonInput().getAttribute('aria-describedby')!)?.textContent).toContain('3 a 1000');
    await editReason('Motivo\u202einválido'); await act(async () => { submitRequest(); });
    expect(container.textContent).toContain('caracteres no admitidos');
    expect(getRequestKey).not.toHaveBeenCalled(); expect(submitCustomerRequestMock).not.toHaveBeenCalled();
  });

  it('locks the request, recovers a failed exact attempt and never rotates its key for an edited draft', async () => {
    getOrderMock.mockResolvedValue(rentalOrder());
    let reject!: (error: Error) => void;
    submitCustomerRequestMock.mockImplementationOnce(() => new Promise((_resolve, fail) => { reject = fail; }));
    submitCustomerRequestMock.mockResolvedValueOnce(createdRequest('rental_extension'));
    await renderPage(); await chooseRequest('Solicitar extensión de fechas');
    await editReason('  Motivo original  ');
    await act(async () => { setInputValue(dateInput(), '2030-02-01'); });
    await act(async () => { submitRequest(); submitRequest(); });
    expect(submitCustomerRequestMock).toHaveBeenCalledTimes(1);
    expect(reasonInput().disabled).toBe(true); expect(dateInput().disabled).toBe(true);
    expect(requestFormElement().querySelector('[role="combobox"]')?.getAttribute('aria-disabled')).toBe('true');
    await act(async () => { reject(new Error('unknown outcome')); });
    expect(reasonInput().value).toBe('  Motivo original  '); expect(clearRequestKey).not.toHaveBeenCalled();
    await editReason('Motivo cambiado'); await act(async () => { submitRequest(); });
    expect(submitCustomerRequestMock).toHaveBeenCalledTimes(1); expect(getRequestKey).toHaveBeenCalledTimes(1);
    const restore = Array.from(requestFormElement().querySelectorAll('button')).find(el => el.textContent?.includes('Usar datos del intento anterior'))!;
    await act(async () => { restore.click(); });
    expect(reasonInput().value).toBe('  Motivo original  '); expect(dateInput().value).toBe('2030-02-01');
    expect(submitCustomerRequestMock).toHaveBeenCalledTimes(1);
    await act(async () => { submitRequest(); });
    expect(submitCustomerRequestMock.mock.calls[1]).toEqual(submitCustomerRequestMock.mock.calls[0]);
    expect(clearRequestKey).toHaveBeenCalledWith('order-1', 'rental_extension');
    expect(container.textContent).toContain('no cancela, devuelve ni extiende automáticamente');
    expect(container.textContent).toContain('Solicitar extensión de fechas · En revisión');
  });

  it('keeps a thousand Unicode characters and rejects another order’s response without clearing its key', async () => {
    getOrderMock.mockResolvedValue(rentalOrder());
    submitCustomerRequestMock.mockResolvedValue({ ...createdRequest('rental_cancellation'), mcrOrderId: 'other-order' });
    await renderPage(); await chooseRequest('Cancelar antes de la entrega del equipo');
    await editReason('𝄞'.repeat(1000)); await act(async () => { submitRequest(); });
    expect(submitCustomerRequestMock).toHaveBeenCalledWith('order-1', { mcrsRequestType: 'rental_cancellation', mcrsReason: '𝄞'.repeat(1000) }, 'lookup-secret', 'marketplace-request-idempotency-0001');
    expect(reasonInput().value).toBe('𝄞'.repeat(1000)); expect(clearRequestKey).not.toHaveBeenCalled();
    expect(container.textContent).not.toContain('request-2');
  });

  it('retains the request draft when preparing its stored retry key throws, without sending', async () => {
    getOrderMock.mockResolvedValue(rentalOrder());
    await renderPage(); await chooseRequest('Cancelar antes de la entrega del equipo');
    await editReason('Borrador que debe conservarse');
    getRequestKey.mockImplementationOnce(() => { throw new Error('Synthetic key creation failure'); });
    await act(async () => { submitRequest(); });
    expect(submitCustomerRequestMock).not.toHaveBeenCalled(); expect(clearRequestKey).not.toHaveBeenCalled();
    expect(reasonInput().value).toBe('Borrador que debe conservarse'); expect(reasonInput().disabled).toBe(false);
    expect(container.textContent).toContain('No pudimos confirmar la solicitud');
  });

  it('submits the customer reference with the scoped lookup token and keeps payment pending', async () => {
    getOrderMock.mockResolvedValue(orderFixture('awaiting_evidence'));
    submitManualEvidenceMock.mockResolvedValue(orderFixture('submitted'));
    await renderPage();

    const input = container.querySelector<HTMLInputElement>('input');
    expect(input).not.toBeNull();
    await act(async () => {
      setInputValue(input!, 'BANK-REFERENCE-1');
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

  it('shows persisted customer requests without implying automatic cancellation or refund', async () => {
    getOrderMock.mockResolvedValue({
      ...orderFixture('submitted'),
      moFulfillmentStatus: 'ready_to_fulfill',
    });
    listCustomerRequestsMock.mockResolvedValue([{
      mcrRequestId: 'request-1',
      mcrOrderId: 'order-1',
      mcrOrderKind: 'sale',
      mcrRequestType: 'sale_cancellation',
      mcrStatus: 'submitted',
      mcrReason: 'Ya no necesito el equipo.',
      mcrRequestedEndDate: null,
      mcrEvidenceUrl: null,
      mcrRequestedAt: '2030-01-01T15:10:00Z',
      mcrReviewedAt: null,
      mcrReviewNotes: null,
    }]);
    await renderPage();

    expect(listCustomerRequestsMock).toHaveBeenCalledWith('order-1', 'lookup-secret');
    expect(container.textContent).toContain('Cancelar antes de la entrega · En revisión');
    expect(container.textContent).toContain('no cancela, devuelve ni extiende automáticamente');
    expect(container.textContent).not.toContain('Reembolso confirmado');
  });
});
