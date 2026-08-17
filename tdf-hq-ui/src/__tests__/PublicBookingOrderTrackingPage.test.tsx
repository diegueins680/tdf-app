/** @jest-environment jsdom */
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter, Route, Routes } from 'react-router-dom';
import { jest } from '@jest/globals';

const getPublicCheckoutMock = jest.fn<() => Promise<unknown>>();
const confirmPublicDatafastStatusMock = jest.fn<() => Promise<unknown>>();

jest.unstable_mockModule('../api/bookings', () => ({
  loadPublicBookingLookupToken: (bookingId: number) =>
    window.sessionStorage.getItem(`tdf-service-booking-order-lookup:${bookingId}`),
  Bookings: {
    getPublicCheckout: getPublicCheckoutMock,
    confirmPublicDatafastStatus: confirmPublicDatafastStatusMock,
  },
}));

const { default: PublicBookingOrderTrackingPage } = await import('../pages/PublicBookingOrderTrackingPage');

const checkoutFixture = (paymentStatus: string) => ({
  booking: {
    bookingId: 456,
    title: 'Studio booking',
    startsAt: '2030-01-01T17:00:00Z',
    endsAt: '2030-01-01T19:00:00Z',
    status: paymentStatus === 'paid' ? 'Confirmed' : 'Tentative',
    resources: [],
  },
  checkoutId: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
  lookupToken: null,
  paymentStatus,
  fulfillmentStatus: paymentStatus === 'paid' ? 'confirmed' : 'on_hold',
  holdExpiresAt: '2030-01-01T16:15:00Z',
  quote: {
    policyVersion: 'studio-v1',
    currency: 'USD',
    durationMinutes: 120,
    subtotalMinor: 20000,
    taxMinor: 3000,
    totalMinor: 23000,
    depositMinor: 11500,
    balanceMinor: 11500,
    depositBps: 5000,
    termsVersion: 'studio-terms-v1',
  },
  paymentMethods: [],
});

const flush = async () => {
  await act(async () => {
    await new Promise((resolve) => setTimeout(resolve, 0));
  });
};

describe('PublicBookingOrderTrackingPage', () => {
  let container: HTMLDivElement;
  let root: Root;

  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    window.sessionStorage.clear();
    window.sessionStorage.setItem('tdf-service-booking-order-lookup:456', 'lookup-secret');
    getPublicCheckoutMock.mockReset();
    confirmPublicDatafastStatusMock.mockReset();
    container = document.createElement('div');
    document.body.appendChild(container);
    root = createRoot(container);
  });

  afterEach(async () => {
    await act(async () => root.unmount());
    container.remove();
  });

  it('treats a Datafast browser return as pending until the server verifies it', async () => {
    confirmPublicDatafastStatusMock.mockResolvedValue(checkoutFixture('processing'));
    await act(async () => {
      root.render(
        <MemoryRouter initialEntries={['/reservas/orden/456?resourcePath=%2Fv1%2Fcheckouts%2Fabc%2Fpayment']}>
          <Routes>
            <Route path="/reservas/orden/:bookingId" element={<PublicBookingOrderTrackingPage />} />
          </Routes>
        </MemoryRouter>,
      );
    });
    await flush();

    expect(confirmPublicDatafastStatusMock).toHaveBeenCalledWith(
      456,
      '/v1/checkouts/abc/payment',
      'lookup-secret',
    );
    expect(container.textContent).toContain('Pago en verificación');
    expect(container.textContent).toContain('no representa un cobro exitoso');
    expect(container.textContent).not.toContain('Depósito verificado');
  });

  it('shows verified wording only for a server-paid checkout', async () => {
    getPublicCheckoutMock.mockResolvedValue(checkoutFixture('paid'));
    await act(async () => {
      root.render(
        <MemoryRouter initialEntries={['/reservas/orden/456']}>
          <Routes>
            <Route path="/reservas/orden/:bookingId" element={<PublicBookingOrderTrackingPage />} />
          </Routes>
        </MemoryRouter>,
      );
    });
    await flush();

    expect(getPublicCheckoutMock).toHaveBeenCalledWith(456, 'lookup-secret');
    expect(container.textContent).toContain('Depósito verificado');
    expect(container.textContent).toContain('Pago: paid');
  });

  it('shows manual evidence review without claiming payment success', async () => {
    getPublicCheckoutMock.mockResolvedValue({
      ...checkoutFixture('awaiting_payment'),
      manualPayment: {
        paymentMethod: 'bank_transfer',
        status: 'under_review',
        submittedAt: '2030-01-01T15:05:00Z',
      },
    });
    await act(async () => {
      root.render(
        <MemoryRouter initialEntries={['/reservas/orden/456']}>
          <Routes>
            <Route path="/reservas/orden/:bookingId" element={<PublicBookingOrderTrackingPage />} />
          </Routes>
        </MemoryRouter>,
      );
    });
    await flush();

    expect(container.textContent).toContain('está bajo revisión financiera');
    expect(container.textContent).toContain('Pago: awaiting_payment');
    expect(container.textContent).not.toContain('Depósito verificado');
  });
});
