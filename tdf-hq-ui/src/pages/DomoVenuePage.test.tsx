import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';

const createPublicMock = jest.fn();
const createQuoteMock = jest.fn();
const getStorefrontMock = jest.fn();
const listPublicMock = jest.fn();
jest.unstable_mockModule('../api/bookings', () => ({
  Bookings: { createPublic: createPublicMock },
  getOrCreatePublicBookingIdempotency: async (
    _scope: string,
    payload: unknown,
    current?: { fingerprint: string; key: string } | null,
  ) => current ?? ({ fingerprint: JSON.stringify(payload), key: 'service-booking-test-idempotency' }),
}));
jest.unstable_mockModule('../api/domoQuotes', () => ({
  DomoQuotes: {
    getStorefront: getStorefrontMock,
    createQuote: createQuoteMock,
  },
}));
jest.unstable_mockModule('../api/services', () => ({
  Services: { listPublic: listPublicMock },
}));

const { default: DomoVenuePage } = await import('./DomoVenuePage');

describe('DomoVenuePage pricing truthfulness', () => {
  beforeEach(() => {
    createPublicMock.mockReset();
    createQuoteMock.mockReset();
    getStorefrontMock.mockReset();
    listPublicMock.mockReset();
    listPublicMock.mockResolvedValue([]);
    getStorefrontMock.mockResolvedValue({
      checkoutAvailable: false,
      unavailableReason: 'No approved rate card',
      rateCardVersion: null,
      currency: null,
      eventTypes: [],
      maximumGuests: null,
      maximumDurationHours: null,
      maximumSetupHours: null,
      quoteHoldMinutes: null,
      timezone: 'America/Guayaquil',
    });
  });

  const renderPage = () => {
    const queryClient = new QueryClient({
      defaultOptions: { queries: { retry: false } },
    });
    return render(
      <MemoryRouter>
        <QueryClientProvider client={queryClient}>
          <DomoVenuePage />
        </QueryClientProvider>
      </MemoryRouter>,
    );
  };

  it('does not calculate client-authoritative prices, taxes, or deposits', async () => {
    renderPage();

    expect(screen.getByRole('heading', { name: 'Resumen de solicitud' })).toBeTruthy();
    expect(await screen.findByText(/El precio, impuestos, depósito y políticas vendrán en una cotización versionada/)).toBeTruthy();
    expect(document.body.textContent).not.toContain('Cotización estimada');
    expect(document.body.textContent).not.toContain('IVA 12%');
    expect(document.body.textContent).not.toMatch(/\$\s?\d/);
  });

  it('reports an authoritative quote API failure without inventing a hold or payment', async () => {
    getStorefrontMock.mockResolvedValue({
      checkoutAvailable: true,
      unavailableReason: null,
      rateCardVersion: 'approved-v1',
      currency: 'USD',
      eventTypes: ['wedding'],
      maximumGuests: 220,
      maximumDurationHours: 24,
      maximumSetupHours: 12,
      quoteHoldMinutes: 15,
      timezone: 'America/Guayaquil',
    });
    createQuoteMock.mockRejectedValue(new Error('provider unavailable'));
    renderPage();

    await screen.findByRole('button', { name: 'Cotizar y retener fecha' });
    fireEvent.change(screen.getByRole('textbox', { name: /Nombre/ }), { target: { value: 'Ana' } });
    fireEvent.change(screen.getByRole('textbox', { name: /Correo/ }), { target: { value: 'ana@example.com' } });
    fireEvent.click(screen.getByRole('button', { name: 'Cotizar y retener fecha' }));

    await waitFor(() => expect(screen.getByText(/No pudimos crear ni retener la cotización/)).toBeTruthy());
    expect(document.body.textContent).not.toContain('pago confirmado');
    expect(document.body.textContent).not.toContain('fecha reservada');
    expect(createPublicMock).not.toHaveBeenCalled();
  }, 15_000);

  it('submits Ecuador local time and reuses one key after an ambiguous manual-request failure', async () => {
    listPublicMock.mockResolvedValue([{
      scId: '77777777-7777-4777-8777-777777777778',
      scCode: 'event-production',
      scName: 'Producción de eventos',
      scActive: true,
    }]);
    createPublicMock
      .mockRejectedValueOnce(new Error('No pudimos confirmar la solicitud. Intenta nuevamente.'))
      .mockResolvedValueOnce({ bookingId: 321 });
    const { container } = renderPage();

    await waitFor(() => expect(listPublicMock).toHaveBeenCalledTimes(1));
    const heroVideo = container.querySelector('video');
    expect(heroVideo?.autoplay).toBe(false);
    expect(heroVideo?.getAttribute('preload')).toBe('none');
    expect(screen.getByRole('button', { name: 'Reproducir fondo' }).getAttribute('aria-pressed')).toBe('false');
    const name = screen.getByRole('textbox', { name: /Nombre/ });
    const email = screen.getByRole('textbox', { name: /Correo/ });
    const phone = screen.getByRole('textbox', { name: /WhatsApp/ });
    expect(name.getAttribute('autocomplete')).toBe('name');
    expect(email.getAttribute('autocomplete')).toBe('email');
    expect(phone.getAttribute('autocomplete')).toBe('tel');
    fireEvent.change(name, { target: { value: 'Elena Paredes' } });
    fireEvent.change(email, { target: { value: 'elena@example.test' } });
    fireEvent.change(screen.getByLabelText('Fecha y hora'), { target: { value: '2030-01-15T10:00' } });

    const submit = screen.getByRole('button', { name: 'Enviar solicitud manual' });
    fireEvent.click(submit);
    await screen.findByText('No pudimos confirmar la solicitud. Intenta nuevamente.');
    expect(submit.hasAttribute('disabled')).toBe(false);

    fireEvent.click(submit);
    await screen.findByText(/Solicitud enviada\. Este flujo manual no retiene la fecha ni confirma un pago/);

    expect(createPublicMock).toHaveBeenCalledTimes(2);
    expect(createPublicMock.mock.calls[0]?.[0]).toMatchObject({
      pbStartsAt: '2030-01-15T15:00:00Z',
      pbServiceOfferingId: '77777777-7777-4777-8777-777777777778',
    });
    expect(createPublicMock.mock.calls[1]?.[1]).toBe(createPublicMock.mock.calls[0]?.[1]);
    expect(document.body.textContent).not.toContain('fecha reservada');
    expect(document.body.textContent).not.toContain('pago confirmado');
  });
});
