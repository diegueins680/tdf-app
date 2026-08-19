import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';

const createPublicMock = jest.fn();
const createQuoteMock = jest.fn();
const getStorefrontMock = jest.fn();
jest.unstable_mockModule('../api/bookings', () => ({
  Bookings: { createPublic: createPublicMock },
}));
jest.unstable_mockModule('../api/domoQuotes', () => ({
  DomoQuotes: {
    getStorefront: getStorefrontMock,
    createQuote: createQuoteMock,
  },
}));
jest.unstable_mockModule('../api/services', () => ({
  Services: { listPublic: jest.fn(() => Promise.resolve([])) },
}));

const { default: DomoVenuePage } = await import('./DomoVenuePage');

describe('DomoVenuePage pricing truthfulness', () => {
  beforeEach(() => {
    createPublicMock.mockReset();
    createQuoteMock.mockReset();
    getStorefrontMock.mockReset();
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
});
