/** @jest-environment jsdom */
import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import { ApiError } from '../../api/client';
import type { EventTicketSaleDTO } from '../../api/socialEvents';

const getMock = jest.fn<(eventId: string) => Promise<EventTicketSaleDTO>>();
const requestMock = jest.fn<(eventId: string) => Promise<EventTicketSaleDTO>>();
const decideMock = jest.fn<(requestId: string, decision: 'approve' | 'decline') => Promise<EventTicketSaleDTO>>();

jest.unstable_mockModule('../../api/socialEvents', () => ({
  SocialEventsAPI: {
    getTicketSale: (eventId: string) => getMock(eventId),
    requestTicketSale: (eventId: string) => requestMock(eventId),
    decideTicketSaleRequest: (requestId: string, decision: 'approve' | 'decline') => decideMock(requestId, decision),
  },
}));

const { default: EventTicketSaleCard, ticketSaleTermsLines } = await import('./EventTicketSaleCard');

const terms = {
  etstCurrency: 'USD', etstBuyerFeeBps: 200, etstOrganizerFeeBps: 200, etstTaxBps: 0, etstHoldMinutes: 15,
  etstMaxTicketsPerOrder: 100, etstRefundPolicy: 'Reembolso total.', etstTermsSummary: 'Resumen',
};
const sale = (overrides: Partial<EventTicketSaleDTO> = {}): EventTicketSaleDTO => ({
  etsEventId: '300', etsStatus: 'none', etsRequestId: null, etsRequestedAt: null, etsDecidedAt: null,
  etsCanRequest: true, etsBlockedReason: null, etsCanDecide: false, etsTerms: terms, ...overrides,
});
const requested = sale({
  etsStatus: 'requested', etsRequestId: '11111111-1111-4111-8111-111111111111', etsRequestedAt: '2026-11-15T15:00:00Z',
  etsCanRequest: false, etsBlockedReason: 'already_requested',
});

const renderCard = () => {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  return render(
    <QueryClientProvider client={client}>
      <MemoryRouter><EventTicketSaleCard eventId="300" /></MemoryRouter>
    </QueryClientProvider>,
  );
};

describe('EventTicketSaleCard', () => {
  beforeEach(() => {
    getMock.mockReset().mockResolvedValue(sale());
    requestMock.mockReset().mockResolvedValue(requested);
    decideMock.mockReset();
  });

  it('states the fees, refund policy and hold of the standard terms', () => {
    expect(ticketSaleTermsLines(terms).join(' ')).toMatch(/2 %.*2 %.*Reembolso total\..*15 minutos.*100 entradas/);
  });

  it('shows the terms and sends the request', async () => {
    renderCard();
    const button = await screen.findByRole('button', { name: 'Solicitar venta de entradas pagadas' });
    expect(screen.getByLabelText('Condiciones de la venta').textContent).toContain('Reembolso total.');
    fireEvent.click(button);
    expect(await screen.findByText(/Solicitud enviada el 15 de noviembre de 2026/)).toBeTruthy();
    expect(requestMock).toHaveBeenCalledTimes(1);
    expect(requestMock).toHaveBeenCalledWith('300');
    expect(screen.queryByRole('button', { name: 'Solicitar venta de entradas pagadas' })).toBeNull();
  });

  it('asks for a paid ticket type before the request can be sent', async () => {
    getMock.mockResolvedValue(sale({ etsCanRequest: false, etsBlockedReason: 'no_paid_tier' }));
    renderCard();
    expect(await screen.findByText(/Primero crea en «Tickets» un tipo de ticket activo con precio en USD/)).toBeTruthy();
    const button = screen.getByRole('button', { name: 'Solicitar venta de entradas pagadas' });
    expect(button.hasAttribute('disabled')).toBe(true);
  });

  it('lets a strict administrator approve a pending request', async () => {
    getMock.mockResolvedValue({ ...requested, etsCanDecide: true });
    decideMock.mockResolvedValue(sale({
      etsStatus: 'approved', etsRequestId: requested.etsRequestId, etsDecidedAt: '2026-11-16T15:00:00Z',
      etsCanRequest: false, etsBlockedReason: 'already_approved',
    }));
    renderCard();
    fireEvent.click(await screen.findByRole('button', { name: 'Aprobar venta' }));
    expect(await screen.findByText(/Venta aprobada el 16 de noviembre de 2026/)).toBeTruthy();
    expect(decideMock).toHaveBeenCalledWith(requested.etsRequestId, 'approve');
    expect(screen.getByRole('link', { name: 'Abrir la página de entradas' }).getAttribute('href')).toBe('/eventos/300/entradas');
    expect(screen.queryByRole('button', { name: 'Aprobar venta' })).toBeNull();
  });

  it('offers no decision to the organizer and lets them ask again after a decline', async () => {
    getMock.mockResolvedValue(sale({ etsStatus: 'declined', etsRequestId: requested.etsRequestId }));
    renderCard();
    expect(await screen.findByText(/TDF no aprobó la solicitud anterior/)).toBeTruthy();
    expect(screen.queryByRole('button', { name: 'Aprobar venta' })).toBeNull();
    expect(screen.getByRole('button', { name: 'Solicitar venta de entradas pagadas' }).hasAttribute('disabled')).toBe(false);
  });

  it('explains a refused request without hiding the terms', async () => {
    requestMock.mockRejectedValue(new ApiError('conflict', 409));
    renderCard();
    fireEvent.click(await screen.findByRole('button', { name: 'Solicitar venta de entradas pagadas' }));
    expect(await screen.findByText(/Revisa que tenga un tipo de ticket activo con precio en USD/)).toBeTruthy();
    expect(screen.getByLabelText('Condiciones de la venta')).toBeTruthy();
  });

  it('renders nothing for a viewer the server does not admit', async () => {
    getMock.mockRejectedValue(new ApiError('forbidden', 403));
    const { container } = renderCard();
    await waitFor(() => expect(container.querySelector('#venta-entradas')).toBeNull());
  });
});
