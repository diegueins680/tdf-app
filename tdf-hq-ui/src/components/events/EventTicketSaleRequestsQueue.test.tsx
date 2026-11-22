/** @jest-environment jsdom */
import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import { ApiError } from '../../api/client';
import type { EventTicketSaleDTO, EventTicketSaleRequestDTO } from '../../api/socialEvents';

const listMock = jest.fn<() => Promise<EventTicketSaleRequestDTO[]>>();
const decideMock = jest.fn<(requestId: string, decision: 'approve' | 'decline') => Promise<EventTicketSaleDTO>>();

jest.unstable_mockModule('../../api/socialEvents', () => ({
  SocialEventsAPI: {
    listTicketSaleRequests: () => listMock(),
    decideTicketSaleRequest: (requestId: string, decision: 'approve' | 'decline') => decideMock(requestId, decision),
  },
}));

const { default: EventTicketSaleRequestsQueue } = await import('./EventTicketSaleRequestsQueue');

const terms = {
  etstCurrency: 'USD', etstBuyerFeeBps: 200, etstOrganizerFeeBps: 200, etstTaxBps: 0, etstHoldMinutes: 15,
  etstMaxTicketsPerOrder: 100, etstRefundPolicy: 'Reembolso total.', etstTermsSummary: 'Resumen',
};
const pending: EventTicketSaleRequestDTO = {
  etsrRequestId: '11111111-1111-4111-8111-111111111111', etsrEventId: '300', etsrEventTitle: 'Wonderground Vol. 1',
  etsrEventStartsAt: '2026-12-05T01:00:00Z', etsrOrganizerName: 'Sari', etsrRequestedAt: '2026-11-15T15:00:00Z',
  etsrTiers: [{ etstiName: 'General', etstiPriceCents: 1000, etstiCurrency: 'USD', etstiQuantityTotal: 80 }], etsrTerms: terms,
};
const decided = (status: 'approved' | 'declined'): EventTicketSaleDTO => ({
  etsEventId: '300', etsStatus: status, etsRequestId: pending.etsrRequestId, etsCanRequest: false, etsCanDecide: false, etsTerms: terms,
});

const renderQueue = () => {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  return render(
    <QueryClientProvider client={client}>
      <MemoryRouter><EventTicketSaleRequestsQueue /></MemoryRouter>
    </QueryClientProvider>,
  );
};

describe('EventTicketSaleRequestsQueue', () => {
  beforeEach(() => {
    listMock.mockReset().mockResolvedValue([pending]);
    decideMock.mockReset().mockResolvedValue(decided('approved'));
  });

  it('renders nothing while no request is pending or the list is refused', async () => {
    listMock.mockResolvedValue([]);
    const empty = renderQueue();
    await waitFor(() => expect(listMock).toHaveBeenCalledTimes(1));
    expect(empty.container.textContent).toBe('');
    empty.unmount();
    listMock.mockRejectedValue(new ApiError('forbidden', 403));
    const refused = renderQueue();
    await waitFor(() => expect(listMock).toHaveBeenCalledTimes(2));
    expect(refused.container.textContent).toBe('');
  });

  it('lists the event, organizer and prices, and approves from the list', async () => {
    renderQueue();
    expect(await screen.findByText('Wonderground Vol. 1')).toBeTruthy();
    expect(screen.getByText(/Organiza Sari · General: .*10,00.* × 80/)).toBeTruthy();
    expect(screen.getByRole('link', { name: 'Ver evento' }).getAttribute('href')).toBe('/social/eventos/300#venta-entradas');
    listMock.mockResolvedValue([]);
    fireEvent.click(screen.getByRole('button', { name: 'Aprobar' }));
    await waitFor(() => expect(decideMock).toHaveBeenCalledWith(pending.etsrRequestId, 'approve'));
    await waitFor(() => expect(screen.queryByText('Wonderground Vol. 1')).toBeNull());
  });

  it('declines from the list and reports a decision that could not be recorded', async () => {
    decideMock.mockRejectedValue(new ApiError('conflict', 409));
    renderQueue();
    fireEvent.click(await screen.findByRole('button', { name: 'Rechazar' }));
    expect(await screen.findByText(/No se pudo registrar la decisión/)).toBeTruthy();
    expect(decideMock).toHaveBeenCalledWith(pending.etsrRequestId, 'decline');
  });
});
