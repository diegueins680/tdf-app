/** @jest-environment jsdom */
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { jest } from '@jest/globals';

const getCheckoutMock = jest.fn<(eventId: number, orderId: number, token: string) => Promise<unknown>>();
const getStorefrontMock = jest.fn<(eventId: number) => Promise<unknown>>();
const listMyTicketOrdersMock = jest.fn<() => Promise<unknown>>();
let sessionValue: { partyId: number } | null = null;

jest.unstable_mockModule('../api/eventTickets', () => ({
  EventTickets: { getCheckout: getCheckoutMock, getStorefront: getStorefrontMock },
}));
jest.unstable_mockModule('../api/myTicketOrders', () => ({
  listMyTicketOrders: listMyTicketOrdersMock,
}));
jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: sessionValue }),
  getStoredSessionToken: () => null,
}));
jest.unstable_mockModule('../contexts/LocalePreferencesContext', () => ({
  useLocalePreferences: () => ({ locale: 'es-EC', timezone: 'America/Guayaquil' }),
}));

const { default: MyTicketsPage } = await import('./MyTicketsPage');
const { advanceAuthSessionEpoch } = await import('../auth/authEvents');

const guestCheckout = (orderId: number, paymentStatus: string) => ({
  orderId,
  eventId: 41,
  checkoutId: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
  paymentStatus,
  fulfillmentStatus: paymentStatus === 'paid' ? 'issued' : 'seat_held',
  holdExpiresAt: '2030-01-01T16:15:00Z',
  quote: { currency: 'USD', quantity: 2, checkoutTotalMinor: 2400 },
  paymentMethods: [],
  tickets: [],
});

const flush = async () => {
  for (let step = 0; step < 4; step += 1) {
    await act(async () => { await new Promise((resolve) => setTimeout(resolve, 0)); });
  }
};

describe('MyTicketsPage', () => {
  let container: HTMLDivElement;
  let root: Root;
  let client: QueryClient;

  const accountOrder = (code: string) => ({
    ticketOrderId: '150', ticketOrderEventId: '41', ticketOrderQuantity: 1,
    ticketOrderAmountCents: 1200, ticketOrderCurrency: 'USD', ticketOrderStatusValue: 'paid',
    ticketOrderTickets: [{ ticketCode: code, ticketStatus: 'issued' }],
  });

  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    window.localStorage.clear();
    sessionValue = null;
    getCheckoutMock.mockReset();
    getStorefrontMock.mockReset();
    listMyTicketOrdersMock.mockReset();
    client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    getStorefrontMock.mockResolvedValue({
      eventId: 41, title: 'Noche Patch', startsAt: '2030-01-10T01:00:00Z', timezone: 'America/Guayaquil', venueName: 'Domo',
    });
    container = document.createElement('div');
    document.body.appendChild(container);
    root = createRoot(container);
  });

  afterEach(async () => {
    await act(async () => root.unmount());
    client.clear();
    container.remove();
  });

  const render = async () => {
    await act(async () => {
      root.render(
        <QueryClientProvider client={client}>
          <MemoryRouter><MyTicketsPage /></MemoryRouter>
        </QueryClientProvider>,
      );
    });
    await flush();
  };

  const linkTo = (href: string) => container.querySelector(`a[href="${href}"]`);

  it('lists a guest order saved in this browser and reads it with its key', async () => {
    window.localStorage.setItem('tdf:event-ticket-checkout:41:92', 'secure-lookup-token');
    getCheckoutMock.mockResolvedValue(guestCheckout(92, 'awaiting_payment'));
    await render();

    expect(getCheckoutMock).toHaveBeenCalledWith(41, 92, 'secure-lookup-token');
    expect(listMyTicketOrdersMock).not.toHaveBeenCalled();
    expect(container.textContent).toContain('Noche Patch');
    expect(container.textContent).toContain('2 entradas');
    expect(container.textContent).toContain('Pago pendiente');
    expect(linkTo('/eventos/41/orden/92')?.textContent).toBe('Completar pago');
    expect(linkTo('/login?redirect=%2Fmis-entradas')).not.toBeNull();
  });

  it('shows an empty state with the events list when nothing is saved', async () => {
    await render();
    expect(getCheckoutMock).not.toHaveBeenCalled();
    expect(container.textContent).toContain('Todavía no tienes entradas aquí.');
    expect(linkTo('/social/eventos')?.textContent).toBe('Ver eventos');
  });

  it('says when a saved order could not be read, without inventing its status', async () => {
    window.localStorage.setItem('tdf:event-ticket-checkout:41:92', 'revoked-token');
    getCheckoutMock.mockRejectedValue(new Error('404'));
    await render();
    expect(container.textContent).toContain('No pudimos leer una compra guardada');
    expect(container.textContent).not.toContain('Pagada');
  });

  it('merges account orders with browser orders once signed in', async () => {
    sessionValue = { partyId: 7 };
    window.localStorage.setItem('tdf:event-ticket-checkout:41:92', 'secure-lookup-token');
    getCheckoutMock.mockResolvedValue(guestCheckout(92, 'paid'));
    listMyTicketOrdersMock.mockResolvedValue([
      {
        ticketOrderId: '92', ticketOrderEventId: '41', ticketOrderQuantity: 2, ticketOrderAmountCents: 2400,
        ticketOrderCurrency: 'USD', ticketOrderStatusValue: 'paid', ticketOrderTickets: [],
        ticketOrderFaceValueCents: 0, ticketOrderBuyerPlatformFeeCents: 0, ticketOrderOrganizerPlatformFeeCents: 0,
      },
      {
        ticketOrderId: '150', ticketOrderEventId: '41', ticketOrderQuantity: 1, ticketOrderAmountCents: 1200,
        ticketOrderCurrency: 'USD', ticketOrderStatusValue: 'paid',
        ticketOrderTickets: [{ ticketCode: 'TDF-ABC123', ticketStatus: 'issued' }],
        ticketOrderFaceValueCents: 0, ticketOrderBuyerPlatformFeeCents: 0, ticketOrderOrganizerPlatformFeeCents: 0,
      },
    ]);
    await render();

    expect(listMyTicketOrdersMock).toHaveBeenCalledTimes(1);
    expect(container.querySelectorAll('h2')).toHaveLength(2);
    expect(linkTo('/eventos/41/orden/92')?.textContent).toBe('Ver la orden');
    expect(linkTo('/eventos/41')?.textContent).toBe('Ver el evento');
    expect(container.textContent).toContain('TDF-ABC123');
    expect(linkTo('/login?redirect=%2Fmis-entradas')).toBeNull();
  });

  it('removes cached account ticket codes when the authoritative read is denied', async () => {
    sessionValue = { partyId: 7 };
    listMyTicketOrdersMock.mockResolvedValue([accountOrder('TDF-REVOKED')]);
    await render();
    expect(container.textContent).toContain('TDF-REVOKED');
    listMyTicketOrdersMock.mockRejectedValue(new Error('403'));
    await act(async () => { await client.invalidateQueries({ queryKey: ['my-ticket-orders'] }); });
    await flush();
    expect(container.textContent).toContain('No pudimos cargar las entradas de tu cuenta.');
    expect(container.textContent).not.toContain('TDF-REVOKED');
    expect(container.textContent).not.toContain('Todavía no tienes entradas aquí.');
  });

  it('removes a cached browser order after its private key is refused', async () => {
    window.localStorage.setItem('tdf:event-ticket-checkout:41:92', 'secure-lookup-token');
    getCheckoutMock.mockResolvedValue(guestCheckout(92, 'paid'));
    await render();
    expect(container.textContent).toContain('Pagada');
    getCheckoutMock.mockRejectedValue(new Error('404'));
    await act(async () => { await client.invalidateQueries({ queryKey: ['my-tickets-device-order'] }); });
    await flush();
    expect(container.textContent).toContain('No pudimos leer una compra guardada');
    expect(container.textContent).not.toContain('Pagada');
    expect(linkTo('/eventos/41/orden/92')).toBeNull();
  });

  it('does not reuse an earlier login response for a new login by the same account', async () => {
    let finishOld!: (value: unknown) => void;
    listMyTicketOrdersMock.mockReturnValueOnce(new Promise(resolve => { finishOld = resolve; }));
    sessionValue = { partyId: 7 };
    await render();
    sessionValue = null;
    advanceAuthSessionEpoch();
    await render();
    listMyTicketOrdersMock.mockResolvedValue([accountOrder('TDF-CURRENT')]);
    sessionValue = { partyId: 7 };
    advanceAuthSessionEpoch();
    await render();
    expect(listMyTicketOrdersMock).toHaveBeenCalledTimes(2);
    await act(async () => finishOld([accountOrder('TDF-OLD-SESSION')]));
    await flush();
    expect(container.textContent).toContain('TDF-CURRENT');
    expect(container.textContent).not.toContain('TDF-OLD-SESSION');
  });

  it('does not display a browser response for a different order identity', async () => {
    window.localStorage.setItem('tdf:event-ticket-checkout:41:92', 'secure-lookup-token');
    getCheckoutMock.mockResolvedValue(guestCheckout(93, 'paid'));
    await render();
    expect(container.textContent).toContain('No pudimos leer una compra guardada');
    expect(container.textContent).not.toContain('Pagada');
    expect(linkTo('/eventos/41/orden/92')).toBeNull();
  });

  it('does not invite a second payment while processing remains unconfirmed', async () => {
    window.localStorage.setItem('tdf:event-ticket-checkout:41:92', 'secure-lookup-token');
    getCheckoutMock.mockResolvedValue(guestCheckout(92, 'processing'));
    await render();
    expect(container.textContent).toContain('Pago pendiente');
    expect(container.textContent).not.toContain('Completar pago');
    expect(linkTo('/eventos/41/orden/92')?.textContent).toBe('Ver estado de pago');
  });

  it('lists only issued codes after a partial refund', async () => {
    sessionValue = { partyId: 7 };
    listMyTicketOrdersMock.mockResolvedValue([{
      ...accountOrder('TDF-CURRENT'), ticketOrderStatusValue: 'partially_refunded',
      ticketOrderTickets: [
        { ticketCode: 'TDF-CURRENT', ticketStatus: 'issued' },
        { ticketCode: 'TDF-REFUNDED', ticketStatus: 'refunded' },
      ],
    }]);
    await render();
    expect(container.textContent).toContain('TDF-CURRENT');
    expect(container.textContent).not.toContain('TDF-REFUNDED');
  });
});
