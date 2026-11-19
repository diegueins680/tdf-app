/** @jest-environment jsdom */
import { DEVICE_ORDER_LIMIT, listDeviceTicketOrders, ticketOrderStage } from './deviceTicketOrders';

describe('listDeviceTicketOrders', () => {
  beforeEach(() => window.localStorage.clear());

  it('reads the keys the ticket checkout saves, newest order first', () => {
    window.localStorage.setItem('tdf:event-ticket-checkout:41:92', 'secure-lookup-token');
    window.localStorage.setItem('tdf:event-ticket-checkout:7:120', 'second-token');
    expect(listDeviceTicketOrders(window.localStorage)).toEqual([
      { eventId: 7, orderId: 120, lookupToken: 'second-token' },
      { eventId: 41, orderId: 92, lookupToken: 'secure-lookup-token' },
    ]);
  });

  it('ignores other storage, malformed ids and empty keys', () => {
    window.localStorage.setItem('tdf:growth-attribution:v1', '{}');
    window.localStorage.setItem('tdf:event-ticket-checkout:0:5', 'token');
    window.localStorage.setItem('tdf:event-ticket-checkout:abc:5', 'token');
    window.localStorage.setItem('tdf:event-ticket-checkout:3:4:5', 'token');
    window.localStorage.setItem('tdf:event-ticket-checkout:3:6', '   ');
    window.localStorage.setItem('tdf-marketplace-order-lookup:9', 'token');
    expect(listDeviceTicketOrders(window.localStorage)).toEqual([]);
  });

  it('reads at most the newest orders on one visit', () => {
    for (let orderId = 1; orderId <= DEVICE_ORDER_LIMIT + 5; orderId += 1) {
      window.localStorage.setItem(`tdf:event-ticket-checkout:1:${orderId}`, `token-${orderId}`);
    }
    const orders = listDeviceTicketOrders(window.localStorage);
    expect(orders).toHaveLength(DEVICE_ORDER_LIMIT);
    expect(orders[0]?.orderId).toBe(DEVICE_ORDER_LIMIT + 5);
  });

  it('returns nothing when storage is unavailable', () => {
    expect(listDeviceTicketOrders(null)).toEqual([]);
  });
});

describe('ticketOrderStage', () => {
  it('groups guest checkout and account order statuses', () => {
    expect(ticketOrderStage('paid')).toBe('paid');
    expect(ticketOrderStage('partially_refunded')).toBe('paid');
    expect(ticketOrderStage('holding')).toBe('pending');
    expect(ticketOrderStage('awaiting_payment')).toBe('pending');
    expect(ticketOrderStage('processing')).toBe('pending');
    expect(ticketOrderStage(' Pending ')).toBe('pending');
    expect(ticketOrderStage('expired')).toBe('closed');
    expect(ticketOrderStage('refunded')).toBe('closed');
    expect(ticketOrderStage('cancelled')).toBe('closed');
  });
});
