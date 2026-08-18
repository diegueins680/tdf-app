import { get, post } from './client';
import type { components } from './generated/types';
import type { DatafastCheckoutDTO, PaypalCreateDTO } from './types';

export type PublicEventTicketTier = components['schemas']['PublicEventTicketTier'];
export type PublicEventTicketStorefront = components['schemas']['PublicEventTicketStorefront'];
export type PublicEventTicketCheckoutRequest = components['schemas']['PublicEventTicketCheckoutRequest'];
export type PublicEventTicketQuote = components['schemas']['PublicEventTicketQuote'];
export type PublicEventTicket = components['schemas']['PublicEventTicket'];
export type PublicEventTicketCheckout = components['schemas']['PublicEventTicketCheckout'];

const requirePositiveInteger = (value: number, field: string): number => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${field} must be a positive integer.`);
  }
  return value;
};

const eventBase = (eventId: number): string =>
  `/public/events/${requirePositiveInteger(eventId, 'eventId')}`;

const orderBase = (eventId: number, orderId: number): string =>
  `${eventBase(eventId)}/ticket-orders/${requirePositiveInteger(orderId, 'orderId')}`;

const lookupHeaders = (lookupToken: string): RequestInit => ({
  headers: { 'X-Order-Lookup-Token': lookupToken },
});

export const EventTickets = {
  getStorefront: (eventId: number) =>
    get<PublicEventTicketStorefront>(`${eventBase(eventId)}/tickets`),
  createCheckout: (
    eventId: number,
    payload: PublicEventTicketCheckoutRequest,
    idempotencyKey: string,
  ) => post<PublicEventTicketCheckout>(
    `${eventBase(eventId)}/ticket-orders`,
    payload,
    { headers: { 'Idempotency-Key': idempotencyKey } },
  ),
  getCheckout: (eventId: number, orderId: number, lookupToken: string) =>
    get<PublicEventTicketCheckout>(orderBase(eventId, orderId), lookupHeaders(lookupToken)),
  createDatafastCheckout: (eventId: number, orderId: number, lookupToken: string) =>
    post<DatafastCheckoutDTO>(
      `${orderBase(eventId, orderId)}/datafast/checkout`,
      {},
      lookupHeaders(lookupToken),
    ),
  confirmDatafastStatus: (
    eventId: number,
    orderId: number,
    resourcePath: string,
    lookupToken: string,
  ) => {
    const search = new URLSearchParams({ resourcePath });
    return get<PublicEventTicketCheckout>(
      `${orderBase(eventId, orderId)}/datafast/status?${search.toString()}`,
      lookupHeaders(lookupToken),
    );
  },
  createPaypalOrder: (eventId: number, orderId: number, lookupToken: string) =>
    post<PaypalCreateDTO>(
      `${orderBase(eventId, orderId)}/paypal/create`,
      {},
      lookupHeaders(lookupToken),
    ),
  capturePaypalOrder: (
    eventId: number,
    orderId: number,
    paypalOrderId: string,
    lookupToken: string,
  ) => post<PublicEventTicketCheckout>(
    `${orderBase(eventId, orderId)}/paypal/capture`,
    { paypalOrderId },
    lookupHeaders(lookupToken),
  ),
};
