// EVT-TICKET-SALE-001: cache keys shared by the event card and the review queue.
export const ticketSaleQueryKey = (eventId: string) => ['social-event-ticket-sale', eventId] as const;
export const TICKET_SALE_REQUESTS_QUERY_KEY = ['social-event-ticket-sale-requests'] as const;
