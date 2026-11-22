import { get } from './client';
import type { SocialTicketOrderDTO } from './socialEvents';

/**
 * Ticket orders bought with the signed-in account (the app's purchase path).
 * The server scopes the list to the caller; guest web checkouts are not included.
 */
export const listMyTicketOrders = () => get<SocialTicketOrderDTO[]>('/social-events/ticket-orders');
