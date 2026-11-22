// Guest ticket checkouts keep each order's private lookup key in this browser
// under `tdf:event-ticket-checkout:<eventId>:<orderId>` (PublicEventTicketsPage).
// "Mis entradas" lists those orders without asking the server who the buyer is:
// only a browser that holds the key can read the order.

const DEVICE_ORDER_KEY = /^tdf:event-ticket-checkout:([1-9]\d{0,15}):([1-9]\d{0,15})$/;

/** The most orders read on one visit; older keys stay stored and reachable by their link. */
export const DEVICE_ORDER_LIMIT = 30;

export interface DeviceTicketOrderRef {
  eventId: number;
  orderId: number;
  lookupToken: string;
}

const storageOrNull = (): Storage | null => {
  try {
    return typeof window === 'undefined' ? null : window.localStorage;
  } catch {
    return null;
  }
};

/** Orders this browser can read, newest first (order ids grow over time). */
export function listDeviceTicketOrders(storage: Storage | null = storageOrNull()): DeviceTicketOrderRef[] {
  if (!storage) return [];
  const found: DeviceTicketOrderRef[] = [];
  try {
    for (let index = 0; index < storage.length; index += 1) {
      const key = storage.key(index);
      const match = key ? DEVICE_ORDER_KEY.exec(key) : null;
      if (!key || !match) continue;
      const eventId = Number(match[1]);
      const orderId = Number(match[2]);
      const lookupToken = storage.getItem(key)?.trim() ?? '';
      if (!Number.isSafeInteger(eventId) || !Number.isSafeInteger(orderId) || !lookupToken) continue;
      found.push({ eventId, orderId, lookupToken });
    }
  } catch {
    return [];
  }
  return found.sort((left, right) => right.orderId - left.orderId).slice(0, DEVICE_ORDER_LIMIT);
}

export type TicketOrderStage = 'paid' | 'pending' | 'closed' | 'unknown';

const PENDING_STATUS = /^(holding|awaiting_payment|processing|pending)$/;
const PAID_STATUS = /^(paid|partially_refunded)$/;
const CLOSED_STATUS = /^(expired|refunded|cancelled)$/;

/** Groups payment statuses into what a buyer needs to know: entry, pending payment, or no entry. */
export function ticketOrderStage(status: string): TicketOrderStage {
  const normalized = status.trim().toLowerCase();
  if (PAID_STATUS.test(normalized)) return 'paid';
  if (PENDING_STATUS.test(normalized)) return 'pending';
  if (CLOSED_STATUS.test(normalized)) return 'closed';
  return 'unknown';
}
