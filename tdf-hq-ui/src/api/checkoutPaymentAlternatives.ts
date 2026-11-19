import { Bookings } from './bookings';
import { Courses } from './courses';
import { DomoQuotes } from './domoQuotes';
import { EventTickets } from './eventTickets';
import { loadAvailableCheckoutMethods } from './paymentCapabilities';
import { ServiceStorefront } from './serviceStorefront';
import { safePaymentReturnPath } from '../utils/providerPaymentResume';

interface CheckoutReference {
  checkoutId: string;
  lookupToken: string;
  returnPath: string;
}

const closed = new Set(['paid', 'succeeded', 'partially_refunded', 'refunded', 'expired', 'cancelled', 'disputed', 'chargeback']);

/** Read the original order, including its current availability. Never create a replacement order. */
export async function loadCheckoutPaymentAlternatives(reference: CheckoutReference): Promise<string[]> {
  const path = safePaymentReturnPath(reference.returnPath)?.split(/[?#]/)[0];
  if (!path) return [];
  const course = /^\/curso\/([^/]+)\/orden\/([1-9]\d*)$/.exec(path);
  const booking = /^\/reservas\/orden\/([1-9]\d*)$/.exec(path);
  const tickets = /^\/eventos\/([1-9]\d*)\/orden\/([1-9]\d*)$/.exec(path);
  const domo = /^\/domo-del-pululahua\/cotizaciones\/([^/]+)$/.exec(path);
  const service = /^\/mezcla-mastering\/pedido\/([^/]+)$/.exec(path);
  const { lookupToken, checkoutId } = reference;

  if (course) {
    const order = await Courses.getCheckout(decodeURIComponent(course[1]!), Number(course[2]), lookupToken);
    return order.checkoutId === checkoutId && order.checkoutAvailable && !closed.has(order.paymentStatus)
      ? order.paymentMethods : [];
  }
  if (booking) {
    const order = await Bookings.getPublicCheckout(Number(booking[1]), lookupToken);
    // This tracking screen currently executes hosted methods only.
    return order.checkoutId === checkoutId && !closed.has(order.paymentStatus)
      ? order.paymentMethods.filter((method) => method.startsWith('placetopay_') || method.startsWith('payphone_')) : [];
  }
  if (tickets) {
    const order = await EventTickets.getCheckout(Number(tickets[1]), Number(tickets[2]), lookupToken);
    return order.checkoutId === checkoutId && !closed.has(order.paymentStatus) ? order.paymentMethods : [];
  }
  if (domo) {
    const order = await DomoQuotes.getQuote(domo[1]!, lookupToken);
    return order.checkoutId === checkoutId && order.quoteStatus === 'deposit_due' && !closed.has(order.paymentStatus)
      ? order.paymentMethods : [];
  }
  if (service) {
    const order = await ServiceStorefront.getOrder(decodeURIComponent(service[1]!), lookupToken);
    if (order.ssoCheckoutId !== checkoutId || !['awaiting_payment', 'payment_failed'].includes(order.ssoStatus)) return [];
    const available = await loadAvailableCheckoutMethods({
      currency: order.ssoCurrency, amountMinor: order.ssoPriceUsdCents, productFlow: 'professional_service',
    });
    return [
      ...(available.placeToPayCard ? ['placetopay_card'] : []),
      ...(available.placeToPayBankRedirect ? ['placetopay_bank_redirect'] : []),
      ...(available.placeToPayDeunaQr ? ['placetopay_deuna_qr'] : []),
      ...(available.payPhoneWallet ? ['payphone_wallet'] : []),
    ];
  }
  return [];
}
