import { ApiError } from '../../api/client';
import { Directory, type MarketplaceAgreement } from '../../api/directory';
import { createProviderPaymentSession } from '../../api/providerPaymentSessions';
import { loadOrCreatePaymentIdempotencyKey, saveProviderPaymentResume } from '../../utils/providerPaymentResume';

/** Where the buyer comes back to after paying an agreement charge. */
export const AGREEMENT_PAYMENT_RETURN_PATH = '/mis-clasificados?tab=acuerdos';

/**
 * The PayPhone Button page the buyer is sent to (ADR 0136), mirroring the
 * backend's safePayPhoneRedirect: HTTPS on a PayPhone host, no credentials or
 * port in the authority. Anything else is refused and nothing is opened.
 */
export function safePayPhoneRedirect(rawUrl: string | null | undefined): string | null {
  if (!rawUrl) return null;
  try {
    const parsed = new URL(rawUrl);
    const host = parsed.hostname.toLowerCase();
    const payPhoneHost = host === 'payphonetodoesposible.com'
      || host.endsWith('.payphonetodoesposible.com')
      || host === 'payphone.app'
      || host.endsWith('.payphone.app');
    if (parsed.protocol !== 'https:' || parsed.username || parsed.password || parsed.port || !payPhoneHost) return null;
    return parsed.toString();
  } catch {
    return null;
  }
}

/** Whether the buyer can pay this agreement inside the platform right now. */
export function agreementPayableByBuyer(agreement: Pick<MarketplaceAgreement, 'participantRole' | 'settlementMode' | 'status' | 'payoutProviders'>): boolean {
  return agreement.participantRole === 'buyer'
    && agreement.settlementMode === 'platform'
    && (agreement.status === 'pending_deposit' || agreement.status === 'balance_due')
    && (agreement.payoutProviders ?? []).includes('payphone');
}

/** How starting a payment ended when it did not throw. */
export interface AgreementPaymentStart {
  /** The buyer was sent to the PayPhone page. */
  redirected: boolean;
  /** Payments inside the platform are disabled: the server answered 503. */
  notEnabled: boolean;
}

/**
 * Start the charge the agreement is waiting for (ADR 0136) and send the buyer
 * to the PayPhone page. The server freezes the split and creates a canonical
 * checkout; its lookup token is kept only in this tab's recovery record, so
 * the return page can follow the payment. While payments are disabled the
 * server answers 503 and nothing is stored.
 */
export async function startAgreementPayment(
  agreementId: string,
  navigate: (url: string) => void = (url) => window.location.assign(url),
): Promise<AgreementPaymentStart> {
  try {
    const checkout = await Directory.createAgreementPayment(agreementId, 'payphone');
    const idempotencyKey = loadOrCreatePaymentIdempotencyKey(checkout.checkoutId, 'payphone', 'card');
    const session = await createProviderPaymentSession(checkout.checkoutId, checkout.lookupToken, idempotencyKey, {
      provider: 'payphone',
      paymentMethod: 'card',
    });
    saveProviderPaymentResume({
      version: 1,
      checkoutId: checkout.checkoutId,
      attemptId: session.attemptId,
      provider: 'payphone',
      paymentMethod: 'card',
      lookupToken: checkout.lookupToken,
      returnPath: AGREEMENT_PAYMENT_RETURN_PATH,
      createdAt: Date.now(),
    });
    // A missing or foreign page is never opened; the charge stays pending.
    const redirect = safePayPhoneRedirect(session.redirectUrl);
    if (!redirect) return { redirected: false, notEnabled: false };
    navigate(redirect);
    return { redirected: true, notEnabled: false };
  } catch (error) {
    if (error instanceof ApiError && error.status === 503) return { redirected: false, notEnabled: true };
    throw error;
  }
}
