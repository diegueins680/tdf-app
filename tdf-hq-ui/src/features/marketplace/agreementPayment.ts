import { ApiError } from '../../api/client';
import { Directory, type MarketplaceAgreement } from '../../api/directory';
import {
  createProviderPaymentSession,
  getProviderPaymentSession,
  type ProviderPaymentSession,
} from '../../api/providerPaymentSessions';
import {
  clearPaymentIdempotencyKey,
  clearProviderPaymentPending,
  clearProviderPaymentResume,
  loadExistingPaymentIdempotencyKey,
  loadOrCreatePaymentIdempotencyKey,
  loadProviderPaymentPending,
  loadProviderPaymentResume,
  saveProviderPaymentPending,
  saveProviderPaymentResume,
} from '../../utils/providerPaymentResume';

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

/** The agreement and the stage whose charge the buyer pays. */
export type AgreementPaymentTarget = Pick<MarketplaceAgreement, 'id' | 'status'>;

/**
 * Where the buyer lands after paying one agreement's charge. The agreement id
 * and its stage scope this tab's recovery records, so a retry reuses only that
 * charge's checkout and a deposit record (even one never marked completed,
 * when the buyer skipped the return page) cannot stand in for the balance. The
 * page itself ignores the extra parameters.
 */
export function agreementPaymentReturnPath(agreement: AgreementPaymentTarget): string {
  return `${AGREEMENT_PAYMENT_RETURN_PATH}&acuerdo=${encodeURIComponent(agreement.id)}`
    + `&cobro=${encodeURIComponent(agreement.status)}`;
}

/** How starting a payment ended when it did not throw. */
export interface AgreementPaymentStart {
  /** The buyer was sent to the PayPhone page. */
  redirected: boolean;
  /** Payments inside the platform are disabled: the server answered 503. */
  notEnabled: boolean;
  /** An earlier attempt may still charge the buyer: it is not paid again. */
  inProgress: boolean;
  /** This tab cannot keep the recovery record, so no payment was started. */
  recoveryUnavailable: boolean;
}

const outcome = (flags: Partial<AgreementPaymentStart>): AgreementPaymentStart => ({
  redirected: false, notEnabled: false, inProgress: false, recoveryUnavailable: false, ...flags,
});

/** The provider is known not to have charged this attempt. */
const sessionEndedWithoutCharge = (session: ProviderPaymentSession): boolean =>
  session.canRetryOrFallback && (session.state === 'failed' || session.state === 'confirmed_no_charge');

/** A definitive refusal before any attempt: the stored checkout is no longer payable. */
const checkoutNoLongerPayable = (error: unknown): boolean =>
  error instanceof ApiError && (error.status === 404 || error.status === 409 || error.status === 410);

/**
 * Start the charge the agreement is waiting for (ADR 0136) and send the buyer
 * to the PayPhone page. The server freezes the split and creates a canonical
 * checkout. Before the session request leaves, this tab records the checkout,
 * its lookup token and the idempotency key, so a lost response or an ambiguous
 * answer is retried as the same request instead of a second charge; once the
 * attempt exists, a retry reopens its page. While payments are disabled the
 * server answers 503 and nothing is stored.
 */
export async function startAgreementPayment(
  agreement: AgreementPaymentTarget,
  navigate: (url: string) => void = (url) => window.location.assign(url),
): Promise<AgreementPaymentStart> {
  const returnPath = agreementPaymentReturnPath(agreement);
  const open = (session: ProviderPaymentSession): AgreementPaymentStart => {
    // A missing or foreign page is never opened; the charge stays pending.
    const redirect = session.state === 'requires_customer_action' ? safePayPhoneRedirect(session.redirectUrl) : null;
    if (redirect) {
      navigate(redirect);
      return outcome({ redirected: true });
    }
    return outcome({ inProgress: !sessionEndedWithoutCharge(session) });
  };
  const forget = (checkoutId: string) => {
    clearProviderPaymentPending(checkoutId);
    clearProviderPaymentResume(checkoutId);
    clearPaymentIdempotencyKey(checkoutId, 'payphone', 'card');
  };
  try {
    // An attempt this tab already started: reopen it rather than start another.
    const resume = loadProviderPaymentResume(undefined, returnPath);
    if (resume) {
      const session = await getProviderPaymentSession(resume.checkoutId, resume.attemptId, resume.lookupToken);
      if (!sessionEndedWithoutCharge(session)) return open(session);
      forget(resume.checkoutId);
    }
    // A session request that may have reached the server: replay it as is.
    const pending = loadProviderPaymentPending(undefined, returnPath);
    let checkout: { checkoutId: string; lookupToken: string } | null = null;
    let idempotencyKey: string | null = null;
    if (pending) {
      idempotencyKey = loadExistingPaymentIdempotencyKey(pending.checkoutId, 'payphone', 'card');
      if (!idempotencyKey) return outcome({ inProgress: true });
      checkout = { checkoutId: pending.checkoutId, lookupToken: pending.lookupToken };
    }
    if (!checkout || !idempotencyKey) {
      const created = await Directory.createAgreementPayment(agreement.id, 'payphone');
      checkout = { checkoutId: created.checkoutId, lookupToken: created.lookupToken };
      idempotencyKey = loadOrCreatePaymentIdempotencyKey(checkout.checkoutId, 'payphone', 'card');
      const recorded = saveProviderPaymentPending({
        version: 1,
        checkoutId: checkout.checkoutId,
        provider: 'payphone',
        paymentMethod: 'card',
        lookupToken: checkout.lookupToken,
        returnPath,
        createdAt: Date.now(),
      });
      if (!recorded) {
        clearPaymentIdempotencyKey(checkout.checkoutId, 'payphone', 'card');
        return outcome({ recoveryUnavailable: true });
      }
    }
    let session: ProviderPaymentSession;
    try {
      session = await createProviderPaymentSession(checkout.checkoutId, checkout.lookupToken, idempotencyKey, {
        provider: 'payphone',
        paymentMethod: 'card',
      });
    } catch (error) {
      // A replay refused outright means the charge was replaced or ended: start over once.
      if (pending && checkoutNoLongerPayable(error)) {
        forget(pending.checkoutId);
        return await startAgreementPayment(agreement, navigate);
      }
      throw error;
    }
    const resumed = saveProviderPaymentResume({
      version: 1,
      checkoutId: checkout.checkoutId,
      attemptId: session.attemptId,
      provider: 'payphone',
      paymentMethod: 'card',
      lookupToken: checkout.lookupToken,
      returnPath,
      createdAt: Date.now(),
    });
    if (resumed) clearProviderPaymentPending(checkout.checkoutId);
    return open(session);
  } catch (error) {
    if (error instanceof ApiError && error.status === 503) return outcome({ notEnabled: true });
    throw error;
  }
}
