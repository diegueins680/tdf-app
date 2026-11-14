import type { AgreementPayment, AgreementStatus, MarketplaceAgreement } from '../../api/directory';

export type AgreementRole = MarketplaceAgreement['participantRole'];

/**
 * Transitions a participant may request, mirroring
 * TDF.Directory.Policy.agreementTransitionAllowedFor (ADR 0135): the provider
 * marks the work as started, only the buyer confirms completion, either party
 * may cancel or dispute while the agreement is live, and only an admin
 * resolves a dispute. ADR 0136: either party may also walk away while the
 * deposit is still unpaid; paying is not a status the participant requests.
 * The server is the authority; this only decides which buttons to show.
 */
export function agreementActions(status: AgreementStatus, role: AgreementRole): AgreementStatus[] {
  if (role === 'admin') return status === 'disputed' ? ['completed', 'cancelled'] : [];
  if (status === 'pending_deposit') return ['cancelled'];
  // A contested or unpaid balance goes to an administrator.
  if (status === 'balance_due') return ['disputed'];
  const live = status === 'confirmed' || status === 'in_progress';
  if (!live) return [];
  const actions: AgreementStatus[] = [];
  if (role === 'provider' && status === 'confirmed') actions.push('in_progress');
  if (role === 'buyer') actions.push('completed');
  actions.push('cancelled', 'disputed');
  return actions;
}

/** Whether the agreement is still open (not completed, cancelled or expired). */
export function isAgreementOpen(status: AgreementStatus): boolean {
  return status !== 'completed' && status !== 'cancelled' && status !== 'expired';
}

/** Percentage label of a rate in basis points (1000 -> "10", 1250 -> "12.5"). */
export function bpsToPercent(bps: number): string {
  return (bps / 100).toFixed(2).replace(/\.?0+$/, '');
}

/**
 * The charge an agreement settled on the platform is waiting for (ADR 0136):
 * the deposit while it is pending_deposit, the balance while it is
 * balance_due, nothing otherwise.
 */
export function agreementChargeDue(agreement: Pick<MarketplaceAgreement, 'status' | 'settlementMode'>): AgreementPayment['kind'] | null {
  if (agreement.settlementMode !== 'platform') return null;
  if (agreement.status === 'pending_deposit') return 'deposit';
  if (agreement.status === 'balance_due') return 'balance';
  return null;
}

/** Charges worth showing: paid ones and the one still pending, oldest first. */
export function visibleAgreementPayments(agreement: Pick<MarketplaceAgreement, 'payments'>): AgreementPayment[] {
  return (agreement.payments ?? []).filter((payment) => payment.status === 'paid' || payment.status === 'pending');
}

/**
 * Shape of a PayPhone payout recipient (ADR 0136), mirroring
 * TDF.Directory.Policy.payoutIdentifierValid: an identification is a cédula
 * (10 digits) or a RUC (13 digits); a phone is in international format. The
 * server is the authority; this only spares a round trip.
 */
export function payoutIdentifierValid(kind: 'identification' | 'phone', identifier: string): boolean {
  if (kind === 'identification') return /^(\d{10}|\d{13})$/.test(identifier);
  return /^\+[1-9]\d{7,14}$/.test(identifier);
}
