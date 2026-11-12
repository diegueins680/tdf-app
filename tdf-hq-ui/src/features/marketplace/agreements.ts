import type { AgreementStatus, MarketplaceAgreement } from '../../api/directory';

export type AgreementRole = MarketplaceAgreement['participantRole'];

/**
 * Transitions a participant may request, mirroring
 * TDF.Directory.Policy.agreementTransitionAllowedFor (ADR 0135): the provider
 * marks the work as started, only the buyer confirms completion, either party
 * may cancel or dispute while the agreement is live, and only an admin
 * resolves a dispute. The server is the authority; this only decides which
 * buttons to show.
 */
export function agreementActions(status: AgreementStatus, role: AgreementRole): AgreementStatus[] {
  if (role === 'admin') return status === 'disputed' ? ['completed', 'cancelled'] : [];
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
