import { submitFeedback } from './feedback';
import { loadSessionSnapshot } from './session';

/** Initiates manual fulfilment; never claims that data has already been erased. */
export async function requestAccountDeletion(input: {
  partyId: number;
  categoryId: string;
  severityId: string;
  locale: string;
}): Promise<void> {
  const current = await loadSessionSnapshot();
  if (!current || !Number.isSafeInteger(input.partyId) || input.partyId <= 0 || current.partyId !== input.partyId) {
    throw new Error('Account session changed; authenticate again');
  }
  const locale = input.locale.startsWith('en') ? 'en' : 'es';
  await submitFeedback({
    title: 'TDF — Account deletion request',
    description: [
      'account_deletion_request',
      `requested_account_party_id: ${current.partyId}`,
      `locale: ${locale}`,
      'The account holder explicitly requests deletion of the entire account and associated personal data, including user-generated content, except records legally required to be retained.',
      'Manual fulfilment requested; this submission does not claim completed deletion.',
      'OPERATOR: verify the server-recorded feedbackCreatedBy matches requested_account_party_id before any action. An anonymous or mismatched record does not authorize deletion. Confirm completion through the verified account contact.',
    ].join('\n'),
    categoryId: input.categoryId,
    severityId: input.severityId,
    consent: true,
    contactEmail: /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(current.username) ? current.username : undefined,
  }, { sessionCookieOnly: true });
}
