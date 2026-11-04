import type { EventWorkspace, EventPlanVersion } from './eventWorkspace';

/** Synthetic data for UI/parser tests only; never seeds an actual event. */
export function workspaceFixture(owner = false): EventWorkspace {
  const plan: EventPlanVersion = {
    event_id: 216, revision: 1, recipient_party_id: 2, created_by: 1,
    document_hash: 'b'.repeat(64), created_at: '2026-11-03T20:00:00Z',
    content: { title: 'Synthetic event plan', overview: 'Synthetic production scope', activities: [],
      budget: [{ label: 'Synthetic production', amountCents: 20000, basis: 'Synthetic fixture' }],
      budgetCapCents: 20000, currency: 'USD', deliverables: ['Synthetic deliverable'], pending: [] },
  };
  return { eventId: '216', owner, plan, planComments: [], agreements: [{
    id: '11111111-1111-4111-8111-111111111111', recipient_party_id: 2,
    current_version: 1, state: 'sent', parent_id: null, acceptance: null, actions: [],
    versions: [{ version: 1, document_hash: 'a'.repeat(64), approved_by: 1,
      approved_at: '2026-11-03T20:01:00Z', sent_at: '2026-11-03T20:02:00Z', created_at: '2026-11-03T20:00:00Z',
      content: { title: 'Synthetic agreement', body: 'Complete synthetic agreement.', recipientLegalName: 'Synthetic Recipient', pendingFields: [], plan } }],
  }] };
}
