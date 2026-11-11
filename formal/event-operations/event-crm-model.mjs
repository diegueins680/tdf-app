// Bounded executable policy model, independent of SQL/Haskell. No claim of
// implementation refinement, unbounded proof or production qualification.
export function verifyEventCrmModel(mutation = '') {
  const initial = { invitation: 'pending', member: 'none', grants: 0, contacts: 0, links: 0,
    imports: 0, evidence: false, paid: false, refunded: false, net: 0, wrongRecipient: false,
    crossEventWrite: false, revokedWrite: false, bundlePlaces: 0 };
  const key = state => JSON.stringify(state);
  const seen = new Set([key(initial)]), queue = [initial]; let transitions = 0;
  const invariant = s => {
    if (s.grants > 1) throw new Error('single_use');
    if (s.wrongRecipient) throw new Error('recipient_binding');
    if (s.crossEventWrite || s.revokedWrite) throw new Error('current_event_authority');
    if (s.contacts > 1 || s.links > 1) throw new Error('import_idempotency');
    if (s.paid && !s.evidence) throw new Error('payment_evidence');
    if (s.refunded && s.net !== 0) throw new Error('refund_net');
    if (s.bundlePlaces !== 0) throw new Error('unverified_bundle_disabled');
  };
  for (let cursor = 0; cursor < queue.length; cursor++) {
    const s = queue[cursor]; invariant(s); const next = [];
    if (s.invitation === 'pending') {
      next.push({ ...s, invitation: 'accepted', member: 'active', grants: s.grants + 1 });
      next.push({ ...s, invitation: 'expired' }, { ...s, invitation: 'revoked' });
      if (mutation === 'wrong_recipient') next.push({ ...s, wrongRecipient: true });
    }
    if (s.invitation === 'accepted' && mutation === 'non_atomic_consume') next.push({ ...s, grants: 2 });
    if (s.member === 'active') next.push({ ...s, member: 'revoked' });
    if (s.member === 'revoked' && mutation === 'cached_authority') next.push({ ...s, revokedWrite: true });
    if (s.member === 'active' && mutation === 'cross_event') next.push({ ...s, crossEventWrite: true });
    if (s.member === 'active' && s.imports < 2) {
      next.push({ ...s, imports: s.imports + 1,
        contacts: mutation === 'duplicate_import' ? s.contacts + 1 : 1, links: 1 });
      if (mutation === 'spreadsheet_payment') next.push({ ...s, paid: true });
    }
    // Owner/explicit independent verifier and immutable order identity are abstracted
    // as the evidence gate. Replays after a refund preserve the refund state.
    if (!s.evidence) next.push({ ...s, evidence: true });
    if (s.evidence && !s.paid) next.push({ ...s, paid: true, net: 1 });
    if (s.paid && !s.refunded) next.push({ ...s, refunded: true, net: 0 });
    if (s.refunded && mutation === 'capture_after_refund') next.push({ ...s, net: 1 });
    if (mutation === 'bundle_without_joint_hold') next.push({ ...s, bundlePlaces: 1 });
    for (const state of next) {
      transitions++; invariant(state); const id = key(state);
      if (!seen.has(id)) { seen.add(id); queue.push(state); }
    }
  }
  return { states: seen.size, transitions, scope: 'one invitation, two events, two import attempts, one order/refund; sequential serialization abstraction' };
}
