import { createHash } from 'node:crypto';

// Candidate hints never prove identity. Do not union connected components:
// A~B and B~C are two reviews, not evidence that A, B and C are one person.
export function candidateGroups(inventory) {
  if (!Array.isArray(inventory?.parties)
    || inventory.parties.some(p => !Number.isSafeInteger(p.id) || p.id <= 0)
    || new Set(inventory.parties.map(p => p.id)).size !== inventory.parties.length) {
    throw new Error('Inventory must contain unique positive integer Party identifiers');
  }
  const archived = new Set(inventory.archived_party_ids ?? []);
  const groups = new Map();
  for (const field of ['primary_email', 'primary_phone', 'instagram', 'display_name', 'tax_id']) {
    const buckets = new Map();
    for (const party of inventory.parties) {
      if (archived.has(party.id)) continue;
      const value = party[field];
      if (typeof value !== 'string' || !value.trim()) continue;
      // Preserve local-part case, punctuation, accents, prefixes and leading zeros.
      // Trimming is only a candidate hint; original values remain in evidence.
      const key = value.trim();
      buckets.set(key, [...(buckets.get(key) ?? []), party.id]);
    }
    for (const ids of buckets.values()) {
      if (ids.length < 2) continue;
      ids.sort((a, b) => a - b);
      const key = ids.join(',');
      const current = groups.get(key) ?? { member_ids: ids, hints: [], classification: 'review' };
      current.hints.push(field);
      groups.set(key, current);
    }
  }
  return [...groups.values()].sort((a, b) => a.member_ids[0] - b.member_ids[0]).map(group => {
    const members = inventory.parties.filter(p => group.member_ids.includes(p.id));
    const credentialCount = (inventory.credentials ?? []).filter(c => group.member_ids.includes(c.party_id)).length;
    const kinds = new Set(members.map(p => p.is_org));
    return {
      ...group,
      reason: kinds.size > 1 ? 'person-and-organization-boundary'
        : credentialCount > 1 ? 'multiple-credentials-require-ownership-review'
          : 'contact-attributes-do-not-establish-identity',
      credential_count: credentialCount,
    };
  });
}

export function inventorySummary(inventory) {
  const groups = candidateGroups(inventory);
  const recorded = inventory.reconciliation ?? {};
  const unqueued = groups.filter(g => !(inventory.review_cases ?? []).some(c => g.member_ids.every(id => c.member_ids.includes(id))));
  return {
    examined: inventory.parties.length,
    active_records: inventory.parties.length - (inventory.archived_party_ids?.length ?? 0),
    credentials: inventory.credentials?.length ?? 0,
    candidate_groups: groups.length,
    confirmed_groups: recorded.confirmed_groups ?? 0,
    awaiting_review: (recorded.awaiting_review ?? 0) + unqueued.length,
    merges_completed: recorded.merges_completed ?? 0,
    merges_rolled_back: recorded.merges_rolled_back ?? 0,
    records_archived: inventory.archived_party_ids?.length ?? 0,
    active_links: recorded.active_links ?? 0,
    // Identity decisions come from the reviewed ledger, never attribute hints.
    inventory_sha256: createHash('sha256').update(JSON.stringify(inventory)).digest('hex'),
  };
}

export function sqlText(value) {
  if (typeof value !== 'string' || value.includes('\0')) throw new Error('Invalid SQL text');
  // Standard-conforming strings are explicitly enabled by the caller.
  return `'${value.replaceAll("'", "''")}'`;
}

export function assertUuid(value) {
  if (!/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(value ?? '')) {
    throw new Error('Expected UUID');
  }
  return value;
}

export function operatorGroups(inventory, supplied) {
  candidateGroups(inventory); // Validate the source inventory before rendering SQL.
  if (!Array.isArray(supplied)) throw new Error('Expected an array of operator groups');
  const existing = new Set(inventory.parties.map(p => p.id));
  return supplied.map(group => {
    const ids = group.member_ids;
    if (!Array.isArray(ids) || ids.length < 2 || new Set(ids).size !== ids.length
      || ids.some(id => !Number.isSafeInteger(id) || !existing.has(id))) {
      throw new Error('Operator groups require distinct existing integer Party IDs');
    }
    return { member_ids: [...ids].sort((a, b) => a - b), hints: [], reason: 'Operator-specified whole group; authoritative review is still required' };
  });
}
