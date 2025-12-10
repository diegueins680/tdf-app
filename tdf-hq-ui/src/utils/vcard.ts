export interface VCardPayload {
  kind: 'vcard-exchange';
  name?: string | null;
  email?: string | null;
  phone?: string | null;
  partyId?: number | null;
  ts?: number;
}

export function buildVCardSharePayload(input: {
  name?: string | null;
  email?: string | null;
  phone?: string | null;
  partyId?: number | null;
}): string {
  const payload: VCardPayload = {
    kind: 'vcard-exchange',
    name: input.name?.trim() || undefined,
    email: input.email?.trim() || undefined,
    phone: input.phone?.trim() || undefined,
    partyId: typeof input.partyId === 'number' && input.partyId > 0 ? input.partyId : undefined,
    ts: Date.now(),
  };
  return JSON.stringify(payload);
}

export function parseVCardPayload(raw: string): VCardPayload | null {
  if (!raw || raw.trim().length === 0) return null;
  try {
    const parsed = JSON.parse(raw) as Partial<VCardPayload>;
    if (!parsed || parsed.kind !== 'vcard-exchange') return null;
    const partyId = typeof parsed.partyId === 'number' && parsed.partyId > 0 ? parsed.partyId : null;
    return {
      kind: 'vcard-exchange',
      name: parsed.name ?? null,
      email: parsed.email ?? null,
      phone: parsed.phone ?? null,
      partyId,
      ts: parsed.ts,
    };
  } catch {
    return null;
  }
}
