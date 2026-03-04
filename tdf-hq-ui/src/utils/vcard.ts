export interface VCardPayload {
  kind: 'vcard-exchange';
  name?: string | null;
  email?: string | null;
  phone?: string | null;
  partyId?: number | null;
  ts?: number;
}

const normalizeTextField = (value: unknown): string | null => {
  if (typeof value !== 'string') return null;
  const trimmed = value.trim();
  return trimmed === '' ? null : trimmed;
};

const parsePositivePartyId = (value: unknown): number | null => {
  if (typeof value === 'number' && Number.isInteger(value) && value > 0) return value;
  if (typeof value === 'string' && /^\d+$/.test(value.trim())) {
    const parsed = Number.parseInt(value.trim(), 10);
    return parsed > 0 ? parsed : null;
  }
  return null;
};

const normalizeTimestamp = (value: unknown): number | undefined =>
  typeof value === 'number' && Number.isFinite(value) ? value : undefined;

export function buildVCardSharePayload(input: {
  name?: string | null;
  email?: string | null;
  phone?: string | null;
  partyId?: number | null;
}): string {
  const normalize = (value?: string | null) => {
    const trimmed = value?.trim();
    if (!trimmed) return undefined;
    return trimmed;
  };
  const payload: VCardPayload = {
    kind: 'vcard-exchange',
    name: normalize(input.name),
    email: normalize(input.email),
    phone: normalize(input.phone),
    partyId: parsePositivePartyId(input.partyId),
    ts: Date.now(),
  };
  return JSON.stringify(payload);
}

export function parseVCardPayload(raw: string): VCardPayload | null {
  if (!raw || raw.trim().length === 0) return null;
  try {
    const parsed = JSON.parse(raw) as Partial<VCardPayload>;
    if (parsed?.kind !== 'vcard-exchange') return null;
    return {
      kind: 'vcard-exchange',
      name: normalizeTextField(parsed.name),
      email: normalizeTextField(parsed.email),
      phone: normalizeTextField(parsed.phone),
      partyId: parsePositivePartyId(parsed.partyId),
      ts: normalizeTimestamp(parsed.ts),
    };
  } catch {
    return null;
  }
}
