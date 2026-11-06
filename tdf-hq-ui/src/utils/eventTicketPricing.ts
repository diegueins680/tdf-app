/** Standard self-service estimate; checkout's current server policy is authoritative. */
export function estimateTicketFees(faceValueCents: number) {
  if (!Number.isSafeInteger(faceValueCents) || faceValueCents < 0) {
    throw new Error('El precio está fuera del rango permitido.');
  }
  const face = BigInt(faceValueCents);
  const total = face * 400n / 10000n;
  const buyer = (total + 1n) / 2n;
  const organizer = total - buyer;
  if (face + buyer > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error('El precio está fuera del rango permitido.');
  }
  return {
    buyer: Number(buyer), organizer: Number(organizer),
    checkout: Number(face + buyer), proceeds: Number(face - organizer),
  };
}

export const parseOptionalPriceCents = (raw: string): number | null => {
  const value = raw.trim();
  if (!value) return null;
  if (!/^\d+(?:[.,]\d{1,2})?$/.test(value)) {
    throw new Error('El precio debe tener como máximo dos decimales.');
  }
  const [whole = '0', fractional = ''] = value.replace(',', '.').split('.');
  const cents = BigInt(whole) * 100n + BigInt(fractional.padEnd(2, '0'));
  if (cents > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error('El precio está fuera del rango permitido.');
  }
  return Number(cents);
};
