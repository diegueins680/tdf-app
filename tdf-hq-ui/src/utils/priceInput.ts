// Cents-valued API fields remain exact integers; forms show whole currency units.
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

export const formatPriceCentsInput = (value: number | null | undefined): string => {
  if (value == null) return '';
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error('El precio está fuera del rango permitido.');
  }
  const cents = BigInt(value);
  return `${cents / 100n}.${String(cents % 100n).padStart(2, '0')}`;
};

// Read-only amount ranges can include signed values. Sale-price inputs keep
// their existing nonnegative contract above.
export const parseOptionalSignedAmountCents = (raw: string): number | null => {
  const value = raw.trim();
  if (!value.startsWith('-')) return parseOptionalPriceCents(value);
  const unsigned = value.slice(1);
  if (unsigned.trim() !== unsigned) throw new Error('Ingresa un importe decimal válido.');
  const magnitude = parseOptionalPriceCents(unsigned);
  if (magnitude === null) throw new Error('Ingresa un importe decimal válido.');
  return -magnitude;
};

export const formatSignedAmountCentsInput = (value: number | null | undefined): string => {
  if (value == null) return '';
  return `${value < 0 ? '-' : ''}${formatPriceCentsInput(Math.abs(value))}`;
};
