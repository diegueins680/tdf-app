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
