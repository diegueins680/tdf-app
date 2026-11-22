import { formatPriceCentsInput, parseOptionalPriceCents } from './priceInput';

/** The public course read uses major-unit numbers, while writes use cents. */
export function formatCourseMetadataPriceInput(value: number | null | undefined): string {
  if (value == null) return '';
  if (!Number.isFinite(value)) throw new Error('Precio recibido inválido.');
  const cents = parseOptionalPriceCents(String(value));
  // Refuse a lossy numeric read: adjacent integer-cent prices must not have
  // the same representation. Never guess a cent from a rounded large price.
  if (cents === null || cents / 100 !== value
    || (cents - 1) / 100 === value || (cents + 1) / 100 === value) {
    throw new Error('El precio recibido no conserva centavos exactos.');
  }
  return formatPriceCentsInput(cents);
}
