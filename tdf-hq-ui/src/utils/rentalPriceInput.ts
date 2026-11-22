import { parseOptionalPriceCents } from './priceInput';

// The rental-terms endpoint stores nonnegative 32-bit minor units. Weekly
// pricing may be omitted; daily and present weekly rates must be positive.
export function rentalPriceField(raw: string, positive: boolean, optional = false): {
  cents: number | null; error: string | null;
} {
  try {
    const cents = parseOptionalPriceCents(raw);
    if (cents === null && !optional) throw new Error(positive
      ? 'Ingresa una tarifa mayor que cero.' : 'Ingresa un importe; usa 0 si no hay cargo.');
    if (cents !== null && positive && cents === 0) throw new Error('La tarifa debe ser mayor que cero.');
    if (cents !== null && cents > 2147483647) throw new Error('El máximo permitido es USD 21474836.47.');
    return { cents, error: null };
  } catch (error) {
    return { cents: null, error: error instanceof Error ? error.message : 'Revisa el importe.' };
  }
}
