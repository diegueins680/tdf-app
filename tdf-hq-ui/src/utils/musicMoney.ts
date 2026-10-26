// PAY-MUSIC-MONEY-WIRE-001: numeric compatibility is limited to exact JSON
// integers. Large amounts remain decimal text from the API through consent.
import type { components } from '../api/generated/types';

export type MusicMinorUnits = components['schemas']['MusicMinorUnits'];
const maximumMinor = 9223372036854775807n;
const maximumNumericMinor = 9007199254740991n;

export function canonicalMusicMinor(value: unknown): string | null {
  if (typeof value === 'number') {
    return Number.isSafeInteger(value) && value >= 0 ? String(value) : null;
  }
  if (typeof value !== 'string' || !/^(0|[1-9][0-9]{0,18})$/.test(value)) return null;
  return BigInt(value) <= maximumMinor ? value : null;
}

export function musicMinorForWire(value: MusicMinorUnits): MusicMinorUnits {
  const canonical = canonicalMusicMinor(value);
  if (canonical === null) throw new Error('El monto no es exacto. Recarga la oferta antes de continuar.');
  return BigInt(canonical) <= maximumNumericMinor ? Number(canonical) : canonical;
}

export function formatMusicMinor(value: MusicMinorUnits, currency: string, locale = 'es-EC'): string {
  const canonical = canonicalMusicMinor(value);
  if (canonical === null || currency.trim().toUpperCase() !== 'USD') {
    throw new Error('El precio no está disponible.');
  }
  const minor = BigInt(canonical);
  const formatter = new Intl.NumberFormat(locale, { style: 'currency', currency: 'USD' });
  const fraction = new Intl.NumberFormat(locale, {
    useGrouping: false, minimumIntegerDigits: 2, maximumFractionDigits: 0,
  }).format(Number(minor % 100n)); // The only numeric conversion is in 0..99.
  return formatter.formatToParts(minor / 100n)
    .map((part) => part.type === 'fraction' ? fraction : part.value).join('');
}
