// PAY-MUSIC-MONEY-WIRE-001: numeric compatibility is limited to exact JSON
// integers. Large amounts remain decimal text from the API through consent.
import type { components } from '../api/generated/types';

export type MusicMinorUnits = components['schemas']['MusicMinorUnits'];
const maximumMinor = 9223372036854775807n;
const maximumNumericMinor = 9007199254740991n;

// Authoring uses USD units; the wire retains its full signed-64-bit positive
// range as canonical decimal minor-unit text, without a Number conversion.
export function parseMusicPriceInput(raw: string): string | null {
  const value = raw.trim();
  if (!value) return null;
  if (!/^\d+(?:[.,]\d{1,2})?$/.test(value)) throw new Error('Usa un precio con hasta dos decimales, por ejemplo 4,99.');
  const [whole = '0', fractional = ''] = value.replace(',', '.').split('.');
  const minor = BigInt(whole) * 100n + BigInt(fractional.padEnd(2, '0'));
  if (minor > maximumMinor) throw new Error('El precio está fuera del rango admitido.');
  return minor.toString();
}

export function formatMusicPriceInput(value: MusicMinorUnits | null): string {
  if (value === null) return '';
  const canonical = canonicalMusicMinor(value);
  if (canonical === null) throw new Error('El precio guardado no es exacto. Ingresa un precio válido.');
  const minor = BigInt(canonical);
  return `${minor / 100n}.${String(minor % 100n).padStart(2, '0')}`;
}

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
