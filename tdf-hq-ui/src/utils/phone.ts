/**
 * Phone normalization for public forms.
 *
 * Accepts what people actually type on a phone keyboard — spaces, dashes,
 * dots, parentheses, a leading `+` or `00` international prefix, and
 * Ecuador's national formats (`09XXXXXXXX` mobiles, `0[2-7]XXXXXXX`
 * landlines) — and returns an E.164 string (`+593991234567`), or `null`
 * when the input cannot be a valid number.
 */

export const PHONE_EXAMPLE_HINT = 'Usa un número como 0991234567 o +593991234567';

const ECUADOR_COUNTRY_CODE = '593';
const ALLOWED_PHONE_CHARACTERS = /^[\d\s+\-().]+$/;
const E164_DIGITS = /^[1-9]\d{7,14}$/;

const fromInternationalDigits = (digits: string): string | null => {
  let normalized = digits;
  // "+593 09..." / "00593 0 2..." — people often keep the national trunk 0.
  if (
    normalized.startsWith(`${ECUADOR_COUNTRY_CODE}0`)
    && (normalized.length === 13 || normalized.length === 12)
  ) {
    normalized = `${ECUADOR_COUNTRY_CODE}${normalized.slice(4)}`;
  }
  if (normalized.startsWith(ECUADOR_COUNTRY_CODE)) {
    const national = normalized.slice(ECUADOR_COUNTRY_CODE.length);
    const validMobile = /^9\d{8}$/.test(national);
    const validLandline = /^[2-7]\d{7}$/.test(national);
    return validMobile || validLandline ? `+${normalized}` : null;
  }
  return E164_DIGITS.test(normalized) ? `+${normalized}` : null;
};

export function normalizePhoneToE164(raw: string | null | undefined): string | null {
  const trimmed = raw?.trim() ?? '';
  if (trimmed === '' || !ALLOWED_PHONE_CHARACTERS.test(trimmed)) return null;
  const plusCount = (trimmed.match(/\+/g) ?? []).length;
  if (plusCount > 1 || (plusCount === 1 && !trimmed.startsWith('+'))) return null;

  const digits = trimmed.replace(/\D/g, '');
  if (digits === '') return null;

  if (trimmed.startsWith('+')) return fromInternationalDigits(digits);
  if (digits.startsWith('00')) return fromInternationalDigits(digits.slice(2));

  // Ecuador national formats.
  if (/^09\d{8}$/.test(digits)) return `+${ECUADOR_COUNTRY_CODE}${digits.slice(1)}`;
  if (/^0[2-7]\d{7}$/.test(digits)) return `+${ECUADOR_COUNTRY_CODE}${digits.slice(1)}`;
  // Mobile typed without the trunk 0 ("98 838 4849").
  if (/^9\d{8}$/.test(digits)) return `+${ECUADOR_COUNTRY_CODE}${digits}`;
  // Country code typed without "+" ("593 98 838 4849").
  if (digits.startsWith(ECUADOR_COUNTRY_CODE) && (digits.length === 12 || digits.length === 11)) {
    return fromInternationalDigits(digits);
  }
  return null;
}
