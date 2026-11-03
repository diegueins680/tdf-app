import { resolveRuntimeCurrency, resolveRuntimeFormatOptions } from './formatters';

const PRECISION_ERROR = 'El importe excede la precisión admitida por el navegador o no contiene centavos enteros.';

// Finance DTO monetary fields share the Cents suffix. Percentages and IDs are not money.
export function assertSafeEventFinanceMoney<T extends object>(value: T): T {
  for (const [field, amount] of Object.entries(value)) {
    if (field.endsWith('Cents') && amount != null && !Number.isSafeInteger(amount)) {
      throw new Error(PRECISION_ERROR);
    }
  }
  return value;
}

export const formatEventMoney = (amountCents?: number | null, currency?: string | null): string => {
  if (typeof amountCents !== 'number') return 'Gratis';
  if (!Number.isSafeInteger(amountCents)) return 'Monto no disponible';
  const locale = resolveRuntimeFormatOptions().locale;
  const formatter = new Intl.NumberFormat(locale, {
    style: 'currency', currency: (currency ?? resolveRuntimeCurrency()).toUpperCase(),
  });
  // Divide integer cents exactly, then apply the currency's default fraction rounding.
  // Passing a floating point amount / 100 loses cents even for safe integers.
  const digits = formatter.resolvedOptions().maximumFractionDigits ?? 0;
  const scale = 10n ** BigInt(digits);
  const cents = BigInt(amountCents);
  const negative = cents < 0n || Object.is(amountCents, -0);
  const magnitude = cents < 0n ? -cents : cents;
  const divisor = 10n ** BigInt(Math.max(0, 2 - digits));
  const units = digits >= 2
    ? magnitude * 10n ** BigInt(digits - 2)
    : (magnitude + divisor / 2n) / divisor;
  const whole = units / scale;
  const signedWhole = negative ? (whole === 0n ? -0 : -whole) : whole;
  const fraction = digits > 0 ? new Intl.NumberFormat(locale, {
    useGrouping: false, minimumIntegerDigits: digits, maximumFractionDigits: 0,
  }).format(units % scale) : '';
  return formatter.formatToParts(signedWhole)
    .map(part => part.type === 'fraction' ? fraction : part.value).join('');
};
