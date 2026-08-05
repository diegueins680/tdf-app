export interface FormatOptions {
  locale: string;
  timeZone: string;
}

export function formatCurrency(
  amount: number,
  currency: string,
  locale: string,
  options: Omit<Intl.NumberFormatOptions, 'style' | 'currency'> = {},
): string {
  return new Intl.NumberFormat(locale, {
    style: 'currency',
    currency: currency.toUpperCase(),
    ...options,
  }).format(amount);
}

export function formatNumber(
  value: number,
  locale: string,
  options: Intl.NumberFormatOptions = {},
): string {
  return new Intl.NumberFormat(locale, options).format(value);
}

export function formatDate(
  value: Date | string | number,
  { locale, timeZone }: FormatOptions,
  options: Intl.DateTimeFormatOptions = { dateStyle: 'medium' },
): string {
  const date = value instanceof Date ? value : new Date(value);
  if (Number.isNaN(date.getTime())) return String(value);
  return new Intl.DateTimeFormat(locale, { ...options, timeZone }).format(date);
}

export function formatDateTime(
  value: Date | string | number,
  options: FormatOptions,
): string {
  return formatDate(value, options, { dateStyle: 'medium', timeStyle: 'short' });
}
