import { hasImpossibleIsoCalendarDate } from './isoDate';

export const parseTimestamp = (value?: string | null): Date | null => {
  const trimmed = value?.trim();
  if (!trimmed) return null;
  if (hasImpossibleIsoCalendarDate(trimmed)) return null;
  const parsed = new Date(trimmed);
  return Number.isNaN(parsed.getTime()) ? null : parsed;
};

export const formatTimestampForDisplay = (
  value: string | null | undefined,
  fallback = '—',
): string => {
  const trimmed = value?.trim();
  if (!trimmed) return fallback;
  const parsed = parseTimestamp(trimmed);
  if (!parsed) return trimmed;
  return parsed.toLocaleString();
};

/**
 * Format a date in an event's stored zone without letting an identifier that
 * Intl rejects (for example a fixed offset such as "UTC+5", which Luxon and the
 * backend accept) throw during render. Such zones fall back to the viewer's
 * zone so the page still shows the activity instead of blanking.
 */
export const formatInEventTimeZone = (
  date: Date,
  locale: string | undefined,
  timeZone: string | null | undefined,
  options: Intl.DateTimeFormatOptions = {},
): string => {
  if (timeZone) {
    try {
      return date.toLocaleString(locale, { ...options, timeZone });
    } catch {
      // fall through to the viewer's zone
    }
  }
  return date.toLocaleString(locale, options);
};
