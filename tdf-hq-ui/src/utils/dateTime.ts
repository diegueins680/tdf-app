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
