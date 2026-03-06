import { hasImpossibleIsoCalendarDate } from './isoDate';

const pad2 = (value: number): string => String(value).padStart(2, '0');
const DATE_ONLY_PATTERN = /^(\d{4})-(\d{2})-(\d{2})$/;

export const toLocalDateInputValue = (date: Date = new Date()): string => {
  if (Number.isNaN(date.getTime())) return '';
  return `${date.getFullYear()}-${pad2(date.getMonth() + 1)}-${pad2(date.getDate())}`;
};

export const parseDateForDisplay = (value: string): Date | null => {
  const trimmed = value.trim();
  if (trimmed === '') return null;

  const dateOnlyMatch = DATE_ONLY_PATTERN.exec(trimmed);
  if (dateOnlyMatch) {
    const yearRaw = dateOnlyMatch[1];
    const monthRaw = dateOnlyMatch[2];
    const dayRaw = dateOnlyMatch[3];
    if (!yearRaw || !monthRaw || !dayRaw) return null;

    const year = Number.parseInt(yearRaw, 10);
    const month = Number.parseInt(monthRaw, 10);
    const day = Number.parseInt(dayRaw, 10);
    const parsed = new Date(year, month - 1, day, 12, 0, 0, 0);
    if (
      parsed.getFullYear() !== year
      || parsed.getMonth() !== month - 1
      || parsed.getDate() !== day
    ) {
      return null;
    }
    return parsed;
  }

  if (hasImpossibleIsoCalendarDate(trimmed)) return null;
  const parsed = new Date(trimmed);
  return Number.isNaN(parsed.getTime()) ? null : parsed;
};
