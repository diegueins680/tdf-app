const ISO_DATE_PREFIX_PATTERN = /^(\d{4})-(\d{2})-(\d{2})(?!\d)/;

const isLeapYear = (year: number): boolean => {
  return (year % 4 === 0 && year % 100 !== 0) || year % 400 === 0;
};

const daysInMonth = (year: number, month: number): number => {
  if (month === 2) return isLeapYear(year) ? 29 : 28;
  if ([4, 6, 9, 11].includes(month)) return 30;
  return 31;
};

export const hasImpossibleIsoCalendarDate = (value: string): boolean => {
  const match = ISO_DATE_PREFIX_PATTERN.exec(value);
  if (!match) return false;

  const yearRaw = match[1];
  const monthRaw = match[2];
  const dayRaw = match[3];
  if (!yearRaw || !monthRaw || !dayRaw) return true;

  const year = Number.parseInt(yearRaw, 10);
  const month = Number.parseInt(monthRaw, 10);
  const day = Number.parseInt(dayRaw, 10);

  if (month < 1 || month > 12) return true;
  if (day < 1 || day > daysInMonth(year, month)) return true;
  return false;
};
