const parseUnsignedSafeIntFromString = (value: string): number | null => {
  if (!/^\d+$/.test(value)) return null;
  const parsed = Number.parseInt(value, 10);
  return Number.isSafeInteger(parsed) ? parsed : null;
};

export const parseUnsignedSafeInt = (value: unknown): number | null => {
  if (typeof value === 'number') {
    return Number.isSafeInteger(value) && value >= 0 ? value : null;
  }
  if (typeof value === 'string') {
    const trimmed = value.trim();
    if (!trimmed) return null;
    return parseUnsignedSafeIntFromString(trimmed);
  }
  return null;
};

export const parsePositiveSafeInt = (value: unknown): number | null => {
  const parsed = parseUnsignedSafeInt(value);
  return parsed !== null && parsed > 0 ? parsed : null;
};
