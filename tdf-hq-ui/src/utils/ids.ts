export const parsePositiveSafeInt = (value: unknown): number | null => {
  if (typeof value === 'number') {
    return Number.isSafeInteger(value) && value > 0 ? value : null;
  }
  if (typeof value === 'string') {
    const trimmed = value.trim();
    if (!/^\d+$/.test(trimmed)) return null;
    const parsed = Number.parseInt(trimmed, 10);
    return Number.isSafeInteger(parsed) && parsed > 0 ? parsed : null;
  }
  return null;
};
