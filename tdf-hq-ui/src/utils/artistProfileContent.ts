export const parseArtistTextItems = (raw?: string | null): string[] => {
  if (!raw) return [];
  try {
    const value: unknown = JSON.parse(raw);
    if (Array.isArray(value)) {
      return value.filter((item): item is string => typeof item === 'string' && item.trim() !== '');
    }
  } catch {
    // Legacy artist profiles store these values as plain text.
  }
  return [raw];
};
