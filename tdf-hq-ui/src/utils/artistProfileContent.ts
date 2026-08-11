export const parseArtistJson = (raw: string): unknown => {
  try {
    return JSON.parse(raw) as unknown;
  } catch {
    return undefined;
  }
};

export const parseArtistTextItems = (raw?: string | null): string[] => {
  if (!raw) return [];
  const parsed = parseArtistJson(raw);
  if (Array.isArray(parsed)) {
    return parsed.filter((item): item is string => typeof item === 'string' && item.trim() !== '');
  }
  // Legacy artist profiles store these values as plain text.
  return [raw];
};
