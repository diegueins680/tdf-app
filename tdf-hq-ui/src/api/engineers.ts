import { get } from './client';

export interface PublicEngineer {
  peId: number;
  peName: string;
}

const ENGINEERS_CACHE_KEY = 'tdf-engineers-cache-v1';

const hasBrowserStorage = () => typeof window !== 'undefined' && typeof window.localStorage !== 'undefined';

const normalizeNonEmptyString = (value: unknown): string | null => {
  if (typeof value !== 'string') return null;
  const trimmed = value.trim();
  return trimmed === '' ? null : trimmed;
};

const normalizePositiveEngineerId = (value: unknown): number | null => {
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

const normalizeEngineers = (rows: unknown): PublicEngineer[] => {
  if (!Array.isArray(rows)) return [];
  const map = new Map<number, PublicEngineer>();
  rows.forEach((row) => {
    if (!row || typeof row !== 'object') return;
    const item = row as Record<string, unknown>;
    const peId = normalizePositiveEngineerId(item['peId']);
    const peName = normalizeNonEmptyString(item['peName']);
    if (peId === null || !peName) return;

    if (!map.has(peId)) {
      map.set(peId, { peId, peName });
    }
  });
  return Array.from(map.values()).sort((a, b) => {
    const nameOrder = a.peName.localeCompare(b.peName);
    return nameOrder !== 0 ? nameOrder : a.peId - b.peId;
  });
};

const readCachedEngineers = (): PublicEngineer[] => {
  if (!hasBrowserStorage()) return [];
  try {
    const raw = window.localStorage.getItem(ENGINEERS_CACHE_KEY);
    if (!raw) return [];
    const parsed = JSON.parse(raw) as unknown;
    return normalizeEngineers(parsed);
  } catch {
    return [];
  }
};

const writeCachedEngineers = (rows: PublicEngineer[]) => {
  if (!hasBrowserStorage()) return;
  try {
    window.localStorage.setItem(ENGINEERS_CACHE_KEY, JSON.stringify(rows));
  } catch {
    // Ignore storage errors; live response still works without cache.
  }
};

export const Engineers = {
  listPublic: async () => {
    try {
      const live = normalizeEngineers(await get<PublicEngineer[]>('/engineers'));
      writeCachedEngineers(live);
      return live;
    } catch (error) {
      const cached = readCachedEngineers();
      if (cached.length > 0) {
        console.warn('Engineer catalog unavailable, using cached engineer list', error);
        return cached;
      }
      console.warn('Engineer catalog unavailable, falling back to manual entry', error);
      return cached;
    }
  },
};
