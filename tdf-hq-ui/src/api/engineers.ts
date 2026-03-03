import { get } from './client';

export interface PublicEngineer {
  peId: number;
  peName: string;
}

const ENGINEERS_CACHE_KEY = 'tdf-engineers-cache-v1';

const hasBrowserStorage = () => typeof window !== 'undefined' && typeof window.localStorage !== 'undefined';

const normalizeEngineers = (rows: PublicEngineer[]): PublicEngineer[] => {
  const map = new Map<string, PublicEngineer>();
  rows.forEach((row) => {
    const name = row.peName?.trim();
    if (!name) return;
    const key = name.toLowerCase();
    if (!map.has(key)) {
      map.set(key, { peId: row.peId, peName: name });
    }
  });
  return Array.from(map.values()).sort((a, b) => a.peName.localeCompare(b.peName));
};

const readCachedEngineers = (): PublicEngineer[] => {
  if (!hasBrowserStorage()) return [];
  try {
    const raw = window.localStorage.getItem(ENGINEERS_CACHE_KEY);
    if (!raw) return [];
    const parsed = JSON.parse(raw) as unknown;
    if (!Array.isArray(parsed)) return [];
    const rows: PublicEngineer[] = parsed.flatMap((item) => {
      if (!item || typeof item !== 'object') return [];
      const obj = item as Record<string, unknown>;
      const peId = typeof obj['peId'] === 'number' ? obj['peId'] : Number(obj['peId']);
      const peName = typeof obj['peName'] === 'string' ? obj['peName'] : '';
      if (!Number.isFinite(peId) || !peName.trim()) return [];
      return [{ peId, peName }];
    });
    return normalizeEngineers(rows);
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
