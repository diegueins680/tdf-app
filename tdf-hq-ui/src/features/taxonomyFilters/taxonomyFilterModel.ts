// TAX-FILTER-001: pure model behind the shared location and genre filters.
// Values inside one dimension are ORed and dimensions are ANDed by the
// backend; this module only reads and writes selections, resolves labels in
// the viewer's language and orders options for display.
import type {
  TaxonomyCityOption,
  TaxonomyFacets,
  TaxonomyFilterOptions,
  TaxonomyGenreOption,
  TaxonomySelection,
} from '../../api/taxonomy';

export type TaxonomyDimension = keyof TaxonomySelection;
export type TaxonomyLanguage = 'es' | 'en';

export const TAXONOMY_DIMENSIONS: readonly TaxonomyDimension[] = ['countryIds', 'regionIds', 'cityIds', 'genreIds'];

/**
 * Language-independent URL keys; values are taxonomy ids. Namespaced so they
 * never collide with a page's own parameters (the events page uses ?city= for
 * its free-text search).
 */
export const TAXONOMY_URL_KEYS: Record<TaxonomyDimension, string> = {
  countryIds: 'f.country',
  regionIds: 'f.region',
  cityIds: 'f.city',
  genreIds: 'f.genre',
};

/** Marks a surface whose filters are deliberately its own, even when empty. */
export const TAXONOMY_LOCAL_SCOPE_KEY = 'f.scope';
export const TAXONOMY_LOCAL_SCOPE_VALUE = 'local';

const UUID_PATTERN = /^[0-9a-f]{8}-[0-9a-f]{4}-[1-8][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;
export const MAX_VALUES_PER_DIMENSION = 50;

export const emptyTaxonomySelection = (): TaxonomySelection => ({ countryIds: [], regionIds: [], cityIds: [], genreIds: [] });

const cleanIds = (values: readonly unknown[]): string[] => {
  const seen = new Set<string>();
  const result: string[] = [];
  values.forEach((value) => {
    if (typeof value !== 'string') return;
    const id = value.trim().toLowerCase();
    if (!UUID_PATTERN.test(id) || seen.has(id)) return;
    seen.add(id);
    result.push(id);
  });
  return result.slice(0, MAX_VALUES_PER_DIMENSION);
};

export const normalizeTaxonomySelection = (value: unknown): TaxonomySelection => {
  if (!value || typeof value !== 'object') return emptyTaxonomySelection();
  const record = value as Record<string, unknown>;
  const read = (key: TaxonomyDimension) => (Array.isArray(record[key]) ? cleanIds(record[key] as unknown[]) : []);
  return { countryIds: read('countryIds'), regionIds: read('regionIds'), cityIds: read('cityIds'), genreIds: read('genreIds') };
};

export const isTaxonomySelectionEmpty = (selection: TaxonomySelection) =>
  TAXONOMY_DIMENSIONS.every((dimension) => selection[dimension].length === 0);

export const taxonomySelectionsEqual = (left: TaxonomySelection, right: TaxonomySelection) =>
  TAXONOMY_DIMENSIONS.every((dimension) => {
    const a = [...left[dimension]].sort();
    const b = [...right[dimension]].sort();
    return a.length === b.length && a.every((value, index) => value === b[index]);
  });

export const hasTaxonomyUrlState = (params: URLSearchParams) =>
  TAXONOMY_DIMENSIONS.some((dimension) => params.has(TAXONOMY_URL_KEYS[dimension]))
  || params.get(TAXONOMY_LOCAL_SCOPE_KEY) === TAXONOMY_LOCAL_SCOPE_VALUE;

export const readTaxonomySelectionFromUrl = (params: URLSearchParams): TaxonomySelection => ({
  countryIds: cleanIds(params.getAll(TAXONOMY_URL_KEYS.countryIds)),
  regionIds: cleanIds(params.getAll(TAXONOMY_URL_KEYS.regionIds)),
  cityIds: cleanIds(params.getAll(TAXONOMY_URL_KEYS.cityIds)),
  genreIds: cleanIds(params.getAll(TAXONOMY_URL_KEYS.genreIds)),
});

/**
 * Returns a copy of `params` carrying `selection`. `local` marks a deliberate
 * per-surface override so an empty local selection survives a reload.
 */
export const writeTaxonomySelectionToUrl = (
  params: URLSearchParams,
  selection: TaxonomySelection | null,
  local = false,
): URLSearchParams => {
  const next = new URLSearchParams(params);
  TAXONOMY_DIMENSIONS.forEach((dimension) => next.delete(TAXONOMY_URL_KEYS[dimension]));
  next.delete(TAXONOMY_LOCAL_SCOPE_KEY);
  if (!selection) return next;
  TAXONOMY_DIMENSIONS.forEach((dimension) => {
    selection[dimension].forEach((id) => next.append(TAXONOMY_URL_KEYS[dimension], id));
  });
  if (local) next.set(TAXONOMY_LOCAL_SCOPE_KEY, TAXONOMY_LOCAL_SCOPE_VALUE);
  return next;
};

export const toggleTaxonomyValue = (selection: TaxonomySelection, dimension: TaxonomyDimension, id: string): TaxonomySelection => {
  const current = selection[dimension];
  const nextValues = current.includes(id) ? current.filter((value) => value !== id) : [...current, id];
  return { ...selection, [dimension]: nextValues.slice(0, MAX_VALUES_PER_DIMENSION) };
};

export const setTaxonomyDimension = (selection: TaxonomySelection, dimension: TaxonomyDimension, ids: string[]): TaxonomySelection => ({
  ...selection,
  [dimension]: cleanIds(ids),
});

export const removeTaxonomyValue = (selection: TaxonomySelection, dimension: TaxonomyDimension, id: string): TaxonomySelection => ({
  ...selection,
  [dimension]: selection[dimension].filter((value) => value !== id),
});

export const taxonomyName = (option: { nameEs: string; nameEn: string }, language: TaxonomyLanguage) => {
  const preferred = language === 'en' ? option.nameEn : option.nameEs;
  const fallback = language === 'en' ? option.nameEs : option.nameEn;
  return preferred?.trim() ? preferred : fallback;
};

/** Spanish for Spanish (and when unknown, the app default); English for every other language, as in i18n. */
export const resolveTaxonomyLanguage = (language: string | undefined | null): TaxonomyLanguage => {
  const normalized = language?.trim().toLowerCase();
  return !normalized || normalized.startsWith('es') ? 'es' : 'en';
};

export interface TaxonomyOption {
  id: string;
  label: string;
  /** Nesting depth for genres (0 = top level). */
  depth: number;
  /** Parent label, shown as context for cities and regions. */
  context?: string;
  count?: number;
}

const facetCounts = (facets: TaxonomyFacets | null | undefined, key: keyof TaxonomyFacets) => {
  const counts = new Map<string, number>();
  (facets?.[key] ?? []).forEach((item) => counts.set(item.id, item.count));
  return counts;
};

const collator = (language: TaxonomyLanguage) => new Intl.Collator(language, { sensitivity: 'base' });

/** Genres as a depth-first tree: each parent is followed by its subgenres. */
export const genreOptions = (
  genres: readonly TaxonomyGenreOption[],
  language: TaxonomyLanguage,
  facets?: TaxonomyFacets | null,
): TaxonomyOption[] => {
  const counts = facetCounts(facets, 'genres');
  const compare = collator(language);
  const known = new Set(genres.map((genre) => genre.id));
  const children = new Map<string | null, TaxonomyGenreOption[]>();
  genres.forEach((genre) => {
    const parent = genre.parentId && known.has(genre.parentId) ? genre.parentId : null;
    children.set(parent, [...(children.get(parent) ?? []), genre]);
  });
  const result: TaxonomyOption[] = [];
  const visit = (parent: string | null, depth: number, seen: Set<string>) => {
    const items = [...(children.get(parent) ?? [])].sort((a, b) => compare.compare(taxonomyName(a, language), taxonomyName(b, language)));
    items.forEach((genre) => {
      if (seen.has(genre.id) || depth > 8) return;
      seen.add(genre.id);
      result.push({ id: genre.id, label: taxonomyName(genre, language), depth, count: facets ? counts.get(genre.id) ?? 0 : undefined });
      visit(genre.id, depth + 1, seen);
    });
  };
  visit(null, 0, new Set());
  return result;
};

/**
 * Countries, regions and cities in display order. Regions and cities are
 * narrowed to the selected countries (and cities to the selected regions) so
 * the lists stay short; a selected value is always kept.
 */
export const locationOptions = (
  options: TaxonomyFilterOptions,
  selection: TaxonomySelection,
  language: TaxonomyLanguage,
  facets?: TaxonomyFacets | null,
) => {
  const compare = collator(language);
  const countryCounts = facetCounts(facets, 'countries');
  const regionCounts = facetCounts(facets, 'regions');
  const cityCounts = facetCounts(facets, 'cities');
  const countryNames = new Map(options.countries.map((country) => [country.id, taxonomyName(country, language)]));
  const regionNames = new Map(options.regions.map((region) => [region.id, taxonomyName(region, language)]));
  const withCount = (counts: Map<string, number>, id: string) => (facets ? counts.get(id) ?? 0 : undefined);
  const byLabel = (a: TaxonomyOption, b: TaxonomyOption) => compare.compare(a.label, b.label);
  // Countries with matching items first, then the rest alphabetically.
  const countries = options.countries
    .map((country) => ({ id: country.id, label: taxonomyName(country, language), depth: 0, count: withCount(countryCounts, country.id) }))
    .sort((a, b) => (b.count ?? 0) - (a.count ?? 0) || byLabel(a, b));
  const regions = options.regions
    .filter((region) => selection.countryIds.length === 0 || selection.countryIds.includes(region.countryId) || selection.regionIds.includes(region.id))
    .map((region) => ({ id: region.id, label: taxonomyName(region, language), depth: 0, context: countryNames.get(region.countryId), count: withCount(regionCounts, region.id) }))
    .sort(byLabel);
  const cityInScope = (city: TaxonomyCityOption) =>
    selection.cityIds.includes(city.id)
    || ((selection.countryIds.length === 0 || selection.countryIds.includes(city.countryId))
      && (selection.regionIds.length === 0 || (city.regionId != null && selection.regionIds.includes(city.regionId))));
  const cities = options.cities
    .filter(cityInScope)
    .map((city) => ({
      id: city.id,
      label: taxonomyName(city, language),
      depth: 0,
      context: (city.regionId ? regionNames.get(city.regionId) : undefined) ?? countryNames.get(city.countryId),
      count: withCount(cityCounts, city.id),
    }))
    .sort(byLabel);
  return { countries, regions, cities };
};

export interface TaxonomyChip {
  dimension: TaxonomyDimension;
  id: string;
  label: string;
}

/** Active selections as removable chips, in dimension order. Unknown ids keep a placeholder label. */
export const taxonomyChips = (
  selection: TaxonomySelection,
  options: TaxonomyFilterOptions | null | undefined,
  language: TaxonomyLanguage,
  unknownLabel: string,
): TaxonomyChip[] => {
  const lookup: Record<TaxonomyDimension, Map<string, string>> = {
    countryIds: new Map((options?.countries ?? []).map((item) => [item.id, taxonomyName(item, language)])),
    regionIds: new Map((options?.regions ?? []).map((item) => [item.id, taxonomyName(item, language)])),
    cityIds: new Map((options?.cities ?? []).map((item) => [item.id, taxonomyName(item, language)])),
    genreIds: new Map((options?.genres ?? []).map((item) => [item.id, taxonomyName(item, language)])),
  };
  return TAXONOMY_DIMENSIONS.flatMap((dimension) =>
    selection[dimension].map((id) => ({ dimension, id, label: lookup[dimension].get(id) ?? unknownLabel })));
};

const normalizeName = (value: string) =>
  value.normalize('NFD').replace(/[̀-ͯ]/g, '').toLowerCase().replace(/[^a-z0-9]+/g, ' ').trim();

/** Match a free-text profile city ("quito ") to exactly one city option. */
export const matchCityByName = (cities: readonly TaxonomyCityOption[], rawCity: string | null | undefined): TaxonomyCityOption | null => {
  const needle = normalizeName(rawCity ?? '');
  if (!needle) return null;
  const matches = cities.filter((city) => normalizeName(city.nameEs) === needle || normalizeName(city.nameEn) === needle);
  return matches.length === 1 ? matches[0]! : null;
};

/** Nearest city with coordinates, within `maxKm` of the given point. */
export const nearestCity = (
  cities: readonly TaxonomyCityOption[],
  latitude: number,
  longitude: number,
  maxKm = 75,
): TaxonomyCityOption | null => {
  const toRadians = (value: number) => (value * Math.PI) / 180;
  let best: { city: TaxonomyCityOption; km: number } | null = null;
  cities.forEach((city) => {
    if (city.latitude == null || city.longitude == null) return;
    const dLat = toRadians(city.latitude - latitude);
    const dLon = toRadians(city.longitude - longitude);
    const a = Math.sin(dLat / 2) ** 2 + Math.cos(toRadians(latitude)) * Math.cos(toRadians(city.latitude)) * Math.sin(dLon / 2) ** 2;
    const km = 6371 * 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
    if (km <= maxKm && (!best || km < best.km)) best = { city, km };
  });
  return best ? (best as { city: TaxonomyCityOption }).city : null;
};
