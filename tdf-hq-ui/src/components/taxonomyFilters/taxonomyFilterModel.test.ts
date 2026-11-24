import type { TaxonomyFilterOptions } from '../../api/taxonomy';
import {
  emptyTaxonomySelection,
  genreOptions,
  hasTaxonomyUrlState,
  isTaxonomySelectionEmpty,
  locationOptions,
  matchCityByName,
  nearestCity,
  normalizeTaxonomySelection,
  readTaxonomySelectionFromUrl,
  removeTaxonomyValue,
  resolveTaxonomyLanguage,
  taxonomyChips,
  taxonomyName,
  taxonomySelectionsEqual,
  toggleTaxonomyValue,
  writeTaxonomySelectionToUrl,
} from './taxonomyFilterModel';

const EC = 'aaaaaaaa-0000-4000-8000-000000000001';
const CO = 'aaaaaaaa-0000-4000-8000-000000000002';
const PICHINCHA = 'bbbbbbbb-0000-4000-8000-000000000001';
const GUAYAS = 'bbbbbbbb-0000-4000-8000-000000000002';
const QUITO = 'cccccccc-0000-4000-8000-000000000001';
const GUAYAQUIL = 'cccccccc-0000-4000-8000-000000000002';
const BOGOTA = 'cccccccc-0000-4000-8000-000000000003';
const LATIN = 'dddddddd-0000-4000-8000-000000000001';
const CUMBIA = 'dddddddd-0000-4000-8000-000000000002';
const ROCK = 'dddddddd-0000-4000-8000-000000000003';

const options: TaxonomyFilterOptions = {
  countries: [
    { id: EC, code: 'EC', nameEs: 'Ecuador', nameEn: 'Ecuador' },
    { id: CO, code: 'CO', nameEs: 'Colombia', nameEn: 'Colombia' },
  ],
  regions: [
    { id: PICHINCHA, countryId: EC, code: 'EC-P', nameEs: 'Pichincha', nameEn: 'Pichincha' },
    { id: GUAYAS, countryId: EC, code: 'EC-G', nameEs: 'Guayas', nameEn: 'Guayas' },
  ],
  cities: [
    { id: QUITO, countryId: EC, regionId: PICHINCHA, code: 'quito', nameEs: 'Quito', nameEn: 'Quito', latitude: -0.18, longitude: -78.47 },
    { id: GUAYAQUIL, countryId: EC, regionId: GUAYAS, code: 'gye', nameEs: 'Guayaquil', nameEn: 'Guayaquil', latitude: -2.19, longitude: -79.89 },
    { id: BOGOTA, countryId: CO, regionId: null, code: 'bog', nameEs: 'Bogotá', nameEn: 'Bogota', latitude: 4.71, longitude: -74.07 },
  ],
  genres: [
    { id: ROCK, code: 'rock', nameEs: 'Rock', nameEn: 'Rock', parentId: null },
    { id: CUMBIA, code: 'cumbia', nameEs: 'Cumbia', nameEn: 'Cumbia', parentId: LATIN },
    { id: LATIN, code: 'latin', nameEs: 'Latina', nameEn: 'Latin', parentId: null },
  ],
};

describe('TAX-FILTER-001 taxonomy filter model', () => {
  it('reads and writes language-independent, namespaced URL parameters', () => {
    const selection = { ...emptyTaxonomySelection(), cityIds: [QUITO, GUAYAQUIL], genreIds: [CUMBIA] };
    const params = writeTaxonomySelectionToUrl(new URLSearchParams('city=quito&q=luna'), selection);
    expect(params.getAll('f.city')).toEqual([QUITO, GUAYAQUIL]);
    expect(params.get('city')).toBe('quito');
    expect(params.get('q')).toBe('luna');
    expect(readTaxonomySelectionFromUrl(params)).toEqual(selection);
    expect(hasTaxonomyUrlState(new URLSearchParams('city=quito'))).toBe(false);
  });

  it('keeps an explicit empty local override and clears it on request', () => {
    const local = writeTaxonomySelectionToUrl(new URLSearchParams(), emptyTaxonomySelection(), true);
    expect(local.get('f.scope')).toBe('local');
    expect(hasTaxonomyUrlState(local)).toBe(true);
    expect(hasTaxonomyUrlState(writeTaxonomySelectionToUrl(local, null))).toBe(false);
  });

  it('drops invalid and duplicate ids and bounds each dimension', () => {
    const params = new URLSearchParams();
    ['not-a-uuid', QUITO, QUITO.toUpperCase(), "'; DROP"].forEach((value) => params.append('f.city', value));
    expect(readTaxonomySelectionFromUrl(params).cityIds).toEqual([QUITO]);
    const many = Array.from({ length: 60 }, (_, index) => `00000000-0000-4000-8000-${String(index).padStart(12, '0')}`);
    expect(normalizeTaxonomySelection({ genreIds: many }).genreIds).toHaveLength(50);
    expect(normalizeTaxonomySelection('garbage')).toEqual(emptyTaxonomySelection());
  });

  it('toggles, removes and compares selections regardless of order', () => {
    let selection = toggleTaxonomyValue(emptyTaxonomySelection(), 'genreIds', ROCK);
    selection = toggleTaxonomyValue(selection, 'genreIds', CUMBIA);
    expect(taxonomySelectionsEqual(selection, { ...emptyTaxonomySelection(), genreIds: [CUMBIA, ROCK] })).toBe(true);
    selection = toggleTaxonomyValue(selection, 'genreIds', ROCK);
    expect(selection.genreIds).toEqual([CUMBIA]);
    expect(isTaxonomySelectionEmpty(removeTaxonomyValue(selection, 'genreIds', CUMBIA))).toBe(true);
  });

  it('shows Spanish for Spanish and English for every other language, falling back when a name is empty', () => {
    expect(resolveTaxonomyLanguage('es-EC')).toBe('es');
    expect(resolveTaxonomyLanguage(undefined)).toBe('es');
    expect(resolveTaxonomyLanguage('en')).toBe('en');
    expect(resolveTaxonomyLanguage('fr')).toBe('en');
    expect(taxonomyName({ nameEs: 'Latina', nameEn: 'Latin' }, 'en')).toBe('Latin');
    expect(taxonomyName({ nameEs: 'Pasillo', nameEn: ' ' }, 'en')).toBe('Pasillo');
  });

  it('orders genres as a tree with each parent before its subgenres', () => {
    const facets = { genres: [{ id: LATIN, count: 4 }, { id: CUMBIA, count: 3 }] };
    expect(genreOptions(options.genres, 'es', facets).map((option) => [option.label, option.depth, option.count]))
      .toEqual([['Latina', 0, 4], ['Cumbia', 1, 3], ['Rock', 0, 0]]);
    expect(genreOptions(options.genres, 'en').map((option) => option.label)).toEqual(['Latin', 'Cumbia', 'Rock']);
  });

  it('narrows regions and cities to the selected places but keeps selected values', () => {
    const lists = locationOptions(options, { ...emptyTaxonomySelection(), countryIds: [CO], cityIds: [QUITO] }, 'es');
    expect(lists.regions).toEqual([]);
    expect(lists.cities.map((city) => city.label).sort()).toEqual(['Bogotá', 'Quito']);
    const inGuayas = locationOptions(options, { ...emptyTaxonomySelection(), regionIds: [GUAYAS] }, 'es');
    expect(inGuayas.cities.map((city) => city.label)).toEqual(['Guayaquil']);
    expect(inGuayas.cities[0]?.context).toBe('Guayas');
  });

  it('lists countries with matching items first', () => {
    const lists = locationOptions(options, emptyTaxonomySelection(), 'es', { countries: [{ id: CO, count: 2 }] });
    expect(lists.countries.map((country) => [country.label, country.count])).toEqual([['Colombia', 2], ['Ecuador', 0]]);
  });

  it('labels chips in dimension order and keeps unknown ids removable', () => {
    const unknown = 'eeeeeeee-0000-4000-8000-000000000001';
    const chips = taxonomyChips({ countryIds: [EC], regionIds: [], cityIds: [QUITO], genreIds: [unknown] }, options, 'en', '?');
    expect(chips.map((chip) => [chip.dimension, chip.label])).toEqual([['countryIds', 'Ecuador'], ['cityIds', 'Quito'], ['genreIds', '?']]);
  });

  it('matches a free-text profile city only when it is unambiguous', () => {
    expect(matchCityByName(options.cities, '  quito ')?.id).toBe(QUITO);
    expect(matchCityByName(options.cities, 'BOGOTA')?.id).toBe(BOGOTA);
    expect(matchCityByName(options.cities, 'Gotham')).toBeNull();
    expect(matchCityByName(options.cities, null)).toBeNull();
  });

  it('finds the nearest city within reach of a consented position', () => {
    expect(nearestCity(options.cities, -0.2, -78.5)?.id).toBe(QUITO);
    expect(nearestCity(options.cities, 40.4, -3.7)).toBeNull();
  });
});
