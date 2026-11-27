import { get, post } from './client';
import type { components } from './generated/types';

// TAX-FILTER-001 / TAX-GENRE-SUGGESTION-001.
export type TaxonomyFilterOptions = components['schemas']['TaxonomyFilterOptions'];
export type TaxonomyCountryOption = components['schemas']['TaxonomyCountryOption'];
export type TaxonomyRegionOption = components['schemas']['TaxonomyRegionOption'];
export type TaxonomyCityOption = components['schemas']['TaxonomyCityOption'];
export type TaxonomyGenreOption = components['schemas']['TaxonomyGenreOption'];
export type TaxonomyFacets = components['schemas']['TaxonomyFacets'];
export type GenreSuggestionRequest = components['schemas']['GenreSuggestionRequest'];
export type GenreSuggestionCheck = components['schemas']['GenreSuggestionCheck'];
export type GenreSuggestion = components['schemas']['GenreSuggestion'];
export type GenreSuggestionSubmission = components['schemas']['GenreSuggestionSubmission'];
export type GenreSuggestionQueueStatus = 'pending' | 'approved' | 'rejected' | 'merged' | 'all';

export interface TaxonomySelection {
  countryIds: string[];
  regionIds: string[];
  cityIds: string[];
  genreIds: string[];
}

/** Append the four repeatable taxonomy parameters (TAX-FILTER-001). */
export const appendTaxonomyParams = (params: URLSearchParams, selection?: Partial<TaxonomySelection>) => {
  if (!selection) return params;
  (['countryIds', 'regionIds', 'cityIds', 'genreIds'] as const).forEach((key) => {
    (selection[key] ?? []).forEach((value) => {
      if (value) params.append(key, value);
    });
  });
  return params;
};

const withQuery = (path: string, params: URLSearchParams) => {
  const qs = params.toString();
  return qs ? `${path}?${qs}` : path;
};

export const Taxonomy = {
  filterOptions: (signal?: AbortSignal) =>
    get<TaxonomyFilterOptions>('/taxonomy/filter-options', signal ? { signal } : undefined),
  radioFacets: (selection: TaxonomySelection, signal?: AbortSignal) =>
    get<TaxonomyFacets>(withQuery('/radio/streams/facets', appendTaxonomyParams(new URLSearchParams(), selection)), signal ? { signal } : undefined),
  musicReleaseFacets: (selection: TaxonomySelection, query = '', signal?: AbortSignal) => {
    const params = new URLSearchParams();
    if (query.trim()) params.set('q', query.trim());
    return get<TaxonomyFacets>(withQuery('/music/facets/releases', appendTaxonomyParams(params, selection)), signal ? { signal } : undefined);
  },
  eventFacets: (
    selection: TaxonomySelection,
    filters: { city?: string; scope?: string; startAfter?: string; eventTypeId?: string; workflowStateId?: string; artistIds?: string[] } = {},
    signal?: AbortSignal,
  ) => {
    const params = new URLSearchParams();
    if (filters.city?.trim()) params.set('city', filters.city.trim());
    if (filters.scope) params.set('scope', filters.scope);
    if (filters.startAfter) params.set('start_after', filters.startAfter);
    if (filters.eventTypeId) params.set('event_type_id', filters.eventTypeId);
    if (filters.workflowStateId) params.set('workflow_state_id', filters.workflowStateId);
    if (filters.artistIds?.length) params.set('artistId', filters.artistIds.join(','));
    return get<TaxonomyFacets>(withQuery('/social-events/events/facets', appendTaxonomyParams(params, selection)), signal ? { signal } : undefined);
  },
  checkGenreSuggestion: (body: GenreSuggestionRequest) => post<GenreSuggestionCheck>('/genres/suggestions/check', body),
  submitGenreSuggestion: (body: GenreSuggestionRequest) => post<GenreSuggestionSubmission>('/genres/suggestions', body),
  myGenreSuggestions: () => get<GenreSuggestion[]>('/genres/suggestions/mine'),
  genreSuggestionQueue: (status: GenreSuggestionQueueStatus = 'pending') =>
    get<GenreSuggestion[]>(`/admin/genre-suggestions?status=${encodeURIComponent(status)}`),
  approveGenreSuggestion: (id: string, body: components['schemas']['GenreSuggestionApproval']) =>
    post<GenreSuggestion>(`/admin/genre-suggestions/${encodeURIComponent(id)}/approve`, body),
  rejectGenreSuggestion: (id: string, rejectionNote: string) =>
    post<GenreSuggestion>(`/admin/genre-suggestions/${encodeURIComponent(id)}/reject`, { rejectionNote }),
  mergeGenreSuggestion: (id: string, mergeIntoGenreId: string) =>
    post<GenreSuggestion>(`/admin/genre-suggestions/${encodeURIComponent(id)}/merge`, { mergeIntoGenreId }),
};
