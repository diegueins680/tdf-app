// TAX-FILTER-001: one shared filter set, overridable per surface.
//
// * A surface shows the shared set unless it carries its own selection.
// * URL-backed surfaces keep the effective selection in the query string, so
//   a copied link reproduces the view. A link whose filters differ from the
//   viewer's shared set opens as a per-surface override.
// * Changing filters updates the shared set (every surface follows) unless
//   the viewer chose "only here", which keeps them on this surface.
import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { useLocation, useSearchParams } from 'react-router-dom';
import { Taxonomy, type TaxonomyFilterOptions, type TaxonomySelection } from '../../api/taxonomy';
import {
  emptyTaxonomySelection,
  hasTaxonomyUrlState,
  isTaxonomySelectionEmpty,
  readTaxonomySelectionFromUrl,
  taxonomySelectionsEqual,
  TAXONOMY_LOCAL_SCOPE_KEY,
  TAXONOMY_LOCAL_SCOPE_VALUE,
  TAXONOMY_URL_KEYS,
  writeTaxonomySelectionToUrl,
} from './taxonomyFilterModel';
import { setSharedTaxonomySelection, useSharedTaxonomySelection } from './taxonomyFilterStore';

export const TAXONOMY_OPTIONS_QUERY_KEY = ['taxonomy', 'filter-options'] as const;

export const useTaxonomyFilterOptions = () =>
  useQuery<TaxonomyFilterOptions>({
    queryKey: TAXONOMY_OPTIONS_QUERY_KEY,
    queryFn: ({ signal }) => Taxonomy.filterOptions(signal),
    staleTime: 10 * 60_000,
  });

export interface SurfaceTaxonomyFilters {
  /** The selection this surface queries with. */
  selection: TaxonomySelection;
  /** True when this surface shows its own filters rather than the shared set. */
  isOverride: boolean;
  /** Replace the selection. `onlyHere` keeps it on this surface. */
  setSelection: (next: TaxonomySelection, onlyHere?: boolean) => void;
  /** Drop this surface's own filters and show the shared set again. */
  resetToShared: () => void;
  /** Make this surface's filters the shared set. */
  applyEverywhere: () => void;
  clearAll: (removeParams?: readonly string[]) => void;
}

interface Options {
  /** Keep the selection in the URL (pages). Overlays such as the radio keep it in memory. */
  urlBacked: boolean;
  legacyFilters?: 'genre' | 'genre-and-city';
  /** Called after a change so a page can reset its pagination. */
  onChange?: () => void;
}

export const useSurfaceTaxonomyFilters = ({ urlBacked, onChange, legacyFilters }: Options): SurfaceTaxonomyFilters => {
  const shared = useSharedTaxonomySelection();
  const [searchParams, setSearchParams] = useSearchParams();
  const [memoryOverride, setMemoryOverride] = useState<TaxonomySelection | null>(null);

  const canonical = useMemo(() => {
    if (!legacyFilters) return searchParams;
    const migrateCity = legacyFilters === 'genre-and-city' && searchParams.has('cityId');
    if (!migrateCity && !searchParams.has('genreId')) return searchParams;
    const next = new URLSearchParams(searchParams);
    const move = (oldKey: string, newKey: string) => {
      for (const value of next.getAll(oldKey)) {
        if (value && !next.getAll(newKey).includes(value)) next.append(newKey, value);
      }
      next.delete(oldKey);
    };
    move('genreId', TAXONOMY_URL_KEYS.genreIds);
    if (migrateCity) move('cityId', TAXONOMY_URL_KEYS.cityIds);
    return next;
  }, [legacyFilters, searchParams]);
  const urlHasState = urlBacked && hasTaxonomyUrlState(canonical);
  const urlSelection = useMemo(() => readTaxonomySelectionFromUrl(canonical), [canonical]);
  const urlMarkedLocal = urlBacked && searchParams.get(TAXONOMY_LOCAL_SCOPE_KEY) === TAXONOMY_LOCAL_SCOPE_VALUE;

  const { state: navigationState } = useLocation();
  const mirrored = useRef<TaxonomySelection | null>(null);
  const matchesShared = taxonomySelectionsEqual(urlSelection, shared);
  const followsShared = !urlMarkedLocal && (!urlHasState || matchesShared ||
    navigationState?.sharedTaxonomy === JSON.stringify(urlSelection) ||
    (mirrored.current !== null && taxonomySelectionsEqual(mirrored.current, urlSelection)));
  const override = urlBacked ? (followsShared ? null : urlSelection) : memoryOverride;
  const selection = override ?? shared;
  const isOverride = override !== null;

  const pendingFrom = useRef<URLSearchParams | null>(null);
  const writeUrl = useCallback((next: TaxonomySelection | null, local: boolean, removeParams: readonly string[] = []) => {
    const updated = writeTaxonomySelectionToUrl(canonical, next, local);
    updated.delete('page');
    removeParams.forEach((key) => updated.delete(key));
    if (updated.toString() !== searchParams.toString()) pendingFrom.current = searchParams;
    setSearchParams(updated, {
      replace: true,
      state: { ...navigationState, sharedTaxonomy: local ? null : JSON.stringify(next) },
    });
  }, [canonical, searchParams, setSearchParams, navigationState]);

  useEffect(() => {
    // A synchronous store notification can render before the router commits
    // the same user action. Wait for that navigation before mirroring again.
    if (!urlBacked || pendingFrom.current === searchParams) return;
    pendingFrom.current = null;
    if (canonical !== searchParams) {
      setSearchParams(canonical, { replace: true, state: navigationState });
      return;
    }
    if (!followsShared) {
      mirrored.current = null;
      return;
    }
    if (matchesShared) mirrored.current = shared;
    else writeUrl(shared, false);
  }, [urlBacked, followsShared, matchesShared, shared, searchParams, writeUrl, canonical, setSearchParams, navigationState]);

  const setSelection = useCallback((next: TaxonomySelection, onlyHere = false) => {
    const local = onlyHere || isOverride;
    if (!local) setSharedTaxonomySelection(next);
    if (urlBacked) writeUrl(next, local);
    else setMemoryOverride(local ? next : null);
    onChange?.();
  }, [isOverride, onChange, urlBacked, writeUrl]);

  const resetToShared = useCallback(() => {
    if (urlBacked) writeUrl(isTaxonomySelectionEmpty(shared) ? null : shared, false);
    else setMemoryOverride(null);
    onChange?.();
  }, [onChange, shared, urlBacked, writeUrl]);

  const applyEverywhere = useCallback(() => {
    setSharedTaxonomySelection(selection);
    if (urlBacked) writeUrl(isTaxonomySelectionEmpty(selection) ? null : selection, false);
    else setMemoryOverride(null);
  }, [selection, urlBacked, writeUrl]);

  const clearAll = useCallback((removeParams: readonly string[] = []) => {
    const empty = emptyTaxonomySelection();
    if (!isOverride) setSharedTaxonomySelection(empty);
    if (urlBacked) writeUrl(empty, isOverride, removeParams);
    else setMemoryOverride(isOverride ? empty : null);
    onChange?.();
  }, [isOverride, onChange, urlBacked, writeUrl]);

  return { selection, isOverride, setSelection, resetToShared, applyEverywhere, clearAll };
};
