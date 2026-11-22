// TAX-FILTER-001: one shared filter set, overridable per surface.
//
// * A surface shows the shared set unless it carries its own selection.
// * URL-backed surfaces keep the effective selection in the query string, so
//   a copied link reproduces the view. A link whose filters differ from the
//   viewer's shared set opens as a per-surface override.
// * Changing filters updates the shared set (every surface follows) unless
//   the viewer chose "only here", which keeps them on this surface.
import { useCallback, useEffect, useMemo, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { useSearchParams } from 'react-router-dom';
import { Taxonomy, type TaxonomyFilterOptions, type TaxonomySelection } from '../../api/taxonomy';
import {
  emptyTaxonomySelection,
  hasTaxonomyUrlState,
  isTaxonomySelectionEmpty,
  readTaxonomySelectionFromUrl,
  taxonomySelectionsEqual,
  TAXONOMY_LOCAL_SCOPE_KEY,
  TAXONOMY_LOCAL_SCOPE_VALUE,
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
  clearAll: () => void;
}

interface Options {
  /** Keep the selection in the URL (pages). Overlays such as the radio keep it in memory. */
  urlBacked: boolean;
  /** Called after a change so a page can reset its pagination. */
  onChange?: () => void;
}

export const useSurfaceTaxonomyFilters = ({ urlBacked, onChange }: Options): SurfaceTaxonomyFilters => {
  const shared = useSharedTaxonomySelection();
  const [searchParams, setSearchParams] = useSearchParams();
  const [memoryOverride, setMemoryOverride] = useState<TaxonomySelection | null>(null);

  const urlHasState = urlBacked && hasTaxonomyUrlState(searchParams);
  const urlSelection = useMemo(() => readTaxonomySelectionFromUrl(searchParams), [searchParams]);
  const urlMarkedLocal = urlBacked && searchParams.get(TAXONOMY_LOCAL_SCOPE_KEY) === TAXONOMY_LOCAL_SCOPE_VALUE;

  const override = urlBacked ? (urlHasState ? urlSelection : null) : memoryOverride;
  const selection = override ?? shared;
  const isOverride = override !== null && (urlMarkedLocal || !taxonomySelectionsEqual(override, shared));

  const writeUrl = useCallback((next: TaxonomySelection | null, local: boolean) => {
    setSearchParams((current) => {
      const updated = writeTaxonomySelectionToUrl(current, next, local);
      updated.delete('page');
      return updated;
    }, { replace: true });
  }, [setSearchParams]);

  // Mirror the shared set into the URL so a copied link reproduces the view.
  useEffect(() => {
    if (!urlBacked || urlHasState || isTaxonomySelectionEmpty(shared)) return;
    setSearchParams((current) => writeTaxonomySelectionToUrl(current, shared, false), { replace: true });
  }, [shared, setSearchParams, urlBacked, urlHasState]);

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

  const clearAll = useCallback(() => setSelection(emptyTaxonomySelection()), [setSelection]);

  return { selection, isOverride, setSelection, resetToShared, applyEverywhere, clearAll };
};
