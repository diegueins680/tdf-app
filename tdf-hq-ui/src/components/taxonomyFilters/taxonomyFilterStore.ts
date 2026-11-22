// TAX-FILTER-001: the one filter set shared by every surface. It lives in a
// tiny external store (persisted per browser) so overlays such as the radio
// widget read it without a provider.
import { useSyncExternalStore } from 'react';
import type { TaxonomySelection } from '../../api/taxonomy';
import { emptyTaxonomySelection, normalizeTaxonomySelection, taxonomySelectionsEqual } from './taxonomyFilterModel';

const STORAGE_KEY = 'tdf-hq-ui/taxonomy-filters/v1';

interface StoredState {
  selection: TaxonomySelection;
  /** The "near me" default has been considered once; never re-applied over the viewer's own choice. */
  initialized: boolean;
}

const listeners = new Set<() => void>();

const readStorage = (): StoredState => {
  try {
    const raw = window.localStorage.getItem(STORAGE_KEY);
    if (!raw) return { selection: emptyTaxonomySelection(), initialized: false };
    const parsed = JSON.parse(raw) as { selection?: unknown; initialized?: unknown };
    return { selection: normalizeTaxonomySelection(parsed.selection), initialized: parsed.initialized === true };
  } catch {
    return { selection: emptyTaxonomySelection(), initialized: false };
  }
};

let state: StoredState = typeof window === 'undefined'
  ? { selection: emptyTaxonomySelection(), initialized: false }
  : readStorage();

const persist = () => {
  try {
    window.localStorage.setItem(STORAGE_KEY, JSON.stringify(state));
  } catch {
    // Storage may be unavailable (private mode); the in-memory state still works.
  }
};

const emit = () => listeners.forEach((listener) => listener());

export const getSharedTaxonomySelection = () => state.selection;
export const isSharedTaxonomyInitialized = () => state.initialized;

export const setSharedTaxonomySelection = (selection: TaxonomySelection) => {
  const normalized = normalizeTaxonomySelection(selection);
  if (state.initialized && taxonomySelectionsEqual(normalized, state.selection)) return;
  state = { selection: normalized, initialized: true };
  persist();
  emit();
};

export const markSharedTaxonomyInitialized = () => {
  if (state.initialized) return;
  state = { ...state, initialized: true };
  persist();
  emit();
};

/** Test seam: reload from storage. */
export const reloadSharedTaxonomyState = () => {
  state = readStorage();
  emit();
};

const subscribe = (listener: () => void) => {
  listeners.add(listener);
  const onStorage = (event: StorageEvent) => {
    if (event.key === STORAGE_KEY) {
      state = readStorage();
      listener();
    }
  };
  window.addEventListener('storage', onStorage);
  return () => {
    listeners.delete(listener);
    window.removeEventListener('storage', onStorage);
  };
};

export const useSharedTaxonomySelection = (): TaxonomySelection =>
  useSyncExternalStore(subscribe, getSharedTaxonomySelection, getSharedTaxonomySelection);
