const DEFAULT_EMPTY_MESSAGE = 'No hay versiones guardadas todavía.';
const FILTERED_EMPTY_MESSAGE =
  'Ninguna versión coincide con los filtros actuales. Limpia los filtros para volver a ver el historial completo.';
const SINGLE_VERSION_FILTER_HINT =
  'Los filtros aparecerán cuando exista más historial para comparar versiones.';

export interface CmsVersionListUiState {
  emptyMessage: string | null;
  showEmptyReset: boolean;
  showToolbarFilters: boolean;
  showToolbarReset: boolean;
  toolbarHint: string | null;
}

export interface CmsVersionListUiStateInput {
  filteredCount: number;
  minVersionFilter: number | null;
  statusFilter: string;
  totalVersions: number;
}

export const getCmsVersionListUiState = ({
  filteredCount,
  minVersionFilter,
  statusFilter,
  totalVersions,
}: CmsVersionListUiStateInput): CmsVersionListUiState => {
  const hasActiveFilters = statusFilter.trim().toLowerCase() !== 'all' || minVersionFilter != null;
  const hasVisibleVersions = filteredCount > 0;
  const showToolbarFilters = totalVersions > 1 || hasActiveFilters;
  const toolbarHint = !hasActiveFilters && totalVersions === 1 ? SINGLE_VERSION_FILTER_HINT : null;

  if (hasVisibleVersions) {
    return {
      emptyMessage: null,
      showEmptyReset: false,
      showToolbarFilters,
      showToolbarReset: hasActiveFilters,
      toolbarHint,
    };
  }

  if (hasActiveFilters && totalVersions > 0) {
    return {
      emptyMessage: FILTERED_EMPTY_MESSAGE,
      showEmptyReset: true,
      showToolbarFilters,
      showToolbarReset: false,
      toolbarHint,
    };
  }

  return {
    emptyMessage: DEFAULT_EMPTY_MESSAGE,
    showEmptyReset: false,
    showToolbarFilters,
    showToolbarReset: false,
    toolbarHint,
  };
};
