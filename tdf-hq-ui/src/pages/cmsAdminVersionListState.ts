const DEFAULT_EMPTY_MESSAGE = 'No hay versiones guardadas todavía.';
const FILTERED_EMPTY_MESSAGE =
  'Ninguna versión coincide con los filtros actuales. Limpia los filtros para volver a ver el historial completo.';

export interface CmsVersionListUiState {
  emptyMessage: string | null;
  showEmptyReset: boolean;
  showToolbarReset: boolean;
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

  if (hasVisibleVersions) {
    return {
      emptyMessage: null,
      showEmptyReset: false,
      showToolbarReset: hasActiveFilters,
    };
  }

  if (hasActiveFilters && totalVersions > 0) {
    return {
      emptyMessage: FILTERED_EMPTY_MESSAGE,
      showEmptyReset: true,
      showToolbarReset: false,
    };
  }

  return {
    emptyMessage: DEFAULT_EMPTY_MESSAGE,
    showEmptyReset: false,
    showToolbarReset: false,
  };
};
