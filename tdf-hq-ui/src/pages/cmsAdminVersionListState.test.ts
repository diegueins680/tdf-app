import { getCmsVersionListUiState } from './cmsAdminVersionListState';

describe('getCmsVersionListUiState', () => {
  it('hides reset chrome when the version list is already unfiltered', () => {
    expect(getCmsVersionListUiState({
      filteredCount: 3,
      minVersionFilter: null,
      statusFilter: 'all',
      totalVersions: 3,
    })).toEqual({
      emptyMessage: null,
      showEmptyReset: false,
      showToolbarReset: false,
    });
  });

  it('keeps a toolbar reset only while filtered versions are still visible', () => {
    expect(getCmsVersionListUiState({
      filteredCount: 2,
      minVersionFilter: 3,
      statusFilter: 'published',
      totalVersions: 5,
    })).toEqual({
      emptyMessage: null,
      showEmptyReset: false,
      showToolbarReset: true,
    });
  });

  it('swaps the generic empty state for a filter-specific reset path when filters hide every version', () => {
    expect(getCmsVersionListUiState({
      filteredCount: 0,
      minVersionFilter: 9,
      statusFilter: 'draft',
      totalVersions: 5,
    })).toEqual({
      emptyMessage:
        'Ninguna versión coincide con los filtros actuales. Limpia los filtros para volver a ver el historial completo.',
      showEmptyReset: true,
      showToolbarReset: false,
    });
  });

  it('keeps the first-run empty state when there are no saved versions at all', () => {
    expect(getCmsVersionListUiState({
      filteredCount: 0,
      minVersionFilter: null,
      statusFilter: 'all',
      totalVersions: 0,
    })).toEqual({
      emptyMessage: 'No hay versiones guardadas todavía.',
      showEmptyReset: false,
      showToolbarReset: false,
    });
  });
});
