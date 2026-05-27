import { useDeferredValue, useEffect, useMemo, useState, type ChangeEvent, type ReactNode } from 'react';
import { Box, LinearProgress, Stack, TablePagination, Typography, type SxProps, type Theme } from '@mui/material';

const DEFAULT_ROWS_PER_PAGE_OPTIONS = [10, 25, 50] as const;

const normalizeRowsPerPageOptions = (options: readonly number[]) => {
  const normalized = options
    .map((value) => Math.trunc(value))
    .filter((value) => Number.isSafeInteger(value) && value > 0);
  const unique = Array.from(new Set(normalized));
  return unique.length > 0 ? unique.sort((left, right) => left - right) : [...DEFAULT_ROWS_PER_PAGE_OPTIONS];
};

interface LazyPaginatedListProps<T> {
  items: readonly T[];
  renderItems: (
    items: readonly T[],
    meta: { page: number; rowsPerPage: number; startIndex: number; totalItems: number },
  ) => ReactNode;
  loading?: boolean;
  pagination?: false | {
    itemLabel?: string;
    initialRowsPerPage?: number;
    rowsPerPageOptions?: readonly number[];
    labelRowsPerPage?: string;
    resetKey?: unknown;
    sx?: SxProps<Theme>;
  };
}

export default function LazyPaginatedList<T>({
  items,
  renderItems,
  loading = false,
  pagination = {},
}: LazyPaginatedListProps<T>) {
  const paginationConfig = pagination === false ? null : pagination;
  const itemLabel = paginationConfig?.itemLabel ?? 'registros';
  const labelRowsPerPage = paginationConfig?.labelRowsPerPage ?? 'Por página';
  const initialRowsPerPage = paginationConfig?.initialRowsPerPage;
  const rowsPerPageOptions = paginationConfig?.rowsPerPageOptions ?? DEFAULT_ROWS_PER_PAGE_OPTIONS;
  const resetKey = paginationConfig?.resetKey;
  const normalizedOptions = useMemo(() => normalizeRowsPerPageOptions(rowsPerPageOptions), [rowsPerPageOptions]);
  const initialPageSize = initialRowsPerPage && normalizedOptions.includes(initialRowsPerPage)
    ? initialRowsPerPage
    : (normalizedOptions[0] ?? DEFAULT_ROWS_PER_PAGE_OPTIONS[0]);
  const [page, setPage] = useState(0);
  const [rowsPerPage, setRowsPerPage] = useState(initialPageSize);
  const deferredItems = useDeferredValue(items);
  const totalItems = deferredItems.length;
  const maxPage = Math.max(0, Math.ceil(totalItems / rowsPerPage) - 1);
  const isUpdating = loading || deferredItems !== items;
  const loadingLabel = loading ? 'Cargando resultados...' : 'Actualizando resultados...';

  useEffect(() => {
    setRowsPerPage((current) => (normalizedOptions.includes(current) ? current : initialPageSize));
  }, [initialPageSize, normalizedOptions]);

  useEffect(() => {
    setPage(0);
  }, [resetKey, rowsPerPage]);

  useEffect(() => {
    if (page > maxPage) {
      setPage(maxPage);
    }
  }, [maxPage, page]);

  const startIndex = page * rowsPerPage;
  const visibleItems = useMemo(
    () => deferredItems.slice(startIndex, startIndex + rowsPerPage),
    [deferredItems, rowsPerPage, startIndex],
  );

  const showPagination = Boolean(paginationConfig) && totalItems > rowsPerPage;

  const handleChangeRowsPerPage = (event: ChangeEvent<HTMLInputElement | HTMLTextAreaElement>) => {
    const nextRowsPerPage = Number(event.target.value);
    setRowsPerPage(Number.isSafeInteger(nextRowsPerPage) && nextRowsPerPage > 0 ? nextRowsPerPage : initialPageSize);
  };

  return (
    <Box aria-busy={isUpdating ? true : undefined}>
      {isUpdating && (
        <Stack
          role="status"
          aria-live="polite"
          direction="row"
          spacing={1}
          alignItems="center"
          sx={{ mb: 1, color: 'text.secondary' }}
        >
          <Box sx={{ flexGrow: 1, minWidth: 48 }}>
            <LinearProgress aria-label={loadingLabel} sx={{ height: 3, borderRadius: 1 }} />
          </Box>
          <Typography variant="caption" sx={{ flex: '0 0 auto' }}>
            {loadingLabel}
          </Typography>
        </Stack>
      )}
      {renderItems(visibleItems, { page, rowsPerPage, startIndex, totalItems })}
      {showPagination && (
        <Box
          sx={[
            { display: 'flex', justifyContent: 'flex-end', mt: 1 },
            ...(Array.isArray(paginationConfig?.sx) ? paginationConfig.sx : paginationConfig?.sx ? [paginationConfig.sx] : []),
          ]}
        >
          <TablePagination
            component="div"
            count={totalItems}
            page={page}
            onPageChange={(_, nextPage) => setPage(nextPage)}
            rowsPerPage={rowsPerPage}
            rowsPerPageOptions={normalizedOptions}
            onRowsPerPageChange={handleChangeRowsPerPage}
            labelRowsPerPage={labelRowsPerPage}
            labelDisplayedRows={({ from, to, count }) =>
              `${from}-${to} de ${count === -1 ? `más de ${to}` : `${count} ${itemLabel}`}`}
          />
        </Box>
      )}
    </Box>
  );
}
