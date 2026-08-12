import { useMemo, useState, type ReactNode } from 'react';
import {
  Box,
  Card,
  CardContent,
  Paper,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  TableSortLabel,
  Typography,
  Skeleton,
} from '@mui/material';
import { EmptyState } from './PageShell';
import LazyPaginatedList from './LazyPaginatedList';

export interface DataTableColumn<T> {
  key: string;
  header: ReactNode;
  width?: string | number;
  align?: 'left' | 'center' | 'right';
  cell: (row: T, index: number) => ReactNode;
  /** Enable client-side sorting on this column */
  sortable?: boolean;
  /** Data field name used for sorting (required when sortable is true) */
  accessorKey?: string;
}

export interface DataTableProps<T> {
  columns: DataTableColumn<T>[];
  rows: T[];
  loading?: boolean;
  skeletonRows?: number;
  keyExtractor: (row: T) => string;
  emptyTitle?: string;
  emptyDescription?: string;
  emptyAction?: ReactNode;
  searchSlot?: ReactNode;
  actionsSlot?: ReactNode;
  caption?: ReactNode;
  size?: 'small' | 'medium';
  stickyHeader?: boolean;
  maxHeight?: string | number;
  pagination?: boolean;
  initialRowsPerPage?: number;
  rowsPerPageOptions?: readonly number[];
  itemLabel?: string;
  ariaLabel?: string;
}

export default function DataTable<T>({
  columns,
  rows,
  loading = false,
  skeletonRows = 4,
  keyExtractor,
  emptyTitle = 'Sin datos',
  emptyDescription = 'Aún no hay registros para mostrar.',
  emptyAction,
  searchSlot,
  actionsSlot,
  caption,
  size = 'small',
  stickyHeader = true,
  maxHeight,
  pagination = true,
  initialRowsPerPage = 25,
  rowsPerPageOptions = [10, 25, 50],
  itemLabel = 'registros',
  ariaLabel = 'Tabla de datos',
}: DataTableProps<T>) {
  const [sort, setSort] = useState<{ key: string; direction: 'asc' | 'desc' | null }>({
    key: '',
    direction: null,
  });

  const handleSort = (accessorKey: string) => {
    setSort((prev) => {
      if (prev.key !== accessorKey) return { key: accessorKey, direction: 'asc' };
      if (prev.direction === 'asc') return { key: accessorKey, direction: 'desc' };
      return { key: '', direction: null };
    });
  };

  const sortedRows = useMemo(() => {
    if (!sort.key || !sort.direction) return rows;
    return [...rows].sort((a, b) => {
      const aVal = (a as Record<string, unknown>)[sort.key];
      const bVal = (b as Record<string, unknown>)[sort.key];
      if (aVal == null) return 1;
      if (bVal == null) return -1;
      const cmp =
        typeof aVal === 'string' && typeof bVal === 'string'
          ? aVal.localeCompare(bVal)
          : (aVal as number) - (bVal as number);
      return sort.direction === 'asc' ? cmp : -cmp;
    });
  }, [rows, sort]);

  const showEmpty = !loading && sortedRows.length === 0;
  const renderTable = (visibleRows: readonly T[], startIndex = 0) => (
    <TableContainer
      component={Paper}
      variant="outlined"
      sx={{
        maxHeight,
        border: 'none',
        boxShadow: 'none',
        bgcolor: 'transparent',
      }}
    >
      <Table size={size} stickyHeader={stickyHeader} aria-label={ariaLabel}>
        <TableHead>
          <TableRow>
            {columns.map((col) => (
              <TableCell
                key={col.key}
                align={col.align}
                sx={{
                  width: col.width,
                  fontWeight: 700,
                  bgcolor: 'background.paper',
                  borderBottom: '1px solid',
                  borderColor: 'divider',
                  whiteSpace: 'nowrap',
                }}
              >
                {col.sortable && col.accessorKey ? (
                  <TableSortLabel
                    active={sort.key === col.accessorKey && sort.direction !== null}
                    direction={
                      sort.key === col.accessorKey && sort.direction ? sort.direction : 'asc'
                    }
                    onClick={() => handleSort(col.accessorKey!)}
                  >
                    {col.header}
                  </TableSortLabel>
                ) : (
                  col.header
                )}
              </TableCell>
            ))}
          </TableRow>
        </TableHead>
        <TableBody>
          {loading ? (
            Array.from({ length: skeletonRows }).map((_, idx) => (
              <TableRow key={`skel-${idx}`}>
                {columns.map((col) => (
                  <TableCell key={`${col.key}-skel-${idx}`} align={col.align}>
                    <Skeleton variant="text" width="80%" />
                  </TableCell>
                ))}
              </TableRow>
            ))
          ) : (
            visibleRows.map((row, idx) => (
              <TableRow
                key={keyExtractor(row)}
                hover
                sx={{
                  '&:nth-of-type(even)': {
                    bgcolor: 'action.hover',
                  },
                }}
              >
                {columns.map((col) => (
                  <TableCell key={col.key} align={col.align}>
                    {col.cell(row, startIndex + idx)}
                  </TableCell>
                ))}
              </TableRow>
            ))
          )}
        </TableBody>
      </Table>
    </TableContainer>
  );

  return (
    <Card>
      <CardContent>
        {(searchSlot || actionsSlot || caption) && (
          <Stack
            direction={{ xs: 'column', md: 'row' }}
            justifyContent="space-between"
            alignItems={{ xs: 'stretch', md: 'center' }}
            spacing={2}
            sx={{ mb: 2 }}
          >
            {searchSlot && <Box sx={{ flexGrow: 1 }}>{searchSlot}</Box>}
            {actionsSlot && <Stack direction="row" spacing={1}>{actionsSlot}</Stack>}
          </Stack>
        )}

        {caption && (
          <Typography variant="caption" color="text.secondary" sx={{ display: 'block', mb: 1.5 }}>
            {caption}
          </Typography>
        )}

        {showEmpty ? (
          <EmptyState
            title={emptyTitle}
            description={emptyDescription}
          >
            {emptyAction}
          </EmptyState>
        ) : loading || !pagination ? (
          renderTable(sortedRows)
        ) : (
          <LazyPaginatedList
            items={sortedRows}
            pagination={{ itemLabel, initialRowsPerPage, rowsPerPageOptions }}
            renderItems={(visibleRows, { startIndex }) => renderTable(visibleRows, startIndex)}
          />
        )}
      </CardContent>
    </Card>
  );
}
