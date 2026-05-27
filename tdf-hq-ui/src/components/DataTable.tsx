import type { ReactNode } from 'react';
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
}: DataTableProps<T>) {
  const showEmpty = !loading && rows.length === 0;
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
      <Table size={size} stickyHeader={stickyHeader}>
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
                {col.header}
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
          renderTable(rows)
        ) : (
          <LazyPaginatedList
            items={rows}
            pagination={{ itemLabel, initialRowsPerPage, rowsPerPageOptions }}
            renderItems={(visibleRows, { startIndex }) => renderTable(visibleRows, startIndex)}
          />
        )}
      </CardContent>
    </Card>
  );
}
