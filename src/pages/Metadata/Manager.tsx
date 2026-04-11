import { useEffect, useMemo, useState, type MouseEvent } from 'react';
import { Box, Button, Menu, MenuItem, Paper, Stack, Typography } from '@mui/material';
import { DataGrid, GridColDef } from '@mui/x-data-grid';
import { api } from '../../api/client';

interface MetadataRow {
  catalog_id: string;
  artist_name: string;
  project_title: string;
  session_type: string;
  bpm: number;
  key: string;
  genre: string;
}

const EMPTY_ROWS: MetadataRow[] = [];

function downloadMetadataFile(content: string, filename: string, mimeType: string) {
  const blob = new Blob([content], { type: mimeType });
  const url = URL.createObjectURL(blob);
  const link = document.createElement('a');
  link.href = url;
  link.download = filename;
  link.click();
  URL.revokeObjectURL(url);
}

function downloadMetadataCsv(rows: readonly MetadataRow[]) {
  const header = ['Catalog ID', 'Artist', 'Project', 'Type', 'BPM', 'Key', 'Genre'];
  const data = rows.map((row) => [
    row.catalog_id,
    row.artist_name,
    row.project_title,
    row.session_type,
    row.bpm,
    row.key,
    row.genre,
  ]);
  const csv = [header, ...data]
    .map((line) => line.map((value) => `"${String(value).replace(/"/g, '""')}"`).join(','))
    .join('\n');

  downloadMetadataFile(csv, 'metadata-export.csv', 'text/csv;charset=utf-8;');
}

function downloadMetadataJson(rows: readonly MetadataRow[]) {
  downloadMetadataFile(JSON.stringify(rows, null, 2), 'metadata-export.json', 'application/json;charset=utf-8;');
}

export default function MetadataManager() {
  const [rows, setRows] = useState<MetadataRow[]>(EMPTY_ROWS);
  const [isLoading, setIsLoading] = useState(true);
  const [exportAnchorEl, setExportAnchorEl] = useState<null | HTMLElement>(null);

  const columns: GridColDef<MetadataRow>[] = useMemo(
    () => [
      { field: 'catalog_id', headerName: 'Catalog ID', flex: 1, minWidth: 140 },
      { field: 'artist_name', headerName: 'Artist', flex: 1, minWidth: 160 },
      { field: 'project_title', headerName: 'Project', flex: 1, minWidth: 180 },
      { field: 'session_type', headerName: 'Type', width: 140 },
      { field: 'bpm', headerName: 'BPM', width: 110 },
      { field: 'key', headerName: 'Key', width: 110 },
      { field: 'genre', headerName: 'Genre', width: 160 },
    ],
    []
  );

  useEffect(() => {
    let isActive = true;
    setIsLoading(true);

    api<MetadataRow[]>('/api/metadata')
      .then((data) => {
        if (isActive && Array.isArray(data)) {
          setRows(data);
        }
      })
      .catch(() => {
        if (isActive) {
          setRows(EMPTY_ROWS);
        }
      })
      .finally(() => {
        if (isActive) {
          setIsLoading(false);
        }
      });

    return () => {
      isActive = false;
    };
  }, []);

  const hasRows = rows.length > 0;
  const showEmptyState = !isLoading && !hasRows;
  const singleMetadataRow = !isLoading && rows.length === 1 ? (rows[0] ?? null) : null;
  const exportMenuOpen = Boolean(exportAnchorEl);
  const toolbarDescription = isLoading
    ? 'Cargando catálogo…'
    : showEmptyState
      ? 'Empieza con Importar metadatos. La exportación aparecerá cuando exista el primer registro.'
      : singleMetadataRow
        ? 'Ya puedes exportar el catálogo actual en CSV o JSON. La tabla aparecerá cuando exista un segundo registro para comparar.'
        : 'Exporta el catálogo actual en CSV o JSON desde un solo menú para evitar acciones duplicadas.';

  const handleOpenExportMenu = (event: MouseEvent<HTMLButtonElement>) => {
    setExportAnchorEl(event.currentTarget);
  };

  const handleCloseExportMenu = () => {
    setExportAnchorEl(null);
  };

  const handleExport = (format: 'csv' | 'json') => {
    if (format === 'csv') {
      downloadMetadataCsv(rows);
    } else {
      downloadMetadataJson(rows);
    }
    handleCloseExportMenu();
  };

  return (
    <Stack spacing={3} sx={{ p: 3 }}>
      <Typography component="h1" variant="h4">
        Metadata Manager
      </Typography>

      {showEmptyState ? (
        <Paper
          variant="outlined"
          sx={{
            minHeight: 320,
            px: 3,
            py: 4,
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            textAlign: 'center',
          }}
        >
          <Stack spacing={1.5} sx={{ maxWidth: 520 }}>
            <Typography variant="h6">Todavía no hay metadatos cargados.</Typography>
            <Typography variant="body2" color="text.secondary">
              Usa Importar metadatos para traer tu primer catálogo. Cuando exista el primero,
              aquí podrás revisarlo y exportarlo en CSV o JSON desde un solo lugar.
            </Typography>
          </Stack>
        </Paper>
      ) : singleMetadataRow ? (
        <Paper variant="outlined" sx={{ px: 3, py: 3 }}>
          <Stack spacing={1.5} sx={{ maxWidth: 640 }}>
            <Box>
              <Typography variant="h6">Primer registro cargado.</Typography>
              <Typography variant="body2" color="text.secondary" sx={{ mt: 0.5 }}>
                Revísalo aquí; cuando exista el segundo, volverá la tabla para comparar catálogo, tempo y tonalidad.
              </Typography>
            </Box>
            <Stack
              spacing={0.75}
              sx={{
                border: '1px solid',
                borderColor: 'divider',
                borderRadius: 2,
                px: 2,
                py: 1.5,
              }}
            >
              <Typography variant="body2">
                <Box component="span" sx={{ fontWeight: 600 }}>Catalog ID:</Box> {singleMetadataRow.catalog_id}
              </Typography>
              <Typography variant="body2" color="text.secondary">
                <Box component="span" sx={{ fontWeight: 600 }}>Artist:</Box> {singleMetadataRow.artist_name}
              </Typography>
              <Typography variant="body2" color="text.secondary">
                <Box component="span" sx={{ fontWeight: 600 }}>Project:</Box> {singleMetadataRow.project_title}
              </Typography>
              <Typography variant="body2" color="text.secondary">
                <Box component="span" sx={{ fontWeight: 600 }}>Type:</Box> {singleMetadataRow.session_type}
              </Typography>
              <Typography variant="body2" color="text.secondary">
                <Box component="span" sx={{ fontWeight: 600 }}>BPM / Key:</Box> {singleMetadataRow.bpm} / {singleMetadataRow.key}
              </Typography>
              <Typography variant="body2" color="text.secondary">
                <Box component="span" sx={{ fontWeight: 600 }}>Genre:</Box> {singleMetadataRow.genre}
              </Typography>
            </Stack>
          </Stack>
        </Paper>
      ) : (
        <Box sx={{ height: 560, width: '100%', backgroundColor: '#fff' }}>
          <DataGrid
            rows={rows}
            columns={columns}
            loading={isLoading}
            getRowId={(row: MetadataRow) => row.catalog_id}
            disableRowSelectionOnClick
          />
        </Box>
      )}

      <Stack spacing={1.5}>
        <Stack direction="row" spacing={1.5} flexWrap="wrap" useFlexGap>
          <Button variant="contained">Importar metadatos</Button>
          {hasRows && (
            <Button
              variant="outlined"
              onClick={handleOpenExportMenu}
              aria-controls={exportMenuOpen ? 'metadata-export-menu' : undefined}
              aria-expanded={exportMenuOpen ? 'true' : undefined}
              aria-haspopup="menu"
            >
              Exportar
            </Button>
          )}
        </Stack>
        <Typography variant="body2" color="text.secondary">
          {toolbarDescription}
        </Typography>
      </Stack>

      <Menu
        id="metadata-export-menu"
        anchorEl={exportAnchorEl}
        open={exportMenuOpen}
        onClose={handleCloseExportMenu}
      >
        <MenuItem onClick={() => handleExport('csv')}>Exportar CSV</MenuItem>
        <MenuItem onClick={() => handleExport('json')}>Exportar JSON</MenuItem>
      </Menu>
    </Stack>
  );
}
