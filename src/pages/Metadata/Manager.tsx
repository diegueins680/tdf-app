import { useEffect, useMemo, useState } from 'react';
import { Button, Stack, Typography } from '@mui/material';
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

export default function MetadataManager() {
  const [rows, setRows] = useState<MetadataRow[]>(EMPTY_ROWS);

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
      });

    return () => {
      isActive = false;
    };
  }, []);

  return (
    <Stack spacing={3} sx={{ p: 3 }}>
      <Typography component="h1" variant="h4">
        Metadata Manager
      </Typography>

      <div style={{ height: 560, width: '100%', backgroundColor: '#fff' }}>
        <DataGrid
          rows={rows}
          columns={columns}
          getRowId={(row: MetadataRow) => row.catalog_id}
          disableRowSelectionOnClick
        />
      </div>

      <Stack direction="row" spacing={1.5}>
        <Button variant="contained">Import CSV/JSON</Button>
        <Button variant="outlined">Export CSV</Button>
        <Button variant="outlined">Export JSON</Button>
      </Stack>
    </Stack>
  );
}
