import { useMemo } from 'react';
import { Box, Button, Chip, Paper, Stack, Typography } from '@mui/material';
import MenuBookIcon from '@mui/icons-material/MenuBook';
import DownloadIcon from '@mui/icons-material/Download';
import MapIcon from '@mui/icons-material/Map';
import { useQuery } from '@tanstack/react-query';
import { Meta } from '../api/meta';

const API_BASE = import.meta.env.VITE_API_BASE || '';

export default function DocsPage() {
  const docsUrl = `${API_BASE}/docs`;
  const openApiUrl = `${API_BASE}/openapi.yaml`;

  const { data: version } = useQuery({ queryKey: ['meta', 'version'], queryFn: Meta.version });

  const buildInfo = useMemo(() => {
    if (!version) return '—';
    const date = version.buildTime ? new Date(version.buildTime) : null;
    const formatted = date && !Number.isNaN(date.getTime())
      ? new Intl.DateTimeFormat('es-EC', { dateStyle: 'medium', timeStyle: 'short' }).format(date)
      : version.buildTime ?? '—';
    return `${version.name} v${version.version} — ${formatted}`;
  }, [version]);

  return (
    <Stack gap={4}>
      <Paper sx={{ p: { xs: 3, md: 4 } }}>
        <Stack gap={1}>
          <Typography variant="overline">Documentación</Typography>
          <Typography variant="h4" fontWeight={600}>
            API & flujos de TDF HQ
          </Typography>
          <Typography variant="body1" color="text.secondary">
            Consulta los contratos de la API, guías de integración y el mapa funcional desde un solo lugar.
          </Typography>
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
            <Button
              variant="contained"
              startIcon={<MenuBookIcon />}
              href={docsUrl}
              target="_blank"
              rel="noreferrer"
            >
              Abrir Redoc
            </Button>
            <Button
              variant="outlined"
              startIcon={<DownloadIcon />}
              href={openApiUrl}
              target="_blank"
              rel="noreferrer"
            >
              Descargar OpenAPI
            </Button>
            <Button
              variant="text"
              startIcon={<MapIcon />}
              href="https://github.com/diegueins680/tdf-app/tree/main/docs"
              target="_blank"
              rel="noreferrer"
            >
              Guías funcionales
            </Button>
          </Stack>
          <Chip label={buildInfo} size="small" sx={{ alignSelf: 'flex-start', mt: 1 }} />
        </Stack>
      </Paper>

      <Paper sx={{ p: 0, overflow: 'hidden' }}>
        <Box
          component="iframe"
          src={docsUrl}
          title="TDF API Docs"
          sx={{
            border: 'none',
            width: '100%',
            minHeight: '70vh',
            bgcolor: 'background.paper',
          }}
        />
      </Paper>
    </Stack>
  );
}
