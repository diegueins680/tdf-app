import { Card, CardContent, Grid, Stack, Typography, Button, Tooltip } from '@mui/material';
import RefreshIcon from '@mui/icons-material/Refresh';
import { useQuery, useQueryClient } from '@tanstack/react-query';
import { Meta } from '../api/meta';
import { API_BASE_URL } from '../api/client';

export default function SystemStatusPage() {
  const qc = useQueryClient();
  const versionQuery = useQuery({ queryKey: ['meta', 'version'], queryFn: Meta.version });
  const healthQuery = useQuery({ queryKey: ['meta', 'health'], queryFn: Meta.health });
  const apiBase = API_BASE_URL.trim();
  const apiBaseConfigured = apiBase.length > 0;

  const handleRefresh = () => {
    void qc.invalidateQueries({ queryKey: ['meta', 'version'] });
    void qc.invalidateQueries({ queryKey: ['meta', 'health'] });
  };

  return (
    <Stack spacing={3}>
      <Stack direction="row" justifyContent="space-between" alignItems="center">
        <Typography variant="h4" fontWeight={700}>Estado del sistema</Typography>
        <Tooltip title="Refrescar">
          <Button startIcon={<RefreshIcon />} onClick={handleRefresh}>
            Actualizar
          </Button>
        </Tooltip>
      </Stack>
      <Grid container spacing={2}>
        <Grid item xs={12} md={6}>
          <Card>
            <CardContent>
              <Typography variant="h6">Versión</Typography>
              {versionQuery.isLoading && <Typography>Cargando versión…</Typography>}
              {versionQuery.error && (
                <Typography color="error">Error al cargar versión</Typography>
              )}
              {versionQuery.data && (
                <Stack spacing={0.5} mt={1}>
                  <Row label="Nombre" value={versionQuery.data.name} />
                  <Row label="Versión" value={versionQuery.data.version} />
                  <Row label="Commit" value={versionQuery.data.commit ?? '—'} />
                  <Row label="Build time" value={versionQuery.data.buildTime ?? '—'} />
                  <Row label="VITE_API_BASE" value={apiBaseConfigured ? 'set' : 'missing'} />
                  <Row label="API base URL" value={apiBaseConfigured ? apiBase : '—'} />
                </Stack>
              )}
            </CardContent>
          </Card>
        </Grid>
        <Grid item xs={12} md={6}>
          <Card>
            <CardContent>
              <Typography variant="h6">Health</Typography>
              {healthQuery.isLoading && <Typography>Cargando health…</Typography>}
              {healthQuery.error && (
                <Typography color="error">Error al cargar health</Typography>
              )}
              {healthQuery.data && (
                <Stack spacing={0.5} mt={1}>
                  <Row label="Status" value={healthQuery.data.status} />
                  <Row label="Versión" value={healthQuery.data.version ?? '—'} />
                </Stack>
              )}
            </CardContent>
          </Card>
        </Grid>
      </Grid>
    </Stack>
  );
}

function Row({ label, value }: { label: string; value: string }) {
  return (
    <Stack direction="row" spacing={1}>
      <Typography variant="body2" color="text.secondary" sx={{ minWidth: 120 }}>
        {label}
      </Typography>
      <Typography variant="body2">{value}</Typography>
    </Stack>
  );
}
