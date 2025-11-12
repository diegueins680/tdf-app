import { useMemo } from 'react';
import { useQuery } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Chip,
  CircularProgress,
  Paper,
  Stack,
  Typography,
} from '@mui/material';
import { Meta } from '../api/meta';

function formatBuildTime(value?: string | null) {
  if (!value) return '—';
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) {
    return value;
  }
  return new Intl.DateTimeFormat('es-EC', {
    dateStyle: 'medium',
    timeStyle: 'medium',
  }).format(date);
}

export default function SystemPage() {
  const {
    data: version,
    isLoading: versionLoading,
    error: versionError,
  } = useQuery({ queryKey: ['meta', 'version'], queryFn: Meta.version });

  const {
    data: health,
    isLoading: healthLoading,
    error: healthError,
  } = useQuery({ queryKey: ['meta', 'health'], queryFn: Meta.health });

  const loading = versionLoading || healthLoading;
  const errMsg = useMemo(() => {
    if (versionError instanceof Error) return versionError.message;
    if (healthError instanceof Error) return healthError.message;
    return null;
  }, [versionError, healthError]);

  const commitInfo = useMemo(() => {
    if (!version?.commit) return null;
    return version.commit.slice(0, 7);
  }, [version]);

  const healthColor = (health?.status ?? '').toLowerCase() === 'ok' ? 'success' : 'warning';

  return (
    <Stack gap={3}>
      <Typography variant="h5">Estado del sistema</Typography>

      {loading && (
        <Box display="flex" alignItems="center" justifyContent="center" minHeight={160}>
          <CircularProgress />
        </Box>
      )}

      {!loading && errMsg && (
        <Alert severity="error">{errMsg}</Alert>
      )}

      {!loading && !errMsg && (
        <Paper sx={{ p: 3 }}>
          <Stack gap={2}>
            <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" gap={2}>
              <Stack>
                <Typography variant="subtitle2" color="text.secondary">
                  Aplicación
                </Typography>
                <Typography variant="h6">{version?.name ?? '—'}</Typography>
              </Stack>
              <Stack direction="row" gap={1} alignItems="center">
                <Typography variant="subtitle2" color="text.secondary">
                  Versión
                </Typography>
                <Chip label={version?.version ?? '—'} color="primary" size="small" />
                {health && (
                  <Chip
                    label={`Estado: ${health.status}`}
                    color={healthColor}
                    size="small"
                  />
                )}
              </Stack>
            </Stack>

            <Stack direction={{ xs: 'column', sm: 'row' }} gap={2}>
              <Stack>
                <Typography variant="subtitle2" color="text.secondary">Commit</Typography>
                <Typography variant="body2">{commitInfo ?? '—'}</Typography>
              </Stack>
              <Stack>
                <Typography variant="subtitle2" color="text.secondary">Compilado</Typography>
                <Typography variant="body2">{formatBuildTime(version?.buildTime)}</Typography>
              </Stack>
              <Stack>
                <Typography variant="subtitle2" color="text.secondary">Código base</Typography>
                <Typography variant="body2">{health?.version ?? '—'}</Typography>
              </Stack>
            </Stack>
          </Stack>
        </Paper>
      )}
    </Stack>
  );
}
