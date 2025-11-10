import { useMemo } from 'react';
import { useQuery } from '@tanstack/react-query';
import {
  Alert,
  Button,
  Chip,
  Grid,
  Link,
  Paper,
  Stack,
  Typography,
} from '@mui/material';
import OpenInNewIcon from '@mui/icons-material/OpenInNew';
import ScienceIcon from '@mui/icons-material/Science';
import ShieldIcon from '@mui/icons-material/Shield';
import GroupsIcon from '@mui/icons-material/Groups';
import { Meta } from '../api/meta';

const RESOURCE_LINKS = [
  { label: 'Documentación (Redoc)', href: '/docs' },
  { label: 'OpenAPI YAML', href: '/openapi.yaml' },
  { label: 'Guía de integración', href: 'https://github.com/diegueins680/tdf-app/tree/main/docs' },
];

const MODULES = [
  'CRM & contactos',
  'Estudio (calendario, órdenes)',
  'Label & distribución',
  'Eventos y operación',
  'Finanzas',
  'Escuela / Academy',
];

export default function AboutPage() {
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
  const hasMetaError = Boolean(versionError ?? healthError);

  const buildDate = useMemo(() => {
    if (!version?.buildTime) return '—';
    const date = new Date(version.buildTime);
    if (Number.isNaN(date.getTime())) return version.buildTime;
    return new Intl.DateTimeFormat('es-EC', {
      dateStyle: 'medium',
      timeStyle: 'short',
    }).format(date);
  }, [version]);

  return (
    <Stack gap={4}>
      <Paper sx={{ p: { xs: 3, md: 4 } }}>
        <Stack gap={1.5}>
          <Typography variant="h4" fontWeight={600}>
            Acerca de TDF Records HQ
          </Typography>
          <Typography variant="body1" color="text.secondary">
            Plataforma interna que orquesta los flujos de CRM, estudio, label, operación y escuela para el equipo
            de TDF. Aquí encontrarás la versión instalada, enlaces a la documentación y el estado actual de los servicios.
          </Typography>
          <Stack direction={{ xs: 'column', sm: 'row' }} gap={2}>
            <Button
              variant="contained"
              size="large"
              endIcon={<OpenInNewIcon />}
              href="/docs"
              target="_blank"
              rel="noreferrer"
            >
              Abrir documentación
            </Button>
            <Button
              variant="outlined"
              size="large"
              endIcon={<ShieldIcon />}
              href="/seguridad"
            >
              Reglas de seguridad
            </Button>
          </Stack>
        </Stack>
      </Paper>

      {hasMetaError && (
        <Alert severity="error">
          {versionError instanceof Error ? versionError.message : ''}
          {healthError instanceof Error ? ` · ${healthError.message}` : ''}
        </Alert>
      )}

      <Grid container spacing={3}>
        <Grid item xs={12} md={4}>
          <Paper sx={{ p: 3, height: '100%' }}>
            <Stack gap={1}>
              <Typography variant="overline">Versión</Typography>
              <Typography variant="h5" fontWeight={600}>
                {version?.name ?? 'Aplicación'}
              </Typography>
              <Typography variant="body1">
                Release <strong>{version?.version ?? '—'}</strong>
              </Typography>
              <Typography variant="body2" color="text.secondary">
                Compilado: {buildDate}
              </Typography>
            </Stack>
          </Paper>
        </Grid>
        <Grid item xs={12} md={4}>
          <Paper sx={{ p: 3, height: '100%' }}>
            <Stack gap={1}>
              <Typography variant="overline">Integridad</Typography>
              {loading ? (
                <Typography>Cargando estado…</Typography>
              ) : (
                <>
                  <Typography variant="h5" fontWeight={600}>
                    {health?.status ?? '—'}
                  </Typography>
                  <Typography variant="body2" color="text.secondary">
                    Código base: {health?.version ?? '—'}
                  </Typography>
                  <Chip
                    icon={<ScienceIcon />}
                    label="API meta"
                    size="small"
                    sx={{ alignSelf: 'flex-start', mt: 1 }}
                    color={(health?.status ?? '').toLowerCase() === 'ok' ? 'success' : 'warning'}
                  />
                </>
              )}
            </Stack>
          </Paper>
        </Grid>
        <Grid item xs={12} md={4}>
          <Paper sx={{ p: 3, height: '100%' }}>
            <Stack gap={1}>
              <Typography variant="overline">Contacto</Typography>
              <Typography variant="h6">Equipo de producto</Typography>
              <Typography variant="body2" color="text.secondary">
                Para soporte, escribe a <Link href="mailto:product@tdfrecords.com">product@tdfrecords.com</Link> o
                abre un ticket en Slack #hq-support.
              </Typography>
              <Chip icon={<GroupsIcon />} label="TDF Ops" size="small" sx={{ alignSelf: 'flex-start', mt: 1 }} />
            </Stack>
          </Paper>
        </Grid>
      </Grid>

      <Paper sx={{ p: 3 }}>
        <Stack gap={2}>
          <Typography variant="h6">Módulos activos</Typography>
          <Stack direction="row" flexWrap="wrap" gap={1}>
            {MODULES.map((module) => (
              <Chip key={module} label={module} variant="outlined" />
            ))}
          </Stack>
        </Stack>
      </Paper>

      <Paper sx={{ p: 3 }}>
        <Stack gap={2}>
          <Typography variant="h6">Recursos</Typography>
          <Stack gap={1}>
            {RESOURCE_LINKS.map((resource) => (
              <Stack
                key={resource.href}
                direction={{ xs: 'column', sm: 'row' }}
                spacing={1}
                alignItems={{ sm: 'center' }}
                justifyContent="space-between"
              >
                <Typography variant="body1">{resource.label}</Typography>
                <Button
                  endIcon={<OpenInNewIcon />}
                  component={Link}
                  href={resource.href}
                  target={resource.href.startsWith('http') ? '_blank' : undefined}
                  rel="noreferrer"
                >
                  Abrir
                </Button>
              </Stack>
            ))}
          </Stack>
        </Stack>
      </Paper>
    </Stack>
  );
}
