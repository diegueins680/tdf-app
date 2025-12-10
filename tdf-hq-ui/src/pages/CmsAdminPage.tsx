import { useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  AlertTitle,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  Divider,
  Grid,
  LinearProgress,
  MenuItem,
  Paper,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { Cms, type CmsContentDTO, type CmsContentIn } from '../api/cms';

const defaultSlugs = [
  'records-public',
  'fan-hub',
  'course-production',
];

const locales = ['es', 'en'];

const PUBLIC_BASE =
  typeof window !== 'undefined' && window.location.origin
    ? window.location.origin.replace(/\/+$/, '')
    : 'https://tdf-app.pages.dev';

const livePathForSlug = (slug: string) => {
  switch (slug) {
    case 'records-public':
      return '/records';
    case 'fan-hub':
      return '/fans';
    case 'course-production':
      return '/curso/produccion-musical-dic-2025';
    default:
      return `/${slug}`;
  }
};

export default function CmsAdminPage() {
  const qc = useQueryClient();
  const [slugFilter, setSlugFilter] = useState<string>('records-public');
  const [localeFilter, setLocaleFilter] = useState<string>('es');
  const [title, setTitle] = useState('');
  const [payload, setPayload] = useState('{}');
  const [status, setStatus] = useState<'draft' | 'published'>('draft');
  const [editingFromId, setEditingFromId] = useState<number | null>(null);

  const listQuery = useQuery({
    queryKey: ['cms-content', slugFilter, localeFilter],
    queryFn: () => Cms.list({ slug: slugFilter, locale: localeFilter }),
  });

  const liveQuery = useQuery({
    queryKey: ['cms-public', slugFilter, localeFilter],
    queryFn: () => Cms.getPublic(slugFilter, localeFilter),
    retry: 1,
    enabled: Boolean(slugFilter && localeFilter),
  });

  const createMutation = useMutation({
    mutationFn: (input: CmsContentIn) => Cms.create(input),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['cms-content'] });
      void qc.invalidateQueries({ queryKey: ['cms-public'] });
    },
  });

  const publishMutation = useMutation({
    mutationFn: (id: number) => Cms.publish(id),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['cms-content'] });
      void qc.invalidateQueries({ queryKey: ['cms-public'] });
    },
  });

  const deleteMutation = useMutation({
    mutationFn: (id: number) => Cms.remove(id),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['cms-content'] });
      void qc.invalidateQueries({ queryKey: ['cms-public'] });
    },
  });

  const versions: CmsContentDTO[] = useMemo(
    () => (Array.isArray(listQuery.data) ? listQuery.data : []),
    [listQuery.data],
  );
  const listDataInvalid = listQuery.data !== undefined && !Array.isArray(listQuery.data);
  const liveContent = liveQuery.data;

  const handleCreate = () => {
    let parsed: unknown = null;
    try {
      parsed = JSON.parse(payload);
    } catch {
      alert('Payload no es JSON válido.');
      return;
    }
    const normalizedTitle = title.trim();
    createMutation.mutate({
      cciSlug: slugFilter,
      cciLocale: localeFilter,
      cciTitle: normalizedTitle.length > 0 ? normalizedTitle : undefined,
      cciStatus: status,
      cciPayload: parsed,
    });
    setEditingFromId(null);
  };

  const handleLoadLive = () => {
    if (!liveContent) return;
    setTitle(liveContent.ccdTitle ?? '');
    setStatus((liveContent.ccdStatus as 'draft' | 'published') ?? 'draft');
    setEditingFromId(liveContent.ccdId);
    try {
      setPayload(JSON.stringify(liveContent.ccdPayload ?? {}, null, 2));
    } catch {
      setPayload('{}');
    }
  };

  const handleLoadVersion = (v: CmsContentDTO) => {
    setSlugFilter(v.ccdSlug);
    setLocaleFilter(v.ccdLocale);
    setTitle(v.ccdTitle ?? '');
    setStatus((v.ccdStatus as 'draft' | 'published') ?? 'draft');
    setEditingFromId(v.ccdId);
    try {
      setPayload(JSON.stringify(v.ccdPayload ?? {}, null, 2));
    } catch {
      setPayload('{}');
    }
  };

  const liveUrl = `${PUBLIC_BASE}${livePathForSlug(slugFilter)}${localeFilter ? `?locale=${encodeURIComponent(localeFilter)}` : ''}`;
  const livePayloadPretty = useMemo(() => {
    if (!liveContent) return '';
    try {
      return JSON.stringify(liveContent.ccdPayload ?? {}, null, 2);
    } catch {
      if (typeof liveContent.ccdPayload === 'string') return liveContent.ccdPayload;
      return JSON.stringify(liveContent.ccdPayload ?? {});
    }
  }, [liveContent]);

  return (
    <Stack spacing={3}>
      <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} justifyContent="space-between" alignItems="flex-start">
        <Box>
          <Typography variant="overline" color="text.secondary">CMS</Typography>
          <Typography variant="h4" fontWeight={800}>Contenido público</Typography>
          <Typography color="text.secondary">
            Crear, publicar y versionar bloques para páginas públicas (records, fan hub, landing cursos).
          </Typography>
        </Box>
        <Button variant="outlined" href={liveUrl} target="_blank" rel="noreferrer">
          Abrir página en vivo
        </Button>
      </Stack>

      <Paper variant="outlined" sx={{ p: 2.5 }}>
        <Stack spacing={2.5}>
          <Grid container spacing={2}>
            <Grid item xs={12} md={4}>
              <TextField
                select
                fullWidth
                label="Slug"
                value={slugFilter}
                onChange={(e) => setSlugFilter(e.target.value)}
                helperText="Identificador de la página"
              >
                {defaultSlugs.map((slug) => (
                  <MenuItem key={slug} value={slug}>{slug}</MenuItem>
                ))}
                <MenuItem value={slugFilter ?? ''}>Otro… escribe abajo</MenuItem>
              </TextField>
              <TextField
                fullWidth
                label="Slug custom"
                value={slugFilter}
                onChange={(e) => setSlugFilter(e.target.value)}
                sx={{ mt: 1 }}
              />
            </Grid>
            <Grid item xs={12} md={2}>
              <TextField
                select
                fullWidth
                label="Locale"
                value={localeFilter}
                onChange={(e) => setLocaleFilter(e.target.value)}
              >
                {locales.map((loc) => (
                  <MenuItem key={loc} value={loc}>{loc}</MenuItem>
                ))}
              </TextField>
            </Grid>
          </Grid>

          <Divider flexItem />

          <Grid container spacing={2}>
            <Grid item xs={12} md={5}>
              <Card variant="outlined">
                <CardContent>
                  <Stack spacing={1.5}>
                    <Stack direction="row" justifyContent="space-between" alignItems="center">
                      <Typography variant="subtitle1" fontWeight={700}>Contenido en vivo</Typography>
                      <Chip label={localeFilter} size="small" />
                    </Stack>
                    {liveQuery.isLoading && <LinearProgress />}
                    {liveQuery.isError && (
                      <Alert severity="warning">
                        <AlertTitle>Sin contenido publicado</AlertTitle>
                        Publica una versión para ver la vista previa en vivo.
                      </Alert>
                    )}
                    {liveContent && (
                      <Stack spacing={1}>
                        <Typography fontWeight={700}>{liveContent.ccdTitle ?? liveContent.ccdSlug}</Typography>
                        <Stack direction="row" spacing={1} flexWrap="wrap">
                          <Chip label={`v${liveContent.ccdVersion}`} size="small" />
                          <Chip label={liveContent.ccdStatus} size="small" color={liveContent.ccdStatus === 'published' ? 'success' : 'default'} />
                          {liveContent.ccdPublishedAt && (
                            <Chip
                              label={`Publicado: ${new Date(liveContent.ccdPublishedAt).toLocaleString()}`}
                              size="small"
                              variant="outlined"
                            />
                          )}
                        </Stack>
                        <TextField
                          label="Payload actual"
                          value={livePayloadPretty}
                          multiline
                          minRows={8}
                          InputProps={{ readOnly: true }}
                        />
                        <Stack direction="row" spacing={1}>
                          <Button size="small" variant="contained" onClick={handleLoadLive} disabled={!liveContent}>
                            Cargar en formulario
                          </Button>
                          <Button size="small" variant="outlined" href={liveUrl} target="_blank" rel="noreferrer">
                            Ver en vivo
                          </Button>
                        </Stack>
                      </Stack>
                    )}
                  </Stack>
                </CardContent>
              </Card>
            </Grid>

            <Grid item xs={12} md={7}>
              <Stack spacing={1}>
                <Typography variant="subtitle1" fontWeight={700}>Editar / crear versión</Typography>
                <TextField
                  label="Título"
                  fullWidth
                  value={title}
                  onChange={(e) => setTitle(e.target.value)}
                />
                <TextField
                  label="Payload JSON"
                  fullWidth
                  multiline
                  minRows={10}
                  value={payload}
                  onChange={(e) => setPayload(e.target.value)}
                />
                <TextField
                  select
                  label="Estado"
                  value={status}
                  onChange={(e) => setStatus(e.target.value as 'draft' | 'published')}
                  sx={{ width: 240 }}
                >
                  <MenuItem value="draft">Borrador</MenuItem>
                  <MenuItem value="published">Publicado</MenuItem>
                </TextField>
                <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap">
                  <Button variant="contained" onClick={handleCreate} disabled={createMutation.isPending}>
                    Guardar versión
                  </Button>
                  {editingFromId && <Chip label={`Editando desde ID ${editingFromId}`} size="small" color="info" />}
                  {createMutation.isError && (
                    <Alert severity="error" sx={{ flexGrow: 1 }}>
                      {createMutation.error instanceof Error ? createMutation.error.message : 'Error al crear.'}
                    </Alert>
                  )}
                  {createMutation.isSuccess && <Alert severity="success">Versión creada.</Alert>}
                </Stack>
              </Stack>
            </Grid>
          </Grid>
        </Stack>
      </Paper>

      <Paper variant="outlined" sx={{ p: 2.5 }}>
        <Stack spacing={2}>
          <Stack direction="row" spacing={1} alignItems="center">
            <Typography variant="h6" fontWeight={800}>Versiones</Typography>
            <Chip label={`${versions.length}`} size="small" />
            {editingFromId && (
              <Chip label={`Editando desde ID ${editingFromId}`} size="small" color="info" />
            )}
          </Stack>
          {listQuery.isLoading && <LinearProgress />}
          {listQuery.error && (
            <Alert severity="error">
              {listQuery.error instanceof Error ? listQuery.error.message : 'Error al cargar contenido.'}
            </Alert>
          )}
          {listDataInvalid && (
            <Alert severity="warning">
              Respuesta inesperada del servidor. Revisa las credenciales o intenta de nuevo.
            </Alert>
          )}
          <Stack spacing={1.5}>
            {versions.map((v) => (
              <Paper key={v.ccdId} variant="outlined" sx={{ p: 1.5, borderRadius: 2 }}>
                <Stack
                  direction={{ xs: 'column', sm: 'row' }}
                  spacing={1}
                  alignItems={{ xs: 'flex-start', sm: 'center' }}
                >
                  <Box sx={{ flexGrow: 1 }}>
                    <Typography fontWeight={700}>{v.ccdTitle ?? v.ccdSlug}</Typography>
                    <Stack direction="row" spacing={1} flexWrap="wrap" sx={{ mt: 0.5 }}>
                      <Chip label={v.ccdSlug} size="small" />
                      <Chip label={v.ccdLocale} size="small" />
                      <Chip label={`v${v.ccdVersion}`} size="small" />
                      <Chip
                        label={v.ccdStatus}
                        size="small"
                        color={v.ccdStatus === 'published' ? 'success' : 'default'}
                      />
                      {v.ccdPublishedAt && (
                        <Chip
                          label={`pub: ${new Date(v.ccdPublishedAt).toLocaleString()}`}
                          size="small"
                          variant="outlined"
                        />
                      )}
                    </Stack>
                  </Box>
                  <Stack direction="row" spacing={1}>
                    <Button
                      size="small"
                      variant="outlined"
                      onClick={() => publishMutation.mutate(v.ccdId)}
                      disabled={publishMutation.isPending}
                    >
                      Publicar
                    </Button>
                    <Button
                      size="small"
                      variant="text"
                      href={`${PUBLIC_BASE}${livePathForSlug(v.ccdSlug)}${v.ccdLocale ? `?locale=${encodeURIComponent(v.ccdLocale)}` : ''}`}
                      target="_blank"
                      rel="noreferrer"
                    >
                      Ver en vivo
                    </Button>
                    <Button size="small" variant="text" onClick={() => handleLoadVersion(v)}>
                      Editar en formulario
                    </Button>
                    <Button
                      size="small"
                      variant="text"
                      color="error"
                      onClick={() => deleteMutation.mutate(v.ccdId)}
                      disabled={deleteMutation.isPending}
                    >
                      Borrar
                    </Button>
                  </Stack>
                </Stack>
              </Paper>
            ))}
            {versions.length === 0 && !listQuery.isLoading && (
              <Typography color="text.secondary">No hay contenido aún.</Typography>
            )}
          </Stack>
        </Stack>
      </Paper>
    </Stack>
  );
}
