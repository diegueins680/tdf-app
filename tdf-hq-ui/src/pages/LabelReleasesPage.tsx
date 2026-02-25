import { useMemo, useState } from 'react';
import {
  Alert,
  Autocomplete,
  Avatar,
  Box,
  Button,
  Card,
  CardContent,
  CardMedia,
  Chip,
  Grid,
  IconButton,
  MenuItem,
  Stack,
  TextField,
  Tooltip,
  Typography,
  Tabs,
  Tab,
} from '@mui/material';
import AddIcon from '@mui/icons-material/Add';
import CloudUploadIcon from '@mui/icons-material/CloudUpload';
import RefreshIcon from '@mui/icons-material/Refresh';
import YouTubeIcon from '@mui/icons-material/YouTube';
import PlayArrowIcon from '@mui/icons-material/PlayArrow';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Admin } from '../api/admin';
import { Fans } from '../api/fans';
import type { ArtistProfileDTO, ArtistReleaseDTO, ArtistReleaseUpsert } from '../api/types';
import StreamingPlayer from '../components/StreamingPlayer';
import { SessionGate } from '../components/SessionGate';
import { buildReleaseStreamingSources } from '../utils/media';

type ReleaseRow = ArtistReleaseDTO & {
  artistName: string;
  artistCity?: string | null;
  artistHeroImageUrl?: string | null;
};
type SortOrder = 'newest' | 'oldest' | 'artist';
const SORT_ORDER_OPTIONS: readonly SortOrder[] = ['newest', 'oldest', 'artist'];
const isSortOrder = (value: string): value is SortOrder =>
  SORT_ORDER_OPTIONS.some((order) => order === value);

const emptyForm = {
  artistId: null as number | null,
  title: '',
  releaseDate: '',
  description: '',
  coverImageUrl: '',
  spotifyUrl: '',
  youtubeUrl: '',
};

const toNullable = (value: string | null | undefined) => {
  const trimmed = (value ?? '').trim();
  return trimmed.length > 0 ? trimmed : null;
};

const isValidSpotify = (url?: string | null) => {
  if (!url) return false;
  return /spotify\.com\/(track|album|artist|playlist)\//i.test(url);
};
const isValidYoutube = (url?: string | null) => {
  if (!url) return false;
  return /youtube\.com|youtu\.be/i.test(url);
};

const parseDate = (value?: string | null) => {
  if (!value) return 0;
  const iso = value.length === 10 ? `${value}T00:00:00Z` : value;
  const ts = Date.parse(iso);
  return Number.isNaN(ts) ? 0 : ts;
};

const formatDate = (value?: string | null) => {
  if (!value) return 'Sin fecha';
  const ts = parseDate(value);
  if (!ts) return 'Sin fecha';
  return new Intl.DateTimeFormat('es-CO', { day: 'numeric', month: 'short', year: 'numeric' }).format(
    new Date(ts),
  );
};

export default function LabelReleasesPage() {
  const qc = useQueryClient();
  const [form, setForm] = useState(emptyForm);
  const [coverFileName, setCoverFileName] = useState('');
  const [search, setSearch] = useState('');
  const [banner, setBanner] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [usedFanFallback, setUsedFanFallback] = useState(false);
  const [filterArtistId, setFilterArtistId] = useState<number | null>(null);
  const [filterWindow, setFilterWindow] = useState<'all' | 'upcoming' | 'past'>('all');
  const [sortOrder, setSortOrder] = useState<SortOrder>('newest');

  const artistsQuery = useQuery({
    queryKey: ['admin', 'artists'],
    queryFn: async () => {
      try {
        const adminArtists = await Admin.listArtistProfiles();
        if (adminArtists.length > 0) {
          setUsedFanFallback(false);
          return adminArtists;
        }
        console.warn('Lista de artistas de admin vacía, usando fallback público.');
      } catch (err) {
        console.warn('Admin artists fetch failed, falling back to fan list', err);
      }
      setUsedFanFallback(true);
      try {
        return await Fans.listArtists();
      } catch (fanErr) {
        console.warn('No se pudo cargar artistas de fans', fanErr);
        throw fanErr;
      }
    },
  });

  const releasesQuery = useQuery({
    queryKey: ['admin', 'artist-releases'],
    enabled: artistsQuery.isSuccess,
    queryFn: async () => {
      const artists = artistsQuery.data ?? [];
      const releasesPerArtist = await Promise.all(
        artists.map(async (artist) => {
          const releases = await Fans.getReleases(artist.apArtistId);
          return releases.map((release) => ({
            ...release,
            artistName: artist.apDisplayName,
            artistCity: artist.apCity,
            artistHeroImageUrl: artist.apHeroImageUrl,
          }));
        }),
      );
      const flat = releasesPerArtist.flat() as ReleaseRow[];
      return flat.sort((a, b) => parseDate(b.arReleaseDate) - parseDate(a.arReleaseDate));
    },
  });

  const createMutation = useMutation({
    mutationFn: (payload: ArtistReleaseUpsert) => Admin.createArtistRelease(payload),
    onSuccess: async () => {
      setBanner('Release guardado.');
      setError(null);
      setForm(emptyForm);
      setCoverFileName('');
      await qc.invalidateQueries({ queryKey: ['admin', 'artist-releases'] });
    },
    onError: (err: unknown) => {
      setBanner(null);
      setError(err instanceof Error ? err.message : 'No se pudo guardar el release.');
    },
  });

  const artists = useMemo(() => artistsQuery.data ?? [], [artistsQuery.data]);
  const releases = useMemo(() => releasesQuery.data ?? [], [releasesQuery.data]);
  const artistsError = useMemo(() => {
    if (!artistsQuery.isError) return null;
    if (artistsQuery.error instanceof Error) return artistsQuery.error.message;
    return 'No pudimos cargar los artistas.';
  }, [artistsQuery.error, artistsQuery.isError]);

  const filteredReleases = useMemo(() => {
    const term = search.trim().toLowerCase();
    const now = Date.now();
    const filtered = releases.filter((release) => {
      if (filterArtistId && release.arArtistId !== filterArtistId) return false;
      if (filterWindow !== 'all') {
        const ts = parseDate(release.arReleaseDate);
        if (filterWindow === 'upcoming' && ts < now) return false;
        if (filterWindow === 'past' && ts >= now) return false;
      }
      if (!term) return true;
      const haystack = [
        release.artistName,
        release.arTitle,
        release.arDescription ?? '',
        release.arSpotifyUrl ?? '',
        release.arYoutubeUrl ?? '',
      ]
        .join(' ')
        .toLowerCase();
      return haystack.includes(term);
    });
    return [...filtered].sort((a, b) => {
      if (sortOrder === 'artist') return a.artistName.localeCompare(b.artistName);
      if (sortOrder === 'oldest') return parseDate(a.arReleaseDate) - parseDate(b.arReleaseDate);
      return parseDate(b.arReleaseDate) - parseDate(a.arReleaseDate);
    });
  }, [filterArtistId, filterWindow, releases, search, sortOrder]);

  const handleCoverFileChange = (file: File | null) => {
    if (!file) {
      setForm((prev) => ({ ...prev, coverImageUrl: '' }));
      setCoverFileName('');
      return;
    }
    const maxBytes = 6 * 1024 * 1024;
    if (file.size > maxBytes) {
      setError('El archivo supera 6 MB. Usa una imagen más liviana.');
      return;
    }
    const reader = new FileReader();
    reader.onload = () => {
      if (typeof reader.result === 'string') {
        setForm((prev) => ({ ...prev, coverImageUrl: reader.result as string }));
        setCoverFileName(file.name);
        setError(null);
      } else {
        setError('No pudimos leer la imagen seleccionada.');
        setForm((prev) => ({ ...prev, coverImageUrl: '' }));
      }
    };
    reader.onerror = () => setError('No pudimos leer la imagen seleccionada.');
    reader.readAsDataURL(file);
  };

  const handleCreate = () => {
    setBanner(null);
    setError(null);
    if (!form.artistId) {
      setError('Selecciona un artista.');
      return;
    }
    const title = form.title.trim();
    if (!title) {
      setError('Agrega un título para el release.');
      return;
    }
    const payload: ArtistReleaseUpsert = {
      aruArtistId: form.artistId,
      aruTitle: title,
      aruReleaseDate: toNullable(form.releaseDate),
      aruDescription: toNullable(form.description),
      aruCoverImageUrl: toNullable(form.coverImageUrl),
      aruSpotifyUrl: toNullable(form.spotifyUrl),
      aruYoutubeUrl: toNullable(form.youtubeUrl),
    };
    createMutation.mutate(payload);
  };

  const totalReleases = releases.length;
  const totalArtistsWithReleases = new Set(releases.map((r) => r.artistName)).size;
  const upcomingOrRecent = releases.filter((r) => parseDate(r.arReleaseDate) >= Date.now() - 30 * 24 * 60 * 60 * 1000).length;

  const alertMessage = error ?? banner;

  return (
    <SessionGate message="Inicia sesión como admin para gestionar releases.">
    <Stack spacing={3}>
      <Stack spacing={0.5}>
        <Typography variant="h4" fontWeight={700}>
          Label / Releases
        </Typography>
        <Typography variant="body1" color="text.secondary">
          Gestiona lanzamientos del catálogo, vincúlalos a artistas y agrega links de streaming para el hub de fans.
        </Typography>
      </Stack>

      {usedFanFallback && (
        <Alert severity="warning">
          No pudimos cargar artistas con credenciales de admin; mostrando la lista pública como referencia.
          Algunos campos internos pueden no estar disponibles.
        </Alert>
      )}

      {alertMessage && (
        <Alert severity={error ? 'error' : 'success'} onClose={() => (error ? setError(null) : setBanner(null))}>
          {alertMessage}
        </Alert>
      )}

      <Stack spacing={1.5}>
        <Tabs
          value={filterWindow}
          onChange={(_, val: typeof filterWindow) => setFilterWindow(val)}
          variant="scrollable"
          scrollButtons="auto"
        >
          <Tab value="all" label="Todos" />
          <Tab value="upcoming" label="Próximos" />
          <Tab value="past" label="Publicados" />
        </Tabs>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ xs: 'flex-start', sm: 'center' }} flexWrap="wrap">
          <Autocomplete
            options={artists}
            value={artists.find((a) => a.apArtistId === filterArtistId) ?? null}
            onChange={(_, value) => setFilterArtistId(value?.apArtistId ?? null)}
            getOptionLabel={(option) => option.apDisplayName}
            renderInput={(params) => <TextField {...params} label="Filtrar por artista" size="small" sx={{ minWidth: 240 }} />}
            sx={{ minWidth: 240 }}
          />
          <TextField
            size="small"
            label="Buscar"
            value={search}
            onChange={(e) => setSearch(e.target.value)}
            placeholder="Busca por título/nota/links"
            sx={{ minWidth: 240 }}
          />
          <TextField
            select
            size="small"
            label="Ordenar"
            value={sortOrder}
            onChange={(e) => {
              const next = e.target.value.trim();
              setSortOrder(isSortOrder(next) ? next : 'newest');
            }}
            sx={{ minWidth: 180 }}
          >
            <MenuItem value="newest">Más recientes</MenuItem>
            <MenuItem value="oldest">Más antiguos</MenuItem>
            <MenuItem value="artist">Por artista</MenuItem>
          </TextField>
          <Button size="small" onClick={() => { setFilterArtistId(null); setFilterWindow('all'); setSearch(''); }}>
            Limpiar filtros
          </Button>
        </Stack>
        <Stack direction="row" spacing={1} flexWrap="wrap">
          {filterArtistId && (
            <Chip
              label={`Artista: ${artists.find((a) => a.apArtistId === filterArtistId)?.apDisplayName ?? ''}`}
              onDelete={() => setFilterArtistId(null)}
            />
          )}
          {filterWindow !== 'all' && (
            <Chip
              label={filterWindow === 'upcoming' ? 'Próximos' : 'Publicados'}
              onDelete={() => setFilterWindow('all')}
            />
          )}
          {search && <Chip label={`Buscar: ${search}`} onDelete={() => setSearch('')} />}
          {sortOrder !== 'newest' && (
            <Chip
              label={
                sortOrder === 'artist'
                  ? 'Orden: artista'
                  : sortOrder === 'oldest'
                    ? 'Orden: antiguos'
                    : 'Orden: recientes'
              }
              onDelete={() => setSortOrder('newest')}
            />
          )}
        </Stack>
      </Stack>
      <Grid container spacing={2}>
        <Grid item xs={12} md={7}>
          <Card>
            <CardContent>
              <Stack spacing={2}>
                <Stack direction="row" justifyContent="space-between" alignItems="center">
                  <Typography variant="h6">Nuevo release</Typography>
                  <Button
                    size="small"
                    startIcon={<RefreshIcon />}
                    onClick={() => {
                      void qc.invalidateQueries({ queryKey: ['admin', 'artists'] });
                      void qc.invalidateQueries({ queryKey: ['admin', 'artist-releases'] });
                    }}
                  >
                    Recargar
                  </Button>
                </Stack>

                {artistsError && (
                  <Alert
                    severity="error"
                    action={
                      <Button
                        size="small"
                        color="inherit"
                        onClick={() => {
                          void artistsQuery.refetch();
                        }}
                      >
                        Reintentar
                      </Button>
                    }
                  >
                    No pudimos cargar los artistas. {artistsError}
                  </Alert>
                )}

                <Autocomplete
                  options={artists}
                  getOptionLabel={(option: ArtistProfileDTO) => option.apDisplayName}
                  loading={artistsQuery.isFetching}
                  noOptionsText={artistsQuery.isFetching ? 'Cargando...' : 'Sin artistas disponibles'}
                  filterOptions={(options, state) => {
                    const term = state.inputValue.trim().toLowerCase();
                    if (!term) return options;
                    return options.filter((opt) => {
                      const haystack = [opt.apDisplayName, opt.apCity ?? '', opt.apSlug ?? '']
                        .join(' ')
                        .toLowerCase();
                      return haystack.includes(term);
                    });
                  }}
                  isOptionEqualToValue={(option, value) => option.apArtistId === value.apArtistId}
                  value={artists.find((a) => a.apArtistId === form.artistId) ?? null}
                  onChange={(_, value) => setForm((prev) => ({ ...prev, artistId: value?.apArtistId ?? null }))}
                  renderInput={(params) => <TextField {...params} label="Artista" placeholder="Busca por nombre" />}
                />
                {!artistsQuery.isFetching && artists.length === 0 && (
                  <Alert severity="info">
                    No encontramos artistas aún. Usa &quot;Recargar&quot; o verifica que existan perfiles en el módulo de artistas.
                  </Alert>
                )}

                <TextField
                  label="Título del release"
                  value={form.title}
                  onChange={(event) => setForm((prev) => ({ ...prev, title: event.target.value }))}
                  required
                  fullWidth
                />

                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
                  <TextField
                    label="Fecha de lanzamiento"
                    type="date"
                    InputLabelProps={{ shrink: true }}
                    value={form.releaseDate}
                    onChange={(event) => setForm((prev) => ({ ...prev, releaseDate: event.target.value }))}
                    fullWidth
                  />
                  <TextField
                    label="Portada (URL o subir)"
                    value={form.coverImageUrl}
                    onChange={(event) => setForm((prev) => ({ ...prev, coverImageUrl: event.target.value }))}
                    fullWidth
                    InputProps={{
                      endAdornment: (
                        <Tooltip title="Subir archivo">
                          <IconButton component="label" size="small">
                            <CloudUploadIcon fontSize="small" />
                            <input
                              type="file"
                              accept="image/*"
                              hidden
                              onChange={(event) => handleCoverFileChange(event.target.files?.[0] ?? null)}
                            />
                          </IconButton>
                        </Tooltip>
                      ),
                    }}
                    helperText={
                      coverFileName
                        ? `${coverFileName} · ideal 1:1 y menor a 6MB`
                        : 'Pega una URL o sube una portada JPG/PNG (ideal 1:1, <6MB)'
                    }
                  />
                </Stack>

                <TextField
                  label="Descripción / notas"
                  multiline
                  minRows={3}
                  value={form.description}
                  onChange={(event) => setForm((prev) => ({ ...prev, description: event.target.value }))}
                  fullWidth
                />

                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
                  <TextField
                    label="Link de Spotify"
                    value={form.spotifyUrl}
                    onChange={(event) => setForm((prev) => ({ ...prev, spotifyUrl: event.target.value }))}
                    fullWidth
                    placeholder="https://open.spotify.com/album/..."
                    error={Boolean(form.spotifyUrl && !isValidSpotify(form.spotifyUrl))}
                    helperText={
                      form.spotifyUrl && !isValidSpotify(form.spotifyUrl)
                        ? 'Usa un enlace válido de Spotify (track/album/artist/playlist).'
                        : undefined
                    }
                  />
                  <TextField
                    label="Link de YouTube"
                    value={form.youtubeUrl}
                    onChange={(event) => setForm((prev) => ({ ...prev, youtubeUrl: event.target.value }))}
                    fullWidth
                    placeholder="https://youtu.be/..."
                    error={Boolean(form.youtubeUrl && !isValidYoutube(form.youtubeUrl))}
                    helperText={
                      form.youtubeUrl && !isValidYoutube(form.youtubeUrl)
                        ? 'Pega un enlace válido de YouTube.'
                        : undefined
                    }
                  />
                </Stack>
                <Stack direction="row" spacing={1} flexWrap="wrap">
                  {form.spotifyUrl && isValidSpotify(form.spotifyUrl) && (
                    <Chip label="Spotify listo" color="success" size="small" icon={<PlayArrowIcon fontSize="small" />} />
                  )}
                  {form.youtubeUrl && isValidYoutube(form.youtubeUrl) && (
                    <Chip label="YouTube listo" color="success" size="small" icon={<YouTubeIcon fontSize="small" />} />
                  )}
                </Stack>

                <Box display="flex" justifyContent="flex-end">
                  <Button
                    variant="contained"
                    startIcon={<AddIcon />}
                    onClick={handleCreate}
                    disabled={createMutation.isPending}
                  >
                    {createMutation.isPending ? 'Guardando...' : 'Agregar release'}
                  </Button>
                </Box>
              </Stack>
            </CardContent>
          </Card>
        </Grid>

        <Grid item xs={12} md={5}>
          <Card>
            <CardContent>
              <Stack spacing={2}>
                <Typography variant="h6">Resumen rápido</Typography>
                <Stack direction="row" spacing={1} flexWrap="wrap">
                  <Chip label={`Releases: ${totalReleases}`} color="primary" />
                  <Chip label={`Artistas: ${totalArtistsWithReleases}`} />
                  <Chip label={`Últimos 30 días: ${upcomingOrRecent}`} />
                </Stack>
                <Typography variant="body2" color="text.secondary">
                  Los releases se muestran automáticamente en el hub de fans y en la vista pública de Records.
                  Completa al menos un link de Spotify o YouTube para habilitar el reproductor embebido.
                </Typography>
              </Stack>
            </CardContent>
          </Card>
        </Grid>
      </Grid>

      <Card>
        <CardContent>
          <Stack spacing={2}>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} alignItems={{ sm: 'center' }}>
              <Typography variant="h6" sx={{ flexGrow: 1 }}>
                Lanzamientos del catálogo
              </Typography>
              <TextField
                size="small"
                label="Buscar"
                placeholder="Filtra por artista, título o plataforma"
                value={search}
                onChange={(event) => setSearch(event.target.value)}
              />
            </Stack>

            {releasesQuery.isLoading && <Typography color="text.secondary">Cargando releases...</Typography>}
            {releasesQuery.isError && (
              <Alert severity="error">No pudimos cargar los releases. Intenta nuevamente.</Alert>
            )}
            {!releasesQuery.isLoading && filteredReleases.length === 0 && (
              <Typography color="text.secondary">No hay releases registrados aún.</Typography>
            )}

            <Grid container spacing={2}>
              {filteredReleases.map((release) => {
                const sources = buildReleaseStreamingSources({
                  arReleaseId: release.arReleaseId,
                  arArtistId: release.arArtistId,
                  arTitle: release.arTitle,
                  arReleaseDate: release.arReleaseDate,
                  arDescription: release.arDescription,
                  arCoverImageUrl: release.arCoverImageUrl,
                  arSpotifyUrl: release.arSpotifyUrl,
                  arYoutubeUrl: release.arYoutubeUrl,
                });
                return (
                  <Grid item xs={12} md={6} key={`${release.arArtistId}-${release.arReleaseId}`}>
                    <Card sx={{ height: '100%', display: 'flex', flexDirection: 'column' }}>
                      <CardContent sx={{ flex: 1, display: 'flex', flexDirection: 'column', gap: 1.5 }}>
                        <Stack direction="row" spacing={2}>
                          {(release.arCoverImageUrl ?? release.artistHeroImageUrl) && (
                            <CardMedia
                              component="img"
                              image={release.arCoverImageUrl ?? release.artistHeroImageUrl ?? undefined}
                              alt={release.arTitle}
                              sx={{
                                width: 120,
                                height: 120,
                                borderRadius: 2,
                                objectFit: 'cover',
                                border: '1px solid',
                                borderColor: 'divider',
                              }}
                            />
                          )}
                          {!release.arCoverImageUrl && !release.artistHeroImageUrl && (
                            <Avatar
                              variant="rounded"
                              sx={{ width: 120, height: 120, fontWeight: 700, bgcolor: 'grey.200', color: 'text.primary' }}
                            >
                              {release.artistName.slice(0, 2).toUpperCase()}
                            </Avatar>
                          )}
                          <Stack spacing={0.5} flex={1} minWidth={0}>
                            <Stack direction="row" alignItems="center" spacing={1} justifyContent="space-between">
                              <Typography variant="h6" noWrap>
                                {release.arTitle}
                              </Typography>
                              <Chip label={formatDate(release.arReleaseDate)} size="small" />
                            </Stack>
                            <Typography variant="body2" color="text.secondary">
                              {release.artistName}
                              {release.artistCity ? ` · ${release.artistCity}` : ''}
                            </Typography>
                            {release.arDescription && (
                              <Typography variant="body2" color="text.secondary" noWrap>
                                {release.arDescription}
                              </Typography>
                            )}
                            <Stack direction="row" spacing={1} flexWrap="wrap">
                              {(() => {
                                const spotifyProps = release.arSpotifyUrl
                                  ? { component: 'a' as const, href: release.arSpotifyUrl, target: '_blank', rel: 'noopener noreferrer' }
                                  : {};
                                const youtubeProps = release.arYoutubeUrl
                                  ? { component: 'a' as const, href: release.arYoutubeUrl, target: '_blank', rel: 'noopener noreferrer' }
                                  : {};
                                return (
                                  <>
                                    <Button
                                      {...spotifyProps}
                                      variant="contained"
                                      size="small"
                                      startIcon={<PlayArrowIcon />}
                                      disabled={!release.arSpotifyUrl}
                                    >
                                      Spotify
                                    </Button>
                                    <Button
                                      {...youtubeProps}
                                      variant="outlined"
                                      size="small"
                                      startIcon={<YouTubeIcon />}
                                      disabled={!release.arYoutubeUrl}
                                    >
                                      YouTube
                                    </Button>
                                    <Button
                                      size="small"
                                      variant="text"
                                      onClick={() => {
                                        setForm({
                                          artistId: release.arArtistId,
                                          title: `${release.arTitle} (copia)`,
                                          releaseDate: release.arReleaseDate ?? '',
                                          description: release.arDescription ?? '',
                                          coverImageUrl: release.arCoverImageUrl ?? '',
                                          spotifyUrl: release.arSpotifyUrl ?? '',
                                          youtubeUrl: release.arYoutubeUrl ?? '',
                                        });
                                        setCoverFileName('');
                                        setBanner('Formulario rellenado a partir de este release.');
                                        window.scrollTo({ top: 0, behavior: 'smooth' });
                                      }}
                                    >
                                      Duplicar
                                    </Button>
                                  </>
                                );
                              })()}
                            </Stack>
                          </Stack>
                        </Stack>

                        {sources.length > 0 && (
                          <StreamingPlayer
                            title={release.arTitle}
                            artist={release.artistName}
                            posterUrl={release.arCoverImageUrl ?? release.artistHeroImageUrl}
                            sources={sources}
                            variant="compact"
                          />
                        )}
                      </CardContent>
                    </Card>
                  </Grid>
                );
              })}
            </Grid>
          </Stack>
        </CardContent>
      </Card>
    </Stack>
    </SessionGate>
  );
}
