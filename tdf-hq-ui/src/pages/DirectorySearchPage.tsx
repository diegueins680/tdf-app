import {
  Alert,
  Autocomplete,
  Box,
  Button,
  Card,
  CardActions,
  CardContent,
  CardMedia,
  Chip,
  CircularProgress,
  Container,
  FormControl,
  FormControlLabel,
  InputLabel,
  MenuItem,
  Paper,
  Select,
  Stack,
  Switch,
  Tab,
  Tabs,
  TextField,
  ToggleButton,
  ToggleButtonGroup,
  Typography,
} from '@mui/material';
import SearchIcon from '@mui/icons-material/Search';
import MyLocationIcon from '@mui/icons-material/MyLocation';
import MapIcon from '@mui/icons-material/Map';
import ViewListIcon from '@mui/icons-material/ViewList';
import GridViewIcon from '@mui/icons-material/GridView';
import ShareIcon from '@mui/icons-material/Share';
import BookmarkBorderIcon from '@mui/icons-material/BookmarkBorder';
import LoginIcon from '@mui/icons-material/Login';
import { useInfiniteQuery, useMutation, useQuery } from '@tanstack/react-query';
import { useEffect, useMemo, useState, type FormEvent } from 'react';
import { Link as RouterLink, useLocation, useNavigate } from 'react-router-dom';

import {
  Directory,
  type DirectoryEntityType,
  type DirectorySearchItem,
  type DirectorySearchQuery,
} from '../api/directory';
import OpenStreetMapResults from '../components/directory/OpenStreetMapResults';
import { getAnalyticsClient } from '../analytics/posthog';
import { useMetaTags } from '../hooks/useMetaTags';
import { useSession } from '../session/SessionContext';
import { buildLoginRedirectPath } from '../utils/loginRouting';
import { API_BASE_URL } from '../api/client';

const resolveImageUrl = (value: string | null | undefined): string | undefined => {
  if (!value) return undefined;
  try { return new URL(value, API_BASE_URL || window.location.origin).toString(); } catch { return undefined; }
};

const DIRECTORY_IMAGE_FALLBACKS: Record<DirectoryEntityType, string> = {
  profile: '/artist-fallback.svg',
  classified: '/directory-fallback.svg',
  event: '/event-fallback.svg',
  venue: '/directory-fallback.svg',
};

const CITY_STORAGE_KEY = 'tdf.directory.cityId';
const ENTITY_LABELS: Record<DirectoryEntityType | 'all', string> = {
  all: 'Todo',
  profile: 'Perfiles',
  classified: 'Clasificados',
  event: 'Eventos',
  venue: 'Venues',
};

const resultPath = (item: DirectorySearchItem) => {
  if (item.type === 'profile') return `/directorio/${item.slug}`;
  if (item.type === 'classified') return `/clasificados/${item.slug}`;
  if (item.type === 'event') return `/eventos/${item.id}`;
  return `/venues/${item.id}`;
};

export default function DirectorySearchPage() {
  const location = useLocation();
  const navigate = useNavigate();
  const { session } = useSession();
  const initial = useMemo(() => new URLSearchParams(location.search), []); // eslint-disable-line react-hooks/exhaustive-deps
  const [draftQuery, setDraftQuery] = useState(initial.get('q') ?? '');
  const [query, setQuery] = useState(initial.get('q') ?? '');
  const [entityType, setEntityType] = useState<DirectoryEntityType | 'all'>(
    (initial.get('entityType') as DirectoryEntityType | null) ?? 'all',
  );
  const [cityId, setCityId] = useState(
    initial.get('cityId') ?? (typeof localStorage === 'undefined' ? '' : localStorage.getItem(CITY_STORAGE_KEY) ?? ''),
  );
  const [professionId, setProfessionId] = useState(initial.get('professionId') ?? '');
  const [serviceId, setServiceId] = useState(initial.get('serviceId') ?? '');
  const [instrumentId, setInstrumentId] = useState(initial.get('instrumentId') ?? '');
  const [genreId, setGenreId] = useState(initial.get('genreId') ?? '');
  const [remote, setRemote] = useState(initial.get('remote') === 'true');
  const [available, setAvailable] = useState(initial.get('available') === 'true');
  const [radiusKm, setRadiusKm] = useState(Number(initial.get('radiusKm') ?? 25));
  const [coordinates, setCoordinates] = useState<{ latitude: number; longitude: number } | null>(null);
  const [geoMessage, setGeoMessage] = useState<string | null>(null);
  const [view, setView] = useState<'list' | 'grid' | 'map'>('list');

  useMetaTags({
    title: query ? `${query} en el directorio musical` : 'Directorio y clasificados musicales',
    description: 'Encuentra músicos, profesionales, bandas, servicios, eventos, venues y oportunidades por ciudad en Ecuador y Latinoamérica.',
    canonical: `${window.location.origin}/buscar`,
    structuredData: {
      '@context': 'https://schema.org',
      '@type': 'SearchResultsPage',
      name: 'Directorio y Clasificados Musicales TDF',
      url: `${window.location.origin}/buscar`,
      inLanguage: 'es',
    },
  });

  const taxonomies = useQuery({
    queryKey: ['directory', 'taxonomies', 'es'],
    queryFn: () => Directory.taxonomies('es'),
    staleTime: 30 * 60 * 1000,
  });

  useEffect(() => {
    if (!cityId && taxonomies.data?.cities.length) {
      const quito = taxonomies.data.cities.find((city) => city.code === 'quito-ec-p');
      if (quito) setCityId(quito.id);
    }
  }, [cityId, taxonomies.data]);

  useEffect(() => {
    if (cityId) localStorage.setItem(CITY_STORAGE_KEY, cityId);
  }, [cityId]);

  const searchBase: DirectorySearchQuery = {
    q: query || undefined,
    entityType: entityType === 'all' ? undefined : entityType,
    cityId: coordinates ? undefined : cityId || undefined,
    professionId: professionId || undefined,
    serviceId: serviceId || undefined,
    instrumentId: instrumentId || undefined,
    genreId: genreId || undefined,
    remote: remote || undefined,
    available: available || undefined,
    latitude: coordinates?.latitude,
    longitude: coordinates?.longitude,
    radiusKm: coordinates ? radiusKm : undefined,
    limit: 20,
  };

  const results = useInfiniteQuery({
    queryKey: ['directory', 'search', searchBase],
    initialPageParam: undefined as string | undefined,
    queryFn: ({ pageParam }) => Directory.search({ ...searchBase, cursor: pageParam }),
    getNextPageParam: (page) => page.nextCursor ?? undefined,
    staleTime: 30_000,
  });
  const pages = results.data?.pages ?? [];
  const items = pages.flatMap((page) => page.items);
  const sponsored = pages[0]?.sponsoredItems ?? [];
  const facets = pages[0]?.facets;

  useEffect(() => {
    const params = new URLSearchParams();
    if (query) params.set('q', query);
    if (entityType !== 'all') params.set('entityType', entityType);
    if (cityId && !coordinates) params.set('cityId', cityId);
    if (professionId) params.set('professionId', professionId);
    if (serviceId) params.set('serviceId', serviceId);
    if (instrumentId) params.set('instrumentId', instrumentId);
    if (genreId) params.set('genreId', genreId);
    if (remote) params.set('remote', 'true');
    if (available) params.set('available', 'true');
    if (coordinates) params.set('radiusKm', String(radiusKm));
    navigate({ pathname: '/buscar', search: params.toString() }, { replace: true });
  }, [available, cityId, coordinates, entityType, genreId, instrumentId, navigate, professionId, query, radiusKm, remote, serviceId]);

  const submitSearch = (event: FormEvent) => {
    event.preventDefault();
    setQuery(draftQuery.trim());
  };

  const locate = () => {
    setGeoMessage(null);
    if (!navigator.geolocation) {
      setGeoMessage('Este dispositivo no ofrece geolocalización. Puedes elegir una ciudad manualmente.');
      return;
    }
    navigator.geolocation.getCurrentPosition(
      ({ coords }) => {
        setCoordinates({ latitude: coords.latitude, longitude: coords.longitude });
        setGeoMessage('Ubicación usada solo para esta búsqueda; TDF no la guarda en tu perfil.');
      },
      () => setGeoMessage('No se obtuvo tu ubicación. Puedes seguir buscando por ciudad.'),
      { enableHighAccuracy: false, timeout: 8_000, maximumAge: 10 * 60 * 1000 },
    );
  };

  const suggestions = useQuery({
    queryKey: ['directory', 'suggestions', draftQuery, cityId],
    queryFn: () => Directory.suggestions(draftQuery, cityId || undefined),
    enabled: draftQuery.trim().length >= 2,
    staleTime: 60_000,
  });

  return (
    <Box sx={{ pb: 8 }}>
      <Box sx={{ background: 'linear-gradient(135deg, #17112d 0%, #3b1d66 52%, #0e6470 100%)', color: 'white', py: { xs: 6, md: 10 } }}>
        <Container maxWidth="lg">
          <Stack spacing={3} maxWidth={900}>
            <Chip label="Ecuador · Quito · Latinoamérica" sx={{ alignSelf: 'flex-start', bgcolor: 'rgba(255,255,255,.14)', color: 'white' }} />
            <Typography component="h1" variant="h2" fontWeight={900} sx={{ fontSize: { xs: '2.2rem', md: '4rem' } }}>
              Encuentra a la gente y las oportunidades que hacen música
            </Typography>
            <Typography variant="h6" sx={{ maxWidth: 760, color: 'rgba(255,255,255,.82)' }}>
              Profesionales, artistas, bandas, venues, eventos, servicios y clasificados, primero por ciudad y cercanía.
            </Typography>
            <Paper component="form" onSubmit={submitSearch} elevation={8} sx={{ p: 1.5, borderRadius: 3 }}>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
                <Autocomplete
                  freeSolo
                  fullWidth
                  options={(suggestions.data ?? []).map((option) => option.label)}
                  inputValue={draftQuery}
                  onInputChange={(_, value) => setDraftQuery(value)}
                  renderInput={(params) => <TextField {...params} label="¿Qué necesitas?" placeholder="Bajista, productor, estudio, concierto…" inputProps={{ ...params.inputProps, maxLength: 160 }} />}
                />
                <FormControl sx={{ minWidth: { sm: 220 } }}>
                  <InputLabel id="directory-city-label">Ciudad</InputLabel>
                  <Select labelId="directory-city-label" label="Ciudad" value={coordinates ? '__nearby' : cityId} onChange={(event) => { setCoordinates(null); setCityId(event.target.value); }}>
                    {coordinates && <MenuItem value="__nearby">Cerca de mí</MenuItem>}
                    {(taxonomies.data?.cities ?? []).map((city) => <MenuItem key={city.id} value={city.id}>{city.name}</MenuItem>)}
                  </Select>
                </FormControl>
                <Button type="submit" variant="contained" size="large" startIcon={<SearchIcon />} sx={{ minWidth: 140 }}>Buscar</Button>
              </Stack>
            </Paper>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ sm: 'center' }}>
              <Button color="inherit" variant="outlined" startIcon={<MyLocationIcon />} onClick={locate} sx={{ alignSelf: 'flex-start', borderColor: 'rgba(255,255,255,.5)' }}>
                Usar mi ubicación
              </Button>
              <Typography variant="caption" sx={{ color: 'rgba(255,255,255,.75)' }}>
                Solo con tu permiso. Las ubicaciones privadas nunca se muestran.
              </Typography>
            </Stack>
            {geoMessage && <Alert severity={coordinates ? 'success' : 'info'}>{geoMessage}</Alert>}
          </Stack>
        </Container>
      </Box>

      <Container maxWidth="xl" sx={{ mt: 4 }}>
        <Stack spacing={3}>
          <Paper variant="outlined" sx={{ p: 2.5, borderRadius: 3 }}>
            <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} alignItems={{ md: 'center' }}>
              <FormControl size="small" sx={{ minWidth: 190 }}>
                <InputLabel id="directory-profession-label">Profesión</InputLabel>
                <Select labelId="directory-profession-label" label="Profesión" value={professionId} onChange={(event) => setProfessionId(event.target.value)}>
                  <MenuItem value="">Todas</MenuItem>
                  {(taxonomies.data?.professions ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}
                </Select>
              </FormControl>
              <FormControl size="small" sx={{ minWidth: 190 }}>
                <InputLabel id="directory-service-label">Servicio</InputLabel>
                <Select labelId="directory-service-label" label="Servicio" value={serviceId} onChange={(event) => setServiceId(event.target.value)}>
                  <MenuItem value="">Todos</MenuItem>
                  {(taxonomies.data?.serviceOfferings ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}
                </Select>
              </FormControl>
              <FormControl size="small" sx={{ minWidth: 190 }}>
                <InputLabel id="directory-instrument-label">Instrumento</InputLabel>
                <Select labelId="directory-instrument-label" label="Instrumento" value={instrumentId} onChange={(event) => setInstrumentId(event.target.value)}>
                  <MenuItem value="">Todos</MenuItem>
                  {(taxonomies.data?.instruments ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}
                </Select>
              </FormControl>
              <FormControl size="small" sx={{ minWidth: 190 }}>
                <InputLabel id="directory-genre-label">Género</InputLabel>
                <Select labelId="directory-genre-label" label="Género" value={genreId} onChange={(event) => setGenreId(event.target.value)}>
                  <MenuItem value="">Todos</MenuItem>
                  {(taxonomies.data?.genres ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}
                </Select>
              </FormControl>
              {coordinates && <TextField size="small" type="number" label="Radio (km)" value={radiusKm} onChange={(event) => setRadiusKm(Math.min(500, Math.max(1, Number(event.target.value))))} inputProps={{ min: 1, max: 500 }} sx={{ width: 140 }} />}
              <FormControlLabel control={<Switch checked={remote} onChange={(event) => setRemote(event.target.checked)} />} label="Remoto" />
              <FormControlLabel control={<Switch checked={available} onChange={(event) => setAvailable(event.target.checked)} />} label="Disponible" />
              <Button onClick={() => { setProfessionId(''); setServiceId(''); setInstrumentId(''); setGenreId(''); setRemote(false); setAvailable(false); }}>Limpiar filtros</Button>
            </Stack>
          </Paper>

          <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" gap={2}>
            <Tabs value={entityType} onChange={(_, value: unknown) => { if (typeof value === 'string' && value in ENTITY_LABELS) setEntityType(value as DirectoryEntityType | 'all'); }} variant="scrollable" aria-label="Tipos de resultado">
              {(Object.keys(ENTITY_LABELS) as (DirectoryEntityType | 'all')[]).map((type) => (
                <Tab key={type} value={type} label={`${ENTITY_LABELS[type]}${type === 'all' ? facets?.total ? ` (${facets.total})` : '' : facets?.entityTypes[type] != null ? ` (${facets.entityTypes[type]})` : ''}`} />
              ))}
            </Tabs>
            <ToggleButtonGroup exclusive size="small" value={view} onChange={(_, value: unknown) => { if (value === 'list' || value === 'grid' || value === 'map') setView(value); }} aria-label="Vista de resultados">
              <ToggleButton value="list" aria-label="Lista"><ViewListIcon /></ToggleButton>
              <ToggleButton value="grid" aria-label="Cuadrícula"><GridViewIcon /></ToggleButton>
              <ToggleButton value="map" aria-label="Mapa"><MapIcon /></ToggleButton>
            </ToggleButtonGroup>
          </Stack>

          {sponsored.length > 0 && (
            <Box component="section" aria-labelledby="sponsored-heading">
              <Typography id="sponsored-heading" variant="overline">Patrocinados</Typography>
              <Stack spacing={1}>{sponsored.map((item) => <ResultCard key={`sponsored-${item.type}-${item.id}`} item={item} sessionActive={Boolean(session)} layout="list" />)}</Stack>
            </Box>
          )}

          <Typography component="h2" variant="h5" fontWeight={800}>Resultados orgánicos · {facets?.total ?? items.length}</Typography>

          {results.isLoading ? <Stack alignItems="center" py={8}><CircularProgress aria-label="Buscando resultados" /><Typography mt={2}>Buscando en TDF…</Typography></Stack> : null}
          {results.isError ? <Alert severity="error" action={<Button onClick={() => { void results.refetch(); }}>Reintentar</Button>}>No se pudo completar la búsqueda.</Alert> : null}
          {!results.isLoading && !results.isError && items.length === 0 ? (
            <Paper variant="outlined" sx={{ p: 5, textAlign: 'center', borderRadius: 3 }}>
              <Typography variant="h5" fontWeight={800}>Todavía no hay coincidencias</Typography>
              <Typography color="text.secondary" mt={1}>Amplía la ciudad o el radio, prueba un sinónimo o quita un filtro.</Typography>
              <Stack direction="row" justifyContent="center" gap={1} mt={3} flexWrap="wrap">
                {['músico', 'productor', 'estudio', 'concierto'].map((value) => <Chip key={value} label={value} onClick={() => { setDraftQuery(value); setQuery(value); }} clickable />)}
              </Stack>
            </Paper>
          ) : null}

          {view === 'map' && items.length > 0 ? <OpenStreetMapResults items={items} /> : null}
          {view !== 'map' && items.length > 0 ? (
            <Box sx={{ display: 'grid', gridTemplateColumns: view === 'grid' ? { xs: '1fr', md: 'repeat(2, minmax(0, 1fr))', xl: 'repeat(3, minmax(0, 1fr))' } : '1fr', gap: 2 }}>
              {items.map((item) => <ResultCard key={`${item.type}-${item.id}`} item={item} sessionActive={Boolean(session)} layout={view === 'grid' ? 'grid' : 'list'} />)}
            </Box>
          ) : null}
          {results.hasNextPage && <Button variant="outlined" size="large" onClick={() => { void results.fetchNextPage(); }} disabled={results.isFetchingNextPage} sx={{ alignSelf: 'center' }}>{results.isFetchingNextPage ? 'Cargando…' : 'Ver más resultados'}</Button>}
        </Stack>
      </Container>
    </Box>
  );
}

function ResultCard({ item, sessionActive, layout }: { item: DirectorySearchItem; sessionActive: boolean; layout: 'list' | 'grid' }) {
  const path = resultPath(item);
  const fallbackImageUrl = new URL(DIRECTORY_IMAGE_FALLBACKS[item.type], window.location.origin).toString();
  const imageUrl = resolveImageUrl(item.imageUrl) ?? fallbackImageUrl;
  const favorite = useMutation({
    mutationFn: () => Directory.addFavorite(item.type, item.id),
  });
  const share = async () => {
    const url = `${window.location.origin}${path}`;
    if (navigator.share) await navigator.share({ title: item.title, text: item.summary ?? undefined, url });
    else await navigator.clipboard.writeText(url);
  };
  return (
    <Card
      variant="outlined"
      sx={{
        borderRadius: 3,
        display: 'flex',
        flexDirection: layout === 'grid' ? 'column' : { xs: 'column', sm: 'row' },
        overflow: 'hidden',
      }}
    >
      <CardMedia
        component="img"
        image={imageUrl}
        alt={item.imageUrl ? `Foto de ${item.title}` : `Imagen de referencia de ${item.title}`}
        loading="lazy"
        onError={(event) => {
          if (event.currentTarget.src !== fallbackImageUrl) event.currentTarget.src = fallbackImageUrl;
        }}
        sx={{
          width: layout === 'grid' ? '100%' : { xs: '100%', sm: 220 },
          height: layout === 'grid' ? 220 : { xs: 240, sm: 'auto' },
          minHeight: layout === 'list' ? { sm: 220 } : undefined,
          objectFit: 'cover',
          objectPosition: 'center',
          flexShrink: 0,
        }}
      />
      <Box sx={{ display: 'flex', flex: 1, flexDirection: 'column', minWidth: 0 }}>
        <CardContent sx={{ flex: 1 }}>
          <Stack direction="row" justifyContent="space-between" gap={2}>
            <Box>
              <Stack direction="row" gap={1} alignItems="center" flexWrap="wrap">
                <Chip size="small" label={ENTITY_LABELS[item.type]} />
                {item.sponsored && (
                  <Chip
                    size="small"
                    label={item.sponsorDisclosure ?? 'Patrocinado'}
                    sx={{ bgcolor: '#7a3e00', color: '#fff' }}
                  />
                )}
              </Stack>
              <Typography component="h2" variant="h5" fontWeight={850} mt={1}>{item.title}</Typography>
              {item.subtitle && <Typography color="text.secondary">{item.subtitle}</Typography>}
            </Box>
            {item.location.distanceKm != null && <Chip label={`≈ ${item.location.distanceKm} km`} color="primary" variant="outlined" />}
          </Stack>
          <Typography mt={2} sx={{ display: '-webkit-box', WebkitLineClamp: 3, WebkitBoxOrient: 'vertical', overflow: 'hidden' }}>{item.summary ?? 'Abre el resultado para conocer más.'}</Typography>
          <Stack direction="row" gap={1} flexWrap="wrap" mt={2}>
            {item.location.city && <Chip size="small" label={`${item.location.city}${item.location.countryCode ? `, ${item.location.countryCode}` : ''}`} />}
            {item.location.precision && <Chip size="small" variant="outlined" label={`Ubicación ${item.location.precision === 'city' ? 'aproximada' : item.location.precision}`} />}
          </Stack>
        </CardContent>
        <CardActions sx={{ px: 2, pb: 2, flexWrap: 'wrap' }}>
          <Button component={RouterLink} to={path} variant="contained" onClick={() => getAnalyticsClient().capture('directory_result_opened', { entity_type: item.type, entity_id: item.id, sponsored: item.sponsored })}>Ver detalle</Button>
          <Button onClick={() => { void share(); }} startIcon={<ShareIcon />}>Compartir</Button>
          {sessionActive ? (
            <Button onClick={() => favorite.mutate()} disabled={favorite.isPending || favorite.isSuccess} startIcon={<BookmarkBorderIcon />}>
              {favorite.isSuccess ? 'Guardado' : 'Guardar'}
            </Button>
          ) : (
            <Button component={RouterLink} to={buildLoginRedirectPath(path)} startIcon={<LoginIcon />}>Ingresar para contactar</Button>
          )}
        </CardActions>
      </Box>
    </Card>
  );
}
