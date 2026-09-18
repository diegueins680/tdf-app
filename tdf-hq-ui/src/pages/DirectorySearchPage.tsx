import { useTranslation } from 'react-i18next';
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
import BookmarkIcon from '@mui/icons-material/Bookmark';
import BookmarkBorderIcon from '@mui/icons-material/BookmarkBorder';
import LoginIcon from '@mui/icons-material/Login';
import { useInfiniteQuery, useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useEffect, useId, useMemo, useRef, useState, type FormEvent } from 'react';
import { Link as RouterLink, useLocation, useNavigate } from 'react-router-dom';

import {
  Directory,
  type DirectoryEntityType,
  type DirectoryFavorite,
  type DirectorySearchItem,
  type DirectorySearchQuery,
} from '../api/directory';
import OpenStreetMapResults from '../components/directory/OpenStreetMapResults';
import { getAnalyticsClient } from '../analytics/posthog';
import { captureFirstValueOnce } from '../analytics/onboardingProgress';
import { useMetaTags } from '../hooks/useMetaTags';
import { getActiveSession, useSession } from '../session/SessionContext';
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
  all: 'directorySearch.entityAll',
  profile: 'directorySearch.entityProfile',
  classified: 'directorySearch.entityClassified',
  event: 'directorySearch.entityEvent',
  venue: 'directorySearch.entityVenue',
};

const resultPath = (item: DirectorySearchItem) => {
  if (item.type === 'profile') return `/directorio/${item.slug}`;
  if (item.type === 'classified') return `/clasificados/${item.slug}`;
  if (item.type === 'event') return `/eventos/${item.id}`;
  return `/venues/${item.id}`;
};

export default function DirectorySearchPage() {
  const { t, i18n } = useTranslation();
  const language = i18n.resolvedLanguage?.startsWith('es') ? 'es' : 'en';
  const location = useLocation();
  const navigate = useNavigate();
  const { session } = useSession();
  const queryClient = useQueryClient();
  const scope = useId();
  const occurrence = useRef({ session, generation: 0 });
  if (occurrence.current.session !== session) occurrence.current = { session, generation: occurrence.current.generation + 1 };
  const generation = occurrence.current.generation;
  const mounted = useRef(true);
  useEffect(() => { mounted.current = true; return () => { mounted.current = false; }; }, []);
  const isActiveParty = (expectedPartyId: number) => mounted.current && session?.partyId === expectedPartyId
    && getActiveSession() === session && occurrence.current.generation === generation;
  const initial = useMemo(() => new URLSearchParams(location.search), []); // eslint-disable-line react-hooks/exhaustive-deps
  const [draftQuery, setDraftQuery] = useState(initial.get('q') ?? '');
  const [query, setQuery] = useState(initial.get('q') ?? '');
  const [entityType, setEntityType] = useState<DirectoryEntityType | 'all'>(
    (initial.get('entityType') as DirectoryEntityType | null) ?? 'all',
  );
  const [cityId, setCityId] = useState(() => {
    const requestedCity = initial.get('cityId');
    if (requestedCity !== null) return requestedCity;
    try { return window.localStorage.getItem(CITY_STORAGE_KEY) ?? ''; }
    catch { return ''; } // A saved city is optional; search remains available.
  });
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
    title: query ? t('directorySearch.searchTitle', { query }) : t('directorySearch.title'),
    description: t('directorySearch.description'),
    canonical: `${window.location.origin}/buscar`,
    structuredData: {
      '@context': 'https://schema.org',
      '@type': 'SearchResultsPage',
      name: t('directorySearch.structuredName'),
      url: `${window.location.origin}/buscar`,
      inLanguage: language,
    },
  });

  const taxonomies = useQuery({
    queryKey: ['directory', 'taxonomies', language],
    queryFn: () => Directory.taxonomies(language),
    staleTime: 30 * 60 * 1000,
  });

  useEffect(() => {
    if (coordinates || !taxonomies.data?.cities.length) return;
    if (taxonomies.data.cities.some((city) => city.id === cityId)) return;
    const quito = taxonomies.data.cities.find((city) => city.code === 'quito-ec-p');
    setCityId(quito?.id ?? taxonomies.data.cities[0]?.id ?? '');
  }, [cityId, coordinates, taxonomies.data]);

  useEffect(() => {
    if (!cityId) return;
    try { window.localStorage.setItem(CITY_STORAGE_KEY, cityId); }
    catch { /* Keep the selected city in memory and in the search URL. */ }
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
  const citySelectValue = coordinates
    ? '__nearby'
    : taxonomies.data?.cities.some((city) => city.id === cityId)
      ? cityId
      : '';
  const favoritesQueryKey = ['directory', 'favorites', session?.partyId, scope, generation] as const;
  const favorites = useQuery({
    queryKey: favoritesQueryKey,
    queryFn: async () => {
      const ownerPartyId = session?.partyId;
      if (ownerPartyId === undefined || !isActiveParty(ownerPartyId)) throw new Error(t('directorySearch.sessionChanged'));
      const saved = await Directory.favorites();
      if (!isActiveParty(ownerPartyId)) throw new Error(t('directorySearch.sessionChanged'));
      return saved;
    },
    enabled: Boolean(session?.partyId),
    retry: false,
  });
  const favoriteKeys = useMemo(
    () => new Set((favorites.data ?? []).map((favorite) => `${favorite.targetKind}:${favorite.targetId}`)),
    [favorites.data],
  );
  const favoriteAvailability = !session?.partyId
    ? 'unauthenticated'
    : favorites.isLoading
      ? 'loading'
      : favorites.isError
        ? 'error'
        : 'ready';
  const refreshFavorites = async (): Promise<boolean> => {
    const ownerPartyId = session?.partyId;
    if (ownerPartyId === undefined || !isActiveParty(ownerPartyId)) return false;
    const result = await favorites.refetch();
    return result.isSuccess && isActiveParty(ownerPartyId);
  };
  const updateFavoriteCache = (
    expectedPartyId: number,
    item: DirectorySearchItem,
    saved: boolean,
  ) => {
    if (!isActiveParty(expectedPartyId)) return;
    queryClient.setQueryData<DirectoryFavorite[]>(favoritesQueryKey, (current = []) => {
      const withoutTarget = current.filter(
        (favorite) => favorite.targetKind !== item.type || favorite.targetId !== item.id,
      );
      if (!saved) return withoutTarget;
      return [
        {
          targetKind: item.type,
          targetId: item.id,
          createdAt: new Date().toISOString(),
          result: {
            type: item.type,
            id: item.id,
            slug: item.slug,
            title: item.title,
            city: item.location.city ?? null,
          },
        },
        ...withoutTarget,
      ];
    });
    void favorites.refetch();
  };

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
      setGeoMessage('directorySearch.locationUnavailable');
      return;
    }
    navigator.geolocation.getCurrentPosition(
      ({ coords }) => {
        setCoordinates({ latitude: coords.latitude, longitude: coords.longitude });
        setGeoMessage('directorySearch.locationUsed');
      },
      () => setGeoMessage('directorySearch.locationFailed'),
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
            <Chip label={t('directorySearch.region')} sx={{ alignSelf: 'flex-start', bgcolor: 'rgba(255,255,255,.14)', color: 'white' }} />
            <Typography component="h1" variant="h2" fontWeight={900} sx={{ fontSize: { xs: '2.2rem', md: '4rem' } }}>
              {t('directorySearch.heading')}
            </Typography>
            <Typography variant="h6" sx={{ maxWidth: 760, color: 'rgba(255,255,255,.82)' }}>
              {t('directorySearch.intro')}
            </Typography>
            <Paper component="form" onSubmit={submitSearch} elevation={8} sx={{ p: 1.5, borderRadius: 3 }}>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
                <Autocomplete
                  freeSolo
                  fullWidth
                  options={(suggestions.data ?? []).map((option) => option.label)}
                  inputValue={draftQuery}
                  onInputChange={(_, value) => setDraftQuery(value)}
                  renderInput={(params) => <TextField {...params} label={t('directorySearch.queryLabel')} placeholder={t('directorySearch.queryPlaceholder')} inputProps={{ ...params.inputProps, maxLength: 160 }} />}
                />
                <FormControl sx={{ minWidth: { sm: 220 } }}>
                  <InputLabel id="directory-city-label">{t('directorySearch.city')}</InputLabel>
                  <Select labelId="directory-city-label" label={t('directorySearch.city')} value={citySelectValue} onChange={(event) => { setCoordinates(null); setCityId(event.target.value); }}>
                    {coordinates && <MenuItem value="__nearby">{t('directorySearch.nearby')}</MenuItem>}
                    {(taxonomies.data?.cities ?? []).map((city) => <MenuItem key={city.id} value={city.id}>{city.name}</MenuItem>)}
                  </Select>
                </FormControl>
                <Button type="submit" variant="contained" size="large" startIcon={<SearchIcon />} sx={{ minWidth: 140 }}>{t('directorySearch.search')}</Button>
              </Stack>
            </Paper>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ sm: 'center' }}>
              <Button color="inherit" variant="outlined" startIcon={<MyLocationIcon />} onClick={locate} sx={{ alignSelf: 'flex-start', borderColor: 'rgba(255,255,255,.5)' }}>
                {t('directorySearch.useLocation')}
              </Button>
              <Typography variant="caption" sx={{ color: 'rgba(255,255,255,.75)' }}>
                {t('directorySearch.locationPrivacy')}
              </Typography>
            </Stack>
            {geoMessage && <Alert severity={coordinates ? 'success' : 'info'}>{t(geoMessage)}</Alert>}
          </Stack>
        </Container>
      </Box>

      <Container maxWidth="xl" sx={{ mt: 4 }}>
        <Stack spacing={3}>
          <Paper variant="outlined" sx={{ p: 2.5, borderRadius: 3 }}>
            <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} alignItems={{ md: 'center' }}>
              <FormControl size="small" sx={{ minWidth: 190 }}>
                <InputLabel id="directory-profession-label">{t('directorySearch.profession')}</InputLabel>
                <Select labelId="directory-profession-label" label={t('directorySearch.profession')} value={professionId} onChange={(event) => setProfessionId(event.target.value)}>
                  <MenuItem value="">{t('directorySearch.allFeminine')}</MenuItem>
                  {(taxonomies.data?.professions ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}
                </Select>
              </FormControl>
              <FormControl size="small" sx={{ minWidth: 190 }}>
                <InputLabel id="directory-service-label">{t('directorySearch.service')}</InputLabel>
                <Select labelId="directory-service-label" label={t('directorySearch.service')} value={serviceId} onChange={(event) => setServiceId(event.target.value)}>
                  <MenuItem value="">{t('directorySearch.allMasculine')}</MenuItem>
                  {(taxonomies.data?.serviceOfferings ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}
                </Select>
              </FormControl>
              <FormControl size="small" sx={{ minWidth: 190 }}>
                <InputLabel id="directory-instrument-label">{t('directorySearch.instrument')}</InputLabel>
                <Select labelId="directory-instrument-label" label={t('directorySearch.instrument')} value={instrumentId} onChange={(event) => setInstrumentId(event.target.value)}>
                  <MenuItem value="">{t('directorySearch.allMasculine')}</MenuItem>
                  {(taxonomies.data?.instruments ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}
                </Select>
              </FormControl>
              <FormControl size="small" sx={{ minWidth: 190 }}>
                <InputLabel id="directory-genre-label">{t('directorySearch.genre')}</InputLabel>
                <Select labelId="directory-genre-label" label={t('directorySearch.genre')} value={genreId} onChange={(event) => setGenreId(event.target.value)}>
                  <MenuItem value="">{t('directorySearch.allMasculine')}</MenuItem>
                  {(taxonomies.data?.genres ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}
                </Select>
              </FormControl>
              {coordinates && <TextField size="small" type="number" label={t('directorySearch.radius')} value={radiusKm} onChange={(event) => setRadiusKm(Math.min(500, Math.max(1, Number(event.target.value))))} inputProps={{ min: 1, max: 500 }} sx={{ width: 140 }} />}
              <FormControlLabel control={<Switch checked={remote} onChange={(event) => setRemote(event.target.checked)} />} label={t('directorySearch.remote')} />
              <FormControlLabel control={<Switch checked={available} onChange={(event) => setAvailable(event.target.checked)} />} label={t('directorySearch.available')} />
              <Button onClick={() => { setProfessionId(''); setServiceId(''); setInstrumentId(''); setGenreId(''); setRemote(false); setAvailable(false); }}>{t('directorySearch.clearFilters')}</Button>
            </Stack>
          </Paper>

          {favorites.isError && session?.partyId ? (
            <Alert
              severity="warning"
              action={<Button onClick={() => { void favorites.refetch(); }}>{t('directorySearch.retry')}</Button>}
            >
              {t('directorySearch.favoritesReadError')}
            </Alert>
          ) : null}

          <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" gap={2}>
            <Tabs value={entityType} onChange={(_, value: unknown) => { if (typeof value === 'string' && value in ENTITY_LABELS) setEntityType(value as DirectoryEntityType | 'all'); }} variant="scrollable" aria-label={t('directorySearch.resultTypes')}>
              {(Object.keys(ENTITY_LABELS) as (DirectoryEntityType | 'all')[]).map((type) => (
                <Tab key={type} value={type} label={`${t(ENTITY_LABELS[type])}${type === 'all' ? facets?.total ? ` (${facets.total})` : '' : facets?.entityTypes[type] != null ? ` (${facets.entityTypes[type]})` : ''}`} />
              ))}
            </Tabs>
            <ToggleButtonGroup exclusive size="small" value={view} onChange={(_, value: unknown) => { if (value === 'list' || value === 'grid' || value === 'map') setView(value); }} aria-label={t('directorySearch.resultView')}>
              <ToggleButton value="list" aria-label={t('directorySearch.list')}><ViewListIcon /></ToggleButton>
              <ToggleButton value="grid" aria-label={t('directorySearch.grid')}><GridViewIcon /></ToggleButton>
              <ToggleButton value="map" aria-label={t('directorySearch.map')}><MapIcon /></ToggleButton>
            </ToggleButtonGroup>
          </Stack>

          {sponsored.length > 0 && (
            <Box component="section" aria-labelledby="sponsored-heading">
              <Typography id="sponsored-heading" variant="overline">{t('directorySearch.sponsoredPlural')}</Typography>
              <Stack spacing={1}>{sponsored.map((item) => <ResultCard key={`sponsored-${generation}-${item.type}-${item.id}`} item={item} partyId={session?.partyId} layout="list" isFavorite={favoriteKeys.has(`${item.type}:${item.id}`)} favoriteAvailability={favoriteAvailability} onFavoriteChanged={updateFavoriteCache} onRefreshFavorites={refreshFavorites} isActiveParty={isActiveParty} />)}</Stack>
            </Box>
          )}

          <Typography component="h2" variant="h5" fontWeight={800}>{t('directorySearch.organicResults', { count: facets?.total ?? items.length })}</Typography>

          {results.isLoading ? <Stack alignItems="center" py={8}><CircularProgress aria-label={t('directorySearch.searchingLabel')} /><Typography mt={2}>{t('directorySearch.searching')}</Typography></Stack> : null}
          {results.isError ? <Alert severity="error" action={<Button onClick={() => { void results.refetch(); }}>{t('directorySearch.retry')}</Button>}>{t('directorySearch.searchError')}</Alert> : null}
          {!results.isLoading && !results.isError && items.length === 0 ? (
            <Paper variant="outlined" sx={{ p: 5, textAlign: 'center', borderRadius: 3 }}>
              <Typography variant="h5" fontWeight={800}>{t('directorySearch.emptyTitle')}</Typography>
              <Typography color="text.secondary" mt={1}>{t('directorySearch.emptyHelp')}</Typography>
              <Stack direction="row" justifyContent="center" gap={1} mt={3} flexWrap="wrap">
                {[t('directorySearch.hintMusician'), t('directorySearch.hintProducer'), t('directorySearch.hintStudio'), t('directorySearch.hintConcert')].map((value) => <Chip key={value} label={value} onClick={() => { setDraftQuery(value); setQuery(value); }} clickable />)}
              </Stack>
            </Paper>
          ) : null}

          {view === 'map' && items.length > 0 ? <OpenStreetMapResults items={items} /> : null}
          {view !== 'map' && items.length > 0 ? (
            <Box sx={{ display: 'grid', gridTemplateColumns: view === 'grid' ? { xs: '1fr', md: 'repeat(2, minmax(0, 1fr))', xl: 'repeat(3, minmax(0, 1fr))' } : '1fr', gap: 2 }}>
              {items.map((item) => <ResultCard key={`${generation}-${item.type}-${item.id}`} item={item} partyId={session?.partyId} layout={view === 'grid' ? 'grid' : 'list'} isFavorite={favoriteKeys.has(`${item.type}:${item.id}`)} favoriteAvailability={favoriteAvailability} onFavoriteChanged={updateFavoriteCache} onRefreshFavorites={refreshFavorites} isActiveParty={isActiveParty} />)}
            </Box>
          ) : null}
          {results.hasNextPage && <Button variant="outlined" size="large" onClick={() => { void results.fetchNextPage(); }} disabled={results.isFetchingNextPage} sx={{ alignSelf: 'center' }}>{results.isFetchingNextPage ? t('directorySearch.loadingMore') : t('directorySearch.loadMore')}</Button>}
        </Stack>
      </Container>
    </Box>
  );
}

function ResultCard({
  item,
  partyId,
  layout,
  isFavorite,
  favoriteAvailability,
  onFavoriteChanged,
  onRefreshFavorites,
  isActiveParty,
}: {
  item: DirectorySearchItem;
  partyId?: number;
  layout: 'list' | 'grid';
  isFavorite: boolean;
  favoriteAvailability: 'unauthenticated' | 'loading' | 'error' | 'ready';
  onFavoriteChanged: (partyId: number, item: DirectorySearchItem, saved: boolean) => void;
  onRefreshFavorites: () => Promise<boolean>;
  isActiveParty: (partyId: number) => boolean;
}) {
  const { t } = useTranslation();
  const path = resultPath(item);
  const fallbackImageUrl = new URL(DIRECTORY_IMAGE_FALLBACKS[item.type], window.location.origin).toString();
  const imageUrl = resolveImageUrl(item.imageUrl) ?? fallbackImageUrl;
  const favorite = useMutation({
    mutationFn: async ({ ownerPartyId, saved }: { ownerPartyId: number; saved: boolean }) => {
      if (!isActiveParty(ownerPartyId)) throw new Error(t('directorySearch.sessionChanged'));
      if (saved) await Directory.addFavorite(item.type, item.id);
      else await Directory.removeFavorite(item.type, item.id);
      return { ownerPartyId, saved };
    },
    onSuccess: ({ ownerPartyId, saved }) => {
      if (!isActiveParty(ownerPartyId)) return;
      onFavoriteChanged(ownerPartyId, item, saved);
      const analytics = getAnalyticsClient();
      analytics.capture('feature_favorite_changed', {
        feature: item.type,
        item_id: item.id,
        state: saved,
        source: 'web_directory_search',
      });
      if (saved && item.type === 'event') {
        void captureFirstValueOnce(analytics, ownerPartyId, 'event_saved', undefined, () => isActiveParty(ownerPartyId));
      }
    },
  });
  const shareBusy = useRef(false);
  const [shareStatus, setShareStatus] = useState<'idle' | 'pending' | 'copied' | 'completed' | 'cancelled' | 'error'>('idle');
  const share = async () => {
    if (shareBusy.current) return;
    shareBusy.current = true;
    setShareStatus('pending');
    const url = `${window.location.origin}${path}`;
    try {
      if (navigator.share) {
        await navigator.share({ title: item.title, text: item.summary ?? undefined, url });
        setShareStatus('completed');
      } else if (navigator.clipboard?.writeText) {
        await navigator.clipboard.writeText(url);
        setShareStatus('copied');
      } else {
        setShareStatus('error');
      }
    } catch (error) {
      const cancelled = typeof error === 'object' && error !== null && 'name' in error && error.name === 'AbortError';
      setShareStatus(cancelled ? 'cancelled' : 'error');
    } finally {
      shareBusy.current = false;
    }
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
        alt={item.imageUrl ? t('directorySearch.photo', { title: item.title }) : t('directorySearch.fallbackPhoto', { title: item.title })}
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
                <Chip size="small" label={t(ENTITY_LABELS[item.type])} />
                {item.sponsored && (
                  <Chip
                    size="small"
                    label={item.sponsorDisclosure ?? t('directorySearch.sponsored')}
                    sx={{ bgcolor: '#7a3e00', color: '#fff' }}
                  />
                )}
              </Stack>
              <Typography component="h2" variant="h5" fontWeight={850} mt={1}>{item.title}</Typography>
              {item.subtitle && <Typography color="text.secondary">{item.subtitle}</Typography>}
            </Box>
            {item.location.distanceKm != null && <Chip label={`≈ ${item.location.distanceKm} km`} color="primary" variant="outlined" />}
          </Stack>
          <Typography mt={2} sx={{ display: '-webkit-box', WebkitLineClamp: 3, WebkitBoxOrient: 'vertical', overflow: 'hidden' }}>{item.summary ?? t('directorySearch.summaryFallback')}</Typography>
          <Stack direction="row" gap={1} flexWrap="wrap" mt={2}>
            {item.location.city && <Chip size="small" label={`${item.location.city}${item.location.countryCode ? `, ${item.location.countryCode}` : ''}`} />}
            {item.location.precision && <Chip size="small" variant="outlined" label={item.location.precision === 'city' ? t('directorySearch.approximateLocation') : t('directorySearch.locationPrecision', { precision: item.location.precision })} />}
          </Stack>
        </CardContent>
        <CardActions sx={{ px: 2, pb: 2, flexWrap: 'wrap' }}>
          <Button component={RouterLink} to={path} variant="contained" onClick={() => getAnalyticsClient().capture('directory_result_opened', { entity_type: item.type, entity_id: item.id, sponsored: item.sponsored })}>{t('directorySearch.detail')}</Button>
          <Button onClick={() => { void share(); }} disabled={shareStatus === 'pending'} aria-busy={shareStatus === 'pending' || undefined} startIcon={<ShareIcon />}>{t(shareStatus === 'pending' ? 'directorySearch.sharePending' : 'directorySearch.share')}</Button>
          {partyId ? (
            <Button
              onClick={() => { if (isActiveParty(partyId)) favorite.mutate({ ownerPartyId: partyId, saved: !isFavorite }); }}
              disabled={favorite.isPending || favoriteAvailability !== 'ready'}
              startIcon={isFavorite ? <BookmarkIcon /> : <BookmarkBorderIcon />}
              aria-pressed={isFavorite}
              aria-busy={favorite.isPending || undefined}
              aria-label={isFavorite ? t('directorySearch.removeLabel', { title: item.title }) : t('directorySearch.saveLabel', { title: item.title })}
            >
              {favorite.isPending
                ? t('directorySearch.updating')
                : favoriteAvailability === 'loading'
                  ? t('directorySearch.checking')
                  : favoriteAvailability === 'error'
                    ? t('directorySearch.favoritesUnavailable')
                    : isFavorite
                      ? t('directorySearch.unsave')
                      : t('directorySearch.save')}
            </Button>
          ) : (
            <Button component={RouterLink} to={`${buildLoginRedirectPath(path)}${item.type === 'event' ? '&intent=events' : ''}`} startIcon={<LoginIcon />}>
              {item.type === 'event' ? t('directorySearch.loginSave') : t('directorySearch.loginContact')}
            </Button>
          )}
        </CardActions>
        {shareStatus !== 'idle' && shareStatus !== 'pending' && (
          <Alert severity={shareStatus === 'error' ? 'error' : shareStatus === 'cancelled' ? 'info' : 'success'} role={shareStatus === 'error' ? 'alert' : 'status'} sx={{ mx: 2, mb: 2 }}>
            {t(shareStatus === 'copied' ? 'directorySearch.shareCopied' : shareStatus === 'completed' ? 'directorySearch.shareCompleted' : shareStatus === 'cancelled' ? 'directorySearch.shareCancelled' : 'directorySearch.shareError')}
          </Alert>
        )}
        {favorite.isError ? (
          <Alert severity="error" sx={{ mx: 2, mb: 2 }} action={<Button onClick={() => { void onRefreshFavorites().then((refreshed) => { if (refreshed && partyId !== undefined && isActiveParty(partyId)) favorite.reset(); }); }}>{t('directorySearch.refreshFavorites')}</Button>}>
            {t('directorySearch.favoriteError')}
          </Alert>
        ) : null}
      </Box>
    </Card>
  );
}
