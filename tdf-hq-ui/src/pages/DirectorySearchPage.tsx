import { directoryResultPath, useDebouncedSearch, useDirectorySearch } from '../features/discovery/useDirectorySearch';
import { useTranslation } from 'react-i18next';
import {
  Alert,
  Autocomplete,
  Box,
  Button,
  Card,
  CardActions,
  CardContent,
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
import EventIcon from '@mui/icons-material/Event';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useEffect, useId, useMemo, useRef, useState, type FormEvent } from 'react';
import { Link as RouterLink, NavigationType, useLocation, useNavigate, useNavigationType } from 'react-router-dom';

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
import DirectoryPreviewImage from '../components/directory/DirectoryPreviewImage';

const ENTITY_LABELS: Record<DirectoryEntityType | 'artist' | 'all', string> = {
  all: 'directorySearch.entityAll',
  artist: 'directorySearch.entityArtist',
  profile: 'directorySearch.entityProfile',
  classified: 'directorySearch.entityClassified',
  event: 'directorySearch.entityEvent',
  venue: 'directorySearch.entityVenue',
};

const resultPath = directoryResultPath;

// Own keys only: `in` would accept prototype names such as "constructor".
const isEntityTypeKey = (value: string): value is DirectoryEntityType | 'artist' | 'all' =>
  Object.prototype.hasOwnProperty.call(ENTITY_LABELS, value);

// Profession, service, instrument, availability and remote filters describe
// people and listings. The server ignores them for events and venues, so the
// controls are hidden on those tabs (their values stay in the URL).
const PERSON_FILTER_KINDS: ReadonlySet<DirectoryEntityType | 'artist' | 'all'> = new Set(['all', 'profile', 'artist', 'classified']);

// The URL is the only source of the search state, so a deep link, a reload
// and the browser's back and forward buttons all hydrate the same way.
interface SearchUrlState {
  q: string;
  profileKind?: DirectorySearchQuery['profileKind'];
  entityType: DirectoryEntityType | 'artist' | 'all';
  cityId: string | null;
  professionId: string;
  serviceId: string;
  instrumentId: string;
  genreId: string;
  remote: boolean;
  available: boolean;
  radiusKm: number;
}

const parseSearchUrl = (search: string): SearchUrlState => {
  const params = new URLSearchParams(search);
  const requestedType = params.get('entityType');
  const radius = Number(params.get('radiusKm') ?? 25);
  return {
    q: params.get('q') ?? '',
    profileKind: (params.get('profileKind') ?? undefined) as DirectorySearchQuery['profileKind'],
    entityType: requestedType !== null && requestedType !== 'all' && isEntityTypeKey(requestedType)
      ? (requestedType as DirectoryEntityType)
      : 'all',
    cityId: params.get('cityId'),
    professionId: params.get('professionId') ?? '',
    serviceId: params.get('serviceId') ?? '',
    instrumentId: params.get('instrumentId') ?? '',
    genreId: params.get('genreId') ?? '',
    remote: params.get('remote') === 'true',
    available: params.get('available') === 'true',
    radiusKm: Number.isFinite(radius) ? Math.min(500, Math.max(1, radius)) : 25,
  };
};

const buildSearchUrl = (state: SearchUrlState, nearby: boolean) => {
  const params = new URLSearchParams();
  if (state.q) params.set('q', state.q);
  if (state.profileKind) params.set('profileKind', state.profileKind);
  if (state.entityType !== 'all') params.set('entityType', state.entityType);
  if (state.cityId && !nearby) params.set('cityId', state.cityId);
  if (state.professionId) params.set('professionId', state.professionId);
  if (state.serviceId) params.set('serviceId', state.serviceId);
  if (state.instrumentId) params.set('instrumentId', state.instrumentId);
  if (state.genreId) params.set('genreId', state.genreId);
  if (state.remote) params.set('remote', 'true');
  if (state.available) params.set('available', 'true');
  if (nearby) params.set('radiusKm', String(state.radiusKm));
  return params.toString();
};

const formatEventDate = (value: string | null | undefined, language: string) => {
  if (!value) return null;
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) return null;
  return new Intl.DateTimeFormat(language, { dateStyle: 'medium', timeStyle: 'short' }).format(date);
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
  const urlState = useMemo(() => parseSearchUrl(location.search), [location.search]);
  const [coordinates, setCoordinates] = useState<{ latitude: number; longitude: number } | null>(null);
  const { q: query, entityType, professionId, serviceId, instrumentId, genreId, remote, available, radiusKm } = urlState;
  const taxonomies = useQuery({
    queryKey: ['directory', 'taxonomies', language],
    queryFn: () => Directory.taxonomies(language),
    staleTime: 30 * 60 * 1000,
  });
  const cityId = urlState.cityId ?? '';
  const [draftQuery, setDraftQuery] = useState(query);
  useEffect(() => { setDraftQuery(query); }, [query]);
  const [geoMessage, setGeoMessage] = useState<string | null>(null);
  const [view, setView] = useState<'list' | 'grid' | 'map'>('list');
  const personFilters = PERSON_FILTER_KINDS.has(entityType);

  // Back or forward restored a city search while nearby coordinates are still
  // held: the restored entry wins. The coordinates are dropped during this
  // render, before the canonicalisation effect below could rewrite the entry
  // as a nearby one, so Back returns to the earlier city search. Only a
  // history move does this; entering nearby mode on a city URL keeps them.
  const navigationType = useNavigationType();
  const [handledLocationKey, setHandledLocationKey] = useState(location.key);
  if (handledLocationKey !== location.key) {
    setHandledLocationKey(location.key);
    if (navigationType === NavigationType.Pop && coordinates && urlState.cityId !== null) setCoordinates(null);
  }

  // User actions push a history entry, so back and forward move between
  // searches; normalisations (default city, canonical path) replace it. In
  // nearby mode every change replaces: the consented coordinates live only in
  // memory, so a nearby entry restored by Back could never show its results.
  const updateSearch = (patch: Partial<SearchUrlState>, mode: 'push' | 'replace' = 'push', nearby = Boolean(coordinates)) => {
    const next = buildSearchUrl({ ...urlState, cityId, ...patch }, nearby);
    if (location.search.replace(/^\?/, '') === next) return;
    navigate({ pathname: location.pathname, search: next }, { replace: mode === 'replace' || Boolean(coordinates) });
  };
  const debouncedQuery = useDebouncedSearch(draftQuery);
  const previousQuery = useRef(query);
  useEffect(() => {
    if (previousQuery.current !== query) { previousQuery.current = query; return; }
    if (debouncedQuery !== query && debouncedQuery === draftQuery.trim().replace(/\s+/gu, ' ')) updateSearch({ q: debouncedQuery }, 'replace');
  }, [debouncedQuery, draftQuery, query]); // eslint-disable-line react-hooks/exhaustive-deps
  const canonicalSearch = buildSearchUrl({ ...urlState, cityId }, Boolean(coordinates));
  useEffect(() => {
    if (location.search.replace(/^\?/, '') === canonicalSearch) return;
    navigate({ pathname: location.pathname, search: canonicalSearch }, { replace: true });
  }, [canonicalSearch, location.pathname, location.search, navigate]);

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



  const searchBase: DirectorySearchQuery = {
    q: query || undefined,
    profileKind: urlState.profileKind,
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

  const results = useDirectorySearch(searchBase);
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

  const submitSearch = (event: FormEvent) => {
    event.preventDefault();
    updateSearch({ q: draftQuery.trim() });
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
    queryKey: ['directory', 'suggestions', debouncedQuery, cityId],
    queryFn: () => Directory.suggestions(debouncedQuery, cityId || undefined),
    enabled: debouncedQuery.trim().length >= 2,
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
            <Paper component="form" role="search" onSubmit={submitSearch} elevation={8} sx={{ p: 1.5, borderRadius: 3 }}>
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
                  {/* Leaving nearby mode replaces the entry: the consented coordinates are held in memory only, so a nearby URL cannot be restored by Back. */}
                  <Select labelId="directory-city-label" label={t('directorySearch.city')} value={citySelectValue} onChange={(event) => { setCoordinates(null); updateSearch({ cityId: event.target.value }, coordinates ? 'replace' : 'push', false); }}>
                    <MenuItem value="">{t('directorySearch.allCities')}</MenuItem>
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
              {personFilters && (
                <>
                  <FormControl size="small" sx={{ minWidth: 190 }}>
                    <InputLabel id="directory-profession-label">{t('directorySearch.profession')}</InputLabel>
                    <Select labelId="directory-profession-label" label={t('directorySearch.profession')} value={professionId} onChange={(event) => updateSearch({ professionId: event.target.value })}>
                      <MenuItem value="">{t('directorySearch.allFeminine')}</MenuItem>
                      {(taxonomies.data?.professions ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}
                    </Select>
                  </FormControl>
                  <FormControl size="small" sx={{ minWidth: 190 }}>
                    <InputLabel id="directory-service-label">{t('directorySearch.service')}</InputLabel>
                    <Select labelId="directory-service-label" label={t('directorySearch.service')} value={serviceId} onChange={(event) => updateSearch({ serviceId: event.target.value })}>
                      <MenuItem value="">{t('directorySearch.allMasculine')}</MenuItem>
                      {(taxonomies.data?.serviceOfferings ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}
                    </Select>
                  </FormControl>
                  <FormControl size="small" sx={{ minWidth: 190 }}>
                    <InputLabel id="directory-instrument-label">{t('directorySearch.instrument')}</InputLabel>
                    <Select labelId="directory-instrument-label" label={t('directorySearch.instrument')} value={instrumentId} onChange={(event) => updateSearch({ instrumentId: event.target.value })}>
                      <MenuItem value="">{t('directorySearch.allMasculine')}</MenuItem>
                      {(taxonomies.data?.instruments ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}
                    </Select>
                  </FormControl>
                </>
              )}
              <FormControl size="small" sx={{ minWidth: 190 }}>
                <InputLabel id="directory-genre-label">{t('directorySearch.genre')}</InputLabel>
                <Select labelId="directory-genre-label" label={t('directorySearch.genre')} value={genreId} onChange={(event) => updateSearch({ genreId: event.target.value })}>
                  <MenuItem value="">{t('directorySearch.allMasculine')}</MenuItem>
                  {(taxonomies.data?.genres ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}
                </Select>
              </FormControl>
              {coordinates && <TextField size="small" type="number" label={t('directorySearch.radius')} value={radiusKm} onChange={(event) => updateSearch({ radiusKm: Math.min(500, Math.max(1, Number(event.target.value))) }, 'replace')} inputProps={{ min: 1, max: 500 }} sx={{ width: 140 }} />}
              {personFilters && (
                <>
                  <FormControlLabel control={<Switch checked={remote} onChange={(event) => updateSearch({ remote: event.target.checked })} />} label={t('directorySearch.remote')} />
                  <FormControlLabel control={<Switch checked={available} onChange={(event) => updateSearch({ available: event.target.checked })} />} label={t('directorySearch.available')} />
                </>
              )}
              {!personFilters && <Typography variant="body2" color="text.secondary">{t('directorySearch.placeFiltersHint')}</Typography>}
              <Button onClick={() => updateSearch({ professionId: '', serviceId: '', instrumentId: '', genreId: '', remote: false, available: false })}>{t('directorySearch.clearFilters')}</Button>
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
            <Tabs value={entityType} onChange={(_, value: unknown) => { if (typeof value === 'string' && isEntityTypeKey(value)) updateSearch({ entityType: value }); }} variant="scrollable" aria-label={t('directorySearch.resultTypes')}>
              {(Object.keys(ENTITY_LABELS) as (DirectoryEntityType | 'artist' | 'all')[]).map((type) => {
                // Every tab is counted under the current filters whichever tab
                // is selected; `all` is the mixed tab's own count.
                const count = type === 'all' ? facets?.entityTypes['all'] ?? (entityType === 'all' ? facets?.total : undefined) : facets?.entityTypes[type];
                return <Tab key={type} value={type} label={`${t(ENTITY_LABELS[type])}${count != null ? ` (${count})` : ''}`} />;
              })}
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
          {!results.isLoading && !results.isError && items.length === 0 && sponsored.length === 0 ? (
            <Paper variant="outlined" sx={{ p: 5, textAlign: 'center', borderRadius: 3 }}>
              <Typography variant="h5" fontWeight={800}>{t('directorySearch.emptyTitle')}</Typography>
              <Typography color="text.secondary" mt={1}>{t('directorySearch.emptyHelp')}</Typography>
              <Stack direction="row" justifyContent="center" gap={1} mt={3} flexWrap="wrap">
                {[t('directorySearch.hintMusician'), t('directorySearch.hintProducer'), t('directorySearch.hintStudio'), t('directorySearch.hintConcert')].map((value) => <Chip key={value} label={value} onClick={() => { setDraftQuery(value); updateSearch({ q: value }); }} clickable />)}
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
  const { t, i18n } = useTranslation();
  const path = resultPath(item);
  const sourceProfile = item.sourceProfile ?? null;
  const eventDate = item.type === 'event' ? formatEventDate(item.effectiveAt, i18n.resolvedLanguage ?? 'es') : null;
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
      <DirectoryPreviewImage
        kind={item.type}
        imageUrl={item.imageUrl}
        alt={t('directorySearch.photo', { title: item.title })}
        fallbackAlt={t('directorySearch.fallbackPhoto', { title: item.title })}
        width={440}
        height={440}
        sizes={layout === 'grid' ? '(min-width: 900px) 33vw, 100vw' : '(min-width: 600px) 220px, 100vw'}
        sx={{
          width: layout === 'grid' ? '100%' : { xs: '100%', sm: 220 },
          height: layout === 'grid' ? 220 : { xs: 240, sm: 220 },
          aspectRatio: 'auto',
          alignSelf: layout === 'list' ? { sm: 'stretch' } : undefined,
          minHeight: layout === 'list' ? { sm: 220 } : undefined,
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
            {eventDate && <Chip size="small" color="primary" variant="outlined" icon={<EventIcon />} label={eventDate} aria-label={t('directorySearch.eventDate', { date: eventDate })} />}
            {item.location.city && <Chip size="small" label={`${item.location.city}${item.location.countryCode ? `, ${item.location.countryCode}` : ''}`} />}
            {item.location.precision && <Chip size="small" variant="outlined" label={item.location.precision === 'city' ? t('directorySearch.approximateLocation') : t('directorySearch.locationPrecision', { precision: item.location.precision })} />}
          </Stack>
        </CardContent>
        <CardActions sx={{ px: 2, pb: 2, flexWrap: 'wrap' }}>
          {sourceProfile ? (
            <Button component={RouterLink} to={sourceProfile.canonicalUrl} variant="contained" onClick={() => getAnalyticsClient().capture('directory_result_opened', { entity_type: 'profile', entity_id: sourceProfile.id, sponsored: item.sponsored, via: 'artist_listing' })}>{t('directorySearch.viewProfile')}</Button>
          ) : (
            <Button component={RouterLink} to={path} variant="contained" onClick={() => getAnalyticsClient().capture('directory_result_opened', { entity_type: item.type, entity_id: item.id, sponsored: item.sponsored })}>{t('directorySearch.detail')}</Button>
          )}
          <Button onClick={() => { void share(); }} disabled={shareStatus === 'pending'} aria-busy={shareStatus === 'pending' || undefined} startIcon={<ShareIcon />}>{t(shareStatus === 'pending' ? 'directorySearch.sharePending' : 'directorySearch.share')}</Button>
          {item.id.startsWith('artist-') ? null : partyId ? (
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
