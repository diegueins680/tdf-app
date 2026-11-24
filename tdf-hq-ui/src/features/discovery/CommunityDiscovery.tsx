import MarkdownText from '../../components/MarkdownText';
import { useEffect, useRef, useState, type FormEvent } from 'react';
import { Alert, Box, Button, Card, CardActionArea, CardContent, LinearProgress, MenuItem, Stack, TablePagination, TextField, Typography } from '@mui/material';
import { Link, useSearchParams } from 'react-router-dom';
import { resolveDirectoryPreviewImage } from '../../services/directoryPreviewImage';
import { useTranslation } from 'react-i18next';
import TaxonomyFilterBar from '../../components/taxonomyFilters/TaxonomyFilterBar';
import { isTaxonomySelectionEmpty, resolveTaxonomyLanguage, taxonomyName, TAXONOMY_URL_KEYS } from '../../components/taxonomyFilters/taxonomyFilterModel';
import { useSurfaceTaxonomyFilters, useTaxonomyFilterOptions } from '../../components/taxonomyFilters/useTaxonomyFilters';
import { useProfileCityDefault } from '../../components/taxonomyFilters/useProfileCityDefault';
import { isArtistPlaceholderImage } from '../../utils/artistFallbacks';
import ArtistProfileImage from '../../components/ArtistProfileImage';
import { type DirectorySearchQuery } from '../../api/directory';
import { directoryResultPath, useDebouncedSearch, useDirectorySearch } from './useDirectorySearch';

const PAGE_SIZE_OPTIONS = [12, 24, 48];

const hasPicture = (imageUrl: unknown) => {
  const resolved = resolveDirectoryPreviewImage(imageUrl);
  return Boolean(resolved) && !isArtistPlaceholderImage(resolved);
};
const picturedResultsFirst = <T extends { imageUrl?: unknown }>(items: readonly T[]): T[] =>
  [...items].sort((left, right) => Number(hasPicture(right.imageUrl)) - Number(hasPicture(left.imageUrl)));

export default function CommunityDiscovery() {
  const [params, setParams] = useSearchParams();
  const query = params.get('q') ?? '';
  const [draft, setDraft] = useState(query);
  const debounced = useDebouncedSearch(draft);
  const previousQuery = useRef(query);
  useEffect(() => { setDraft(query); }, [query]);
  // The result page and page size live in the URL with the filters, so coming
  // back from a profile restores the same results. Any other change starts
  // again from the first page.
  const update = (key: string, value: string) => setParams((current) => {
    const next = new URLSearchParams(current);
    if (value) next.set(key, value); else next.delete(key);
    if (key !== 'page') next.delete('page');
    return next;
  }, { replace: true });
  useEffect(() => {
    if (previousQuery.current !== query) { previousQuery.current = query; return; }
    if (debounced !== query && debounced === draft.trim().replace(/\s+/gu, ' ')) update('q', debounced);
  }, [debounced, draft, query]); // eslint-disable-line react-hooks/exhaustive-deps
  const profileKind = params.get('profileKind') ?? '';
  // TAX-FILTER-001: location and genre come from the shared filters. Links
  // from before the shared filters (?genreId=&cityId=) open as the same filter.
  useEffect(() => {
    const legacyGenre = params.get('genreId');
    const legacyCity = params.get('cityId');
    if (!legacyGenre && !legacyCity) return;
    setParams((current) => {
      const next = new URLSearchParams(current);
      next.delete('genreId'); next.delete('cityId');
      if (legacyGenre && !next.getAll(TAXONOMY_URL_KEYS.genreIds).includes(legacyGenre)) next.append(TAXONOMY_URL_KEYS.genreIds, legacyGenre);
      if (legacyCity && !next.getAll(TAXONOMY_URL_KEYS.cityIds).includes(legacyCity)) next.append(TAXONOMY_URL_KEYS.cityIds, legacyCity);
      return next;
    }, { replace: true });
  }, [params, setParams]);
  const { i18n } = useTranslation();
  const language = resolveTaxonomyLanguage(i18n?.resolvedLanguage ?? i18n?.language);
  const taxonomyOptions = useTaxonomyFilterOptions();
  useProfileCityDefault(taxonomyOptions.data);
  const taxonomyFilters = useSurfaceTaxonomyFilters({ urlBacked: true });
  const { selection } = taxonomyFilters;
  const genreNames = new Map((taxonomyOptions.data?.genres ?? []).map((genre) => [genre.id, taxonomyName(genre, language)]));
  const requestedPage = Number(params.get('page'));
  const page = Number.isInteger(requestedPage) && requestedPage > 1 ? requestedPage - 1 : 0;
  const setPage = (next: number) => update('page', next > 0 ? String(next + 1) : '');
  const requestedSize = Number(params.get('perPage'));
  const rowsPerPage = PAGE_SIZE_OPTIONS.includes(requestedSize) ? requestedSize : PAGE_SIZE_OPTIONS[0]!;
  const setRowsPerPage = (size: number) => update('perPage', size === PAGE_SIZE_OPTIONS[0] ? '' : String(size));
  const section = useRef<HTMLDivElement>(null);
  const results = useDirectorySearch({ q: query || undefined, entityType: 'profile',
    profileKind: (profileKind || undefined) as DirectorySearchQuery['profileKind'],
    countryIds: selection.countryIds, regionIds: selection.regionIds, cityIds: selection.cityIds, genreIds: selection.genreIds,
    limit: rowsPerPage });
  // The server pages by cursor, so pages are reached one step at a time and
  // already visited ones are shown again without another request.
  const pages = results.data?.pages ?? [];
  const total = pages[0]?.facets.total ?? 0;
  // A restored page that is not loaded yet is reached one server page at a time.
  const catchingUp = pages.length > 0 && page >= pages.length && Boolean(results.hasNextPage);
  // Until every API ranks pictured profiles first, keep that order within the
  // page shown; the sort is stable, so the server order holds inside each group.
  const items = picturedResultsFirst(catchingUp ? [] : pages[Math.min(page, pages.length - 1)]?.items ?? []);
  const { fetchNextPage, isFetching, isFetchNextPageError } = results;
  // After a failed page request the catch-up stops; only the retry button asks again.
  useEffect(() => {
    if (catchingUp && !isFetching && !isFetchNextPageError) void fetchNextPage();
  }, [catchingUp, isFetching, isFetchNextPageError, fetchNextPage, pages.length]);
  const retryResults = () => { void (isFetchNextPageError ? fetchNextPage() : results.refetch()); };
  const searchKey = [query, profileKind, JSON.stringify(selection), rowsPerPage].join('\u0000');
  const currentSearch = useRef(searchKey);
  useEffect(() => { currentSearch.current = searchKey; }, [searchKey]);
  const goToPage = async (next: number) => {
    const startedFor = searchKey;
    if (next >= pages.length) {
      const fetched = await results.fetchNextPage();
      // The filters may have changed while the page loaded; that navigation
      // belongs to the previous search and must not move the new one.
      if (currentSearch.current !== startedFor || (fetched.data?.pages.length ?? 0) <= next) return;
    }
    setPage(next);
    section.current?.scrollIntoView?.({ block: 'start' });
  };
  const globalSearchParams = new URLSearchParams(params);
  globalSearchParams.delete('page');
  globalSearchParams.delete('perPage');
  const submit = (event: FormEvent) => { event.preventDefault(); update('q', draft.trim().replace(/\s+/gu, ' ')); };
  const clear = () => { setDraft(''); setParams((current) => {
    const next = new URLSearchParams(current);
    for (const key of ['q', 'profileKind', 'genreId', 'cityId', 'page']) next.delete(key);
    return next;
  }, { replace: true }); if (!isTaxonomySelectionEmpty(selection)) taxonomyFilters.clearAll(); };
  return <Box ref={section} component="section" aria-labelledby="community-discovery-heading" sx={{ color: 'text.primary', bgcolor: 'background.paper', p: { xs: 2, sm: 3 }, borderRadius: 2, '& .MuiInputLabel-root': { color: 'text.primary' }, '& .MuiButton-text': { color: 'text.primary' } }}>
    <Typography id="community-discovery-heading" component="h2" variant="h5" mb={2}>Descubre tu comunidad</Typography>
    <Stack component="form" role="search" aria-label="Buscar en Comunidad" onSubmit={submit} spacing={2}>
      <Stack direction={{ xs: 'column', sm: 'row' }} gap={1}>
        <TextField fullWidth type="search" label="Buscar artistas y perfiles" placeholder="Nombre, banda, proyecto, género o ciudad" value={draft} onChange={(event) => setDraft(event.target.value)} inputProps={{ maxLength: 160, enterKeyHint: 'search' }} />
        <Button type="submit" variant="contained">Buscar</Button>
        <Button onClick={clear} disabled={!draft && !profileKind && isTaxonomySelectionEmpty(selection)}>Limpiar búsqueda</Button>
      </Stack>
      <Stack direction={{ xs: 'column', sm: 'row' }} gap={1}>
        <TextField select fullWidth label="Tipo de perfil" value={profileKind} onChange={(event) => update('profileKind', event.target.value)}>
          <MenuItem value="">Todos los perfiles</MenuItem><MenuItem value="artist">Artistas</MenuItem><MenuItem value="band">Bandas</MenuItem><MenuItem value="project">Proyectos musicales</MenuItem><MenuItem value="person">Personas</MenuItem>
        </TextField>
      </Stack>
      <TaxonomyFilterBar idPrefix="community" filters={taxonomyFilters} options={taxonomyOptions.data} optionsError={taxonomyOptions.isError}
        facets={pages[0]?.facets.taxonomy} />
    </Stack>
    <Box sx={{ height: 4, my: 2 }}>{results.isFetching && <LinearProgress aria-label="Buscando perfiles" />}</Box>
    {taxonomyOptions.isError && <Alert severity="warning" action={<Button onClick={() => { void taxonomyOptions.refetch(); }}>Reintentar</Button>}>No pudimos cargar los filtros. Puedes seguir buscando por nombre.</Alert>}
    {results.isError && <Alert severity="error" action={<Button onClick={retryResults}>Reintentar</Button>}>No pudimos cargar los perfiles. Intenta de nuevo.</Alert>}
    <Typography role="status" aria-live="polite" mb={2}>{results.isFetching ? 'Buscando perfiles…' : `${total} perfiles · Ordenados por relevancia`}</Typography>
    {!results.isPending && !results.isError && !catchingUp && !items.length && <Alert severity="info">No encontramos perfiles con esta búsqueda. Prueba otro nombre o <Button onClick={clear}>explora todos los perfiles</Button>.</Alert>}
    <Box data-scroll-restoration-pending={results.isPending ? 'true' : undefined} aria-busy={results.isFetching} sx={{ display: 'grid', gridTemplateColumns: { xs: '1fr', sm: 'repeat(2, minmax(0, 1fr))', lg: 'repeat(3, minmax(0, 1fr))' }, gap: 2 }}>
      {items.map((item) => <Card key={`${item.type}:${item.id}`} component="article" variant="outlined">
        <CardActionArea component={Link} to={directoryResultPath(item)} sx={{ height: '100%', alignItems: 'start' }}>
          <CardContent>
            <ArtistProfileImage artistName={item.title} imageUrl={resolveDirectoryPreviewImage(item.imageUrl)} alt="" width={72} height={72} sx={{ borderRadius: '50%', mb: 2 }} />
            <Typography component="h3" variant="h6">{item.title}</Typography>
            {item.subtitle && <Typography variant="body2">{item.subtitle}</Typography>}
            <Typography variant="body2" color="text.secondary">{[item.location.city, item.location.countryCode].filter(Boolean).join(' · ')}</Typography>
            <Typography variant="body2">{item.taxonomy?.genreIds.map((id) => genreNames.get(id)).filter(Boolean).join(' · ')}</Typography>
            {item.summary && <MarkdownText disableLinks inline variant="body2" mt={1} sx={{ display: '-webkit-box', WebkitLineClamp: 3, WebkitBoxOrient: 'vertical', overflow: 'hidden' }}>{item.summary}</MarkdownText>}
            <Typography color="primary" mt={2}>Ver perfil</Typography>
          </CardContent>
        </CardActionArea>
      </Card>)}
    </Box>
    {total > PAGE_SIZE_OPTIONS[0]! && <TablePagination component="div" count={total} page={Math.min(page, Math.max(0, pages.length - 1))} rowsPerPage={rowsPerPage} rowsPerPageOptions={PAGE_SIZE_OPTIONS}
      disabled={results.isFetching} onPageChange={(_, next) => { void goToPage(next); }} onRowsPerPageChange={(event) => setRowsPerPage(Number(event.target.value))}
      labelRowsPerPage="Por página" labelDisplayedRows={({ from, to, count }) => `${from}–${to} de ${count} perfiles`}
      getItemAriaLabel={(type) => (type === 'next' ? 'Ir a la página siguiente' : type === 'previous' ? 'Ir a la página anterior' : type === 'first' ? 'Ir a la primera página' : 'Ir a la última página')}
      slotProps={{ actions: { nextButton: { disabled: results.isFetching || (page >= pages.length - 1 && !results.hasNextPage) } } }} sx={{ mt: 1 }} />}
    <Button component={Link} to={`/buscar?${globalSearchParams.toString()}`} sx={{ mt: 2 }}>Abrir búsqueda global</Button>
  </Box>;
}
