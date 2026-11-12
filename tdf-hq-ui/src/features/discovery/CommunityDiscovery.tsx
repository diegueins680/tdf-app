import MarkdownText from '../../components/MarkdownText';
import { useEffect, useRef, useState, type FormEvent } from 'react';
import { Alert, Box, Button, Card, CardActionArea, CardContent, LinearProgress, MenuItem, Stack, TablePagination, TextField, Typography } from '@mui/material';
import { useQuery } from '@tanstack/react-query';
import { Link, useSearchParams } from 'react-router-dom';
import { resolveDirectoryPreviewImage } from '../../utils/directoryPreviewImage';
import ArtistProfileImage from '../../components/ArtistProfileImage';
import { Directory, type DirectorySearchQuery } from '../../api/directory';
import { directoryResultPath, useDebouncedSearch, useDirectorySearch } from './useDirectorySearch';

const PAGE_SIZE_OPTIONS = [12, 24, 48];

export default function CommunityDiscovery() {
  const [params, setParams] = useSearchParams();
  const query = params.get('q') ?? '';
  const [draft, setDraft] = useState(query);
  const debounced = useDebouncedSearch(draft);
  const previousQuery = useRef(query);
  useEffect(() => { setDraft(query); }, [query]);
  const update = (key: string, value: string) => setParams((current) => {
    const next = new URLSearchParams(current);
    if (value) next.set(key, value); else next.delete(key);
    return next;
  }, { replace: true });
  useEffect(() => {
    if (previousQuery.current !== query) { previousQuery.current = query; return; }
    if (debounced !== query && debounced === draft.trim().replace(/\s+/gu, ' ')) update('q', debounced);
  }, [debounced, draft, query]); // eslint-disable-line react-hooks/exhaustive-deps
  const profileKind = params.get('profileKind') ?? '';
  const genreId = params.get('genreId') ?? '';
  const cityId = params.get('cityId') ?? '';
  const [page, setPage] = useState(0);
  const [rowsPerPage, setRowsPerPage] = useState(PAGE_SIZE_OPTIONS[0]!);
  const section = useRef<HTMLDivElement>(null);
  const taxonomies = useQuery({ queryKey: ['directory', 'taxonomies', 'es'], queryFn: () => Directory.taxonomies('es'), staleTime: 30 * 60_000 });
  const results = useDirectorySearch({ q: query || undefined, entityType: 'profile',
    profileKind: (profileKind || undefined) as DirectorySearchQuery['profileKind'],
    genreId: genreId || undefined, cityId: cityId || undefined, limit: rowsPerPage });
  // The server pages by cursor, so pages are reached one step at a time and
  // already visited ones are shown again without another request.
  const pages = results.data?.pages ?? [];
  const total = pages[0]?.facets.total ?? 0;
  const items = pages[Math.min(page, pages.length - 1)]?.items ?? [];
  useEffect(() => { setPage(0); }, [query, profileKind, genreId, cityId, rowsPerPage]);
  const goToPage = async (next: number) => {
    if (next >= pages.length) {
      const fetched = await results.fetchNextPage();
      if ((fetched.data?.pages.length ?? 0) <= next) return;
    }
    setPage(next);
    section.current?.scrollIntoView?.({ block: 'start' });
  };
  const submit = (event: FormEvent) => { event.preventDefault(); update('q', draft.trim().replace(/\s+/gu, ' ')); };
  const clear = () => { setDraft(''); setParams((current) => {
    const next = new URLSearchParams(current);
    for (const key of ['q', 'profileKind', 'genreId', 'cityId']) next.delete(key);
    return next;
  }, { replace: true }); };
  return <Box ref={section} component="section" aria-labelledby="community-discovery-heading" sx={{ color: 'text.primary', bgcolor: 'background.paper', p: { xs: 2, sm: 3 }, borderRadius: 2, '& .MuiInputLabel-root': { color: 'text.primary' }, '& .MuiButton-text': { color: 'text.primary' } }}>
    <Typography id="community-discovery-heading" component="h2" variant="h5" mb={2}>Descubre tu comunidad</Typography>
    <Stack component="form" role="search" aria-label="Buscar en Comunidad" onSubmit={submit} spacing={2}>
      <Stack direction={{ xs: 'column', sm: 'row' }} gap={1}>
        <TextField fullWidth type="search" label="Buscar artistas y perfiles" placeholder="Nombre, banda, proyecto, género o ciudad" value={draft} onChange={(event) => setDraft(event.target.value)} inputProps={{ maxLength: 160, enterKeyHint: 'search' }} />
        <Button type="submit" variant="contained">Buscar</Button>
        <Button onClick={clear} disabled={!draft && !profileKind && !genreId && !cityId}>Limpiar búsqueda</Button>
      </Stack>
      <Stack direction={{ xs: 'column', sm: 'row' }} gap={1}>
        <TextField select fullWidth label="Tipo de perfil" value={profileKind} onChange={(event) => update('profileKind', event.target.value)}>
          <MenuItem value="">Todos los perfiles</MenuItem><MenuItem value="artist">Artistas</MenuItem><MenuItem value="band">Bandas</MenuItem><MenuItem value="project">Proyectos musicales</MenuItem><MenuItem value="person">Personas</MenuItem>
        </TextField>
        <TextField select fullWidth label="Género musical" value={genreId} onChange={(event) => update('genreId', event.target.value)}>
          <MenuItem value="">Todos los géneros</MenuItem>{taxonomies.data?.genres.map((genre) => <MenuItem key={genre.id} value={genre.id}>{genre.name}</MenuItem>)}
        </TextField>
        <TextField select fullWidth label="Ciudad" value={cityId} onChange={(event) => update('cityId', event.target.value)}>
          <MenuItem value="">Todas las ciudades</MenuItem>{taxonomies.data?.cities.map((city) => <MenuItem key={city.id} value={city.id}>{city.name}</MenuItem>)}
        </TextField>
      </Stack>
    </Stack>
    <Box sx={{ height: 4, my: 2 }}>{results.isFetching && <LinearProgress aria-label="Buscando perfiles" />}</Box>
    {taxonomies.isError && <Alert severity="warning" action={<Button onClick={() => { void taxonomies.refetch(); }}>Reintentar</Button>}>No pudimos cargar los filtros. Puedes seguir buscando por nombre.</Alert>}
    {results.isError && <Alert severity="error" action={<Button onClick={() => { void results.refetch(); }}>Reintentar</Button>}>No pudimos cargar los perfiles. Intenta de nuevo.</Alert>}
    <Typography role="status" aria-live="polite" mb={2}>{results.isFetching ? 'Buscando perfiles…' : `${total} perfiles · Ordenados por relevancia`}</Typography>
    {!results.isPending && !results.isError && !items.length && <Alert severity="info">No encontramos perfiles con esta búsqueda. Prueba otro nombre o <Button onClick={clear}>explora todos los perfiles</Button>.</Alert>}
    <Box data-scroll-restoration-pending={results.isPending ? 'true' : undefined} aria-busy={results.isFetching} sx={{ display: 'grid', gridTemplateColumns: { xs: '1fr', sm: 'repeat(2, minmax(0, 1fr))', lg: 'repeat(3, minmax(0, 1fr))' }, gap: 2 }}>
      {items.map((item) => <Card key={`${item.type}:${item.id}`} component="article" variant="outlined">
        <CardActionArea component={Link} to={directoryResultPath(item)} sx={{ height: '100%', alignItems: 'start' }}>
          <CardContent>
            <ArtistProfileImage artistName={item.title} imageUrl={resolveDirectoryPreviewImage(item.imageUrl)} alt="" width={72} height={72} sx={{ borderRadius: '50%', mb: 2 }} />
            <Typography component="h3" variant="h6">{item.title}</Typography>
            {item.subtitle && <Typography variant="body2">{item.subtitle}</Typography>}
            <Typography variant="body2" color="text.secondary">{[item.location.city, item.location.countryCode].filter(Boolean).join(' · ')}</Typography>
            <Typography variant="body2">{item.taxonomy?.genreIds.map((id) => taxonomies.data?.genres.find((genre) => genre.id === id)?.name).filter(Boolean).join(' · ')}</Typography>
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
    <Button component={Link} to={`/buscar?${params.toString()}`} sx={{ mt: 2 }}>Abrir búsqueda global</Button>
  </Box>;
}
