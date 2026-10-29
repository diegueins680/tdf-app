import MarkdownText from '../../components/MarkdownText';
import { useEffect, useRef, useState, type FormEvent } from 'react';
import { Alert, Avatar, Box, Button, Card, CardActionArea, CardContent, LinearProgress, MenuItem, Stack, TextField, Typography } from '@mui/material';
import { useQuery } from '@tanstack/react-query';
import { Link, useSearchParams } from 'react-router-dom';
import { resolveDirectoryPreviewImage } from '../../utils/directoryPreviewImage';
import { Directory, type DirectorySearchQuery } from '../../api/directory';
import { directoryResultPath, useDebouncedSearch, useDirectorySearch } from './useDirectorySearch';

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
  const taxonomies = useQuery({ queryKey: ['directory', 'taxonomies', 'es'], queryFn: () => Directory.taxonomies('es'), staleTime: 30 * 60_000 });
  const results = useDirectorySearch({ q: query || undefined, entityType: 'profile',
    profileKind: (profileKind || undefined) as DirectorySearchQuery['profileKind'],
    genreId: genreId || undefined, cityId: cityId || undefined, limit: 20 });
  const items = results.data?.pages.flatMap((page) => page.items) ?? [];
  const submit = (event: FormEvent) => { event.preventDefault(); update('q', draft.trim().replace(/\s+/gu, ' ')); };
  const clear = () => { setDraft(''); setParams((current) => {
    const next = new URLSearchParams(current);
    for (const key of ['q', 'profileKind', 'genreId', 'cityId']) next.delete(key);
    return next;
  }, { replace: true }); };
  return <Box component="section" aria-labelledby="community-discovery-heading" sx={{ color: 'text.primary', bgcolor: 'background.paper', p: { xs: 2, sm: 3 }, borderRadius: 2, '& .MuiInputLabel-root': { color: 'text.primary' }, '& .MuiButton-text': { color: 'text.primary' } }}>
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
    <Typography role="status" aria-live="polite" mb={2}>{results.isFetching ? 'Buscando perfiles…' : `${results.data?.pages[0]?.facets.total ?? 0} perfiles · Ordenados por relevancia`}</Typography>
    {!results.isPending && !results.isError && !items.length && <Alert severity="info">No encontramos perfiles con esta búsqueda. Prueba otro nombre o <Button onClick={clear}>explora todos los perfiles</Button>.</Alert>}
    <Box data-scroll-restoration-pending={results.isPending ? 'true' : undefined} aria-busy={results.isFetching} sx={{ display: 'grid', gridTemplateColumns: { xs: '1fr', sm: 'repeat(2, minmax(0, 1fr))', lg: 'repeat(3, minmax(0, 1fr))' }, gap: 2 }}>
      {items.map((item) => <Card key={`${item.type}:${item.id}`} component="article" variant="outlined">
        <CardActionArea component={Link} to={directoryResultPath(item)} sx={{ height: '100%', alignItems: 'start' }}>
          <CardContent>
            <Avatar src={resolveDirectoryPreviewImage(item.imageUrl) ?? '/artist-fallback.svg'} alt="" sx={{ width: 72, height: 72, mb: 2 }} />
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
    {results.hasNextPage && <Button onClick={() => { void results.fetchNextPage(); }} disabled={results.isFetching} sx={{ mt: 2 }}>Cargar más perfiles</Button>}
    <Button component={Link} to={`/buscar?${params.toString()}`} sx={{ mt: 2 }}>Abrir búsqueda global</Button>
  </Box>;
}
