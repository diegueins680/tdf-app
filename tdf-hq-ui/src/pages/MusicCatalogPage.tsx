import { PublishedReleaseCard } from '../features/releases/PublishedReleaseCard';
import SearchIcon from '@mui/icons-material/Search';
import {
  Alert,
  Box,
  Button,
  CircularProgress,
  Grid,
  InputAdornment,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { useQuery } from '@tanstack/react-query';
import { useDeferredValue, useMemo } from 'react';
import { useSearchParams } from 'react-router-dom';

import { musicReleases } from '../api/musicReleases';
import { Taxonomy } from '../api/taxonomy';
import { useDocumentTitle } from '../hooks/useDocumentTitle';
import TaxonomyFilterBar from '../components/taxonomyFilters/TaxonomyFilterBar';
import { useProfileCityDefault } from '../components/taxonomyFilters/useProfileCityDefault';
import { useSurfaceTaxonomyFilters, useTaxonomyFilterOptions } from '../components/taxonomyFilters/useTaxonomyFilters';

const PAGE_SIZE = 24;
const MAX_PAGE = Math.floor(Number.MAX_SAFE_INTEGER / PAGE_SIZE);

export default function MusicCatalogPage() {
  useDocumentTitle('Música en TDF');
  const [searchParams, setSearchParams] = useSearchParams();
  const query = searchParams.get('q') ?? '';
  const requestedPage = Number(searchParams.get('page') ?? '1');
  const page = Number.isSafeInteger(requestedPage) && requestedPage > 0 && requestedPage <= MAX_PAGE ? requestedPage : 1;
  // Defer the query and page together, avoiding a request for the previous
  // search at the newly reset page while the user edits the search field.
  // TAX-FILTER-001: releases follow the shared location and genre filters
  // (a release's genres, or its artist's; its artist's location).
  const taxonomyOptions = useTaxonomyFilterOptions();
  const profileCity = useProfileCityDefault(taxonomyOptions.data);
  const taxonomyFilters = useSurfaceTaxonomyFilters({ urlBacked: true });
  const taxonomyKey = JSON.stringify(taxonomyFilters.selection);
  const request = useMemo(() => ({ query: query.trim(), page, taxonomy: taxonomyFilters.selection }),
    [query, page, taxonomyKey]); // eslint-disable-line react-hooks/exhaustive-deps
  const deferredRequest = useDeferredValue(request);
  const changingSearch = request !== deferredRequest;
  const releases = useQuery({
    queryKey: ['music-release-catalog', deferredRequest.query, deferredRequest.page, JSON.stringify(deferredRequest.taxonomy)],
    queryFn: ({ signal }) => musicReleases.listPublic(deferredRequest.query, undefined, {
      limit: PAGE_SIZE + 1, offset: (deferredRequest.page - 1) * PAGE_SIZE, signal, taxonomy: deferredRequest.taxonomy,
    }),
    retry: false,
  });
  const facets = useQuery({
    queryKey: ['music-release-catalog', 'facets', deferredRequest.query, JSON.stringify(deferredRequest.taxonomy)],
    queryFn: ({ signal }) => Taxonomy.musicReleaseFacets(deferredRequest.taxonomy, deferredRequest.query, signal),
    retry: false,
    staleTime: 60_000,
  });
  // One extra authorized row establishes whether another page exists without
  // an additional count endpoint. Offsets advance by displayed rows, not 25.
  const hasNextPage = (releases.data?.length ?? 0) > PAGE_SIZE && page < MAX_PAGE;
  const visibleReleases = [...new Map((releases.data ?? []).slice(0, PAGE_SIZE).map((release) => [release.id, release])).values()];
  const pending = changingSearch || releases.isFetching;
  const changePage = (nextPage: number) => setSearchParams((previous) => {
    const next = new URLSearchParams(previous);
    if (nextPage === 1) next.delete('page'); else next.set('page', String(nextPage));
    return next;
  });

  return <Stack spacing={3} sx={{ maxWidth: 1120, mx: 'auto', px: 2, py: 4 }}>
    <Box><Typography component="h1" variant="h3" fontWeight={900}>Música en TDF</Typography>
      <Typography color="text.secondary">Lanzamientos publicados y disponibles en tu territorio.</Typography></Box>
    <TextField label="Buscar por pista, lanzamiento o artista" value={query} onChange={(event) => {
      const value = event.target.value;
      setSearchParams((previous) => {
        const next = new URLSearchParams(previous);
        if (value) next.set('q', value); else next.delete('q');
        next.delete('page');
        return next;
      }, { replace: true });
    }} inputProps={{ maxLength: 200 }} InputProps={{ startAdornment: <InputAdornment position="start"><SearchIcon /></InputAdornment> }} />
    <TaxonomyFilterBar profileCity={profileCity} idPrefix="music" filters={taxonomyFilters} options={taxonomyOptions.data} optionsError={taxonomyOptions.isError} facets={facets.data} />
    <Box role="region" aria-label="Resultados musicales" aria-busy={pending}>
      {(releases.isLoading || changingSearch) && <Box display="grid" sx={{ placeItems: 'center' }}><CircularProgress aria-label="Buscando lanzamientos" /></Box>}
      {!changingSearch && releases.isError && <Alert severity="warning" action={<Button onClick={() => void releases.refetch()} disabled={pending} aria-label="Reintentar catálogo">Reintentar</Button>}>El catálogo musical no está disponible en este momento.</Alert>}
      {!changingSearch && releases.data?.length === 0 && <Typography color="text.secondary">{page > 1 ? 'No hay lanzamientos en esta página. Vuelve a la anterior.' : 'No encontramos lanzamientos publicados.'}</Typography>}
      {!changingSearch && visibleReleases.length > 0 && <Grid container spacing={2}>
        {visibleReleases.map((release) => <Grid item key={release.id} xs={12} sm={6} md={4} lg={3}><PublishedReleaseCard heading="h2" release={release} /></Grid>)}
      </Grid>}
    </Box>
    <Stack component="nav" aria-label="Páginas del catálogo musical" direction="row" spacing={2} alignItems="center" justifyContent="center" flexWrap="wrap" useFlexGap>
      <Button aria-label="Página anterior" disabled={page === 1 || pending} onClick={() => changePage(page - 1)} sx={{ minHeight: 44 }}>Anterior</Button>
      <Typography role="status" aria-live="polite">Página {page}</Typography>
      <Button aria-label="Página siguiente" disabled={!hasNextPage || pending || releases.isError} onClick={() => changePage(page + 1)} sx={{ minHeight: 44 }}>Siguiente</Button>
    </Stack>
  </Stack>;
}
