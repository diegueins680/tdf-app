import SearchIcon from '@mui/icons-material/Search';
import {
  Alert,
  Box,
  Card,
  CardActionArea,
  CardContent,
  CardMedia,
  CircularProgress,
  Grid,
  InputAdornment,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { useQuery } from '@tanstack/react-query';
import { useDeferredValue, useState } from 'react';
import { Link as RouterLink } from 'react-router-dom';

import { musicReleases, type MusicPublicReleaseSummary } from '../api/musicReleases';
import { useDocumentTitle } from '../hooks/useDocumentTitle';

function CatalogRelease({ release }: { release: MusicPublicReleaseSummary }) {
  const cover = useQuery({
    queryKey: ['music-release-cover', release.coverAssetId],
    queryFn: () => musicReleases.getAssetAccess(release.coverAssetId!),
    enabled: Boolean(release.coverAssetId),
    retry: false,
  });
  return <Card variant="outlined" sx={{ height: '100%', borderRadius: 3 }}>
    <CardActionArea component={RouterLink} to={`/musica/${release.slug}`} sx={{ height: '100%', alignItems: 'stretch' }}>
      {cover.data?.url
        ? <CardMedia component="img" image={cover.data.url} alt={`Portada de ${release.title}`} sx={{ aspectRatio: '1', objectFit: 'cover' }} />
        : <Box sx={{ aspectRatio: '1', bgcolor: 'action.hover' }} aria-hidden="true" />}
      <CardContent><Typography component="h2" variant="h6" fontWeight={800}>{release.title}</Typography>
        <Typography color="text.secondary">{release.displayArtist}</Typography>
        <Typography variant="caption" color="text.secondary">{release.kind.toUpperCase()}</Typography>
      </CardContent>
    </CardActionArea>
  </Card>;
}

export default function MusicCatalogPage() {
  useDocumentTitle('Música en TDF');
  const [query, setQuery] = useState('');
  const deferredQuery = useDeferredValue(query.trim());
  const releases = useQuery({
    queryKey: ['music-release-catalog', deferredQuery],
    queryFn: () => musicReleases.listPublic(deferredQuery),
    retry: false,
  });

  return <Stack spacing={3} sx={{ maxWidth: 1120, mx: 'auto', px: 2, py: 4 }}>
    <Box><Typography component="h1" variant="h3" fontWeight={900}>Música en TDF</Typography>
      <Typography color="text.secondary">Lanzamientos publicados y disponibles en tu territorio.</Typography></Box>
    <TextField label="Buscar por lanzamiento o artista" value={query} onChange={(event) => setQuery(event.target.value)}
      inputProps={{ maxLength: 200 }} InputProps={{ startAdornment: <InputAdornment position="start"><SearchIcon /></InputAdornment> }} />
    {releases.isLoading && <Box display="grid" sx={{ placeItems: 'center' }}><CircularProgress aria-label="Buscando lanzamientos" /></Box>}
    {releases.isError && <Alert severity="warning">El catálogo musical no está disponible en este momento.</Alert>}
    {releases.data?.length === 0 && <Typography color="text.secondary">No encontramos lanzamientos publicados.</Typography>}
    {releases.data && releases.data.length > 0 && <Grid container spacing={2}>
      {releases.data.map((release) => <Grid item key={release.id} xs={12} sm={6} md={4} lg={3}><CatalogRelease release={release} /></Grid>)}
    </Grid>}
  </Stack>;
}
