import { useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Alert, Box, Card, CardActionArea, CardContent, CardMedia, Chip, CircularProgress, Grid, MenuItem, Stack, TextField, Typography } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import { useTranslation } from 'react-i18next';
import { Merch } from '../api/merch';
import { useMetaTags } from '../hooks/useMetaTags';
import { formatMerchMoney, merchLanguage, resolveMerchImageUrl } from '../utils/merch';

const categories = ['', 'apparel', 'vinyl', 'cd', 'cassette', 'poster', 'accessory', 'limited_edition', 'bundle'] as const;

export default function MerchDiscoveryPage() {
  const { i18n } = useTranslation();
  const language = merchLanguage(i18n.resolvedLanguage);
  const [query, setQuery] = useState('');
  const [category, setCategory] = useState('');
  const capabilities = useQuery({ queryKey: ['merch-capabilities'], queryFn: Merch.capabilities, retry: false });
  const storefronts = useQuery({
    queryKey: ['merch-storefronts', query, category],
    queryFn: () => Merch.storefronts({ q: query.trim() || undefined, category: category || undefined }),
    enabled: capabilities.data?.features.storefronts === true && capabilities.data.features.publicCatalog,
    retry: false,
  });

  useMetaTags({
    title: language === 'en' ? 'Artist merch' : 'Merch de artistas',
    description: language === 'en' ? 'Official artist merchandise on TDF.' : 'Merch oficial de artistas en TDF.',
  });

  if (capabilities.isLoading) return <Box py={8} textAlign="center"><CircularProgress aria-label="Cargando disponibilidad" /></Box>;
  if (capabilities.isError) return <Box py={4}><Alert severity="error">{language === 'en' ? 'We could not verify store availability.' : 'No pudimos verificar la disponibilidad de las tiendas.'}</Alert></Box>;
  if (!capabilities.data?.features.storefronts || !capabilities.data.features.publicCatalog) {
    return <Box py={4}><Alert severity="info">{language === 'en' ? 'Artist stores are still in a closed pilot.' : 'Las tiendas de artistas siguen en piloto cerrado.'}</Alert></Box>;
  }

  return (
    <Box component="main" py={{ xs: 3, md: 6 }}>
      <Stack spacing={3}>
        <Box>
          <Typography component="h1" variant="h3" fontWeight={900}>{language === 'en' ? 'Artist merch' : 'Merch de artistas'}</Typography>
          <Typography color="text.secondary">{language === 'en' ? 'Physical products sold directly by each participating artist.' : 'Productos físicos vendidos por cada artista participante.'}</Typography>
        </Box>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} component="form" role="search" onSubmit={(event) => event.preventDefault()}>
          <TextField fullWidth label={language === 'en' ? 'Search stores or products' : 'Buscar tiendas o productos'} value={query} onChange={(event) => setQuery(event.target.value)} />
          <TextField select label={language === 'en' ? 'Category' : 'Categoría'} value={category} onChange={(event) => setCategory(event.target.value)} sx={{ minWidth: 200 }}>
            {categories.map((value) => <MenuItem key={value || 'all'} value={value}>{value ? value.replace(/_/g, ' ') : language === 'en' ? 'All' : 'Todas'}</MenuItem>)}
          </TextField>
        </Stack>
        {storefronts.isLoading && <Box textAlign="center"><CircularProgress /></Box>}
        {storefronts.isError && <Alert severity="error">{language === 'en' ? 'The catalog could not be loaded. Try again.' : 'No se pudo cargar el catálogo. Inténtalo otra vez.'}</Alert>}
        {storefronts.data?.length === 0 && <Alert severity="info">{language === 'en' ? 'No stores match these filters.' : 'No hay tiendas que coincidan con estos filtros.'}</Alert>}
        <Grid container spacing={2}>
          {storefronts.data?.map((store) => {
            const firstProduct = store.products?.[0];
            const price = firstProduct?.priceFromMinor;
            return (
              <Grid item xs={12} sm={6} lg={4} key={store.id}>
                <Card variant="outlined" sx={{ height: '100%', borderRadius: 3 }}>
                  <CardActionArea component={RouterLink} to={`/tienda/${store.slug}`} sx={{ height: '100%' }}>
                    {(store.coverImageUrl ?? firstProduct?.imageUrl) && <CardMedia component="img" height="180" image={resolveMerchImageUrl(store.coverImageUrl ?? firstProduct?.imageUrl)} alt="" loading="lazy" />}
                    <CardContent>
                      <Stack spacing={1}>
                        <Typography component="h2" variant="h6" fontWeight={800}>{store.displayName}</Typography>
                        {store.description && <Typography color="text.secondary" sx={{ display: '-webkit-box', WebkitLineClamp: 2, WebkitBoxOrient: 'vertical', overflow: 'hidden' }}>{store.description}</Typography>}
                        <Stack direction="row" spacing={1} alignItems="center">
                          <Chip size="small" label={`${store.products?.length ?? 0} ${language === 'en' ? 'products' : 'productos'}`} />
                          {typeof price === 'number' && <Typography variant="body2">{language === 'en' ? 'From ' : 'Desde '}{formatMerchMoney(price, firstProduct?.currency ?? 'USD', language === 'en' ? 'en-US' : 'es-EC')}</Typography>}
                        </Stack>
                      </Stack>
                    </CardContent>
                  </CardActionArea>
                </Card>
              </Grid>
            );
          })}
        </Grid>
      </Stack>
    </Box>
  );
}
