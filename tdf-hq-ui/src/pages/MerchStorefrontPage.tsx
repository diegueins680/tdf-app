import { useEffect } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Alert, Avatar, Box, Button, Card, CardActionArea, CardContent, CardMedia, Chip, CircularProgress, Grid, Stack, Typography } from '@mui/material';
import GroupsIcon from '@mui/icons-material/Groups';
import { Link as RouterLink, useParams } from 'react-router-dom';
import { useTranslation } from 'react-i18next';
import { Merch } from '../api/merch';
import { useMetaTags } from '../hooks/useMetaTags';
import { getAnalyticsClient } from '../analytics/posthog';
import { formatMerchMoney, merchLanguage, resolveMerchImageUrl } from '../utils/merch';

export default function MerchStorefrontPage() {
  const { storeSlug = '' } = useParams();
  const { i18n } = useTranslation();
  const language = merchLanguage(i18n.resolvedLanguage);
  const capabilities = useQuery({ queryKey: ['merch-capabilities'], queryFn: Merch.capabilities, retry: false });
  const store = useQuery({
    queryKey: ['merch-storefront', storeSlug],
    queryFn: () => Merch.storefront(storeSlug),
    enabled: Boolean(storeSlug && capabilities.data?.features.storefronts && capabilities.data.features.publicCatalog),
    retry: false,
  });

  useEffect(() => {
    if (store.data) getAnalyticsClient().capture('merch_storefront_viewed', { store_id: store.data.id, source: 'public_storefront' });
  }, [store.data]);

  useMetaTags({
    title: store.data ? `${store.data.displayName} · Merch` : 'Tienda de artista',
    description: store.data?.description?.slice(0, 160),
    ogImage: resolveMerchImageUrl(store.data?.coverImageUrl),
    ogType: 'website',
  });

  if (capabilities.isLoading || store.isLoading) return <Box py={8} textAlign="center"><CircularProgress aria-label="Cargando tienda" /></Box>;
  if (!capabilities.data?.features.storefronts || !capabilities.data.features.publicCatalog) return <Box py={4}><Alert severity="info">{language === 'en' ? 'This store is not publicly available yet.' : 'Esta tienda todavía no está disponible públicamente.'}</Alert></Box>;
  if (store.isError || !store.data) return <Box py={4}><Alert severity="error">{language === 'en' ? 'We could not find this store.' : 'No pudimos encontrar esta tienda.'}</Alert></Box>;

  const data = store.data;
  const profile = data['profile'] as { url?: string; name?: string } | undefined;
  const policies = data.policies as { shipping?: unknown; returns?: unknown } | undefined;
  const shippingPolicy = typeof policies?.shipping === 'string' ? policies.shipping : '';
  const returnPolicy = typeof policies?.returns === 'string' ? policies.returns : '';
  return (
    <Box component="main" py={{ xs: 2, md: 5 }}>
      <Stack spacing={4}>
        <Box sx={{ borderRadius: 4, overflow: 'hidden', bgcolor: 'background.paper' }}>
          {data.coverImageUrl && <Box component="img" src={resolveMerchImageUrl(data.coverImageUrl)} alt="" sx={{ width: '100%', height: { xs: 180, md: 300 }, objectFit: 'cover' }} />}
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} alignItems={{ xs: 'flex-start', sm: 'center' }} p={{ xs: 2, md: 3 }}>
            <Avatar src={resolveMerchImageUrl(data.logoImageUrl)} alt="" sx={{ width: 72, height: 72 }}>{data.displayName.slice(0, 1)}</Avatar>
            <Box flex={1}>
              <Typography component="h1" variant="h3" fontWeight={900}>{data.displayName}</Typography>
              {data.description && <Typography color="text.secondary">{data.description}</Typography>}
            </Box>
            {profile?.url && <Button component={RouterLink} to={profile.url} startIcon={<GroupsIcon />}>{language === 'en' ? 'Artist profile & community' : 'Perfil y comunidad'}</Button>}
          </Stack>
        </Box>

        <Box>
          <Typography component="h2" variant="h5" fontWeight={800} mb={2}>{language === 'en' ? 'Products' : 'Productos'}</Typography>
          {data.products?.length === 0 && <Alert severity="info">{language === 'en' ? 'This artist has not published products yet.' : 'Este artista todavía no ha publicado productos.'}</Alert>}
          <Grid container spacing={2}>
            {data.products?.map((product) => (
              <Grid item xs={12} sm={6} md={4} key={product.id}>
                <Card variant="outlined" sx={{ height: '100%', borderRadius: 3 }}>
                  <CardActionArea component={RouterLink} to={`/tienda/${data.slug}/producto/${product.slug}`} sx={{ height: '100%' }}>
                    {product.imageUrl && <CardMedia component="img" height="220" image={resolveMerchImageUrl(product.imageUrl)} alt="" loading="lazy" />}
                    <CardContent>
                      <Stack spacing={1}>
                        <Stack direction="row" justifyContent="space-between" spacing={1}>
                          <Typography component="h3" variant="h6" fontWeight={800}>{product.name}</Typography>
                          {!product.available && <Chip size="small" color="default" label={language === 'en' ? 'Sold out' : 'Agotado'} />}
                        </Stack>
                        {typeof product.priceFromMinor === 'number' && <Typography>{formatMerchMoney(product.priceFromMinor, product.currency ?? 'USD', language === 'en' ? 'en-US' : 'es-EC')}</Typography>}
                        {product.availabilityMode === 'preorder' && <Chip size="small" color="secondary" label={language === 'en' ? 'Preorder' : 'Preventa'} sx={{ alignSelf: 'flex-start' }} />}
                      </Stack>
                    </CardContent>
                  </CardActionArea>
                </Card>
              </Grid>
            ))}
          </Grid>
        </Box>

        {policies && <Box component="section" aria-labelledby="store-policies"><Typography id="store-policies" component="h2" variant="h6" fontWeight={800}>{language === 'en' ? 'Shipping and returns' : 'Envíos y devoluciones'}</Typography><Typography sx={{ whiteSpace: 'pre-wrap' }}>{shippingPolicy}</Typography><Typography sx={{ whiteSpace: 'pre-wrap' }}>{returnPolicy}</Typography></Box>}
      </Stack>
    </Box>
  );
}
