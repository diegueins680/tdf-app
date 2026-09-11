import { useEffect, useMemo, useState } from 'react';
import { useMutation, useQuery } from '@tanstack/react-query';
import { Alert, Box, Button, Card, CardActionArea, CardContent, CardMedia, Chip, CircularProgress, FormControl, InputLabel, MenuItem, Select, Snackbar, Stack, Typography } from '@mui/material';
import FavoriteBorderIcon from '@mui/icons-material/FavoriteBorder';
import ShoppingCartIcon from '@mui/icons-material/ShoppingCart';
import { Link as RouterLink, useNavigate, useParams } from 'react-router-dom';
import { useTranslation } from 'react-i18next';
import { Merch, readStoredMerchCart, storeMerchCart, type MerchProduct } from '../api/merch';
import { MerchReputation } from '../api/merchReputation';
import { MerchReputationSummary } from '../components/merch/MerchReputationSummary';
import { useMetaTags } from '../hooks/useMetaTags';
import { getAnalyticsClient } from '../analytics/posthog';
import { formatMerchMoney, merchLanguage, resolveMerchImageUrl } from '../utils/merch';
import { useSession } from '../session/SessionContext';

type Variant = NonNullable<MerchProduct['variants']>[number];

export default function MerchProductPage() {
  const { storeSlug = '', productSlug = '' } = useParams();
  const navigate = useNavigate();
  const { i18n } = useTranslation();
  const language = merchLanguage(i18n.resolvedLanguage);
  const { session } = useSession();
  const [variantId, setVariantId] = useState('');
  const [quantity, setQuantity] = useState(1);
  const [notice, setNotice] = useState('');
  const capabilities = useQuery({ queryKey: ['merch-capabilities'], queryFn: Merch.capabilities, retry: false });
  const productQuery = useQuery({
    queryKey: ['merch-product', storeSlug, productSlug],
    queryFn: () => Merch.product(storeSlug, productSlug),
    enabled: Boolean(storeSlug && productSlug && capabilities.data?.features.publicCatalog),
    retry: false,
  });
  const productReputation = useQuery({
    queryKey: ['merch-product-reputation', productQuery.data?.id],
    queryFn: () => MerchReputation.product(productQuery.data!.id),
    enabled: Boolean(productQuery.data?.id),
    retry: false,
  });
  const storeReputation = useQuery({
    queryKey: ['merch-store-reputation', productQuery.data?.storeId],
    queryFn: () => MerchReputation.store(productQuery.data!.storeId),
    enabled: Boolean(productQuery.data?.storeId),
    retry: false,
  });
  const variants = useMemo(() => productQuery.data?.variants ?? [], [productQuery.data?.variants]);
  const selectedVariant = useMemo(() => variants.find((variant) => variant.id === variantId) ?? variants.find((variant) => variant.available !== false), [variants, variantId]);

  useEffect(() => {
    if (productQuery.data) {
      setVariantId((current) => current.length > 0 ? current : (productQuery.data?.variants?.find((variant) => variant.available !== false)?.id ?? ''));
      getAnalyticsClient().capture('merch_product_viewed', { store_id: productQuery.data.storeId, product_id: productQuery.data.id });
    }
  }, [productQuery.data]);

  useMetaTags({
    title: productQuery.data?.name ?? (language === 'en' ? 'Artist product' : 'Producto del artista'),
    description: productQuery.data?.description?.slice(0, 160),
    ogImage: resolveMerchImageUrl(productQuery.data?.images?.[0]?.url ?? productQuery.data?.imageUrl),
    ogType: 'product',
  });

  const addMutation = useMutation({
    mutationFn: async () => {
      if (!selectedVariant) throw new Error(language === 'en' ? 'Select an available variant.' : 'Selecciona una variante disponible.');
      let stored = readStoredMerchCart(storeSlug);
      if (!stored) {
        const created = await Merch.createCart(storeSlug);
        storeMerchCart(storeSlug, created);
        stored = readStoredMerchCart(storeSlug);
      }
      if (!stored) throw new Error(language === 'en' ? 'The cart could not be secured.' : 'No se pudo proteger el carrito.');
      const cart = await Merch.putCartItem(stored.id, stored.token, { variantId: selectedVariant.id, quantity });
      getAnalyticsClient().capture('merch_cart_item_added', { store_id: productQuery.data?.storeId, product_id: productQuery.data?.id, quantity });
      return cart;
    },
    onSuccess: () => navigate(`/tienda/${storeSlug}/carrito`),
  });

  const favoriteMutation = useMutation({
    mutationFn: () => Merch.favorite(productQuery.data!.id),
    onSuccess: () => setNotice(language === 'en' ? 'Saved to favorites.' : 'Guardado en favoritos.'),
  });

  if (capabilities.isLoading || productQuery.isLoading) return <Box py={8} textAlign="center"><CircularProgress aria-label="Cargando producto" /></Box>;
  if (!capabilities.data?.features.publicCatalog) return <Box py={4}><Alert severity="info">{language === 'en' ? 'This product is not publicly available yet.' : 'Este producto todavía no está disponible públicamente.'}</Alert></Box>;
  if (productQuery.isError || !productQuery.data) return <Box py={4}><Alert severity="error">{language === 'en' ? 'We could not load this product.' : 'No pudimos cargar este producto.'}</Alert></Box>;
  const product = productQuery.data;
  const maxQuantity = Math.min(product.buyerLimit ?? 100, selectedVariant?.availableQuantity ?? 100);

  return (
    <Box component="main" py={{ xs: 2, md: 5 }}>
      <Stack direction={{ xs: 'column', md: 'row' }} spacing={4} alignItems="flex-start">
        <Box sx={{ width: { xs: '100%', md: '55%' } }}>
          {product.images?.[0] ? <Box component="img" src={resolveMerchImageUrl(product.images[0].url)} alt={product.images[0].altText} sx={{ display: 'block', width: '100%', maxHeight: 620, objectFit: 'contain', borderRadius: 3, bgcolor: 'background.paper' }} /> : <Alert severity="info">{language === 'en' ? 'Image pending review.' : 'Imagen pendiente de revisión.'}</Alert>}
        </Box>
        <Stack spacing={2.5} sx={{ width: { xs: '100%', md: '45%' } }}>
          <Button component={RouterLink} to={`/tienda/${storeSlug}`} sx={{ alignSelf: 'flex-start' }}>← {product.storeName ?? storeSlug}</Button>
          <Typography component="h1" variant="h3" fontWeight={900}>{product.name}</Typography>
          <Stack direction="row" spacing={1} flexWrap="wrap">
            <Chip label={product.category.replace(/_/g, ' ')} />
            {product.availabilityMode === 'preorder' && <Chip color="secondary" label={language === 'en' ? 'Preorder' : 'Preventa'} />}
            {product.availabilityMode === 'made_to_order' && <Chip color="info" label={language === 'en' ? 'Made to order' : 'Hecho bajo pedido'} />}
          </Stack>
          <Typography variant="h5">{selectedVariant ? formatMerchMoney(selectedVariant.priceMinor, selectedVariant.currency, language === 'en' ? 'en-US' : 'es-EC') : '—'}</Typography>
          <Typography sx={{ whiteSpace: 'pre-wrap' }}>{product.description}</Typography>
          {productReputation.data && <Card variant="outlined"><CardContent><MerchReputationSummary summary={productReputation.data} compact /><Button component={RouterLink} to={`/merch/productos/${product.id}`} size="small">{language === 'en' ? 'Product reviews' : 'Valoraciones del producto'}</Button></CardContent></Card>}
          {storeReputation.data && <Card variant="outlined"><CardContent><MerchReputationSummary summary={storeReputation.data} compact /><Button component={RouterLink} to={`/merch/tiendas/${product.storeId}`} size="small">{language === 'en' ? 'Store reputation' : 'Reputación de la tienda'}</Button></CardContent></Card>}
          <FormControl fullWidth>
            <InputLabel id="merch-variant-label">{language === 'en' ? 'Variant' : 'Variante'}</InputLabel>
            <Select labelId="merch-variant-label" label={language === 'en' ? 'Variant' : 'Variante'} value={selectedVariant?.id ?? ''} onChange={(event) => setVariantId(event.target.value)}>
              {variants.map((variant: Variant) => <MenuItem key={variant.id} value={variant.id} disabled={variant.available === false}>{variant.name}{variant.available === false ? ` — ${language === 'en' ? 'sold out' : 'agotada'}` : ''}</MenuItem>)}
            </Select>
          </FormControl>
          <FormControl fullWidth>
            <InputLabel id="merch-quantity-label">{language === 'en' ? 'Quantity' : 'Cantidad'}</InputLabel>
            <Select labelId="merch-quantity-label" label={language === 'en' ? 'Quantity' : 'Cantidad'} value={quantity} onChange={(event) => setQuantity(Number(event.target.value))}>
              {Array.from({ length: Math.max(1, maxQuantity) }, (_, index) => index + 1).slice(0, 10).map((value) => <MenuItem key={value} value={value}>{value}</MenuItem>)}
            </Select>
          </FormControl>
          {!capabilities.data.features.checkout && <Alert severity="info">{language === 'en' ? 'Purchases are still disabled during the pilot. You can browse the catalog.' : 'Las compras siguen deshabilitadas durante el piloto. Puedes explorar el catálogo.'}</Alert>}
          {addMutation.isError && <Alert severity="error">{addMutation.error instanceof Error ? addMutation.error.message : language === 'en' ? 'The cart changed. Try again.' : 'El carrito cambió. Inténtalo otra vez.'}</Alert>}
          <Button variant="contained" size="large" startIcon={<ShoppingCartIcon />} disabled={!capabilities.data.features.checkout || !selectedVariant || selectedVariant.available === false || addMutation.isPending} onClick={() => addMutation.mutate()}>{addMutation.isPending ? (language === 'en' ? 'Adding…' : 'Agregando…') : (language === 'en' ? 'Add to cart' : 'Agregar al carrito')}</Button>
          <Button variant="outlined" startIcon={<FavoriteBorderIcon />} disabled={!session || favoriteMutation.isPending} onClick={() => favoriteMutation.mutate()}>{session ? (language === 'en' ? 'Save' : 'Guardar') : (language === 'en' ? 'Sign in to save' : 'Inicia sesión para guardar')}</Button>
          {product.buyerLimit && <Typography variant="caption" color="text.secondary">{language === 'en' ? `Limit: ${product.buyerLimit} per buyer.` : `Límite: ${product.buyerLimit} por comprador.`}</Typography>}
        </Stack>
      </Stack>
      {product.related && product.related.length > 0 && <Box mt={6}><Typography component="h2" variant="h5" fontWeight={800} mb={2}>{language === 'en' ? 'You may also like' : 'También podría gustarte'}</Typography><Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>{product.related.map((raw) => { const related = raw as { id?: string; slug?: string; name?: string; imageUrl?: string | null; priceFromMinor?: number; currency?: string }; return <Card key={related.id} variant="outlined" sx={{ width: { xs: '100%', sm: 260 } }}><CardActionArea component={RouterLink} to={`/tienda/${storeSlug}/producto/${related.slug}`}><CardMedia component="img" height="150" image={resolveMerchImageUrl(related.imageUrl)} alt="" /><CardContent><Typography fontWeight={700}>{related.name}</Typography>{typeof related.priceFromMinor === 'number' && <Typography>{formatMerchMoney(related.priceFromMinor, related.currency)}</Typography>}</CardContent></CardActionArea></Card>; })}</Stack></Box>}
      <Snackbar open={Boolean(notice)} autoHideDuration={3000} onClose={() => setNotice('')} message={notice} />
    </Box>
  );
}
