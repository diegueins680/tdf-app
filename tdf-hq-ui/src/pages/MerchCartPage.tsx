import { useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Alert, Box, Button, Checkbox, CircularProgress, Divider, FormControl, FormControlLabel, InputLabel, MenuItem, Paper, Select, Stack, TextField, Typography } from '@mui/material';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import LockOutlinedIcon from '@mui/icons-material/LockOutlined';
import { Link as RouterLink, useNavigate, useParams } from 'react-router-dom';
import { useTranslation } from 'react-i18next';
import { Merch, createMerchIdempotencyKey, readStoredMerchCart, storeMerchOrder, type MerchCheckoutRequest } from '../api/merch';
import { getAnalyticsClient } from '../analytics/posthog';
import { formatMerchMoney, merchLanguage } from '../utils/merch';

interface CartLine {
  variantId: string;
  productName: string;
  variantName: string;
  sku: string;
  quantity: number;
  unitPriceMinor: number;
  subtotalMinor: number;
  available: boolean;
}

const checkoutKey = (cartId: string) => {
  const key = `tdf-merch-checkout-idempotency:${cartId}`;
  const existing = window.sessionStorage.getItem(key);
  if (existing) return existing;
  const created = createMerchIdempotencyKey('checkout');
  window.sessionStorage.setItem(key, created);
  return created;
};

export default function MerchCartPage() {
  const { storeSlug = '' } = useParams();
  const navigate = useNavigate();
  const queryClient = useQueryClient();
  const { i18n } = useTranslation();
  const language = merchLanguage(i18n.resolvedLanguage);
  const stored = useMemo(() => readStoredMerchCart(storeSlug), [storeSlug]);
  const [zoneId, setZoneId] = useState('');
  const [form, setForm] = useState({ name: '', email: '', phone: '', subdivision: '', city: '', addressLine1: '', addressLine2: '', postalCode: '', deliveryNote: '' });
  const [accountAfter, setAccountAfter] = useState(false);
  const capabilities = useQuery({ queryKey: ['merch-capabilities'], queryFn: Merch.capabilities, retry: false });
  const storefront = useQuery({ queryKey: ['merch-storefront', storeSlug], queryFn: () => Merch.storefront(storeSlug), enabled: Boolean(storeSlug && capabilities.data?.features.publicCatalog), retry: false });
  const cart = useQuery({ queryKey: ['merch-cart', stored?.id], queryFn: () => Merch.cart(stored!.id, stored!.token), enabled: Boolean(stored), retry: false });
  const lines = (cart.data?.items ?? []) as unknown as CartLine[];
  const zones = storefront.data?.shippingZones ?? [];
  const selectedZone = zones.find((zone) => zone.id === zoneId) ?? zones[0];
  const productSubtotal = cart.data?.productSubtotalMinor ?? 0;
  const shipping = selectedZone && selectedZone.freeShippingMinMinor != null && productSubtotal >= selectedZone.freeShippingMinMinor ? 0 : selectedZone?.rateMinor ?? 0;

  const removeMutation = useMutation({
    mutationFn: (variantId: string) => Merch.deleteCartItem(stored!.id, stored!.token, variantId),
    onSuccess: (next) => queryClient.setQueryData(['merch-cart', stored?.id], next),
  });
  const checkoutMutation = useMutation({
    mutationFn: () => {
      if (!stored || !selectedZone) throw new Error(language === 'en' ? 'Select a delivery option.' : 'Selecciona una opción de entrega.');
      const payload: MerchCheckoutRequest = {
        recipient: {
          name: form.name.trim(), email: form.email.trim(), phone: form.phone.trim() || null,
          countryCode: 'EC', subdivision: form.subdivision.trim() || null, city: form.city.trim(),
          addressLine1: form.addressLine1.trim(), addressLine2: form.addressLine2.trim() || null,
          postalCode: form.postalCode.trim() || null, deliveryNote: form.deliveryNote.trim() || null,
        },
        shippingZoneId: selectedZone.id,
        createAccount: false,
        locale: language,
      };
      getAnalyticsClient().capture('merch_checkout_started', { store_id: cart.data?.storeId, item_count: lines.length });
      return Merch.checkout(stored.id, stored.token, checkoutKey(stored.id), payload);
    },
    onSuccess: (order) => {
      storeMerchOrder(order, stored?.token);
      getAnalyticsClient().capture('merch_checkout_order_created', { store_id: cart.data?.storeId, order_id: order.id, payment_status: order.paymentStatus });
      navigate(`/tienda/orden/${order.id}`);
    },
  });

  if (!stored) return <Box py={4}><Alert severity="info">{language === 'en' ? 'This browser has no active cart for this artist.' : 'Este navegador no tiene un carrito activo para este artista.'}</Alert><Button component={RouterLink} to={`/tienda/${storeSlug}`} sx={{ mt: 2 }}>{language === 'en' ? 'Back to store' : 'Volver a la tienda'}</Button></Box>;
  if (capabilities.isLoading || cart.isLoading || storefront.isLoading) return <Box py={8} textAlign="center"><CircularProgress aria-label="Cargando carrito" /></Box>;
  if (cart.isError) return <Box py={4}><Alert severity="error">{language === 'en' ? 'This cart expired or cannot be recovered.' : 'Este carrito venció o no se puede recuperar.'}</Alert></Box>;

  return (
    <Box component="main" py={{ xs: 2, md: 5 }} maxWidth="md" mx="auto">
      <Stack spacing={3}>
        <Box><Typography component="h1" variant="h3" fontWeight={900}>{language === 'en' ? 'Your cart' : 'Tu carrito'}</Typography><Typography color="text.secondary">{storefront.data?.displayName}</Typography></Box>
        {!capabilities.data?.features.checkout && <Alert severity="info">{language === 'en' ? 'Checkout is not enabled for this pilot yet. No payment can be submitted.' : 'El checkout todavía no está habilitado para este piloto. No se puede enviar ningún pago.'}</Alert>}
        <Paper variant="outlined" sx={{ p: { xs: 2, sm: 3 }, borderRadius: 3 }}>
          <Stack spacing={2} divider={<Divider flexItem />}>
            {lines.map((line) => <Stack key={line.variantId} direction="row" spacing={2} alignItems="center"><Box flex={1}><Typography fontWeight={800}>{line.productName}</Typography><Typography variant="body2" color="text.secondary">{line.variantName} · {line.sku} · {language === 'en' ? 'Qty.' : 'Cant.'} {line.quantity}</Typography>{!line.available && <Alert severity="warning" sx={{ mt: 1 }}>{language === 'en' ? 'Stock changed. Remove this item or try again.' : 'El stock cambió. Quita este artículo o inténtalo de nuevo.'}</Alert>}</Box><Typography>{formatMerchMoney(line.subtotalMinor, cart.data?.currency ?? 'USD')}</Typography><Button aria-label={`${language === 'en' ? 'Remove' : 'Quitar'} ${line.productName}`} onClick={() => removeMutation.mutate(line.variantId)}><DeleteOutlineIcon /></Button></Stack>)}
            {lines.length === 0 && <Typography>{language === 'en' ? 'Your cart is empty.' : 'Tu carrito está vacío.'}</Typography>}
          </Stack>
        </Paper>

        {lines.length > 0 && <Paper component="form" variant="outlined" sx={{ p: { xs: 2, sm: 3 }, borderRadius: 3 }} onSubmit={(event) => { event.preventDefault(); checkoutMutation.mutate(); }}>
          <Stack spacing={2}>
            <Typography component="h2" variant="h5" fontWeight={800}>{language === 'en' ? 'Delivery details' : 'Datos de entrega'}</Typography>
            <TextField required autoComplete="name" label={language === 'en' ? 'Full name' : 'Nombre completo'} value={form.name} onChange={(event) => setForm({ ...form, name: event.target.value })} />
            <TextField required type="email" autoComplete="email" label="Email" value={form.email} onChange={(event) => setForm({ ...form, email: event.target.value })} />
            <TextField type="tel" autoComplete="tel" label={language === 'en' ? 'Phone (optional)' : 'Teléfono (opcional)'} value={form.phone} onChange={(event) => setForm({ ...form, phone: event.target.value })} />
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}><TextField required fullWidth autoComplete="address-level2" label={language === 'en' ? 'City' : 'Ciudad'} value={form.city} onChange={(event) => setForm({ ...form, city: event.target.value })} /><TextField fullWidth autoComplete="address-level1" label={language === 'en' ? 'Province' : 'Provincia'} value={form.subdivision} onChange={(event) => setForm({ ...form, subdivision: event.target.value })} /></Stack>
            <TextField required autoComplete="address-line1" label={language === 'en' ? 'Address' : 'Dirección'} value={form.addressLine1} onChange={(event) => setForm({ ...form, addressLine1: event.target.value })} />
            <TextField autoComplete="address-line2" label={language === 'en' ? 'Address details (optional)' : 'Referencia (opcional)'} value={form.addressLine2} onChange={(event) => setForm({ ...form, addressLine2: event.target.value })} />
            <TextField multiline minRows={2} label={language === 'en' ? 'Delivery note (optional)' : 'Nota de entrega (opcional)'} value={form.deliveryNote} onChange={(event) => setForm({ ...form, deliveryNote: event.target.value })} />
            <FormControl fullWidth required><InputLabel id="shipping-zone-label">{language === 'en' ? 'Delivery option' : 'Opción de entrega'}</InputLabel><Select labelId="shipping-zone-label" label={language === 'en' ? 'Delivery option' : 'Opción de entrega'} value={selectedZone?.id ?? ''} onChange={(event) => setZoneId(event.target.value)}>{zones.map((zone) => <MenuItem key={zone.id} value={zone.id}>{zone.name} · {formatMerchMoney(zone.rateMinor, 'USD')}</MenuItem>)}</Select></FormControl>
            {zones.length === 0 && <Alert severity="warning">{language === 'en' ? 'This seller has not configured a delivery option.' : 'Este vendedor no ha configurado una opción de entrega.'}</Alert>}
            <FormControlLabel control={<Checkbox checked={accountAfter} onChange={(event) => setAccountAfter(event.target.checked)} />} label={language === 'en' ? 'Remind me to create an account after ordering' : 'Recordarme crear una cuenta después del pedido'} />
            <Divider />
            <Stack direction="row" justifyContent="space-between"><Typography>{language === 'en' ? 'Products' : 'Productos'}</Typography><Typography>{formatMerchMoney(productSubtotal, 'USD')}</Typography></Stack>
            <Stack direction="row" justifyContent="space-between"><Typography>{language === 'en' ? 'Delivery' : 'Entrega'}</Typography><Typography>{formatMerchMoney(shipping, 'USD')}</Typography></Stack>
            <Stack direction="row" justifyContent="space-between"><Typography fontWeight={900}>{language === 'en' ? 'Total calculated by server' : 'Total calculado por el servidor'}</Typography><Typography fontWeight={900}>{formatMerchMoney(productSubtotal + shipping, 'USD')}</Typography></Stack>
            <Typography variant="caption" color="text.secondary"><LockOutlinedIcon sx={{ fontSize: 14, verticalAlign: 'text-bottom' }} /> {language === 'en' ? 'Final totals and stock are revalidated atomically by the server.' : 'El servidor vuelve a validar los totales y el stock de forma atómica.'}</Typography>
            {checkoutMutation.isError && <Alert severity="error">{checkoutMutation.error instanceof Error ? checkoutMutation.error.message : language === 'en' ? 'Checkout could not continue.' : 'No se pudo continuar el checkout.'}</Alert>}
            <Button type="submit" variant="contained" size="large" disabled={!capabilities.data?.features.checkout || !selectedZone || lines.some((line) => !line.available) || checkoutMutation.isPending}>{checkoutMutation.isPending ? (language === 'en' ? 'Reserving stock…' : 'Reservando stock…') : (language === 'en' ? 'Review and continue to payment' : 'Revisar y continuar al pago')}</Button>
            <Typography variant="caption" color="text.secondary">{language === 'en' ? 'Creating an order does not mark it as paid. Payment is confirmed only by verified provider evidence or independent manual review.' : 'Crear una orden no la marca como pagada. El pago se confirma solo con evidencia verificada del proveedor o revisión manual independiente.'}</Typography>
          </Stack>
        </Paper>}
      </Stack>
    </Box>
  );
}
