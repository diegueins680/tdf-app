import { useMemo, useRef, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Alert, Box, Button, Checkbox, CircularProgress, Divider, FormControl, FormControlLabel, InputLabel, MenuItem, Paper, Select, Stack, TextField, Typography } from '@mui/material';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import LockOutlinedIcon from '@mui/icons-material/LockOutlined';
import { Link as RouterLink, useNavigate, useParams } from 'react-router-dom';
import { useTranslation } from 'react-i18next';
import { Merch, createMerchIdempotencyKey, readStoredMerchCart, storeMerchOrder, type MerchCheckoutRequest } from '../api/merch';
import { MerchReputation } from '../api/merchReputation';
import { MerchReputationSummary } from '../components/merch/MerchReputationSummary';
import { getAnalyticsClient } from '../analytics/posthog';
import { formatMerchMoney, merchLanguage } from '../utils/merch';
import { useSession } from '../session/SessionContext';
import { sessionContactDefaults } from '../session/sessionContactDefaults';

// Mirrors the existing recipient text bounds; the API remains authoritative.
function recipientTextError(raw: string, limit: number, required: boolean, en: boolean): string | null {
  const text = raw.trim();
  if (required && !text) return en ? 'Complete this field.' : 'Completa este campo.';
  if ([...text].length > limit) return en ? `Use at most ${limit} characters.` : `Usa hasta ${limit} caracteres.`;
  if (/\p{Cc}/u.test(text)) return en ? 'Remove line breaks and unsupported characters.' : 'Quita los saltos de línea y los caracteres no admitidos.';
  return null;
}

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
  const { session } = useSession();
  const [form, setForm] = useState(() => ({ ...sessionContactDefaults(session), phone: '', subdivision: '', city: '', addressLine1: '', addressLine2: '', postalCode: '', deliveryNote: '' }));
  type Field = keyof typeof form;
  const fields = useRef<Partial<Record<Field, HTMLInputElement>>>({});
  const [touched, setTouched] = useState<Partial<Record<Field, boolean>>>({});
  const [submitted, setSubmitted] = useState(false);
  const en = language === 'en';
  const errors = {
    name: recipientTextError(form.name, 200, true, en),
    email: recipientTextError(form.email, 320, true, en) ?? (/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(form.email.trim()) ? null : en ? 'Enter a valid email.' : 'Ingresa un correo válido.'),
    phone: recipientTextError(form.phone, 500, false, en),
    city: recipientTextError(form.city, 120, true, en),
    subdivision: recipientTextError(form.subdivision, 500, false, en),
    addressLine1: recipientTextError(form.addressLine1, 300, true, en),
    addressLine2: recipientTextError(form.addressLine2, 500, false, en),
    deliveryNote: recipientTextError(form.deliveryNote, 500, false, en),
    postalCode: recipientTextError(form.postalCode, 500, false, en),
  };
  const fieldProps = (field: Field) => ({
    value: form[field],
    disabled: checkoutMutation.isPending,
    onChange: (event: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement>) => {
      if (checkoutMutation.isPending) return;
      const value = event.target.value;
      setForm(current => ({ ...current, [field]: value }));
    },
    onBlur: () => setTouched(current => ({ ...current, [field]: true })),
    inputRef: (node: HTMLInputElement | null) => { if (node) fields.current[field] = node; },
    error: Boolean((submitted || touched[field]) && errors[field]),
    helperText: (submitted || touched[field]) ? errors[field] : null,
  });
  const [accountAfter, setAccountAfter] = useState(false);
  const capabilities = useQuery({ queryKey: ['merch-capabilities'], queryFn: Merch.capabilities, retry: false });
  const storefront = useQuery({ queryKey: ['merch-storefront', storeSlug], queryFn: () => Merch.storefront(storeSlug), enabled: Boolean(storeSlug && capabilities.data?.features.publicCatalog), retry: false });
  const storeReputation = useQuery({ queryKey: ['merch-store-reputation', storefront.data?.id], queryFn: () => MerchReputation.store(storefront.data!.id), enabled: Boolean(storefront.data?.id), retry: false });
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
    mutationFn: (payload: MerchCheckoutRequest) => {
      if (!stored) throw new Error(en ? 'This cart is no longer available.' : 'Este carrito ya no está disponible.');
      getAnalyticsClient().capture('merch_checkout_started', { store_id: cart.data?.storeId, item_count: lines.length });
      return Merch.checkout(stored.id, stored.token, checkoutKey(stored.id), payload);
    },
    onSuccess: (order) => {
      storeMerchOrder(order, stored?.token);
      getAnalyticsClient().capture('merch_checkout_order_created', { store_id: cart.data?.storeId, order_id: order.id, payment_status: order.paymentStatus });
      navigate(`/tienda/orden/${order.id}`);
    },
  });

  const canCheckout = Boolean(capabilities.data?.features.checkout && !capabilities.isError && !capabilities.isFetching
    && cart.data && !cart.isError && !cart.isFetching && storefront.data && !storefront.isError && !storefront.isFetching
    && selectedZone && lines.length > 0 && lines.every(line => line.available)
    && !removeMutation.isPending && !checkoutMutation.isPending);
  const handleSubmit = (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    if (checkoutMutation.isPending) return;
    setSubmitted(true);
    const firstInvalid = (Object.keys(errors) as Field[]).find(field => errors[field]);
    if (firstInvalid) {
      const input = fields.current[firstInvalid];
      input?.focus();
      input?.scrollIntoView?.({ block: 'center' });
      return;
    }
    if (!canCheckout || !stored || !selectedZone) return;
    checkoutMutation.mutate({
      recipient: {
        name: form.name.trim(), email: form.email.trim(), phone: form.phone.trim() || null,
        countryCode: 'EC', subdivision: form.subdivision.trim() || null, city: form.city.trim(),
        addressLine1: form.addressLine1.trim(), addressLine2: form.addressLine2.trim() || null,
        postalCode: form.postalCode.trim() || null, deliveryNote: form.deliveryNote.trim() || null,
      },
      shippingZoneId: selectedZone.id, createAccount: false, locale: language,
    });
  };

  if (!stored) return <Box py={4}><Alert severity="info">{language === 'en' ? 'This browser has no active cart for this artist.' : 'Este navegador no tiene un carrito activo para este artista.'}</Alert><Button component={RouterLink} to={`/tienda/${storeSlug}`} sx={{ mt: 2 }}>{language === 'en' ? 'Back to store' : 'Volver a la tienda'}</Button></Box>;
  if (capabilities.isLoading || cart.isLoading || storefront.isLoading) return <Box py={8} textAlign="center"><CircularProgress aria-label="Cargando carrito" /></Box>;
  if (cart.isError) return <Box py={4}><Alert severity="error">{language === 'en' ? 'This cart expired or cannot be recovered.' : 'Este carrito venció o no se puede recuperar.'}</Alert></Box>;

  return (
    <Box component="main" py={{ xs: 2, md: 5 }} maxWidth="md" mx="auto">
      <Stack spacing={3}>
        <Box><Typography component="h1" variant="h3" fontWeight={900}>{language === 'en' ? 'Your cart' : 'Tu carrito'}</Typography><Typography color="text.secondary">{storefront.data?.displayName}</Typography></Box>
        {storeReputation.data && <Paper component="section" variant="outlined" sx={{ p: 2, borderRadius: 3 }}><MerchReputationSummary summary={storeReputation.data} compact /></Paper>}
        {!capabilities.data?.features.checkout && <Alert severity="info">{language === 'en' ? 'Checkout is not enabled for this pilot yet. No payment can be submitted.' : 'El checkout todavía no está habilitado para este piloto. No se puede enviar ningún pago.'}</Alert>}
        <Paper variant="outlined" sx={{ p: { xs: 2, sm: 3 }, borderRadius: 3 }}>
          <Stack spacing={2} divider={<Divider flexItem />}>
            {lines.map((line) => <Stack key={line.variantId} direction="row" spacing={2} alignItems="center"><Box flex={1}><Typography fontWeight={800}>{line.productName}</Typography><Typography variant="body2" color="text.secondary">{line.variantName} · {line.sku} · {language === 'en' ? 'Qty.' : 'Cant.'} {line.quantity}</Typography>{!line.available && <Alert severity="warning" sx={{ mt: 1 }}>{language === 'en' ? 'Stock changed. Remove this item or try again.' : 'El stock cambió. Quita este artículo o inténtalo de nuevo.'}</Alert>}</Box><Typography>{formatMerchMoney(line.subtotalMinor, cart.data?.currency ?? 'USD')}</Typography><Button aria-label={`${language === 'en' ? 'Remove' : 'Quitar'} ${line.productName}`} disabled={checkoutMutation.isPending || removeMutation.isPending} onClick={() => removeMutation.mutate(line.variantId)}><DeleteOutlineIcon /></Button></Stack>)}
            {lines.length === 0 && <Typography>{language === 'en' ? 'Your cart is empty.' : 'Tu carrito está vacío.'}</Typography>}
          </Stack>
        </Paper>

        {lines.length > 0 && <Paper component="form" aria-label={en ? 'Delivery details' : 'Datos de entrega'} noValidate variant="outlined" sx={{ p: { xs: 2, sm: 3 }, borderRadius: 3 }} onSubmit={handleSubmit}>
          <Stack spacing={2}>
            <Typography component="h2" variant="h5" fontWeight={800}>{language === 'en' ? 'Delivery details' : 'Datos de entrega'}</Typography>
            <TextField required autoComplete="name" label={language === 'en' ? 'Full name' : 'Nombre completo'} {...fieldProps('name')} />
            <TextField required type="email" autoComplete="email" label="Email" {...fieldProps('email')} />
            <TextField type="tel" autoComplete="tel" label={language === 'en' ? 'Phone (optional)' : 'Teléfono (opcional)'} {...fieldProps('phone')} />
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}><TextField required fullWidth autoComplete="address-level2" label={language === 'en' ? 'City' : 'Ciudad'} {...fieldProps('city')} /><TextField fullWidth autoComplete="address-level1" label={language === 'en' ? 'Province' : 'Provincia'} {...fieldProps('subdivision')} /></Stack>
            <TextField required autoComplete="address-line1" label={language === 'en' ? 'Address' : 'Dirección'} {...fieldProps('addressLine1')} />
            <TextField autoComplete="address-line2" label={language === 'en' ? 'Address details (optional)' : 'Referencia (opcional)'} {...fieldProps('addressLine2')} />
            <TextField multiline minRows={2} label={language === 'en' ? 'Delivery note (optional)' : 'Nota de entrega (opcional)'} {...fieldProps('deliveryNote')} />
            <FormControl fullWidth required disabled={checkoutMutation.isPending}><InputLabel id="shipping-zone-label">{language === 'en' ? 'Delivery option' : 'Opción de entrega'}</InputLabel><Select labelId="shipping-zone-label" label={language === 'en' ? 'Delivery option' : 'Opción de entrega'} value={selectedZone?.id ?? ''} onChange={(event) => setZoneId(event.target.value)}>{zones.map((zone) => <MenuItem key={zone.id} value={zone.id}>{zone.name} · {formatMerchMoney(zone.rateMinor, 'USD')}</MenuItem>)}</Select></FormControl>
            {zones.length === 0 && <Alert severity="warning">{language === 'en' ? 'This seller has not configured a delivery option.' : 'Este vendedor no ha configurado una opción de entrega.'}</Alert>}
            <FormControlLabel control={<Checkbox disabled={checkoutMutation.isPending} checked={accountAfter} onChange={(event) => setAccountAfter(event.target.checked)} />} label={language === 'en' ? 'Remind me to create an account after ordering' : 'Recordarme crear una cuenta después del pedido'} />
            <Divider />
            <Stack direction="row" justifyContent="space-between"><Typography>{language === 'en' ? 'Products' : 'Productos'}</Typography><Typography>{formatMerchMoney(productSubtotal, 'USD')}</Typography></Stack>
            <Stack direction="row" justifyContent="space-between"><Typography>{language === 'en' ? 'Delivery' : 'Entrega'}</Typography><Typography>{formatMerchMoney(shipping, 'USD')}</Typography></Stack>
            <Stack direction="row" justifyContent="space-between"><Typography fontWeight={900}>{language === 'en' ? 'Estimated total' : 'Total estimado'}</Typography><Typography fontWeight={900}>{formatMerchMoney(productSubtotal + shipping, 'USD')}</Typography></Stack>
            <Typography variant="caption" color="text.secondary"><LockOutlinedIcon sx={{ fontSize: 14, verticalAlign: 'text-bottom' }} /> {language === 'en' ? 'We will confirm the final total and availability when you continue.' : 'Confirmaremos el total y la disponibilidad al continuar.'}</Typography>
            {(cart.isFetching || storefront.isFetching || capabilities.isFetching) && <Typography role="status" variant="body2">{en ? 'Updating your cart…' : 'Actualizando tu carrito…'}</Typography>}
            {checkoutMutation.isError && <Alert severity="error">{checkoutMutation.error instanceof Error ? checkoutMutation.error.message : language === 'en' ? 'Checkout could not continue.' : 'No se pudo continuar el checkout.'}</Alert>}
            <Button type="submit" variant="contained" size="large" disabled={!canCheckout}>{checkoutMutation.isPending ? (language === 'en' ? 'Reserving stock…' : 'Reservando stock…') : (language === 'en' ? 'Review and continue to payment' : 'Revisar y continuar al pago')}</Button>
            <Typography variant="caption" color="text.secondary">{language === 'en' ? 'Creating an order does not mark it as paid. Payment is confirmed only by verified provider evidence or independent manual review.' : 'Crear una orden no la marca como pagada. El pago se confirma solo con evidencia verificada del proveedor o revisión manual independiente.'}</Typography>
          </Stack>
        </Paper>}
      </Stack>
    </Box>
  );
}
