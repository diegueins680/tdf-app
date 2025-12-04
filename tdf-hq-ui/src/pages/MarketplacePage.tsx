import { useEffect, useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  CardHeader,
  Chip,
  CircularProgress,
  Grid,
  IconButton,
  Snackbar,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import ShoppingCartIcon from '@mui/icons-material/ShoppingCart';
import DeleteIcon from '@mui/icons-material/Delete';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import type {
  MarketplaceCartDTO,
  MarketplaceCartItemDTO,
  MarketplaceItemDTO,
  MarketplaceOrderDTO,
} from '../api/types';
import { Marketplace } from '../api/marketplace';

const CART_STORAGE_KEY = 'tdf-marketplace-cart-id';
const CART_META_KEY = 'tdf-marketplace-cart-meta';
const CART_EVENT = 'tdf-cart-updated';

const fireCartMetaEvent = () => {
  if (typeof window !== 'undefined') {
    window.dispatchEvent(new Event(CART_EVENT));
  }
};

export default function MarketplacePage() {
  const qc = useQueryClient();
  const [search, setSearch] = useState('');
  const [toast, setToast] = useState<string | null>(null);
  const [cartId, setCartId] = useState<string | null>(() => {
    if (typeof window === 'undefined') return null;
    return localStorage.getItem(CART_STORAGE_KEY);
  });
  const [buyerName, setBuyerName] = useState('');
  const [buyerEmail, setBuyerEmail] = useState('');
  const [buyerPhone, setBuyerPhone] = useState('');
  const [lastOrder, setLastOrder] = useState<MarketplaceOrderDTO | null>(null);
  const isValidEmail = useMemo(() => /\S+@\S+\.\S+/.test(buyerEmail.trim()), [buyerEmail]);
  const isValidName = useMemo(() => buyerName.trim().length > 1, [buyerName]);

  const listingsQuery = useQuery({
    queryKey: ['marketplace-listings'],
    queryFn: Marketplace.list,
  });

  const cartQuery = useQuery<MarketplaceCartDTO>({
    queryKey: ['marketplace-cart', cartId ?? ''],
    enabled: Boolean(cartId),
    queryFn: async () => {
      if (!cartId) throw new Error('no-cart');
      return Marketplace.getCart(cartId);
    },
  });

  useEffect(() => {
    if (cartId) {
      localStorage.setItem(CART_STORAGE_KEY, cartId);
    }
  }, [cartId]);

  useEffect(() => {
    if (!cartQuery.data) return;
    const count = cartQuery.data.mcItems.reduce(
      (acc: number, it: MarketplaceCartItemDTO) => acc + it.mciQuantity,
      0,
    );
    const preview = cartQuery.data.mcItems.slice(0, 3).map((it) => ({
      title: it.mciTitle,
      subtotal: it.mciSubtotalDisplay,
    }));
    localStorage.setItem(CART_META_KEY, JSON.stringify({ cartId: cartQuery.data.mcCartId, count, preview }));
    fireCartMetaEvent();
  }, [cartQuery.data]);

  const createCartMutation = useMutation<MarketplaceCartDTO, Error, void>({
    mutationFn: Marketplace.createCart,
    onSuccess: (data) => {
      setCartId(data.mcCartId);
    },
  });

  const upsertItemMutation = useMutation<MarketplaceCartDTO, Error, { listingId: string; quantity: number }>({
    mutationFn: async ({ listingId, quantity }) => {
      const ensuredCart = cartId ?? (await createCartMutation.mutateAsync()).mcCartId;
      const nextId = cartId ?? ensuredCart;
      setCartId(nextId);
      return Marketplace.upsertItem(nextId, { mciuListingId: listingId, mciuQuantity: quantity });
    },
    onSuccess: (data) => {
      qc.setQueryData(['marketplace-cart', data.mcCartId], data);
      const count = data.mcItems.reduce((acc, it) => acc + it.mciQuantity, 0);
      const preview = data.mcItems.slice(0, 3).map((it) => ({
        title: it.mciTitle,
        subtotal: it.mciSubtotalDisplay,
      }));
      localStorage.setItem(CART_META_KEY, JSON.stringify({ cartId: data.mcCartId, count, preview }));
      setToast('Carrito actualizado');
      fireCartMetaEvent();
    },
  });

  const checkoutMutation = useMutation({
    mutationFn: async () => {
      const ensuredCart = cartId ?? (await createCartMutation.mutateAsync()).mcCartId;
      setCartId(ensuredCart);
      const order = await Marketplace.checkout(ensuredCart, {
        mcrBuyerName: buyerName,
        mcrBuyerEmail: buyerEmail,
        mcrBuyerPhone: buyerPhone || undefined,
      });
      return order;
    },
    onSuccess: (order) => {
      setLastOrder(order);
      void qc.invalidateQueries({ queryKey: ['marketplace-cart', cartId] });
      localStorage.setItem(CART_META_KEY, JSON.stringify({ cartId: cartId ?? '', count: 0, preview: [] }));
      setToast('Pedido enviado');
      fireCartMetaEvent();
    },
  });

  const listings = listingsQuery.data ?? [];
  const filteredListings = listings.filter((item) => {
    if (!search.trim()) return true;
    const haystack = `${item.miTitle} ${item.miBrand ?? ''} ${item.miModel ?? ''}`.toLowerCase();
    return haystack.includes(search.trim().toLowerCase());
  });
  const cart = cartQuery.data;
  const cartItems: MarketplaceCartItemDTO[] = cart?.mcItems ?? [];

  const cartSubtotal = cart?.mcSubtotalDisplay ?? 'USD $0.00';

  const handleAdd = (listing: MarketplaceItemDTO) => {
    const currentQty =
      cart?.mcItems.find((item) => item.mciListingId === listing.miListingId)?.mciQuantity ?? 0;
    upsertItemMutation.mutate({ listingId: listing.miListingId, quantity: currentQty + 1 });
  };

  const handleUpdateQty = (item: MarketplaceCartItemDTO, quantity: number) => {
    upsertItemMutation.mutate({ listingId: item.mciListingId, quantity });
  };

  const hasCartItems = cartItems.length > 0;

  const orderSummary = useMemo(() => {
    if (!lastOrder) return null;
    return (
      <Card variant="outlined">
        <CardHeader title="Pedido enviado" subheader={`Total: ${lastOrder.moTotalDisplay}`} />
        <CardContent>
          <Stack spacing={1}>
            {lastOrder.moItems.map((item) => (
              <Stack
                key={item.moiListingId}
                direction="row"
                justifyContent="space-between"
                alignItems="center"
              >
                <Typography variant="body2">
                  {item.moiQuantity} × {item.moiTitle || 'Ítem'}
                </Typography>
                <Typography variant="body2" fontWeight={700}>
                  {item.moiSubtotalDisplay}
                </Typography>
              </Stack>
            ))}
            <Typography variant="body2" color="text.secondary">
              Estado: {lastOrder.moStatus}
            </Typography>
          </Stack>
        </CardContent>
      </Card>
    );
  }, [lastOrder]);

  return (
    <Box sx={{ minHeight: '100vh', bgcolor: 'background.default', py: 4, px: { xs: 2, md: 6 } }}>
      <Stack spacing={3} maxWidth="lg" sx={{ mx: 'auto' }}>
        <Stack spacing={0.5}>
          <Typography variant="h4" fontWeight={800}>
            Marketplace de Inventario
          </Typography>
          <Typography variant="body1" color="text.secondary">
            Publicamos equipo del inventario con un margen del 25% sobre el valor de referencia. Compra sin
            iniciar sesión: agrega al carrito y deja tus datos para coordinar pago y entrega.
          </Typography>
        </Stack>

        {listingsQuery.isLoading && (
          <Box display="flex" justifyContent="center">
            <CircularProgress />
          </Box>
        )}

        {listingsQuery.isError && (
          <Alert severity="error">No pudimos cargar el marketplace. Intenta de nuevo.</Alert>
        )}

        <Grid container spacing={3}>
          <Grid item xs={12} md={8}>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems="center" sx={{ mb: 1 }}>
              <TextField
                label="Buscar equipo"
                value={search}
                onChange={(e) => setSearch(e.target.value)}
                size="small"
                fullWidth
              />
              <Chip label={`${filteredListings.length} resultados`} size="small" />
            </Stack>
            <Grid container spacing={2}>
              {filteredListings.map((item) => (
                <Grid item xs={12} sm={6} key={item.miListingId}>
                  <Card variant="outlined" sx={{ height: '100%', display: 'flex', flexDirection: 'column' }}>
                    <CardContent sx={{ display: 'flex', flexDirection: 'column', gap: 1.25, flexGrow: 1 }}>
                      <Stack direction="row" justifyContent="space-between" alignItems="center">
                        <Typography variant="h6" fontWeight={700} noWrap>
                          {item.miTitle}
                        </Typography>
                        <Chip label={item.miCategory} size="small" />
                      </Stack>
                      <Typography variant="body2" color="text.secondary">
                        {item.miBrand ?? 'Sin marca'} {item.miModel ?? ''}
                      </Typography>
                      <Typography variant="h5" fontWeight={800}>
                        {item.miPriceDisplay}
                      </Typography>
                      <Stack direction="row" spacing={1} alignItems="center" sx={{ mt: 'auto' }}>
                        <Button
                          variant="contained"
                          size="small"
                          startIcon={<ShoppingCartIcon />}
                          onClick={() => handleAdd(item)}
                          disabled={upsertItemMutation.isPending}
                        >
                          Agregar
                        </Button>
                        <Typography variant="caption" color="text.secondary">
                          Incluye markup {item.miMarkupPct}% sobre precio referencia.
                        </Typography>
                      </Stack>
                    </CardContent>
                  </Card>
                </Grid>
              ))}
            </Grid>
          </Grid>

          <Grid item xs={12} md={4}>
            <Stack spacing={2}>
              <Card variant="outlined">
                <CardHeader
                  title="Carrito"
                  subheader={hasCartItems ? `${cartItems.length} productos` : 'Sin productos aún'}
                  action={<ShoppingCartIcon />}
                />
                <CardContent>
                  {cartQuery.isLoading && (
                    <Box display="flex" justifyContent="center">
                      <CircularProgress size={20} />
                    </Box>
                  )}
                  {!hasCartItems && !cartQuery.isLoading && (
                    <Typography variant="body2" color="text.secondary">
                      Agrega artículos para continuar al checkout.
                    </Typography>
                  )}
                  <Stack spacing={1.5}>
                    {cartItems.map((item) => (
                      <Box
                        key={item.mciListingId}
                        sx={{
                          border: '1px solid',
                          borderColor: 'divider',
                          borderRadius: 1.5,
                          p: 1.25,
                        }}
                      >
                        <Stack direction="row" justifyContent="space-between" alignItems="center">
                          <Box>
                            <Typography fontWeight={700}>{item.mciTitle}</Typography>
                            <Typography variant="caption" color="text.secondary">
                              {item.mciUnitPriceDisplay} · {item.mciCategory}
                            </Typography>
                          </Box>
                          <IconButton
                            aria-label="Quitar"
                            size="small"
                            onClick={() => handleUpdateQty(item, 0)}
                          >
                            <DeleteIcon fontSize="small" />
                          </IconButton>
                        </Stack>
                        <Stack direction="row" spacing={1} alignItems="center" mt={1}>
                          <TextField
                            type="number"
                            size="small"
                            label="Cantidad"
                            value={item.mciQuantity}
                            onChange={(e) =>
                              handleUpdateQty(item, Math.max(0, parseInt(e.target.value || '0', 10)))
                            }
                            inputProps={{ min: 0, max: 99 }}
                            sx={{ width: 110 }}
                          />
                          <Typography variant="body2" fontWeight={700} sx={{ ml: 'auto' }}>
                            {item.mciSubtotalDisplay}
                          </Typography>
                        </Stack>
                      </Box>
                    ))}
                    <Stack direction="row" justifyContent="space-between" alignItems="center" mt={1}>
                      <Typography variant="subtitle2">Subtotal</Typography>
                      <Typography variant="h6" fontWeight={800}>
                        {cartSubtotal}
                      </Typography>
                    </Stack>
                  </Stack>
                </CardContent>
              </Card>

              <Card variant="outlined">
                <CardHeader title="Checkout" />
                <CardContent>
                  <Stack spacing={1.5}>
                    <TextField
                      label="Nombre completo"
                      value={buyerName}
                      onChange={(e) => setBuyerName(e.target.value)}
                      fullWidth
                      error={Boolean(buyerName) && !isValidName}
                      helperText={Boolean(buyerName) && !isValidName ? 'Ingresa tu nombre' : undefined}
                    />
                    <TextField
                      label="Email"
                      value={buyerEmail}
                      onChange={(e) => setBuyerEmail(e.target.value)}
                      type="email"
                      fullWidth
                      error={Boolean(buyerEmail) && !isValidEmail}
                      helperText={Boolean(buyerEmail) && !isValidEmail ? 'Correo no válido' : undefined}
                    />
                    <TextField
                      label="Teléfono (opcional)"
                      value={buyerPhone}
                      onChange={(e) => setBuyerPhone(e.target.value)}
                      fullWidth
                    />
                    <Button
                      variant="contained"
                      disabled={!hasCartItems || checkoutMutation.isPending || !isValidName || !isValidEmail}
                      onClick={() => checkoutMutation.mutate()}
                    >
                      {checkoutMutation.isPending ? 'Enviando pedido…' : 'Confirmar pedido'}
                    </Button>
                    {checkoutMutation.isError && (
                      <Alert severity="error">No pudimos crear el pedido. Revisa tus datos.</Alert>
                    )}
                  </Stack>
                </CardContent>
              </Card>

              {orderSummary}
            </Stack>
      </Grid>
    </Grid>
      <Snackbar
        open={Boolean(toast)}
        autoHideDuration={2200}
        onClose={() => setToast(null)}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'center' }}
      >
        <Alert severity="success" onClose={() => setToast(null)} sx={{ width: '100%' }}>
          {toast}
        </Alert>
      </Snackbar>
    </Stack>
  </Box>
);
}
