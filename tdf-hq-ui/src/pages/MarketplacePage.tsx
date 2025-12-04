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
  FormControl,
  Grid,
  IconButton,
  ButtonGroup,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
  InputLabel,
  MenuItem,
  Select,
  type SelectChangeEvent,
  Snackbar,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import ShoppingCartIcon from '@mui/icons-material/ShoppingCart';
import DeleteIcon from '@mui/icons-material/Delete';
import AddIcon from '@mui/icons-material/Add';
import RemoveIcon from '@mui/icons-material/Remove';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';
import WarningAmberIcon from '@mui/icons-material/WarningAmber';
import InventoryIcon from '@mui/icons-material/Inventory';
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
const BUYER_INFO_KEY = 'tdf-marketplace-buyer';

const fireCartMetaEvent = () => {
  if (typeof window !== 'undefined') {
    window.dispatchEvent(new Event(CART_EVENT));
  }
};

export default function MarketplacePage() {
  const qc = useQueryClient();
  const [search, setSearch] = useState('');
  const [toast, setToast] = useState<string | null>(null);
  const [copyToast, setCopyToast] = useState<string | null>(null);
  const [cartId, setCartId] = useState<string | null>(() => {
    if (typeof window === 'undefined') return null;
    return localStorage.getItem(CART_STORAGE_KEY);
  });
  const [category, setCategory] = useState<string>('all');
  const [sort, setSort] = useState<'relevance' | 'price-asc' | 'price-desc' | 'title-asc'>('relevance');
  const [buyerName, setBuyerName] = useState('');
  const [buyerEmail, setBuyerEmail] = useState('');
  const [buyerPhone, setBuyerPhone] = useState('');
  const [contactPref, setContactPref] = useState<'email' | 'phone'>('email');
  const [lastOrder, setLastOrder] = useState<MarketplaceOrderDTO | null>(null);
  const [compareIds, setCompareIds] = useState<string[]>([]);
  const [compareOpen, setCompareOpen] = useState(false);
  const cartQuery = useQuery<MarketplaceCartDTO>({
    queryKey: ['marketplace-cart', cartId ?? ''],
    enabled: Boolean(cartId),
    queryFn: async () => {
      if (!cartId) throw new Error('no-cart');
      return Marketplace.getCart(cartId);
    },
  });
  const cart = cartQuery.data;
  const cartItems: MarketplaceCartItemDTO[] = cart?.mcItems ?? [];
  const savedCartMeta = useMemo(() => {
    if (typeof window === 'undefined') return null;
    try {
      const raw = localStorage.getItem(CART_META_KEY);
      if (!raw) return null;
      const parsed = JSON.parse(raw);
      if (!parsed?.cartId) return null;
      return {
        cartId: parsed.cartId as string,
        count: typeof parsed.count === 'number' ? parsed.count : 0,
        updatedAt: typeof parsed.updatedAt === 'number' ? parsed.updatedAt : null,
      };
    } catch {
      return null;
    }
  }, [cartId, cartItems.length]);
  const isValidEmail = useMemo(() => /\S+@\S+\.\S+/.test(buyerEmail.trim()), [buyerEmail]);
  const isValidName = useMemo(() => buyerName.trim().length > 1, [buyerName]);

  const listingsQuery = useQuery({
    queryKey: ['marketplace-listings'],
    queryFn: Marketplace.list,
  });

  useEffect(() => {
    if (cartId) {
      localStorage.setItem(CART_STORAGE_KEY, cartId);
    }
  }, [cartId]);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      const raw = localStorage.getItem(BUYER_INFO_KEY);
      if (!raw) return;
      const parsed = JSON.parse(raw);
      setBuyerName(parsed?.name ?? '');
      setBuyerEmail(parsed?.email ?? '');
      setBuyerPhone(parsed?.phone ?? '');
      setContactPref(parsed?.pref ?? 'email');
    } catch {
      // ignore malformed payloads
    }
  }, []);

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
    localStorage.setItem(
      CART_META_KEY,
      JSON.stringify({ cartId: cartQuery.data.mcCartId, count, preview, updatedAt: Date.now() }),
    );
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
      localStorage.setItem(
        CART_META_KEY,
        JSON.stringify({ cartId: data.mcCartId, count, preview, updatedAt: Date.now() }),
      );
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
      localStorage.setItem(CART_META_KEY, JSON.stringify({ cartId: cartId ?? '', count: 0, preview: [], updatedAt: Date.now() }));
      setToast(
        `Pedido enviado. Te contactaremos por ${
          contactPref === 'email' ? 'correo' : 'teléfono/WhatsApp'
        } en menos de 24 h.`,
      );
      fireCartMetaEvent();
    },
  });

  const listings = listingsQuery.data ?? [];
  const categories = useMemo(
    () => Array.from(new Set(listings.map((item) => item.miCategory).filter(Boolean))),
    [listings],
  );
  const filteredListings = listings.filter((item) => {
    const matchesCategory = category === 'all' ? true : item.miCategory === category;
    if (!search.trim()) return matchesCategory;
    const haystack = `${item.miTitle} ${item.miBrand ?? ''} ${item.miModel ?? ''}`.toLowerCase();
    return matchesCategory && haystack.includes(search.trim().toLowerCase());
  });
  const sortedListings = useMemo(() => {
    const list = [...filteredListings];
    switch (sort) {
      case 'price-asc':
        return list.sort((a, b) => a.miPriceUsdCents - b.miPriceUsdCents);
      case 'price-desc':
        return list.sort((a, b) => b.miPriceUsdCents - a.miPriceUsdCents);
      case 'title-asc':
        return list.sort((a, b) => a.miTitle.localeCompare(b.miTitle));
      default:
        return list;
    }
  }, [filteredListings, sort]);
  const cartItemCount = cartItems.reduce((acc, it) => acc + it.mciQuantity, 0);

  const cartSubtotal = cart?.mcSubtotalDisplay ?? 'USD $0.00';
  const resetFilters = () => {
    setSearch('');
    setCategory('all');
  };

  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      const payload = { name: buyerName, email: buyerEmail, phone: buyerPhone, pref: contactPref };
      localStorage.setItem(BUYER_INFO_KEY, JSON.stringify(payload));
    } catch {
      // ignore storage errors
    }
  }, [buyerName, buyerEmail, buyerPhone, contactPref]);

  const handleAdd = (listing: MarketplaceItemDTO) => {
    const currentQty =
      cart?.mcItems.find((item) => item.mciListingId === listing.miListingId)?.mciQuantity ?? 0;
    upsertItemMutation.mutate({ listingId: listing.miListingId, quantity: currentQty + 1 });
  };

  const handleUpdateQty = (item: MarketplaceCartItemDTO, quantity: number) => {
    upsertItemMutation.mutate({ listingId: item.mciListingId, quantity });
  };

  const hasCartItems = cartItems.length > 0;
  const clearCart = async () => {
    if (!hasCartItems) return;
    const ensuredCart = cartId ?? (await createCartMutation.mutateAsync()).mcCartId;
    setCartId(ensuredCart);
    for (const item of cartItems) {
      // eslint-disable-next-line no-await-in-loop
      await upsertItemMutation.mutateAsync({ listingId: item.mciListingId, quantity: 0 });
    }
    setToast('Carrito vacío');
  };

  const handleCheckout = () => {
    checkoutMutation.mutate();
  };

  const handleRestoreCart = () => {
    if (!savedCartMeta?.cartId) return;
    setCartId(savedCartMeta.cartId);
    void qc.invalidateQueries({ queryKey: ['marketplace-cart', savedCartMeta.cartId] });
    setToast('Carrito restaurado');
  };

  const toggleCompare = (id: string) => {
    setCompareIds((prev) => {
      if (prev.includes(id)) return prev.filter((x) => x !== id);
      if (prev.length >= 3) {
        setToast('Máximo 3 artículos en el comparador');
        return prev;
      }
      return [...prev, id];
    });
  };

  const compareItems = useMemo(
    () => listings.filter((item) => compareIds.includes(item.miListingId)),
    [listings, compareIds],
  );

  const getStatusChipProps = (status?: string | null) => {
    if (!status) return null;
    const lower = status.toLowerCase();
    if (lower.includes('stock')) return { color: 'success' as const, icon: <CheckCircleIcon /> };
    if (lower.includes('reserva') || lower.includes('reservado')) return { color: 'warning' as const, icon: <InventoryIcon /> };
    if (lower.includes('mantenimiento')) return { color: 'warning' as const, icon: <WarningAmberIcon /> };
    return { color: 'default' as const, icon: undefined };
  };
  const formatLastSaved = () => {
    if (!savedCartMeta?.updatedAt) return null;
    const diffMs = Date.now() - savedCartMeta.updatedAt;
    const minutes = Math.floor(diffMs / 60000);
    if (minutes < 1) return 'Actualizado hace <1 min';
    if (minutes < 60) return `Actualizado hace ${minutes} min`;
    const hours = Math.floor(minutes / 60);
    return `Actualizado hace ${hours} h`;
  };

  const orderSummary = useMemo(() => {
    if (!lastOrder) return null;
    const summaryLines = lastOrder.moItems
      .map((item) => `${item.moiQuantity} × ${item.moiTitle || 'Ítem'} — ${item.moiSubtotalDisplay}`)
      .join('\n');
    const orderText = `Pedido ${lastOrder.moOrderId}\nTotal: ${lastOrder.moTotalDisplay}\nEstado: ${lastOrder.moStatus}\n${summaryLines}`;
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
            <Button
              size="small"
              variant="outlined"
              onClick={async () => {
                try {
                  await navigator.clipboard.writeText(lastOrder.moOrderId);
                  setCopyToast('ID de pedido copiado');
                } catch {
                  setCopyToast('No se pudo copiar el ID');
                }
              }}
              sx={{ alignSelf: 'flex-start', mt: 1 }}
            >
              Copiar ID: {lastOrder.moOrderId}
            </Button>
            <Button
              size="small"
              variant="text"
              onClick={() => {
                const blob = new Blob([orderText], { type: 'text/plain' });
                const url = URL.createObjectURL(blob);
                const a = document.createElement('a');
                a.href = url;
                a.download = `pedido-${lastOrder.moOrderId}.txt`;
                a.click();
                URL.revokeObjectURL(url);
                setCopyToast('Resumen descargado');
              }}
            >
              Descargar resumen
            </Button>
            <Button
              size="small"
              variant="outlined"
              onClick={async () => {
                try {
                  await navigator.clipboard.writeText(orderText);
                  setCopyToast('Resumen copiado');
                } catch {
                  setCopyToast('No se pudo copiar el resumen');
                }
              }}
            >
              Copiar resumen
            </Button>
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
              <FormControl size="small" sx={{ minWidth: 160 }}>
                <InputLabel id="marketplace-category-label">Categoría</InputLabel>
                <Select
                  labelId="marketplace-category-label"
                  value={category}
                  label="Categoría"
                  onChange={(event: SelectChangeEvent<string>) => setCategory(event.target.value)}
                >
                  <MenuItem value="all">Todas</MenuItem>
                  {categories.map((cat) => (
                    <MenuItem key={cat} value={cat}>
                      {cat}
                    </MenuItem>
                  ))}
                </Select>
              </FormControl>
              <FormControl size="small" sx={{ minWidth: 180 }}>
                <InputLabel id="marketplace-sort-label">Ordenar por</InputLabel>
                <Select
                  labelId="marketplace-sort-label"
                  value={sort}
                  label="Ordenar por"
                  onChange={(event: SelectChangeEvent<'relevance' | 'price-asc' | 'price-desc' | 'title-asc'>) =>
                    setSort(event.target.value as typeof sort)
                  }
                >
                  <MenuItem value="relevance">Relevancia</MenuItem>
                  <MenuItem value="price-asc">Precio: bajo a alto</MenuItem>
                  <MenuItem value="price-desc">Precio: alto a bajo</MenuItem>
                  <MenuItem value="title-asc">Título A-Z</MenuItem>
                </Select>
              </FormControl>
              <Chip label={`${filteredListings.length} resultados`} size="small" />
              <Button
                variant="outlined"
                size="small"
                disabled={compareIds.length === 0}
                onClick={() => setCompareOpen(true)}
              >
                Comparar ({compareIds.length})
              </Button>
            </Stack>
            <Grid container spacing={2}>
              {!listingsQuery.isLoading && filteredListings.length === 0 && (
                <Grid item xs={12}>
                  <Alert
                    severity="info"
                    action={
                      <Button color="inherit" size="small" onClick={resetFilters}>
                        Limpiar filtros
                      </Button>
                    }
                  >
                    No encontramos resultados con estos filtros.
                  </Alert>
                </Grid>
              )}
              {sortedListings.map((item) => (
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
                      <Stack direction="row" spacing={1} flexWrap="wrap">
                        {item.miBrand && <Chip size="small" label={item.miBrand} variant="outlined" />}
                        {item.miModel && <Chip size="small" label={item.miModel} variant="outlined" />}
                        <Chip size="small" label={item.miCategory} color="default" variant="outlined" />
                      </Stack>
                      <Box
                        sx={{
                          width: '100%',
                          maxHeight: 140,
                          borderRadius: 1.5,
                          border: '1px solid',
                          borderColor: 'divider',
                          overflow: 'hidden',
                          bgcolor: 'rgba(148,163,184,0.08)',
                          display: 'flex',
                          alignItems: 'center',
                          justifyContent: 'center',
                        }}
                      >
                        {item.miPhotoUrl ? (
                          <Box
                            component="img"
                            src={item.miPhotoUrl}
                            alt={item.miTitle}
                            sx={{ width: '100%', height: '100%', objectFit: 'cover' }}
                            loading="lazy"
                          />
                        ) : (
                          <Stack spacing={0.5} alignItems="center" py={3}>
                            <InventoryIcon sx={{ color: 'text.secondary' }} />
                            <Typography variant="caption" color="text.secondary">
                              Sin foto
                            </Typography>
                          </Stack>
                        )}
                      </Box>
                      <Typography variant="h5" fontWeight={800}>
                        {item.miPriceDisplay}
                      </Typography>
                      {(() => {
                        const props = getStatusChipProps(item.miStatus);
                        return props ? (
                          <Chip
                            size="small"
                            color={props.color}
                            icon={props.icon}
                            label={item.miStatus}
                            sx={{ alignSelf: 'flex-start' }}
                          />
                        ) : null;
                      })()}
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
                        <Chip
                          label={compareIds.includes(item.miListingId) ? 'En comparador' : 'Comparar'}
                          size="small"
                          color={compareIds.includes(item.miListingId) ? 'primary' : 'default'}
                          onClick={() => toggleCompare(item.miListingId)}
                          variant={compareIds.includes(item.miListingId) ? 'filled' : 'outlined'}
                        />
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
                  subheader={
                    hasCartItems
                      ? `${cartItems.length} productos · ${cartItemCount} en total`
                      : 'Sin productos aún'
                  }
                  action={<ShoppingCartIcon />}
                />
                <CardContent>
                  {cartQuery.isLoading && (
                    <Box display="flex" justifyContent="center">
                      <CircularProgress size={20} />
                    </Box>
                  )}
                  {!hasCartItems && !cartQuery.isLoading && (
                    <Stack spacing={1}>
                      <Typography variant="body2" color="text.secondary">
                        Agrega artículos para continuar al checkout.
                      </Typography>
                      {savedCartMeta?.count > 0 && savedCartMeta?.cartId && (
                        <Button size="small" variant="outlined" onClick={handleRestoreCart}>
                          Recuperar carrito guardado ({savedCartMeta.count} productos)
                        </Button>
                      )}
                    </Stack>
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
                          <IconButton
                            aria-label="Disminuir cantidad"
                            size="small"
                            onClick={() => handleUpdateQty(item, Math.max(0, item.mciQuantity - 1))}
                            disabled={upsertItemMutation.isPending}
                          >
                            <RemoveIcon fontSize="small" />
                          </IconButton>
                          <TextField
                            type="number"
                            size="small"
                            label="Cantidad"
                            value={item.mciQuantity}
                            onChange={(e) =>
                              handleUpdateQty(item, Math.max(0, parseInt(e.target.value || '0', 10)))
                            }
                            inputProps={{ min: 0, max: 99 }}
                            sx={{ width: 120 }}
                          />
                          <IconButton
                            aria-label="Incrementar cantidad"
                            size="small"
                            onClick={() => handleUpdateQty(item, Math.min(99, item.mciQuantity + 1))}
                            disabled={upsertItemMutation.isPending}
                          >
                            <AddIcon fontSize="small" />
                          </IconButton>
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
                    {hasCartItems && (
                      <Button
                        variant="text"
                        color="inherit"
                        size="small"
                        onClick={() => clearCart()}
                        disabled={upsertItemMutation.isPending}
                        sx={{ alignSelf: 'flex-start' }}
                      >
                        Vaciar carrito
                      </Button>
                    )}
                    {formatLastSaved() && (
                      <Typography variant="caption" color="text.secondary">
                        {formatLastSaved()}
                      </Typography>
                    )}
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
                    <Stack spacing={0.5}>
                      <Typography variant="caption" color="text.secondary">
                        Preferencia de contacto
                      </Typography>
                      <Stack direction="row" spacing={1}>
                        <Chip
                          label="Email"
                          color={contactPref === 'email' ? 'primary' : 'default'}
                          size="small"
                          variant={contactPref === 'email' ? 'filled' : 'outlined'}
                          onClick={() => setContactPref('email')}
                        />
                        <Chip
                          label="Teléfono / WhatsApp"
                          color={contactPref === 'phone' ? 'primary' : 'default'}
                          size="small"
                          variant={contactPref === 'phone' ? 'filled' : 'outlined'}
                          onClick={() => setContactPref('phone')}
                        />
                      </Stack>
                      <Typography variant="caption" color="text.secondary">
                        Te contactaremos por {contactPref === 'email' ? 'correo' : 'teléfono/WhatsApp'} en menos de 24 h para coordinar pago y entrega.
                      </Typography>
                    </Stack>
                    <Button
                      variant="contained"
                      disabled={
                        !hasCartItems ||
                        checkoutMutation.isPending ||
                        !isValidName ||
                        !isValidEmail ||
                        cartItemCount === 0
                      }
                      onClick={handleCheckout}
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
      <Snackbar
        open={Boolean(copyToast)}
        autoHideDuration={2000}
        onClose={() => setCopyToast(null)}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'center' }}
      >
        <Alert severity="info" onClose={() => setCopyToast(null)} sx={{ width: '100%' }}>
          {copyToast}
        </Alert>
      </Snackbar>
      <Dialog open={compareOpen} onClose={() => setCompareOpen(false)} maxWidth="md" fullWidth>
        <DialogTitle>Comparar artículos</DialogTitle>
        <DialogContent dividers>
          {compareItems.length === 0 && (
            <Typography variant="body2" color="text.secondary">
              Agrega hasta 3 artículos al comparador.
            </Typography>
          )}
          {compareItems.length > 0 && (
            <Table size="small">
              <TableHead>
                <TableRow>
                  <TableCell>Título</TableCell>
                  <TableCell>Categoría</TableCell>
                  <TableCell>Marca</TableCell>
                  <TableCell>Modelo</TableCell>
                  <TableCell align="right">Precio</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {compareItems.map((item) => (
                  <TableRow key={item.miListingId}>
                    <TableCell>{item.miTitle}</TableCell>
                    <TableCell>{item.miCategory}</TableCell>
                    <TableCell>{item.miBrand ?? '-'}</TableCell>
                    <TableCell>{item.miModel ?? '-'}</TableCell>
                    <TableCell align="right">{item.miPriceDisplay}</TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          )}
        </DialogContent>
        <DialogActions>
          {compareItems.length > 0 && (
            <Button onClick={() => setCompareIds([])} color="inherit">
              Limpiar
            </Button>
          )}
          <Button onClick={() => setCompareOpen(false)} variant="contained">
            Cerrar
          </Button>
        </DialogActions>
      </Dialog>
    </Stack>
  </Box>
);
}
