import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
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
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  InputLabel,
  MenuItem,
  Select,
  type SelectChangeEvent,
  Tooltip,
  Snackbar,
  Stack,
  TextField,
  Typography,
  Link,
} from '@mui/material';
import ShoppingCartIcon from '@mui/icons-material/ShoppingCart';
import DeleteIcon from '@mui/icons-material/Delete';
import AddIcon from '@mui/icons-material/Add';
import RemoveIcon from '@mui/icons-material/Remove';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';
import WarningAmberIcon from '@mui/icons-material/WarningAmber';
import InventoryIcon from '@mui/icons-material/Inventory';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import ShareIcon from '@mui/icons-material/Share';
import AddPhotoAlternateIcon from '@mui/icons-material/AddPhotoAlternate';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import type {
  MarketplaceCartDTO,
  MarketplaceCartItemDTO,
  MarketplaceItemDTO,
  MarketplaceOrderDTO,
  DatafastCheckoutDTO,
} from '../api/types';
import { Marketplace } from '../api/marketplace';
import { formatLastSavedTimestamp, getOrderStatusMeta } from '../utils/marketplace';
import CreditCardIcon from '@mui/icons-material/CreditCard';
import { Inventory } from '../api/inventory';
import GoogleDriveUploadWidget from '../components/GoogleDriveUploadWidget';
import { deriveModulesFromRoles } from '../components/SidebarNav';
import { useSession } from '../session/SessionContext';
import { buildPublicContentUrl, type DriveFileInfo } from '../services/googleDrive';

interface PaypalButtonOptions {
  createOrder?: () => string;
  onApprove?: (data: PaypalApproveData) => void | Promise<void>;
  onCancel?: () => void;
  onError?: () => void;
  [key: string]: unknown;
}
// eslint-disable-next-line @typescript-eslint/consistent-type-definitions
type PaypalButtons = (options: PaypalButtonOptions) => {
  render: (selector: string | HTMLElement) => Promise<void>;
  close?: () => void;
};
interface PaypalApproveData {
  orderID?: string;
}

declare global {
interface Window {
  paypal?: {
    Buttons: PaypalButtons;
  };
  wpwlOptions?: Record<string, unknown>;
}
}
const CART_STORAGE_KEY = 'tdf-marketplace-cart-id';
const CART_META_KEY = 'tdf-marketplace-cart-meta';
const CART_EVENT = 'tdf-cart-updated';
const BUYER_INFO_KEY = 'tdf-marketplace-buyer';
const PAYMENT_PREF_KEY = 'tdf-marketplace-payment-pref';
const FILTERS_KEY = 'tdf-marketplace-filters';

interface SavedFilters {
  search?: string;
  category?: string;
  sort?: 'relevance' | 'price-asc' | 'price-desc' | 'title-asc';
  purpose?: 'all' | 'rent' | 'sale';
  condition?: string;
}

interface SavedBuyer {
  name?: string;
  email?: string;
  phone?: string;
  pref?: 'email' | 'phone';
}

const parseEnvNumber = (key: string): number | null => {
  const raw = import.meta.env[key];
  if (!raw) return null;
  const val = Number(raw);
  return Number.isFinite(val) ? val : null;
};

const normalizeText = (value: string) =>
  value
    .toLowerCase()
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .replace(/[^a-z0-9\s]/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();
const normalizePhone = (value: string) => {
  const trimmed = value.trim();
  return trimmed.length > 0 ? trimmed : undefined;
};

const fireCartMetaEvent = () => {
  if (typeof window !== 'undefined') {
    window.dispatchEvent(new Event(CART_EVENT));
  }
};

export default function MarketplacePage() {
  const qc = useQueryClient();
  const { session } = useSession();
  const modules = useMemo(() => {
    const provided = session?.modules ?? [];
    const fromRoles = deriveModulesFromRoles(session?.roles);
    const baseSet = new Set([...provided, ...fromRoles].map((m) => m.toLowerCase()));
    if (baseSet.has('packages')) {
      baseSet.add('ops');
    }
    if (baseSet.has('ops')) {
      baseSet.add('packages');
    }
    return baseSet;
  }, [session?.modules, session?.roles]);
  const canManagePhotos = modules.has('ops') || modules.has('admin');
  const paypalClientId = import.meta.env['VITE_PAYPAL_CLIENT_ID'] ?? '';
  const [search, setSearch] = useState('');
  const [toast, setToast] = useState<string | null>(null);
  const [copyToast, setCopyToast] = useState<string | null>(null);
  const [photoDialogListing, setPhotoDialogListing] = useState<MarketplaceItemDTO | null>(null);
  const [photoError, setPhotoError] = useState<string | null>(null);
  const [pendingPhotoUrl, setPendingPhotoUrl] = useState<string | null>(null);
  const [cartId, setCartId] = useState<string | null>(() => {
    if (typeof window === 'undefined') return null;
    return localStorage.getItem(CART_STORAGE_KEY);
  });
  const [category, setCategory] = useState<string>('all');
  const [purpose, setPurpose] = useState<'all' | 'rent' | 'sale'>('all');
  const [condition, setCondition] = useState<string>('all');
  const [sort, setSort] = useState<'relevance' | 'price-asc' | 'price-desc' | 'title-asc'>('relevance');
  const [buyerName, setBuyerName] = useState('');
  const [buyerEmail, setBuyerEmail] = useState('');
  const [buyerPhone, setBuyerPhone] = useState('');
  const [contactPref, setContactPref] = useState<'email' | 'phone'>('email');
  const [savedBuyerSnapshot, setSavedBuyerSnapshot] = useState<{ name?: string; email?: string; phone?: string; pref?: 'email' | 'phone' } | null>(null);
  const [lastOrder, setLastOrder] = useState<MarketplaceOrderDTO | null>(null);
  const [paypalReady, setPaypalReady] = useState(false);
  const [paypalDialogOpen, setPaypalDialogOpen] = useState(false);
  const [paypalOrder, setPaypalOrder] = useState<{ orderId: string; paypalOrderId: string } | null>(null);
  const [paypalError, setPaypalError] = useState<string | null>(null);
  const datafastFormRef = useRef<HTMLDivElement>(null);
  const [datafastDialogOpen, setDatafastDialogOpen] = useState(false);
  const [datafastCheckout, setDatafastCheckout] = useState<DatafastCheckoutDTO | null>(null);
  const [datafastError, setDatafastError] = useState<string | null>(null);
  const [datafastWidgetKey, setDatafastWidgetKey] = useState(0);
  const [paymentMethod, setPaymentMethod] = useState<'contact' | 'card' | 'paypal'>(() => {
    if (typeof window === 'undefined') return 'contact';
    const saved = localStorage.getItem(PAYMENT_PREF_KEY);
    return saved === 'card' || saved === 'paypal' || saved === 'contact' ? saved : 'contact';
  });
  const [reviewOpen, setReviewOpen] = useState(false);
  const [detailOpen, setDetailOpen] = useState(false);
  const [selectedListing, setSelectedListing] = useState<MarketplaceItemDTO | null>(null);
  const [showRestoreBanner, setShowRestoreBanner] = useState(false);
  const adaUsdRate = useMemo(() => parseEnvNumber('VITE_ADA_USD_RATE'), []);
  const sedUsdRate = useMemo(() => parseEnvNumber('VITE_SED_USD_RATE'), []);
  const showTokenRates = Boolean(adaUsdRate ?? sedUsdRate);
  const normalizedSearchTerm = useMemo(() => normalizeText(search.trim()), [search]);
  const searchTokens = useMemo(() => normalizedSearchTerm.split(' ').filter(Boolean), [normalizedSearchTerm]);
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
  const [savedCartMeta, setSavedCartMeta] = useState<{ cartId: string; count: number; updatedAt: number | null } | null>(() => {
    if (typeof window === 'undefined') return null;
    try {
      const raw = localStorage.getItem(CART_META_KEY);
      if (!raw) return null;
      const parsed = JSON.parse(raw);
      if (!parsed?.cartId) return null;
      return {
        cartId: String(parsed.cartId),
        count: typeof parsed.count === 'number' ? parsed.count : 0,
        updatedAt: typeof parsed.updatedAt === 'number' ? parsed.updatedAt : null,
      };
    } catch {
      return null;
    }
  });
  const datafastReturnUrl = useMemo(() => {
    if (!datafastCheckout || typeof window === 'undefined') return '';
    const url = new URL('/marketplace/pago-datafast', window.location.origin);
    url.searchParams.set('orderId', datafastCheckout.dcOrderId);
    return url.toString();
  }, [datafastCheckout]);
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
    localStorage.setItem(PAYMENT_PREF_KEY, paymentMethod);
  }, [paymentMethod]);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      const params = new URLSearchParams(window.location.search);
      const urlSearch = params.get('q');
      const urlCategory = params.get('cat');
      const urlSort = params.get('sort');
      const urlPurpose = params.get('purpose');
      const urlCondition = params.get('cond');
      if (urlSearch) setSearch(urlSearch);
      if (urlCategory) setCategory(urlCategory);
      if (urlSort === 'relevance' || urlSort === 'price-asc' || urlSort === 'price-desc' || urlSort === 'title-asc') {
        setSort(urlSort);
      }
      if (urlPurpose === 'rent' || urlPurpose === 'sale' || urlPurpose === 'all') {
        setPurpose(urlPurpose);
      }
      if (urlCondition) setCondition(urlCondition);
    } catch {
      // ignore url parse errors
    }
  }, []);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      const raw = localStorage.getItem(FILTERS_KEY);
      if (!raw) return;
      const parsed = JSON.parse(raw) as SavedFilters;
      if (parsed?.search) setSearch(String(parsed.search));
      if (parsed?.category) setCategory(String(parsed.category));
      if (parsed?.sort) setSort(parsed.sort);
      if (parsed?.purpose) setPurpose(parsed.purpose);
      if (parsed?.condition) setCondition(parsed.condition);
    } catch {
      // ignore malformed filters
    }
  }, []);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      const raw = localStorage.getItem(BUYER_INFO_KEY);
      if (!raw) return;
      const parsed = JSON.parse(raw) as SavedBuyer;
      const nameVal = typeof parsed?.name === 'string' ? parsed.name : '';
      const emailVal = typeof parsed?.email === 'string' ? parsed.email : '';
      const phoneVal = typeof parsed?.phone === 'string' ? parsed.phone : '';
      const prefVal = parsed?.pref === 'phone' ? 'phone' : 'email';
      setBuyerName(nameVal);
      setBuyerEmail(emailVal);
      setBuyerPhone(phoneVal);
      setContactPref(prefVal);
      setSavedBuyerSnapshot({
        name: nameVal,
        email: emailVal,
        phone: phoneVal,
        pref: prefVal,
      });
    } catch {
      // ignore malformed payloads
    }
  }, []);

  useEffect(() => {
    if (!paypalClientId || typeof window === 'undefined') return;
    if (window.paypal) {
      setPaypalReady(true);
      return;
    }
    const script = document.createElement('script');
    script.src = `https://www.paypal.com/sdk/js?client-id=${paypalClientId}&currency=USD`;
    script.async = true;
    script.onload = () => setPaypalReady(true);
    script.onerror = () => setPaypalError('No se pudo cargar PayPal. Intenta de nuevo.');
    document.body.appendChild(script);
    return () => {
      document.body.removeChild(script);
    };
  }, [paypalClientId]);

  useEffect(() => {
    if (!datafastDialogOpen || !datafastCheckout || typeof window === 'undefined') return;
    if (datafastFormRef.current) {
      datafastFormRef.current.innerHTML = '';
    }
    window.wpwlOptions = {
      locale: 'es',
      style: 'card',
    };
    const script = document.createElement('script');
    script.src = datafastCheckout.dcWidgetUrl;
    script.async = true;
    script.onerror = () => setDatafastError('No se pudo cargar el formulario de pago.');
    document.body.appendChild(script);
    return () => {
      document.body.removeChild(script);
    };
  }, [datafastCheckout, datafastDialogOpen, datafastWidgetKey]);

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
    setSavedCartMeta({ cartId: cartQuery.data.mcCartId, count, updatedAt: Date.now() });
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
        mcrBuyerPhone: normalizePhone(buyerPhone),
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

  const datafastCheckoutMutation = useMutation<DatafastCheckoutDTO, Error, void>({
    mutationFn: async () => {
      const ensuredCart = cartId ?? (await createCartMutation.mutateAsync()).mcCartId;
      setCartId(ensuredCart);
      return Marketplace.datafastCheckout(ensuredCart, {
        mcrBuyerName: buyerName,
        mcrBuyerEmail: buyerEmail,
        mcrBuyerPhone: normalizePhone(buyerPhone),
      });
    },
    onSuccess: (dto) => {
      setDatafastCheckout(dto);
      setDatafastDialogOpen(true);
      setDatafastError(null);
      setDatafastWidgetKey((k) => k + 1);
    },
    onError: () => setDatafastError('No pudimos iniciar el pago con tarjeta. Revisa tus datos.'),
  });

  const createPaypalOrderMutation = useMutation({
    mutationFn: async () => {
      const ensuredCart = cartId ?? (await createCartMutation.mutateAsync()).mcCartId;
      setCartId(ensuredCart);
      return Marketplace.createPaypalOrder(ensuredCart, {
        mcrBuyerName: buyerName,
        mcrBuyerEmail: buyerEmail,
        mcrBuyerPhone: normalizePhone(buyerPhone),
      });
    },
    onSuccess: (data) => {
      setPaypalOrder({ orderId: data.pcOrderId, paypalOrderId: data.pcPaypalOrderId });
      setPaypalDialogOpen(true);
      setPaypalError(null);
    },
    onError: () => setPaypalError('No pudimos crear la orden PayPal. Revisa tus datos.'),
  });

  const capturePaypalMutation = useMutation<
    MarketplaceOrderDTO,
    Error,
    { orderId: string; paypalOrderId: string }
  >({
    mutationFn: ({ orderId, paypalOrderId }) =>
      Marketplace.capturePaypalOrder({ pcCaptureOrderId: orderId, pcCapturePaypalId: paypalOrderId }),
    onSuccess: (order) => {
      setLastOrder(order);
      setPaypalDialogOpen(false);
      setPaypalOrder(null);
      void qc.invalidateQueries({ queryKey: ['marketplace-cart', cartId] });
      localStorage.setItem(
        CART_META_KEY,
        JSON.stringify({ cartId: cartId ?? '', count: 0, preview: [], updatedAt: Date.now() }),
      );
      setToast('Pago recibido con PayPal. Gracias por tu compra.');
      fireCartMetaEvent();
    },
    onError: () => setPaypalError('No pudimos confirmar el pago. Intenta de nuevo.'),
  });

  const updatePhotoMutation = useMutation({
    mutationFn: ({ assetId, photoUrl }: { assetId: string; photoUrl: string | null }) =>
      Inventory.update(assetId, { uPhotoUrl: photoUrl }),
    onSuccess: (_asset, { assetId, photoUrl }) => {
      const normalized = photoUrl ?? null;
      qc.setQueryData<MarketplaceItemDTO[]>(['marketplace-listings'], (prev) =>
        prev?.map((item) => (item.miAssetId === assetId ? { ...item, miPhotoUrl: normalized } : item)),
      );
      setSelectedListing((prev) =>
        prev && prev.miAssetId === assetId ? { ...prev, miPhotoUrl: normalized } : prev,
      );
      setPhotoDialogListing((prev) =>
        prev && prev.miAssetId === assetId ? { ...prev, miPhotoUrl: normalized } : prev,
      );
      setPendingPhotoUrl(null);
      setPhotoError(null);
      setToast(photoUrl ? 'Foto guardada para este equipo' : 'Foto eliminada');
      void qc.invalidateQueries({ queryKey: ['marketplace-listings'] });
    },
    onError: (err) => {
      setPhotoError(err instanceof Error ? err.message : 'No se pudo actualizar la foto.');
      setPendingPhotoUrl(null);
    },
  });

  const listings = useMemo(() => listingsQuery.data ?? [], [listingsQuery.data]);
  const categories = useMemo(
    () => Array.from(new Set(listings.map((item) => item.miCategory).filter(Boolean))),
    [listings],
  );
  const conditions: string[] = useMemo(
    () => Array.from(new Set(listings.map((item) => item.miCondition).filter((v): v is string => Boolean(v)))),
    [listings],
  );
  const filteredListings = useMemo(() => {
    return listings.filter((item) => {
      const matchesCategory = category === 'all' ? true : item.miCategory === category;
      const matchesPurpose = purpose === 'all' ? true : item.miPurpose === purpose;
      const matchesCondition = condition === 'all' ? true : item.miCondition === condition;
      if (searchTokens.length === 0) return matchesCategory && matchesPurpose && matchesCondition;
      const haystack = [
        item.miTitle,
        item.miBrand,
        item.miModel,
        item.miCategory,
        item.miListingId,
        item.miAssetId,
      ]
        .filter(Boolean)
        .join(' ');
      const haystackNormalized = normalizeText(haystack);
      const matchesText = searchTokens.every((token) => haystackNormalized.includes(token));
      return matchesCategory && matchesPurpose && matchesCondition && matchesText;
    });
  }, [category, condition, listings, purpose, searchTokens]);
  const computeRelevanceScore = useCallback((item: MarketplaceItemDTO) => {
    const status = (item.miStatus ?? '').toLowerCase();
    const isInStock = status.includes('stock') || status.includes('disponible');
    let score = 0;
    if (isInStock) score += 3;
    if (item.miPurpose === 'sale') score += 1;
    if (normalizedSearchTerm) {
      const title = normalizeText(item.miTitle ?? '');
      const brand = normalizeText(item.miBrand ?? '');
      if (title.startsWith(normalizedSearchTerm)) score += 3;
      else if (title.includes(normalizedSearchTerm)) score += 2;
      if (!title.startsWith(normalizedSearchTerm) && brand.startsWith(normalizedSearchTerm)) score += 1;
    }
    return score;
  }, [normalizedSearchTerm]);
  const sortedListings = useMemo(() => {
    const list = [...filteredListings];
    switch (sort) {
      case 'price-asc':
        return list.sort((a, b) => a.miPriceUsdCents - b.miPriceUsdCents);
      case 'price-desc':
        return list.sort((a, b) => b.miPriceUsdCents - a.miPriceUsdCents);
      case 'title-asc':
        return list.sort((a, b) => a.miTitle.localeCompare(b.miTitle));
      case 'relevance':
      default:
        return list.sort(
          (a, b) => computeRelevanceScore(b) - computeRelevanceScore(a) || a.miPriceUsdCents - b.miPriceUsdCents,
        );
    }
  }, [computeRelevanceScore, filteredListings, sort]);
  const cartItemCount = cartItems.reduce((acc, it) => acc + it.mciQuantity, 0);
  const hasCartItems = cartItems.length > 0;
  const cartSubtotal = cart?.mcSubtotalDisplay ?? 'USD $0.00';
  const checkoutDisabledReason = useMemo(() => {
    if (!hasCartItems || cartItemCount === 0) return 'Agrega al menos un producto para continuar.';
    if (!isValidName) return 'Ingresa tu nombre para coordinar el pedido.';
    if (!isValidEmail) return 'Ingresa un correo válido.';
    return '';
  }, [cartItemCount, hasCartItems, isValidEmail, isValidName]);
  const resetFilters = () => {
    setSearch('');
    setCategory('all');
    setSort('relevance');
    setPurpose('all');
    setCondition('all');
  };
  const filtersActiveCount =
    (search.trim() ? 1 : 0) +
    (category !== 'all' ? 1 : 0) +
    (purpose !== 'all' ? 1 : 0) +
    (condition !== 'all' ? 1 : 0) +
    (sort !== 'relevance' ? 1 : 0);

  useEffect(() => {
    if (!savedCartMeta || hasCartItems) {
      setShowRestoreBanner(false);
      return;
    }
    setShowRestoreBanner(true);
  }, [hasCartItems, savedCartMeta]);
  const canRestoreCart = savedCartMeta && !hasCartItems;
  const scrollToListings = () => {
    if (typeof window === 'undefined') return;
    const el = document.getElementById('marketplace-listings');
    if (el?.scrollIntoView) {
      el.scrollIntoView({ behavior: 'smooth', block: 'start' });
      return;
    }
    window.scrollTo({ top: 0, behavior: 'smooth' });
  };

  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      const payload = { name: buyerName, email: buyerEmail, phone: buyerPhone, pref: contactPref };
      localStorage.setItem(BUYER_INFO_KEY, JSON.stringify(payload));
      setSavedBuyerSnapshot(payload);
    } catch {
      // ignore storage errors
    }
  }, [buyerName, buyerEmail, buyerPhone, contactPref]);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      const payload = { search, category, sort, purpose, condition };
      localStorage.setItem(FILTERS_KEY, JSON.stringify(payload));
      const params = new URLSearchParams(window.location.search);
      if (search) params.set('q', search); else params.delete('q');
      if (category !== 'all') params.set('cat', category); else params.delete('cat');
      if (sort !== 'relevance') params.set('sort', sort); else params.delete('sort');
      if (purpose !== 'all') params.set('purpose', purpose); else params.delete('purpose');
      if (condition !== 'all') params.set('cond', condition); else params.delete('cond');
      const next = params.toString();
      const nextUrl = `${window.location.pathname}${next ? `?${next}` : ''}${window.location.hash}`;
      window.history.replaceState({}, '', nextUrl);
    } catch {
      // ignore storage errors
    }
  }, [search, category, sort, purpose, condition]);

  useEffect(() => {
    if (!paypalDialogOpen || !paypalOrder || !paypalReady || typeof window === 'undefined' || !window.paypal) return;
    const container = document.getElementById('paypal-button-container');
    if (!container) return;
    container.innerHTML = '';
    const buttons = window.paypal.Buttons({
      style: { layout: 'vertical' },
      createOrder: () => paypalOrder.paypalOrderId,
      onApprove: async (data: PaypalApproveData) => {
        const orderId = data?.orderID ?? paypalOrder.paypalOrderId;
        await capturePaypalMutation.mutateAsync({ orderId: paypalOrder.orderId, paypalOrderId: orderId });
      },
      onCancel: () => {
        setToast('Pago cancelado.');
        setPaypalDialogOpen(false);
      },
      onError: () => {
        setPaypalError('Error con PayPal. Intenta nuevamente.');
        setPaypalDialogOpen(false);
      },
    });
    void buttons.render(container);
    return () => {
      if (buttons.close) buttons.close();
      container.innerHTML = '';
    };
  }, [capturePaypalMutation, paypalDialogOpen, paypalOrder, paypalReady]);

  const handleAdd = (listing: MarketplaceItemDTO) => {
    if (!isListingAvailable(listing.miStatus)) {
      setToast('Este artículo no está disponible en este momento.');
      return;
    }
    const currentQty =
      cart?.mcItems.find((item) => item.mciListingId === listing.miListingId)?.mciQuantity ?? 0;
    upsertItemMutation.mutate({ listingId: listing.miListingId, quantity: currentQty + 1 });
  };

  const handleUpdateQty = (item: MarketplaceCartItemDTO, quantity: number) => {
    upsertItemMutation.mutate({ listingId: item.mciListingId, quantity });
  };

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

  const handleDatafastCheckout = () => {
    setDatafastError(null);
    if (!hasCartItems || !isValidName || !isValidEmail || cartItemCount === 0) {
      setDatafastError('Completa tu nombre y correo para pagar con tarjeta.');
      return;
    }
    datafastCheckoutMutation.mutate();
  };

  const handlePaypalCheckout = () => {
    setPaypalError(null);
    if (!hasCartItems || !isValidName || !isValidEmail) {
      setPaypalError('Completa tu nombre y correo para pagar con PayPal.');
      return;
    }
    createPaypalOrderMutation.mutate();
  };

  const handleContinueCheckout = () => {
    if (paymentMethod === 'card') {
      handleDatafastCheckout();
    } else if (paymentMethod === 'paypal') {
      handlePaypalCheckout();
    } else {
      handleCheckout();
    }
  };

  const openReview = () => {
    setReviewOpen(true);
  };

  const closeReview = () => {
    setReviewOpen(false);
  };

  const openDetail = (listing: MarketplaceItemDTO) => {
    setSelectedListing(listing);
    setDetailOpen(true);
  };

  const closeDetail = () => {
    setDetailOpen(false);
    setSelectedListing(null);
  };

  const openPhotoDialog = (listing: MarketplaceItemDTO) => {
    if (!canManagePhotos) return;
    setPhotoDialogListing(listing);
    setPhotoError(null);
    setPendingPhotoUrl(null);
  };

  const closePhotoDialog = () => {
    setPhotoDialogListing(null);
    setPhotoError(null);
    setPendingPhotoUrl(null);
  };

  const handlePhotoUploadComplete = (files: DriveFileInfo[]) => {
    const listing = photoDialogListing;
    if (!listing) return;
    const file = files[0];
    const link = file?.webViewLink ?? file?.webContentLink ?? (file?.id ? buildPublicContentUrl(file.id) : null);
    if (!link) {
      setPhotoError('No pudimos obtener el enlace público de Drive.');
      return;
    }
    setPendingPhotoUrl(link);
    updatePhotoMutation.mutate({ assetId: listing.miAssetId, photoUrl: link });
  };

  const handlePhotoRemoval = () => {
    if (!photoDialogListing) return;
    setPendingPhotoUrl(null);
    updatePhotoMutation.mutate({ assetId: photoDialogListing.miAssetId, photoUrl: null });
  };

  const handleRestoreCart = () => {
    if (!savedCartMeta?.cartId) return;
    setCartId(savedCartMeta.cartId);
    void qc.invalidateQueries({ queryKey: ['marketplace-cart', savedCartMeta.cartId] });
    setToast('Carrito restaurado');
  };

  const handleRestoreBuyer = () => {
    if (!savedBuyerSnapshot) return;
    setBuyerName(savedBuyerSnapshot.name ?? '');
    setBuyerEmail(savedBuyerSnapshot.email ?? '');
    setBuyerPhone(savedBuyerSnapshot.phone ?? '');
    setContactPref(savedBuyerSnapshot.pref ?? 'email');
  };

  const handleClearBuyer = () => {
    setBuyerName('');
    setBuyerEmail('');
    setBuyerPhone('');
    setContactPref('email');
    if (typeof window !== 'undefined') {
      localStorage.removeItem(BUYER_INFO_KEY);
    }
    setSavedBuyerSnapshot(null);
  };

  const getStatusChipProps = (status?: string | null) => {
    if (!status) return null;
    const lower = status.toLowerCase();
    if (lower.includes('stock')) return { color: 'success' as const, icon: <CheckCircleIcon />, label: 'En stock' };
    if (lower.includes('reserva') || lower.includes('reservado')) return { color: 'warning' as const, icon: <InventoryIcon />, label: 'Reservado' };
    if (lower.includes('mantenimiento')) return { color: 'warning' as const, icon: <WarningAmberIcon />, label: 'Mantenimiento' };
    return { color: 'default' as const, icon: undefined, label: status };
  };
  const isListingAvailable = (status?: string | null) => {
    if (!status) return true;
    const lower = status.toLowerCase();
    return lower.includes('stock') || lower.includes('disponible');
  };
  const formatTokenAmount = (usdCents: number, usdPerToken: number | null) => {
    if (!usdPerToken || usdPerToken <= 0) return null;
    const amount = (usdCents / 100) / usdPerToken;
    if (!Number.isFinite(amount)) return null;
    return amount.toFixed(4);
  };
  const orderSummary = useMemo(() => {
    if (!lastOrder) return null;
    const summaryLines = lastOrder.moItems
      .map((item) => `${item.moiQuantity} × ${item.moiTitle || 'Ítem'} — ${item.moiSubtotalDisplay}`)
      .join('\n');
    const orderText = `Pedido ${lastOrder.moOrderId}\nTotal: ${lastOrder.moTotalDisplay}\nEstado: ${lastOrder.moStatus}\n${summaryLines}`;
    const statusMeta = getOrderStatusMeta(lastOrder.moStatus);
    const history = lastOrder.moStatusHistory ?? [];
    const lastHistoryEntry = history.length > 0 ? history[history.length - 1] : null;
    const lastUpdatedAt = lastOrder.moUpdatedAt ?? (lastHistoryEntry ? lastHistoryEntry[1] : null);
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
            <Stack direction="row" spacing={1} alignItems="center">
              <Chip label={statusMeta.label} color={statusMeta.color} size="small" />
              <Typography variant="body2" color="text.secondary">
                {statusMeta.desc || lastOrder.moStatus}
              </Typography>
            </Stack>
            <Button
              variant="text"
              size="small"
              component="a"
              href={`/marketplace/orden/${lastOrder.moOrderId}`}
              sx={{ alignSelf: 'flex-start', mt: 0.5, px: 0 }}
            >
              Ver seguimiento público
            </Button>
            <Button
              variant="text"
              size="small"
              startIcon={<ContentCopyIcon fontSize="small" />}
              sx={{ alignSelf: 'flex-start', px: 0 }}
              onClick={() => {
                const link = `${window.location.origin}/marketplace/orden/${lastOrder.moOrderId}`;
                if (navigator?.clipboard?.writeText) {
                  navigator.clipboard.writeText(link).then(
                    () => setToast('Enlace de seguimiento copiado'),
                    () => setToast('No pudimos copiar el enlace'),
                  );
                }
              }}
            >
              Copiar enlace
            </Button>
            {lastOrder.moPaymentProvider && (
              <Typography variant="caption" color="text.secondary">
                Pago: {lastOrder.moPaymentProvider?.toUpperCase()} {lastOrder.moPaypalOrderId ? ` · ${lastOrder.moPaypalOrderId}` : ''}
              </Typography>
            )}
            {lastOrder.moPaidAt && (
              <Typography variant="caption" color="text.secondary">
                Pagado el {new Date(lastOrder.moPaidAt).toLocaleString()}
              </Typography>
            )}
            {history.length > 0 && (
              <Stack spacing={0.5}>
                <Typography variant="caption" color="text.secondary">
                  Historial de estado
                </Typography>
                {history.map(([st, ts]) => (
                  <Typography key={`${st}-${ts}`} variant="caption" color="text.secondary">
                    {new Date(ts).toLocaleString()} — {st}
                  </Typography>
                ))}
              </Stack>
            )}
            {lastUpdatedAt && (
              <Typography variant="caption" color="text.secondary">
                Última actualización: {new Date(lastUpdatedAt).toLocaleString()}
              </Typography>
            )}
            <Button
              size="small"
              variant="outlined"
              onClick={() => {
                void navigator.clipboard.writeText(lastOrder.moOrderId).then(
                  () => setCopyToast('ID de pedido copiado'),
                  () => setCopyToast('No se pudo copiar el ID'),
                );
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
              onClick={() => {
                void navigator.clipboard.writeText(orderText).then(
                  () => setCopyToast('Resumen copiado'),
                  () => setCopyToast('No se pudo copiar el resumen'),
                );
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
        <Stack spacing={1}>
          <Typography variant="h4" fontWeight={800}>
            Marketplace de Inventario
          </Typography>
          <Typography variant="body2" color="text.secondary">
            1) Elige equipos, 2) Ingresa contacto, 3) Selecciona cómo pagar. Guarda tu carrito y retoma cuando quieras.
          </Typography>
          {canRestoreCart ? (
            <Alert
              severity="info"
              action={
                <Stack direction="row" spacing={1}>
                  <Button size="small" onClick={handleRestoreCart} variant="contained">
                    Recuperar carrito
                  </Button>
                  <Button size="small" onClick={scrollToListings} variant="text" color="inherit">
                    Ver catálogo
                  </Button>
                </Stack>
              }
            >
              Tienes un carrito guardado con {savedCartMeta.count} productos · actualizado{' '}
              {formatLastSavedTimestamp(savedCartMeta.updatedAt) ?? 'hace poco'}.
            </Alert>
          ) : null}
        </Stack>

        {listingsQuery.isLoading && (
          <Box display="flex" justifyContent="center">
            <CircularProgress />
          </Box>
        )}

        {listingsQuery.isError && (
          <Alert severity="error">No pudimos cargar el marketplace. Intenta de nuevo.</Alert>
        )}

        <Grid container spacing={3} id="marketplace-listings">
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
              <Stack direction="row" spacing={0.5} alignItems="center" sx={{ flexWrap: 'wrap' }}>
                <Typography variant="caption" color="text.secondary">
                  Modalidad
                </Typography>
                <Chip
                  label="Todo"
                  size="small"
                  variant={purpose === 'all' ? 'filled' : 'outlined'}
                  color={purpose === 'all' ? 'primary' : 'default'}
                  onClick={() => setPurpose('all')}
                />
                <Chip
                  label="Venta"
                  size="small"
                  variant={purpose === 'sale' ? 'filled' : 'outlined'}
                  color={purpose === 'sale' ? 'primary' : 'default'}
                  onClick={() => setPurpose('sale')}
                />
                <Chip
                  label="Renta"
                  size="small"
                  variant={purpose === 'rent' ? 'filled' : 'outlined'}
                  color={purpose === 'rent' ? 'primary' : 'default'}
                  onClick={() => setPurpose('rent')}
                />
              </Stack>
              {conditions.length > 0 && (
                <Stack direction="row" spacing={0.5} alignItems="center" sx={{ flexWrap: 'wrap' }}>
                  <Typography variant="caption" color="text.secondary">
                    Condición
                  </Typography>
                  <Chip
                    label="Todas"
                    size="small"
                    variant={condition === 'all' ? 'filled' : 'outlined'}
                    color={condition === 'all' ? 'primary' : 'default'}
                    onClick={() => setCondition('all')}
                  />
                  {conditions.map((c) => (
                    <Chip
                      key={c}
                      label={c}
                      size="small"
                      variant={condition === c ? 'filled' : 'outlined'}
                      color={condition === c ? 'primary' : 'default'}
                      onClick={() => setCondition(c || 'all')}
                    />
                  ))}
                </Stack>
              )}
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
                  <MenuItem value="relevance">Relevancia (disponible primero)</MenuItem>
                  <MenuItem value="price-asc">Precio: bajo a alto</MenuItem>
                  <MenuItem value="price-desc">Precio: alto a bajo</MenuItem>
                  <MenuItem value="title-asc">Título A-Z</MenuItem>
                </Select>
              </FormControl>
              <Chip label={`${filteredListings.length} resultados`} size="small" />
              {filtersActiveCount > 0 && (
                <Chip label={`Filtros activos: ${filtersActiveCount}`} size="small" color="primary" variant="outlined" />
              )}
              <Button variant="text" size="small" onClick={resetFilters}>
                Limpiar filtros
              </Button>
              {filtersActiveCount > 0 && (
                <Link
                  component="button"
                  variant="body2"
                  onClick={() => {
                    const url = window.location.href;
                    void navigator.clipboard.writeText(url).then(
                      () => setToast('Enlace de filtros copiado'),
                      () => setToast('No se pudo copiar el enlace'),
                    );
                  }}
                  sx={{ textDecoration: 'none' }}
                >
                  Copiar enlace de filtros
                </Link>
              )}
            </Stack>
            {filtersActiveCount > 0 && (
              <Stack direction="row" spacing={0.5} flexWrap="wrap" sx={{ mb: 1 }}>
                {category !== 'all' && <Chip label={`Categoría: ${category}`} size="small" />}
                {purpose !== 'all' && <Chip label={`Modalidad: ${purpose === 'sale' ? 'Venta' : 'Renta'}`} size="small" />}
                {condition !== 'all' && <Chip label={`Condición: ${condition}`} size="small" />}
                {sort !== 'relevance' && <Chip label={`Orden: ${sort}`} size="small" />}
                {search.trim() && <Chip label="Búsqueda activa" size="small" />}
              </Stack>
            )}
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
                        <Tooltip title={item.miTitle}>
                          <Typography variant="h6" fontWeight={700} noWrap>
                            {item.miTitle}
                          </Typography>
                        </Tooltip>
                        <Chip label={item.miCategory} size="small" />
                      </Stack>
                      <Tooltip title={`${item.miBrand ?? 'Sin marca'} ${item.miModel ?? ''}`}>
                        <Typography variant="body2" color="text.secondary" noWrap>
                          {item.miBrand ?? 'Sin marca'} {item.miModel ?? ''}
                        </Typography>
                      </Tooltip>
                      <Stack direction="row" spacing={1} flexWrap="wrap">
                        {item.miBrand && <Chip size="small" label={item.miBrand} variant="outlined" />}
                        {item.miModel && <Chip size="small" label={item.miModel} variant="outlined" />}
                        <Chip size="small" label={item.miCategory} color="default" variant="outlined" />
                        <Chip size="small" color="primary" label="Venta y Renta" />
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
                          position: 'relative',
                        }}
                      >
                        {!isListingAvailable(item.miStatus) && (
                          <Chip
                            size="small"
                            color="warning"
                            label={item.miStatus ?? 'No disponible'}
                            sx={{ position: 'absolute', top: 8, left: 8, bgcolor: 'warning.light', fontWeight: 600 }}
                          />
                        )}
                        {item.miPhotoUrl ? (
                          <Box
                            component="img"
                            src={item.miPhotoUrl}
                            alt={item.miTitle}
                            sx={{ width: '100%', height: '100%', objectFit: 'cover' }}
                            loading="lazy"
                          />
                        ) : (
                          <Stack spacing={0.75} alignItems="center" py={2.5} px={1.5} textAlign="center">
                            <InventoryIcon sx={{ color: 'text.secondary' }} />
                            <Typography variant="caption" color="text.secondary">
                              Sin foto
                            </Typography>
                            {!canManagePhotos && (
                              <Typography variant="caption" color="text.secondary">
                                Las fotos las gestiona Operación.
                              </Typography>
                            )}
                            {canManagePhotos && (
                              <Button
                                size="small"
                                variant="outlined"
                                startIcon={<AddPhotoAlternateIcon />}
                                onClick={() => openPhotoDialog(item)}
                                disabled={updatePhotoMutation.isPending}
                              >
                                Agregar foto
                              </Button>
                            )}
                          </Stack>
                        )}
                      </Box>
                      <Stack spacing={0.25}>
                        <Typography variant="h5" fontWeight={800}>
                          {item.miPriceDisplay}
                        </Typography>
                        {showTokenRates && (
                          <Typography variant="body2" color="text.secondary">
                            {adaUsdRate
                              ? `≈ ${formatTokenAmount(item.miPriceUsdCents, adaUsdRate) ?? '—'} ADA`
                              : null}
                            {adaUsdRate && sedUsdRate ? ' · ' : null}
                            {sedUsdRate
                              ? `≈ ${formatTokenAmount(item.miPriceUsdCents, sedUsdRate) ?? '—'} SED`
                              : null}
                          </Typography>
                        )}
                      </Stack>
                      {(() => {
                        const props = getStatusChipProps(item.miStatus);
                        return (
                          <Stack direction="row" spacing={1} flexWrap="wrap">
                            <Button
                              size="small"
                              variant="outlined"
                              startIcon={<ContentCopyIcon fontSize="small" />}
                              onClick={() => {
                                const detail = `${item.miTitle} · ${item.miBrand ?? ''} ${item.miModel ?? ''} · ${
                                  item.miPriceDisplay
                                }`;
                                void navigator.clipboard.writeText(detail.trim()).then(
                                  () => setToast('Detalle copiado'),
                                  () => setToast('No se pudo copiar el detalle'),
                                );
                              }}
                            >
                              Copiar detalle
                            </Button>
                            <Button size="small" variant="text" onClick={() => openDetail(item)}>
                              Ver detalles
                            </Button>
                            <Button
                              size="small"
                              variant="outlined"
                              onClick={() => {
                                const link = `${window.location.origin}/marketplace?listing=${item.miListingId}`;
                                void navigator.clipboard.writeText(link).then(
                                  () => setToast('Enlace copiado'),
                                  () => setToast('No se pudo copiar el enlace'),
                                );
                              }}
                            >
                              <ShareIcon fontSize="small" sx={{ mr: 0.5 }} />
                              Copiar enlace
                            </Button>
                            {props && (
                              <Chip
                                size="small"
                                color={props.color}
                                icon={props.icon}
                                label={props.label ?? item.miStatus}
                                sx={{ alignSelf: 'flex-start' }}
                              />
                            )}
                            {item.miCondition && (
                              <Chip size="small" label={`Condición: ${item.miCondition}`} variant="outlined" />
                            )}
                          </Stack>
                        );
                      })()}
                      <Stack direction="row" spacing={1} alignItems="center" sx={{ mt: 'auto' }}>
                        <Tooltip
                          title={
                            isListingAvailable(item.miStatus)
                              ? ''
                              : `No disponible para agregar (${item.miStatus ?? 'estado desconocido'})`
                          }
                          disableHoverListener={isListingAvailable(item.miStatus)}
                        >
                          <span>
                            <Button
                              variant="contained"
                              size="small"
                              startIcon={<ShoppingCartIcon />}
                              onClick={() => handleAdd(item)}
                              disabled={upsertItemMutation.isPending || !isListingAvailable(item.miStatus)}
                            >
                    {item.miPurpose === 'rent' ? 'Agregar renta' : 'Agregar'}
                  </Button>
                </span>
              </Tooltip>
                      </Stack>
                    </CardContent>
                  </Card>
                </Grid>
              ))}
            </Grid>
          </Grid>

          <Grid item xs={12} md={4}>
            <Stack spacing={2} sx={{ position: { md: 'sticky' }, top: { md: 16 }, alignSelf: { md: 'flex-start' } }}>
              {showRestoreBanner && savedCartMeta && (
                <Alert
                  severity="info"
                  action={
                    <Button size="small" onClick={handleRestoreCart}>
                      Recuperar
                    </Button>
                  }
                  onClose={() => setShowRestoreBanner(false)}
                >
                  Tienes un carrito guardado con {savedCartMeta.count} productos.
                </Alert>
              )}
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
                      {(savedCartMeta?.count ?? 0) > 0 && savedCartMeta?.cartId && (
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
                              handleUpdateQty(item, Math.max(0, parseInt(e.target.value ?? '0', 10)))
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
                      <Stack direction="row" spacing={1}>
                        <Button
                          variant="text"
                          color="inherit"
                          size="small"
                          onClick={() => {
                            void clearCart();
                          }}
                          disabled={upsertItemMutation.isPending}
                          sx={{ alignSelf: 'flex-start' }}
                        >
                          Vaciar carrito
                        </Button>
                        <Button
                          variant="outlined"
                          size="small"
                          onClick={() => {
                            openReview();
                          }}
                          disabled={!isValidName || !isValidEmail || cartItemCount === 0}
                        >
                          Continuar al checkout
                        </Button>
                      </Stack>
                    )}
                    {formatLastSavedTimestamp(savedCartMeta?.updatedAt) && (
                      <Typography variant="caption" color="text.secondary">
                        {formatLastSavedTimestamp(savedCartMeta?.updatedAt)}
                      </Typography>
                    )}
                  </Stack>
                </CardContent>
              </Card>

              <Card variant="outlined">
                <CardHeader title="Checkout" />
                <CardContent>
                  <Stack spacing={1.5}>
                    <Stack direction="row" spacing={1} alignItems="center">
                      <Chip label="Paso 1: contacto" size="small" color="primary" />
                      <Chip label="Paso 2: pago" size="small" color="secondary" variant="outlined" />
                    </Stack>
                    {!isValidName || !isValidEmail ? (
                      <Alert severity="info" variant="outlined">
                        Completa nombre y correo antes de elegir tarjeta o PayPal. Si prefieres coordinar por WhatsApp, elige
                        “Correo/WhatsApp”.
                      </Alert>
                    ) : null}
                    <Stack direction="row" spacing={1} justifyContent="flex-end">
                      <Button
                        size="small"
                        variant="text"
                        disabled={!savedBuyerSnapshot}
                        onClick={handleRestoreBuyer}
                      >
                        Usar datos guardados
                      </Button>
                      <Button size="small" variant="text" color="inherit" onClick={handleClearBuyer}>
                        Limpiar datos
                      </Button>
                    </Stack>
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
                    <Stack spacing={1}>
                      <Typography variant="caption" color="text.secondary">
                        Elige cómo pagar
                      </Typography>
                      <Stack direction="row" spacing={1} flexWrap="wrap">
                        <Chip
                          label="Coordinar por correo/WhatsApp"
                          color={paymentMethod === 'contact' ? 'primary' : 'default'}
                          variant={paymentMethod === 'contact' ? 'filled' : 'outlined'}
                          onClick={() => setPaymentMethod('contact')}
                          size="small"
                        />
                        <Chip
                          label="Tarjeta (Datafast)"
                          color={paymentMethod === 'card' ? 'primary' : 'default'}
                          variant={paymentMethod === 'card' ? 'filled' : 'outlined'}
                          onClick={() => setPaymentMethod('card')}
                          size="small"
                        />
                        <Chip
                          label="PayPal"
                          color={paymentMethod === 'paypal' ? 'primary' : 'default'}
                          variant={paymentMethod === 'paypal' ? 'filled' : 'outlined'}
                          onClick={() => setPaymentMethod('paypal')}
                          size="small"
                          disabled={!paypalClientId}
                        />
                      </Stack>
                      {!paypalClientId && (
                        <Typography variant="caption" color="text.secondary">
                          Configura VITE_PAYPAL_CLIENT_ID para habilitar PayPal.
                        </Typography>
                      )}
                    {paymentMethod === 'card' && (
                      <Stack direction="row" spacing={0.5} alignItems="center">
                        <CreditCardIcon fontSize="small" color="primary" />
                        <Typography variant="caption" color="text.secondary">
                          Aceptamos Visa, Mastercard, Diners, Discover y Amex.
                        </Typography>
                      </Stack>
                    )}
                    <Button
                      variant="contained"
                      onClick={openReview}
                      disabled={Boolean(checkoutDisabledReason)}
                    >
                      {paymentMethod === 'paypal'
                        ? 'Continuar con PayPal'
                        : paymentMethod === 'card'
                          ? 'Continuar con tarjeta'
                          : 'Confirmar pedido'}
                    </Button>
                    {checkoutDisabledReason && (
                      <Stack spacing={0.5}>
                        <Alert severity="info" variant="outlined">
                          {checkoutDisabledReason}
                        </Alert>
                        {!hasCartItems && (
                          <Button size="small" variant="text" onClick={scrollToListings} sx={{ alignSelf: 'flex-start' }}>
                            Volver al catálogo
                          </Button>
                        )}
                      </Stack>
                    )}
                  </Stack>
                  {checkoutMutation.isError && (
                    <Alert severity="error">No pudimos crear el pedido. Revisa tus datos.</Alert>
                  )}
                    {datafastError && (
                      <Alert severity="warning" onClose={() => setDatafastError(null)}>
                        {datafastError}
                      </Alert>
                    )}
                    {paypalError && (
                      <Alert severity="warning" onClose={() => setPaypalError(null)}>
                        {paypalError}
                      </Alert>
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
      <Dialog open={Boolean(photoDialogListing)} onClose={closePhotoDialog} maxWidth="sm" fullWidth>
        <DialogTitle>Gestionar foto</DialogTitle>
        <DialogContent dividers>
          <Stack spacing={2}>
            <Stack spacing={0.25}>
              <Typography variant="h6" fontWeight={700}>
                {photoDialogListing?.miTitle ?? 'Equipo'}
              </Typography>
              <Typography variant="body2" color="text.secondary">
                Sube una imagen y guardaremos el enlace público de Google Drive en el inventario y marketplace.
              </Typography>
            </Stack>
            {photoDialogListing?.miPhotoUrl && (
              <Box
                component="img"
                src={photoDialogListing.miPhotoUrl}
                alt={photoDialogListing.miTitle}
                sx={{ width: '100%', borderRadius: 1.5, maxHeight: 220, objectFit: 'cover', border: '1px solid', borderColor: 'divider' }}
              />
            )}
            <GoogleDriveUploadWidget
              label="Subir imagen (Drive)"
              helperText="JPG o PNG. El enlace quedará visible solo si el archivo es público."
              onComplete={handlePhotoUploadComplete}
              accept="image/*"
              multiple={false}
              dense
            />
            <Stack direction="row" spacing={1} alignItems="center">
              <Button
                variant="text"
                color="inherit"
                onClick={handlePhotoRemoval}
                disabled={
                  updatePhotoMutation.isPending ||
                  (!photoDialogListing?.miPhotoUrl && !pendingPhotoUrl)
                }
                startIcon={<DeleteIcon fontSize="small" />}
              >
                Quitar foto
              </Button>
              {updatePhotoMutation.isPending && (
                <Stack direction="row" spacing={0.5} alignItems="center">
                  <CircularProgress size={16} />
                  <Typography variant="caption" color="text.secondary">
                    Guardando foto...
                  </Typography>
                </Stack>
              )}
              {pendingPhotoUrl && (
                <Typography variant="caption" color="text.secondary" sx={{ wordBreak: 'break-all' }}>
                  Enlace: {pendingPhotoUrl}
                </Typography>
              )}
            </Stack>
            {photoError && (
              <Alert severity="error" onClose={() => setPhotoError(null)}>
                {photoError}
              </Alert>
            )}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={closePhotoDialog} color="inherit">
            Cerrar
          </Button>
        </DialogActions>
      </Dialog>
      <Dialog
        open={datafastDialogOpen}
        onClose={() => {
          setDatafastDialogOpen(false);
          setDatafastError(null);
        }}
        maxWidth="xs"
        fullWidth
      >
        <DialogTitle>Pagar con tarjeta</DialogTitle>
        <DialogContent dividers>
          <Stack spacing={1}>
            <Typography variant="body2" color="text.secondary">
              Paga con tarjeta de crédito o débito. Al finalizar serás redirigido para confirmar tu pedido. Si no
              ves el formulario, pulsa “Reintentar” y revisa tu conexión.
            </Typography>
            {datafastError && (
              <Alert severity="warning" onClose={() => setDatafastError(null)}>
                {datafastError}
              </Alert>
            )}
            {datafastCheckout && datafastReturnUrl && (
              <Box ref={datafastFormRef} key={datafastWidgetKey} sx={{ minHeight: 360, '& form': { width: '100%' } }}>
                <form
                  action={datafastReturnUrl}
                  className="paymentWidgets"
                  data-brands="VISA MASTER DINERS AMEX DISCOVER"
                ></form>
              </Box>
            )}
            {datafastCheckout && (
              <Alert severity="info" variant="outlined">
                {cartItems.length} artículo(s) · Total: {cartSubtotal}
              </Alert>
            )}
            {!datafastCheckout && (
              <Alert severity="info" variant="outlined">
                Preparando el pago…
              </Alert>
            )}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button
            onClick={() => {
              setDatafastError(null);
              setDatafastWidgetKey((k) => k + 1);
            }}
          >
            Reintentar
          </Button>
          <Button
            onClick={() => {
              setDatafastDialogOpen(false);
            }}
            color="inherit"
          >
            Cerrar
          </Button>
        </DialogActions>
      </Dialog>
      <Dialog open={reviewOpen} onClose={closeReview} maxWidth="sm" fullWidth>
        <DialogTitle>Revisa tu pedido</DialogTitle>
        <DialogContent dividers>
          <Stack spacing={1.5}>
            <Typography variant="subtitle1" fontWeight={700}>
              Total: {cartSubtotal}
            </Typography>
            <Stack spacing={0.5}>
              {cartItems.map((item) => (
                <Typography key={item.mciListingId} variant="body2">
                  {item.mciQuantity} × {item.mciTitle} — {item.mciSubtotalDisplay} ({item.mciUnitPriceDisplay} c/u)
                </Typography>
              ))}
            </Stack>
            <Typography variant="body2" color="text.secondary">
              Método de pago: {paymentMethod === 'paypal' ? 'PayPal' : paymentMethod === 'card' ? 'Tarjeta (Datafast)' : 'Coordinar por correo/WhatsApp'}
            </Typography>
            {paymentMethod === 'paypal' && !paypalClientId && (
              <Alert severity="warning">Falta configurar VITE_PAYPAL_CLIENT_ID para usar PayPal.</Alert>
            )}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={closeReview} color="inherit">
            Volver
          </Button>
          <Button
            variant="contained"
            onClick={() => {
              closeReview();
              handleContinueCheckout();
            }}
            disabled={
              checkoutMutation.isPending ||
              datafastCheckoutMutation.isPending ||
              createPaypalOrderMutation.isPending ||
              capturePaypalMutation.isPending ||
              (paymentMethod === 'paypal' && (!paypalClientId || !paypalReady))
            }
          >
            {paymentMethod === 'paypal'
              ? capturePaypalMutation.isPending
                ? 'Confirmando PayPal…'
                : createPaypalOrderMutation.isPending
                  ? 'Abriendo PayPal…'
                  : 'Pagar con PayPal'
              : paymentMethod === 'card'
                ? datafastCheckoutMutation.isPending
                  ? 'Abriendo pago con tarjeta…'
                  : 'Pagar con tarjeta'
                : checkoutMutation.isPending
                  ? 'Enviando pedido…'
                  : 'Confirmar pedido'}
          </Button>
        </DialogActions>
      </Dialog>
      <Dialog
        open={paypalDialogOpen}
        onClose={() => {
          setPaypalDialogOpen(false);
          setPaypalOrder(null);
        }}
        maxWidth="xs"
        fullWidth
      >
        <DialogTitle>Pagar con PayPal</DialogTitle>
        <DialogContent dividers>
          <Stack spacing={1}>
            <Typography variant="body2" color="text.secondary">
              Confirma tu pago de {cartSubtotal}. Se procesará con PayPal de forma segura.
            </Typography>
            {!paypalReady && <Alert severity="info">Cargando PayPal...</Alert>}
            <Box id="paypal-button-container" sx={{ mt: 1 }} />
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button
            onClick={() => {
              setPaypalDialogOpen(false);
              setPaypalOrder(null);
            }}
            color="inherit"
          >
            Cerrar
          </Button>
        </DialogActions>
      </Dialog>
      <Dialog open={detailOpen && Boolean(selectedListing)} onClose={closeDetail} maxWidth="sm" fullWidth>
        <DialogTitle>Detalle del equipo</DialogTitle>
        <DialogContent dividers>
          {selectedListing && (
            <Stack spacing={1.5}>
              <Stack direction="row" spacing={1} alignItems="center" justifyContent="space-between">
                <Typography variant="h6" fontWeight={700}>
                  {selectedListing.miTitle}
                </Typography>
                <Chip size="small" label={selectedListing.miCategory} />
              </Stack>
              <Typography variant="body2" color="text.secondary">
                {selectedListing.miBrand ?? 'Sin marca'} {selectedListing.miModel ?? ''}
              </Typography>
              {selectedListing.miPhotoUrl && (
                <Box
                  component="img"
                  src={selectedListing.miPhotoUrl}
                  alt={selectedListing.miTitle}
                  sx={{ width: '100%', borderRadius: 2, objectFit: 'cover', maxHeight: 260 }}
                />
              )}
              <Stack direction="row" spacing={1} flexWrap="wrap">
                <Chip size="small" label={selectedListing.miPurpose === 'rent' ? 'Renta' : 'Venta'} />
                {selectedListing.miCondition && <Chip size="small" label={`Condición: ${selectedListing.miCondition}`} />}
                {selectedListing.miStatus && <Chip size="small" label={selectedListing.miStatus} />}
              </Stack>
              <Stack spacing={0.25}>
                <Typography variant="h5" fontWeight={800}>
                  {selectedListing.miPriceDisplay}
                </Typography>
              </Stack>
              <Stack direction="row" spacing={1}>
                <Button
                  variant="contained"
                  size="small"
                  startIcon={<ShoppingCartIcon />}
                  onClick={() => {
                    handleAdd(selectedListing);
                    closeDetail();
                  }}
                  disabled={upsertItemMutation.isPending || !isListingAvailable(selectedListing.miStatus)}
                >
                  {selectedListing.miPurpose === 'rent' ? 'Agregar renta' : 'Agregar'}
                </Button>
                <Button
                  size="small"
                  variant="outlined"
                  onClick={() => {
                    const detail = `${selectedListing.miTitle} · ${selectedListing.miBrand ?? ''} ${selectedListing.miModel ?? ''} · ${
                      selectedListing.miPriceDisplay
                    }`;
                    void navigator.clipboard.writeText(detail.trim()).then(
                      () => setToast('Detalle copiado'),
                      () => setToast('No se pudo copiar el detalle'),
                    );
                  }}
                >
                  Copiar detalle
                </Button>
              </Stack>
            </Stack>
          )}
        </DialogContent>
        <DialogActions>
          <Button onClick={closeDetail} color="inherit">
            Cerrar
          </Button>
        </DialogActions>
      </Dialog>
    </Stack>
  </Box>
);
}
