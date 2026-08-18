import { useEffect, useMemo, useState, type ChangeEvent } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  CardHeader,
  Chip,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Divider,
  FormControlLabel,
  FormControl,
  Grid,
  IconButton,
  InputAdornment,
  InputLabel,
  Link,
  Menu,
  MenuItem,
  Paper,
  Select,
  Snackbar,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  TextField,
  Checkbox,
  Tooltip,
  Typography,
  type ChipProps,
} from '@mui/material';
import RefreshIcon from '@mui/icons-material/Refresh';
import LocalMallIcon from '@mui/icons-material/LocalMall';
import InventoryIcon from '@mui/icons-material/Inventory';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import ClearIcon from '@mui/icons-material/Clear';
import type {
  MarketplaceFulfillmentUpdatePayload,
  MarketplaceCommerceDTO,
  MarketplaceCustomerRequestDTO,
  MarketplaceDepositSettlementDTO,
  MarketplaceDepositSettlementSubmitPayload,
  MarketplaceOrderDTO,
  MarketplaceOrderUpdatePayload,
  MarketplaceRentalUpdatePayload,
} from '../api/types';
import {
  Marketplace,
  clearMarketplaceDepositIdempotencyKey,
  getMarketplaceDepositIdempotencyKey,
} from '../api/marketplace';
import { DateTime } from 'luxon';
import { Link as RouterLink } from 'react-router-dom';
import { useSession } from '../session/SessionContext';
import {
  applyMarketplaceOrderPreset,
  createDefaultMarketplaceOrderFilters,
  getMarketplacePaymentProviderLabel,
  getOrderStatusMeta,
  isPaidOrderStatus,
  summarizeMarketplaceOrderList,
  type MarketplaceOrderFilters,
} from '../utils/marketplace';
import ConfirmDialog from '../components/ConfirmDialog';
import LazyPaginatedList from '../components/LazyPaginatedList';

const STATUS_PRESETS: { value: string; label: string; color: ChipProps['color'] }[] = [
  { value: 'paid', label: 'Pagado', color: 'success' },
  { value: 'pending', label: 'Pendiente', color: 'warning' },
  { value: 'stripe_pending', label: 'Stripe pendiente', color: 'warning' },
  { value: 'stripe_failed', label: 'Stripe falló', color: 'error' },
  { value: 'paypal_pending', label: 'PayPal pendiente', color: 'info' },
  { value: 'datafast_init', label: 'Tarjeta iniciada', color: 'info' },
  { value: 'datafast_pending', label: 'Tarjeta en revisión', color: 'warning' },
  { value: 'datafast_failed', label: 'Tarjeta falló', color: 'error' },
  { value: 'contact', label: 'Contactar', color: 'default' },
  { value: 'cancelled', label: 'Cancelado', color: 'default' },
  { value: 'failed', label: 'Falló', color: 'error' },
  { value: 'refunded', label: 'Reembolsado', color: 'default' },
];

const FULFILLMENT_STATUS_LABELS: Record<string, string> = {
  on_hold: 'Reserva activa',
  ready_to_fulfill: 'Lista para preparar',
  picking: 'En preparación',
  ready_for_pickup: 'Lista para retiro',
  shipped: 'Enviada',
  delivered: 'Entregada',
  cancellation_requested: 'Cancelación solicitada',
  cancelled: 'Cancelada',
  return_requested: 'Devolución solicitada',
  return_authorized: 'Devolución autorizada',
  return_in_transit: 'Devolución en tránsito',
  returned: 'Devuelta',
  closed: 'Cerrada',
  expired: 'Reserva vencida',
};

const FULFILLMENT_METHOD_LABELS: Record<string, string> = {
  pickup: 'Retiro',
  local_delivery: 'Entrega local',
  shipping: 'Envío',
};

const RENTAL_STATUS_LABELS: Record<string, string> = {
  on_hold: 'Reserva con vencimiento',
  confirmed: 'Renta confirmada',
  ready_for_handoff: 'Lista para entrega',
  checked_out: 'En custodia del cliente',
  return_due: 'Devolución pendiente',
  returned_pending_inspection: 'Devuelta; inspección pendiente',
  damage_review: 'Daños en revisión',
  deposit_refund_due: 'Devolución de depósito pendiente',
  closed: 'Renta cerrada',
  cancellation_requested: 'Cancelación solicitada',
  cancelled: 'Renta cancelada',
  no_show: 'Cliente no se presentó',
  lost: 'Activo perdido',
  disputed: 'Renta disputada',
  expired: 'Reserva vencida',
};

const RENTAL_TRANSITIONS: Record<string, string[]> = {
  on_hold: ['confirmed', 'cancelled', 'expired'],
  confirmed: ['ready_for_handoff', 'cancellation_requested', 'no_show'],
  ready_for_handoff: ['checked_out', 'cancellation_requested', 'no_show'],
  checked_out: ['return_due', 'returned_pending_inspection', 'lost', 'disputed'],
  return_due: ['returned_pending_inspection', 'lost', 'disputed'],
  returned_pending_inspection: ['damage_review', 'deposit_refund_due', 'disputed'],
  damage_review: ['deposit_refund_due', 'disputed'],
  deposit_refund_due: ['closed', 'disputed'],
  cancellation_requested: ['cancelled'],
  no_show: ['cancelled'],
  lost: ['disputed'],
  disputed: ['damage_review', 'deposit_refund_due', 'closed'],
};

const rentalStatusLabel = (value?: string | null) => (
  value ? RENTAL_STATUS_LABELS[value] ?? value : 'Sin runtime de renta'
);

const COMMON_FULFILLMENT_TRANSITIONS: Record<string, string[]> = {
  on_hold: ['ready_to_fulfill', 'cancelled', 'expired'],
  ready_to_fulfill: ['picking', 'cancellation_requested'],
  picking: ['cancellation_requested'],
  ready_for_pickup: ['delivered', 'cancellation_requested'],
  shipped: ['delivered'],
  cancellation_requested: ['cancelled'],
  delivered: ['return_requested', 'closed'],
  return_requested: ['return_authorized', 'closed'],
  return_authorized: ['return_in_transit', 'returned'],
  return_in_transit: ['returned'],
  returned: ['closed'],
};

const fulfillmentStatusLabel = (value?: string | null) => (
  value ? FULFILLMENT_STATUS_LABELS[value] ?? value : 'Sin runtime de entrega'
);

const fulfillmentMethodLabel = (value?: string | null) => (
  value ? FULFILLMENT_METHOD_LABELS[value] ?? value : '—'
);

const fulfillmentTransitionsFor = (method?: string | null, current?: string | null) => {
  if (!method || !current) return [];
  const common = COMMON_FULFILLMENT_TRANSITIONS[current] ?? [];
  if (current !== 'picking') return common;
  return method === 'pickup'
    ? [...common, 'ready_for_pickup']
    : [...common, 'shipped'];
};

const QUICK_VIEW_PRESETS = [
  { value: 'last7', label: 'Últimos 7 días' },
  { value: 'paid', label: 'Pagado' },
  { value: 'stripe', label: 'Stripe' },
  { value: 'paypal', label: 'PayPal' },
  { value: 'card', label: 'Tarjeta pendiente' },
] as const;

type QuickViewPreset = (typeof QUICK_VIEW_PRESETS)[number]['value'];

const statusColor = (value: string): ChipProps['color'] => {
  const match = STATUS_PRESETS.find((p) => p.value === value);
  return match?.color ?? getOrderStatusMeta(value).color;
};

const statusLabel = (value: string): string => {
  const match = STATUS_PRESETS.find((p) => p.value === value);
  if (match) return match.label;
  return getOrderStatusMeta(value).label;
};

const formatDate = (iso?: string | null, withTime = true) => {
  if (!iso) return '—';
  const dt = DateTime.fromISO(iso);
  if (!dt.isValid) return '—';
  return withTime ? dt.toLocaleString(DateTime.DATETIME_SHORT) : dt.toLocaleString(DateTime.DATE_MED);
};

const formatInputDate = (iso?: string | null) => {
  if (!iso) return '';
  const dt = DateTime.fromISO(iso);
  if (!dt.isValid) return '';
  return dt.toFormat("yyyy-LL-dd'T'HH:mm");
};

const summarizeItems = (items: MarketplaceOrderDTO['moItems']) =>
  items.length > 0 ? items.map((it) => `${it.moiQuantity} × ${it.moiTitle}`).join(' · ') : 'Sin items';

const formatItemCountLabel = (items: MarketplaceOrderDTO['moItems']) =>
  items.length > 1 ? `${items.length} items` : '';

const normalizeProviderFilterValue = (value?: string | null) => value?.trim().toLowerCase() ?? '';
const normalizeBuyerPhoneValue = (value?: string | null) => value?.trim() ?? '';
const normalizeBuyerPhoneDigits = (value?: string | null) => normalizeBuyerPhoneValue(value).replace(/\D/g, '');
const normalizeEmailValue = (value?: string | null) => value?.trim() ?? '';
const normalizeEmailComparisonValue = (value?: string | null) => normalizeEmailValue(value).toLowerCase();
const getOrderBuyerIdentity = (
  order: Pick<MarketplaceOrderDTO, 'moBuyerName' | 'moBuyerEmail' | 'moBuyerPhone'>,
) => {
  const name = order.moBuyerName.trim();
  if (name) return name;

  const email = normalizeEmailValue(order.moBuyerEmail);
  if (email) return email;

  const phone = normalizeBuyerPhoneValue(order.moBuyerPhone);
  if (phone) return phone;

  return 'Sin comprador identificado';
};
const shouldShowBuyerEmailDetail = (
  email: string,
  buyerIdentity: string,
) => Boolean(email) && normalizeEmailComparisonValue(email) !== normalizeEmailComparisonValue(buyerIdentity);
const shouldShowBuyerPhoneDetail = (
  phone: string,
  buyerIdentity: string,
) => {
  if (!phone) return false;
  const identityDigits = normalizeBuyerPhoneDigits(buyerIdentity);
  return !identityDigits || normalizeBuyerPhoneDigits(phone) !== identityDigits;
};
const MIN_PHONE_SEARCH_DIGITS = 4;
const MIN_DEFAULT_CSV_EXPORT_ORDERS = 8;
const FIRST_ORDER_EMPTY_STATE_MESSAGE =
  'Todavía no hay órdenes. Comparte el marketplace para recibir la primera; cuando llegue, aparecerá aquí con estado, pago y datos del comprador.';
const formatPaymentProvider = (value?: string | null) => (
  normalizeProviderFilterValue(value) ? getMarketplacePaymentProviderLabel(value ?? '') : '—'
);
const normalizeOrderCurrency = (value: string) => value.trim().toUpperCase();
const getOrderCurrencyCaption = (order: Pick<MarketplaceOrderDTO, 'moCurrency' | 'moTotalDisplay'>) => {
  const currency = normalizeOrderCurrency(order.moCurrency);
  if (!currency) return '';
  return order.moTotalDisplay.toUpperCase().includes(currency) ? '' : currency;
};
const getDistinctPaypalPayerEmail = (
  order: Pick<MarketplaceOrderDTO, 'moBuyerEmail' | 'moPaypalPayerEmail'>,
) => {
  const payerEmail = normalizeEmailValue(order.moPaypalPayerEmail);
  if (!payerEmail) return '';
  return normalizeEmailComparisonValue(payerEmail) === normalizeEmailComparisonValue(order.moBuyerEmail)
    ? ''
    : payerEmail;
};

export default function MarketplaceOrdersPage() {
  const defaultFilters = createDefaultMarketplaceOrderFilters();
  const { session } = useSession();
  const isAuthed = Boolean(session);
  const qc = useQueryClient();
  const [statusFilter, setStatusFilter] = useState<string>(defaultFilters.statusFilter);
  const [providerFilter, setProviderFilter] = useState<string>(defaultFilters.providerFilter);
  const [fromDate, setFromDate] = useState<string>(defaultFilters.fromDate);
  const [toDate, setToDate] = useState<string>(defaultFilters.toDate);
  const [search, setSearch] = useState(defaultFilters.search);
  const [paidOnly, setPaidOnly] = useState(defaultFilters.paidOnly);
  const [showAdvancedFilters, setShowAdvancedFilters] = useState(false);
  const [selectedId, setSelectedId] = useState<string | null>(null);
  const [statusInput, setStatusInput] = useState<string>('');
  const [paymentProviderInput, setPaymentProviderInput] = useState<string>('');
  const [paidAtInput, setPaidAtInput] = useState<string>('');
  const [toast, setToast] = useState<string | null>(null);
  const [copyMenuAnchorEl, setCopyMenuAnchorEl] = useState<HTMLElement | null>(null);
  const [statusConfirmOpen, setStatusConfirmOpen] = useState(false);
  const [pendingSavePayload, setPendingSavePayload] = useState<{ id: string; payload: MarketplaceOrderUpdatePayload } | null>(null);
  const [fulfillmentStatusInput, setFulfillmentStatusInput] = useState('');
  const [fulfillmentCarrierInput, setFulfillmentCarrierInput] = useState('');
  const [fulfillmentTrackingInput, setFulfillmentTrackingInput] = useState('');
  const [fulfillmentReasonInput, setFulfillmentReasonInput] = useState('');
  const [fulfillmentNotesInput, setFulfillmentNotesInput] = useState('');
  const [rentalStatusInput, setRentalStatusInput] = useState('');
  const [rentalConditionOutInput, setRentalConditionOutInput] = useState('');
  const [rentalConditionInInput, setRentalConditionInInput] = useState('');
  const [rentalEvidenceUrlInput, setRentalEvidenceUrlInput] = useState('');
  const [rentalDepositDeductionInput, setRentalDepositDeductionInput] = useState('');
  const [rentalReasonInput, setRentalReasonInput] = useState('');
  const [rentalNotesInput, setRentalNotesInput] = useState('');
  const [manualReviewNotes, setManualReviewNotes] = useState('');
  const [customerRequestReviewNotes, setCustomerRequestReviewNotes] = useState('');
  const [depositSettlementMethod, setDepositSettlementMethod] = useState<MarketplaceDepositSettlementSubmitPayload['mdssSettlementMethod']>('bank_transfer');
  const [depositExternalReference, setDepositExternalReference] = useState('');
  const [depositEvidenceUrl, setDepositEvidenceUrl] = useState('');
  const [depositReviewNotes, setDepositReviewNotes] = useState('');

  const ordersQuery = useQuery<MarketplaceOrderDTO[], Error>({
    queryKey: ['marketplace-orders', statusFilter],
    queryFn: () =>
      Marketplace.listOrders({
        status: statusFilter === 'all' ? undefined : statusFilter,
        limit: 200,
      }),
    enabled: isAuthed,
    retry: false,
  });

  const orders = useMemo(() => ordersQuery.data ?? [], [ordersQuery.data]);
  const sortedOrders = useMemo(
    () => [...orders].sort((a, b) => b.moCreatedAt.localeCompare(a.moCreatedAt)),
    [orders],
  );
  const selectedOrder = useMemo(
    () => sortedOrders.find((o) => o.moOrderId === selectedId) ?? null,
    [sortedOrders, selectedId],
  );
  const marketplaceCommerceQuery = useQuery<MarketplaceCommerceDTO, Error>({
    queryKey: ['marketplace-order-commerce', selectedId],
    queryFn: () => Marketplace.getCommerce(selectedId!),
    enabled: isAuthed
      && Boolean(selectedId)
      && ['bank_transfer', 'cash', 'pos'].includes(selectedOrder?.moPaymentProvider ?? ''),
    retry: false,
  });
  const customerRequestsQuery = useQuery<MarketplaceCustomerRequestDTO[], Error>({
    queryKey: ['marketplace-customer-requests', selectedId],
    queryFn: () => Marketplace.listCustomerRequestsAdmin?.(selectedId!) ?? Promise.resolve([]),
    enabled: isAuthed && Boolean(selectedId),
    retry: false,
  });
  const depositSettlementsQuery = useQuery<MarketplaceDepositSettlementDTO[], Error>({
    queryKey: ['marketplace-deposit-settlements', selectedId],
    queryFn: () => Marketplace.listDepositSettlements?.(selectedId!) ?? Promise.resolve([]),
    enabled: isAuthed && Boolean(selectedId) && selectedOrder?.moOrderKind === 'rental',
    retry: false,
  });

  useEffect(() => {
    if (!selectedOrder) return;
    setStatusInput('');
    setPaymentProviderInput(selectedOrder.moPaymentProvider ?? '');
    setPaidAtInput(formatInputDate(selectedOrder.moPaidAt));
    setFulfillmentStatusInput('');
    setFulfillmentCarrierInput('');
    setFulfillmentTrackingInput(selectedOrder.moTrackingReference ?? '');
    setFulfillmentReasonInput('');
    setFulfillmentNotesInput('');
    setRentalStatusInput('');
    setRentalConditionOutInput(selectedOrder.moConditionOut ?? '');
    setRentalConditionInInput(selectedOrder.moConditionIn ?? '');
    setRentalEvidenceUrlInput('');
    setRentalDepositDeductionInput(String(selectedOrder.moDepositDeductionUsdCents ?? ''));
    setRentalReasonInput('');
    setRentalNotesInput('');
    setManualReviewNotes('');
    setCustomerRequestReviewNotes('');
    setDepositSettlementMethod('bank_transfer');
    setDepositExternalReference('');
    setDepositEvidenceUrl('');
    setDepositReviewNotes('');
  }, [selectedOrder]);

  const statusFilterImpliesPaid = statusFilter !== 'all' && isPaidOrderStatus(statusFilter);
  const activePaidOnlyFilter = paidOnly && !statusFilterImpliesPaid;

  useEffect(() => {
    if (!statusFilterImpliesPaid || !paidOnly) return;
    setPaidOnly(false);
  }, [paidOnly, statusFilterImpliesPaid]);

  const normalizedProviderFilter = normalizeProviderFilterValue(providerFilter);

  const baseContextOrders = useMemo(() => {
    const term = search.trim().toLowerCase();
    const termDigits = normalizeBuyerPhoneDigits(search);
    const isPhoneLikeSearch = /^[\d\s()+.-]+$/.test(search.trim());
    const fromDt = fromDate ? DateTime.fromISO(fromDate) : null;
    const toDt = toDate ? DateTime.fromISO(toDate).endOf('day') : null;
    return sortedOrders.filter((order) => {
      if (activePaidOnlyFilter && !order.moPaidAt) return false;
      const created = DateTime.fromISO(order.moCreatedAt);
      if (fromDt && created < fromDt) return false;
      if (toDt && created > toDt) return false;
      if (!term) return true;
      const haystack = [
        order.moOrderId,
        order.moBuyerName,
        order.moBuyerEmail,
        normalizeBuyerPhoneValue(order.moBuyerPhone),
        order.moStatus,
        order.moPaymentProvider ?? '',
      ]
        .join(' ')
        .toLowerCase();
      if (haystack.includes(term)) return true;
      return isPhoneLikeSearch
        && termDigits.length >= MIN_PHONE_SEARCH_DIGITS
        && normalizeBuyerPhoneDigits(order.moBuyerPhone).includes(termDigits);
    });
  }, [sortedOrders, search, fromDate, toDate, activePaidOnlyFilter]);

  const statusContextOrders = useMemo(() => {
    if (normalizedProviderFilter === 'all') return baseContextOrders;
    return baseContextOrders.filter(
      (order) => normalizeProviderFilterValue(order.moPaymentProvider) === normalizedProviderFilter,
    );
  }, [baseContextOrders, normalizedProviderFilter]);

  const availableStatusFilters = useMemo(
    () =>
      Array.from(
        new Set(statusContextOrders.map((order) => order.moStatus.trim()).filter(Boolean)),
      ),
    [statusContextOrders],
  );
  const statusFilterOptions = useMemo(() => {
    const availableValues = new Set(availableStatusFilters);
    if (availableStatusFilters.length > 0) {
      availableValues.add('paid');
    }
    if (statusFilter !== 'all') {
      availableValues.add(statusFilter);
    }

    const presetOptions = STATUS_PRESETS
      .filter((preset) => availableValues.has(preset.value))
      .map(({ value, label }) => ({ value, label }));
    const presetValues = new Set(presetOptions.map((option) => option.value));
    const customOptions = Array.from(availableValues)
      .filter((value) => value && !presetValues.has(value))
      .sort((left, right) => statusLabel(left).localeCompare(statusLabel(right), 'es'))
      .map((value) => ({ value, label: statusLabel(value) }));

    return [...presetOptions, ...customOptions];
  }, [availableStatusFilters, statusFilter]);

  const showStatusFilter = statusFilter !== 'all' || availableStatusFilters.length > 1;
  const singleVisibleStatusLabel =
    !showStatusFilter && statusContextOrders.length > 0
      ? statusLabel(availableStatusFilters[0] ?? '')
      : '';
  const statusFilterHelperText = (() => {
    if (statusFilter !== 'all' || statusContextOrders.length === 0 || showStatusFilter) return null;
    return `Todos los pedidos visibles comparten el estado ${singleVisibleStatusLabel}. El filtro de estado aparecerá cuando esta vista mezcle más de un estado.`;
  })();

  const providerContextOrders = useMemo(() => {
    if (statusFilter === 'all') return baseContextOrders;
    return baseContextOrders.filter((order) => order.moStatus === statusFilter);
  }, [baseContextOrders, statusFilter]);

  const availableProviderFilters = useMemo(
    () =>
      Array.from(
        new Set(
          providerContextOrders
            .map((order) => normalizeProviderFilterValue(order.moPaymentProvider))
            .filter(Boolean),
        ),
      ),
    [providerContextOrders],
  );
  const hasOrdersWithoutProvider = useMemo(
    () => providerContextOrders.some((order) => !normalizeProviderFilterValue(order.moPaymentProvider)),
    [providerContextOrders],
  );
  const showProviderFilter =
    providerFilter !== 'all' ||
    availableProviderFilters.length > 1 ||
    (availableProviderFilters.length === 1 && hasOrdersWithoutProvider);
  const singleVisibleProviderSummary = (() => {
    if (showProviderFilter || providerContextOrders.length === 0) return '';
    if (availableProviderFilters.length === 0) return 'todavía no tienen método de pago registrado';
    return `usan ${getMarketplacePaymentProviderLabel(availableProviderFilters[0] ?? '')}`;
  })();
  const providerFilterHelperText = (() => {
    if (providerFilter !== 'all' || providerContextOrders.length === 0 || showProviderFilter) return null;
    if (availableProviderFilters.length === 0) {
      return 'Los pedidos visibles todavía no tienen método de pago registrado.';
    }
    return `Todos los pedidos visibles usan ${getMarketplacePaymentProviderLabel(availableProviderFilters[0] ?? '')}. El filtro de método aparecerá cuando esta vista mezcle más de un canal de pago.`;
  })();
  const combinedFilterContextHelperText =
    statusFilterHelperText && providerFilterHelperText && singleVisibleStatusLabel && singleVisibleProviderSummary
      ? `Todos los pedidos visibles comparten el estado ${singleVisibleStatusLabel} y ${singleVisibleProviderSummary}. Los filtros de estado y método aparecerán cuando esta vista mezcle más de un estado o canal de pago.`
      : null;
  const filtered = useMemo(() => {
    return baseContextOrders.filter((order) => {
      if (statusFilter !== 'all' && order.moStatus !== statusFilter) return false;
      if (normalizedProviderFilter !== 'all' && normalizeProviderFilterValue(order.moPaymentProvider) !== normalizedProviderFilter) {
        return false;
      }
      return true;
    });
  }, [baseContextOrders, normalizedProviderFilter, statusFilter]);
  const sharedVisibleCurrencyCaption = useMemo(() => {
    if (filtered.length < 2) return '';
    const captions = filtered.map(getOrderCurrencyCaption);
    const [firstCaption] = captions;
    if (!firstCaption) return '';
    return captions.every((caption) => caption === firstCaption) ? firstCaption : '';
  }, [filtered]);
  const visiblePaymentProviderValues = useMemo(
    () => filtered.map((order) => normalizeProviderFilterValue(order.moPaymentProvider)),
    [filtered],
  );
  const visiblePaymentProviderSet = useMemo(
    () => new Set(visiblePaymentProviderValues.filter(Boolean)),
    [visiblePaymentProviderValues],
  );
  const hasVisibleOrdersWithoutPaymentProvider = visiblePaymentProviderValues.some((provider) => !provider);
  const hasVisiblePayerEmail = filtered.some((order) => getDistinctPaypalPayerEmail(order) !== '');
  const showPaymentProviderColumn =
    hasVisiblePayerEmail
    || visiblePaymentProviderSet.size > 1
    || (visiblePaymentProviderSet.size === 1 && (filtered.length === 1 || hasVisibleOrdersWithoutPaymentProvider));
  const showBuyerPhoneColumn = filtered.some((order) => (
    shouldShowBuyerPhoneDetail(normalizeBuyerPhoneValue(order.moBuyerPhone), getOrderBuyerIdentity(order))
  ));
  const showPaidAtColumn = filtered.some((order) => Boolean(order.moPaidAt));
  const showItemsColumn = filtered.some((order) => order.moItems.length > 0);
  const showSharedEmptyItemsSummary = filtered.length > 1 && !showItemsColumn;
  const visibleStatusLabelSet = useMemo(
    () => new Set(filtered.map((order) => statusLabel(order.moStatus))),
    [filtered],
  );
  const allVisibleOrdersShareStatus = filtered.length > 1 && visibleStatusLabelSet.size === 1;
  const showStatusColumn = !allVisibleOrdersShareStatus || (showStatusFilter && statusFilter === 'all');

  const filtersDirty =
    statusFilter !== 'all'
    || providerFilter !== 'all'
    || search.trim() !== ''
    || Boolean(fromDate)
    || Boolean(toDate)
    || activePaidOnlyFilter;
  const hasSearchInput = search.trim() !== '';
  const hasNonSearchFiltersActive =
    statusFilter !== 'all'
    || providerFilter !== 'all'
    || Boolean(fromDate)
    || Boolean(toDate)
    || activePaidOnlyFilter;
  const nonSearchFiltersActiveCount =
    (statusFilter !== 'all' ? 1 : 0) +
    (providerFilter !== 'all' ? 1 : 0) +
    (fromDate ? 1 : 0) +
    (toDate ? 1 : 0) +
    (activePaidOnlyFilter ? 1 : 0);
  const showSearchWithExtraFilters = hasSearchInput && hasNonSearchFiltersActive;
  const showSearchOwnedFilterHelper = hasSearchInput && !hasNonSearchFiltersActive;
  const filtersActiveCount =
    nonSearchFiltersActiveCount +
    (search.trim() ? 1 : 0);
  const visiblePaidCount = filtered.filter((o) => isPaidOrderStatus(o.moStatus)).length;
  const visiblePendingCount = Math.max(filtered.length - visiblePaidCount, 0);
  const showVisibleOrderBreakdown = visiblePaidCount > 0 && visiblePendingCount > 0;
  const showExportCsvAction =
    filtered.length > 0
    && (filtersDirty || filtered.length >= MIN_DEFAULT_CSV_EXPORT_ORDERS);
  const paidTotal = orders.filter((o) => isPaidOrderStatus(o.moStatus)).length;
  const paidVisible = filtered.filter((o) => isPaidOrderStatus(o.moStatus)).length;
  const ordersSummary = filtersDirty && orders.length === 0
    ? `Sin resultados en esta vista. ${filtersActiveCount} filtro${filtersActiveCount === 1 ? '' : 's'} activo${filtersActiveCount === 1 ? '' : 's'}.`
    : summarizeMarketplaceOrderList({
      totalOrders: orders.length,
      visibleOrders: filtered.length,
      activeFilterCount: filtersActiveCount,
    });
  const showFirstOrderEmptyState = !filtersDirty && !ordersQuery.isLoading && !ordersQuery.isError && orders.length === 0;
  const showSingleOrderFocusedState =
    !ordersQuery.isLoading && !ordersQuery.isError && orders.length === 1 && !filtersDirty;
  const showSingleVisibleOrderSummary =
    !ordersQuery.isLoading && !ordersQuery.isError && filtered.length === 1;
  const showEmptyOrdersState =
    !showFirstOrderEmptyState && !ordersQuery.isLoading && !ordersQuery.isError && filtered.length === 0;
  const showInitialOrdersErrorState = ordersQuery.isError && orders.length === 0;
  const ordersErrorMessage = ordersQuery.error?.message ?? 'Error al cargar órdenes';
  const showFilteredSingleOrderSummary = showSingleVisibleOrderSummary && !showSingleOrderFocusedState;
  const singleVisibleOrder = showSingleVisibleOrderSummary ? (filtered[0] ?? null) : null;
  const singleVisibleBuyerIdentity = singleVisibleOrder ? getOrderBuyerIdentity(singleVisibleOrder) : '';
  const singleVisibleBuyerEmail = singleVisibleOrder ? normalizeEmailValue(singleVisibleOrder.moBuyerEmail) : '';
  const singleVisibleBuyerPhone = singleVisibleOrder ? normalizeBuyerPhoneValue(singleVisibleOrder.moBuyerPhone) : '';
  const showSingleVisibleBuyerEmail = shouldShowBuyerEmailDetail(
    singleVisibleBuyerEmail,
    singleVisibleBuyerIdentity,
  );
  const showSingleVisibleBuyerPhone = shouldShowBuyerPhoneDetail(
    singleVisibleBuyerPhone,
    singleVisibleBuyerIdentity,
  );
  const showSingleVisibleContactEmptyState = Boolean(
    singleVisibleOrder && !singleVisibleBuyerEmail && !singleVisibleBuyerPhone,
  );
  const singleVisibleItemsSummary = singleVisibleOrder && singleVisibleOrder.moItems.length > 0
    ? summarizeItems(singleVisibleOrder.moItems)
    : '';
  const singleVisibleOrderSummaryText = showSingleOrderFocusedState
    ? 'Solo hay una orden por ahora. Revisa estado, pago y datos del comprador desde este resumen. Cuando llegue la segunda, aquí aparecerán filtros y exportación.'
    : showSearchWithExtraFilters
      ? 'La búsqueda y los filtros dejaron una sola orden visible. Revísala aquí y usa Limpiar dentro del campo o Limpiar otros filtros para volver a comparar pedidos.'
      : showSearchOwnedFilterHelper
        ? 'La búsqueda dejó una sola orden visible. Revísala aquí y usa Limpiar dentro del campo para volver a comparar pedidos.'
        : 'Los filtros dejaron una sola orden visible. Revísala aquí y usa Limpiar filtros para volver a comparar pedidos.';
  const showOrderListHeaderActions =
    !showFirstOrderEmptyState
    && !showSingleVisibleOrderSummary
    && (showVisibleOrderBreakdown || showExportCsvAction);
  const showListChrome = !ordersQuery.isLoading && (filtersDirty || (orders.length > 0 && !showSingleOrderFocusedState));
  const showQuickViewControl = !filtersDirty;
  const showActiveFiltersTray = hasNonSearchFiltersActive;
  const showStatusFilterTrayChip = statusFilter !== 'all' && !showSingleVisibleOrderSummary;
  const showProviderFilterTrayChip = providerFilter !== 'all' && !showSingleVisibleOrderSummary;
  const showCopyFiltersLinkAction = filtersActiveCount > 0 && !showSingleVisibleOrderSummary;
  const showFilterTrayHelper = showSearchOwnedFilterHelper && !showEmptyOrdersState;
  const showPaidOnlyAdvancedFilter = !statusFilterImpliesPaid;
  const hasAdvancedFiltersActive = Boolean(fromDate) || Boolean(toDate) || activePaidOnlyFilter;
  const advancedFiltersButtonSubject = showPaidOnlyAdvancedFilter ? 'fechas y pago' : 'fechas';
  const advancedFiltersButtonLabel = showAdvancedFilters
    ? `Ocultar ${advancedFiltersButtonSubject}`
    : hasAdvancedFiltersActive
      ? `Editar ${advancedFiltersButtonSubject}`
      : `Mostrar ${advancedFiltersButtonSubject}`;
  const showHeaderRefreshAction =
    (Boolean(ordersQuery.error) && !showInitialOrdersErrorState)
    || (
      !ordersQuery.isLoading
      && !showEmptyOrdersState
      && !showSingleVisibleOrderSummary
      && !hasSearchInput
      && (orders.length > 1 || filtersDirty)
    );
  const emptyOrdersMessage = showSearchOwnedFilterHelper
    ? 'No hay órdenes para la búsqueda actual. Usa Limpiar dentro del campo de búsqueda para volver a la bandeja completa.'
    : showSearchWithExtraFilters
      ? 'No hay órdenes en la vista actual. Usa Limpiar otros filtros para conservar la búsqueda o Limpiar dentro del campo para volver a la bandeja completa.'
      : 'No hay órdenes en la vista actual. Usa Limpiar filtros para volver a la bandeja completa.';
  const searchOwnedFilterHelperText =
    'La búsqueda activa se maneja desde el campo superior. Usa Limpiar ahí para volver a la bandeja completa. Los demás filtros aparecerán aquí cuando combines más criterios.';
  const clearFiltersActionLabel = showSearchWithExtraFilters ? 'Limpiar otros filtros' : 'Limpiar filtros';
  const paidOrdersRecoveryMessage = activePaidOnlyFilter
    ? 'Hay órdenes pagadas, pero no coinciden con los filtros actuales. Ajusta los filtros o desmarca "Solo con pago".'
    : 'Hay órdenes pagadas, pero no coinciden con los filtros actuales. Ajusta los filtros para volver a incluirlas.';

  const exportCsv = () => {
    if (filtered.length === 0) return;
    const header = ['pedido', 'estado', 'pago', 'total', 'moneda', 'comprador', 'email', 'teléfono', 'creado', 'pagado', 'items'];
    const escape = (val: string | number | null | undefined) => {
      const safe = val ?? '';
      return `"${String(safe).replace(/"/g, '""')}"`;
    };
    const rows = filtered.map((o) => [
      o.moOrderId,
      o.moStatus,
      o.moPaymentProvider ?? '',
      o.moTotalDisplay,
      o.moCurrency,
      o.moBuyerName,
      o.moBuyerEmail,
      o.moBuyerPhone ?? '',
      o.moCreatedAt,
      o.moPaidAt ?? '',
      summarizeItems(o.moItems),
    ]);
    const csv = [header, ...rows].map((cols) => cols.map(escape).join(',')).join('\n');
    const blob = new Blob([csv], { type: 'text/csv;charset=utf-8;' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = `ordenes-marketplace-${Date.now()}.csv`;
    link.click();
    URL.revokeObjectURL(url);
  };

  const copyFiltersLink = () => {
    const url = new URL(window.location.href);
    const params = url.searchParams;
    if (statusFilter !== 'all') params.set('status', statusFilter);
    else params.delete('status');
    if (providerFilter !== 'all') params.set('provider', providerFilter);
    else params.delete('provider');
    if (activePaidOnlyFilter) params.set('paidOnly', '1');
    else params.delete('paidOnly');
    if (search.trim()) params.set('q', search.trim());
    else params.delete('q');
    if (fromDate) params.set('from', fromDate);
    else params.delete('from');
    if (toDate) params.set('to', toDate);
    else params.delete('to');
    url.search = params.toString();
    void navigator.clipboard.writeText(url.toString()).then(
      () => setToast('Enlace de filtros copiado'),
      () => setToast('No se pudo copiar el enlace'),
    );
  };

  const updateMutation = useMutation<MarketplaceOrderDTO, Error, { id: string; payload: MarketplaceOrderUpdatePayload }>({
    mutationFn: ({ id, payload }) => Marketplace.updateOrder(id, payload),
    onSuccess: (data) => {
      qc.setQueryData(['marketplace-orders', statusFilter], (prev: MarketplaceOrderDTO[] | undefined) =>
        prev ? prev.map((o) => (o.moOrderId === data.moOrderId ? data : o)) : prev,
      );
      void qc.invalidateQueries({ queryKey: ['marketplace-orders'] });
      setToast('Orden actualizada');
      closeDialog();
    },
  });

  const fulfillmentMutation = useMutation<
    MarketplaceOrderDTO,
    Error,
    { id: string; payload: MarketplaceFulfillmentUpdatePayload }
  >({
    mutationFn: ({ id, payload }) => Marketplace.updateFulfillment(id, payload),
    onSuccess: (data) => {
      qc.setQueryData(['marketplace-orders', statusFilter], (prev: MarketplaceOrderDTO[] | undefined) =>
        prev ? prev.map((o) => (o.moOrderId === data.moOrderId ? data : o)) : prev,
      );
      void qc.invalidateQueries({ queryKey: ['marketplace-orders'] });
      setFulfillmentStatusInput('');
      setFulfillmentCarrierInput('');
      setFulfillmentReasonInput('');
      setFulfillmentNotesInput('');
      setFulfillmentTrackingInput(data.moTrackingReference ?? '');
      setToast('Entrega actualizada');
    },
  });

  const rentalMutation = useMutation<
    MarketplaceOrderDTO,
    Error,
    { id: string; payload: MarketplaceRentalUpdatePayload }
  >({
    mutationFn: ({ id, payload }) => Marketplace.updateRental(id, payload),
    onSuccess: (data) => {
      qc.setQueryData(['marketplace-orders', statusFilter], (prev: MarketplaceOrderDTO[] | undefined) =>
        prev ? prev.map((order) => (order.moOrderId === data.moOrderId ? data : order)) : prev,
      );
      void qc.invalidateQueries({ queryKey: ['marketplace-orders'] });
      setRentalStatusInput('');
      setRentalEvidenceUrlInput('');
      setRentalReasonInput('');
      setRentalNotesInput('');
      setToast('Renta actualizada');
    },
  });

  const manualReviewMutation = useMutation<
    MarketplaceCommerceDTO,
    Error,
    'approve' | 'reject'
  >({
    mutationFn: (action) => Marketplace.reviewManualPayment(
      selectedId!,
      action,
      manualReviewNotes.trim(),
    ),
    onSuccess: (data, action) => {
      qc.setQueryData(['marketplace-order-commerce', data.mpcOrderId], data);
      void qc.invalidateQueries({ queryKey: ['marketplace-orders'] });
      setManualReviewNotes('');
      setToast(action === 'approve'
        ? 'Pago manual verificado; la entrega o custodia sigue separada'
        : 'Evidencia rechazada; la orden continúa impaga');
    },
  });

  const customerRequestReviewMutation = useMutation<
    MarketplaceCustomerRequestDTO,
    Error,
    { requestId: string; action: 'approve' | 'reject' | 'needs_quote' }
  >({
    mutationFn: ({ requestId, action }) => Marketplace.reviewCustomerRequest(
      selectedId!, requestId, action, customerRequestReviewNotes.trim(),
    ),
    onSuccess: (updated) => {
      qc.setQueryData<MarketplaceCustomerRequestDTO[]>(
        ['marketplace-customer-requests', updated.mcrOrderId],
        (current) => current?.map((request) =>
          request.mcrRequestId === updated.mcrRequestId ? updated : request) ?? [updated],
      );
      void qc.invalidateQueries({ queryKey: ['marketplace-orders'] });
      setCustomerRequestReviewNotes('');
      setToast('Solicitud revisada; pago y cumplimiento permanecen separados');
    },
  });

  const depositSettlementMutation = useMutation<
    MarketplaceDepositSettlementDTO,
    Error,
    MarketplaceDepositSettlementSubmitPayload
  >({
    mutationFn: (payload) => Marketplace.submitDepositSettlement(
      selectedId!, payload, getMarketplaceDepositIdempotencyKey(selectedId!),
    ),
    onSuccess: (created) => {
      clearMarketplaceDepositIdempotencyKey(created.mdsOrderId);
      qc.setQueryData<MarketplaceDepositSettlementDTO[]>(
        ['marketplace-deposit-settlements', created.mdsOrderId],
        (current) => [created, ...(current ?? []).filter((entry) =>
          entry.mdsSettlementId !== created.mdsSettlementId)],
      );
      setDepositExternalReference('');
      setDepositEvidenceUrl('');
      setToast('Evidencia de devolución enviada; aún requiere revisión independiente');
    },
  });

  const depositReviewMutation = useMutation<
    MarketplaceDepositSettlementDTO,
    Error,
    { settlementId: string; action: 'approve' | 'reject' | 'requires_reconciliation' }
  >({
    mutationFn: ({ settlementId, action }) => Marketplace.reviewDepositSettlement(
      selectedId!, settlementId, action, depositReviewNotes.trim(),
    ),
    onSuccess: (updated, variables) => {
      qc.setQueryData<MarketplaceDepositSettlementDTO[]>(
        ['marketplace-deposit-settlements', updated.mdsOrderId],
        (current) => current?.map((entry) =>
          entry.mdsSettlementId === updated.mdsSettlementId ? updated : entry) ?? [updated],
      );
      void qc.invalidateQueries({ queryKey: ['marketplace-orders'] });
      setDepositReviewNotes('');
      setToast(variables.action === 'approve'
        ? 'Devolución manual verificada y contabilizada; no se registró un reembolso de proveedor'
        : 'Evidencia de depósito revisada sin afirmar movimiento de proveedor');
    },
  });

  const handleRefresh = () => {
    void qc.invalidateQueries({ queryKey: ['marketplace-orders'] });
  };

  const applyFilters = (nextFilters: MarketplaceOrderFilters) => {
    setStatusFilter(nextFilters.statusFilter);
    setProviderFilter(nextFilters.providerFilter);
    setSearch(nextFilters.search);
    setFromDate(nextFilters.fromDate);
    setToDate(nextFilters.toDate);
    setPaidOnly(nextFilters.paidOnly);
  };

  const clearFilters = () => {
    if (hasSearchInput) {
      setStatusFilter(defaultFilters.statusFilter);
      setProviderFilter(defaultFilters.providerFilter);
      setFromDate(defaultFilters.fromDate);
      setToDate(defaultFilters.toDate);
      setPaidOnly(defaultFilters.paidOnly);
    } else {
      applyFilters(createDefaultMarketplaceOrderFilters());
    }
    setShowAdvancedFilters(false);
  };

  const applyPreset = (preset: QuickViewPreset) => {
    applyFilters(applyMarketplaceOrderPreset(preset));
  };

  const openOrder = (id: string) => {
    setSelectedId(id);
    setStatusInput('');
    setPaymentProviderInput('');
    setPaidAtInput('');
  };

  const closeDialog = () => {
    setSelectedId(null);
    setStatusInput('');
    setPaymentProviderInput('');
    setPaidAtInput('');
    setFulfillmentStatusInput('');
    setFulfillmentCarrierInput('');
    setFulfillmentTrackingInput('');
    setFulfillmentReasonInput('');
    setFulfillmentNotesInput('');
    setRentalStatusInput('');
    setRentalConditionOutInput('');
    setRentalConditionInInput('');
    setRentalEvidenceUrlInput('');
    setRentalDepositDeductionInput('');
    setRentalReasonInput('');
    setRentalNotesInput('');
    setManualReviewNotes('');
    setCopyMenuAnchorEl(null);
    updateMutation.reset();
    fulfillmentMutation.reset();
    rentalMutation.reset();
    manualReviewMutation.reset();
  };

  const closeCopyMenu = () => {
    setCopyMenuAnchorEl(null);
  };

  const runCopyMenuAction = (action: (order: MarketplaceOrderDTO) => void | Promise<void>) => {
    const order = selectedOrder;
    closeCopyMenu();
    if (order) {
      void action(order);
    }
  };

  const executeSave = async (saveData: { id: string; payload: MarketplaceOrderUpdatePayload }) => {
    await updateMutation.mutateAsync(saveData);
    setPendingSavePayload(null);
  };

  const handleSave = async () => {
    if (!selectedOrder) return;
    const payload: MarketplaceOrderUpdatePayload = {};
    const nextStatus = statusInput.trim();
    if (nextStatus && nextStatus !== selectedOrder.moStatus) {
      payload.mouStatus = nextStatus;
    }
    const normalizedProvider = paymentProviderInput.trim();
    if (normalizedProvider !== (selectedOrder.moPaymentProvider ?? '')) {
      payload.mouPaymentProvider = normalizedProvider ? normalizedProvider : null;
    }
    if (paidAtInput !== formatInputDate(selectedOrder.moPaidAt)) {
      payload.mouPaidAt = paidAtInput ? DateTime.fromISO(paidAtInput).toISO() : null;
    }
    if (Object.keys(payload).length === 0) {
      closeDialog();
      return;
    }
    const saveData = { id: selectedOrder.moOrderId, payload };
    const risky = ['paid', 'cancelled', 'refunded', 'failed'];
    if (payload.mouStatus && risky.includes(payload.mouStatus)) {
      setPendingSavePayload(saveData);
      setStatusConfirmOpen(true);
      return;
    }
    await executeSave(saveData);
  };

  const handleStatusConfirm = async () => {
    if (pendingSavePayload) {
      await executeSave(pendingSavePayload);
    }
    setStatusConfirmOpen(false);
  };

  const markPaidNow = () => {
    const nowStr = DateTime.now().toFormat("yyyy-LL-dd'T'HH:mm");
    setStatusInput('paid');
    setPaidAtInput(nowStr);
  };

  const handleFulfillmentSave = async () => {
    if (!selectedOrder || !fulfillmentStatusInput) return;
    const payload: MarketplaceFulfillmentUpdatePayload = {
      mfuStatus: fulfillmentStatusInput,
    };
    const carrier = fulfillmentCarrierInput.trim();
    const trackingReference = fulfillmentTrackingInput.trim();
    const reasonCode = fulfillmentReasonInput.trim();
    const notes = fulfillmentNotesInput.trim();
    if (carrier) payload.mfuCarrier = carrier;
    if (trackingReference) payload.mfuTrackingReference = trackingReference;
    if (reasonCode) payload.mfuReasonCode = reasonCode;
    if (notes) payload.mfuNotes = notes;
    await fulfillmentMutation.mutateAsync({ id: selectedOrder.moOrderId, payload });
  };

  const handleRentalSave = async () => {
    if (!selectedOrder || !rentalStatusInput) return;
    const payload: MarketplaceRentalUpdatePayload = { mruStatus: rentalStatusInput };
    const conditionOut = rentalConditionOutInput.trim();
    const conditionIn = rentalConditionInInput.trim();
    const evidenceUrl = rentalEvidenceUrlInput.trim();
    const reasonCode = rentalReasonInput.trim();
    const notes = rentalNotesInput.trim();
    if (conditionOut) payload.mruConditionOut = conditionOut;
    if (conditionIn) payload.mruConditionIn = conditionIn;
    if (evidenceUrl) payload.mruEvidenceUrl = evidenceUrl;
    if (
      rentalDepositDeductionInput.trim()
      && ['damage_review', 'deposit_refund_due', 'disputed'].includes(rentalStatusInput)
    ) {
      payload.mruDepositDeductionUsdCents = Number(rentalDepositDeductionInput);
    }
    if (reasonCode) payload.mruReasonCode = reasonCode;
    if (notes) payload.mruNotes = notes;
    await rentalMutation.mutateAsync({ id: selectedOrder.moOrderId, payload });
  };

  const handleCopyOrderId = async (orderId: string) => {
    try {
      await navigator.clipboard.writeText(orderId);
    } catch {
      // ignore clipboard failures silently
    }
  };

  const copyOrderSummary = async (order: MarketplaceOrderDTO) => {
    const buyerIdentity = getOrderBuyerIdentity(order);
    const buyerEmail = normalizeEmailValue(order.moBuyerEmail);
    const buyerEmailDetail = shouldShowBuyerEmailDetail(buyerEmail, buyerIdentity) ? ` (${buyerEmail})` : '';
    const summaryLines = [
      `Pedido: ${order.moOrderId}`,
      `Estado: ${statusLabel(order.moStatus)}`,
      `Total: ${order.moTotalDisplay} (${order.moCurrency.toUpperCase()})`,
      `Pago: ${formatPaymentProvider(order.moPaymentProvider)}`,
      `Comprador: ${buyerIdentity}${buyerEmailDetail}`,
      `Items: ${summarizeItems(order.moItems)}`,
    ];
    try {
      await navigator.clipboard.writeText(summaryLines.join('\n'));
      setToast('Resumen copiado');
    } catch {
      // ignore clipboard failures silently
    }
  };

  const trimmedStatusInput = statusInput.trim();
  const effectiveStatus = (trimmedStatusInput.length > 0 ? trimmedStatusInput : selectedOrder?.moStatus ?? '').trim();
  const effectiveProvider = (paymentProviderInput ?? selectedOrder?.moPaymentProvider ?? '').trim();
  const warnMissingProvider = Boolean(selectedOrder && isPaidOrderStatus(effectiveStatus) && !effectiveProvider);
  const warnMissingPaidAt = Boolean(selectedOrder && isPaidOrderStatus(effectiveStatus) && !paidAtInput);
  const showCombinedPaidRequirementsWarning = warnMissingProvider && warnMissingPaidAt;
  const blockSave =
    isPaidOrderStatus(effectiveStatus) && (warnMissingProvider || warnMissingPaidAt);
  const selectedPaidAtInput = selectedOrder ? formatInputDate(selectedOrder.moPaidAt) : '';
  const hasOrderUpdateChange = Boolean(
    selectedOrder
      && (
        (statusInput.trim() !== '' && statusInput.trim() !== selectedOrder.moStatus)
        || paymentProviderInput.trim() !== (selectedOrder.moPaymentProvider ?? '')
        || paidAtInput !== selectedPaidAtInput
      ),
  );
  const showPaymentTimestampInput = Boolean(
    selectedOrder && (
      isPaidOrderStatus(effectiveStatus)
      || Boolean(selectedPaidAtInput)
      || Boolean(paidAtInput)
    ),
  );
  const statusHint = (() => {
    if (!effectiveStatus) return null;
    if (effectiveStatus === 'datafast_pending') {
      return 'Pago con tarjeta en revisión. Espera confirmación o reintenta el cobro antes de marcar pagado.';
    }
    if (effectiveStatus === 'stripe_pending') {
      return 'Pago Stripe iniciado. Espera el webhook de confirmación antes de intervenir manualmente.';
    }
    if (effectiveStatus === 'stripe_failed') {
      return 'Pago Stripe fallido. Reintenta el cobro o cambia el estado a contactar.';
    }
    if (effectiveStatus === 'datafast_failed' || effectiveStatus === 'failed') {
      return 'Pago con tarjeta fallido. Reintenta el cobro o cambia el estado a contactar.';
    }
    if (effectiveStatus === 'paypal_pending') {
      return 'El cliente inició PayPal pero aún no confirma. Verifica en PayPal o comunícate con el cliente.';
    }
    return null;
  })();
  const paymentProviderRequiredForShortcut = Boolean(selectedOrder && !isPaidOrderStatus(effectiveStatus) && !effectiveProvider);
  const paymentProviderHelperText = paymentProviderRequiredForShortcut
    ? 'Requerido antes de marcar una orden como pagada.'
    : undefined;
  const isCanonicalRental = selectedOrder?.moOrderKind === 'rental';
  const hasCanonicalFulfillment = Boolean(
    !isCanonicalRental && selectedOrder?.moFulfillmentMethod && selectedOrder.moFulfillmentStatus,
  );
  const showMarkPaidShortcut = Boolean(selectedOrder)
    && !hasCanonicalFulfillment
    && !isPaidOrderStatus(effectiveStatus)
    && Boolean(effectiveProvider);
  const selectedBuyerIdentity = selectedOrder ? getOrderBuyerIdentity(selectedOrder) : '';
  const selectedBuyerEmail = selectedOrder ? normalizeEmailValue(selectedOrder.moBuyerEmail) : '';
  const selectedBuyerPhone = selectedOrder ? normalizeBuyerPhoneValue(selectedOrder.moBuyerPhone) : '';
  const showSelectedBuyerEmail = shouldShowBuyerEmailDetail(selectedBuyerEmail, selectedBuyerIdentity);
  const showSelectedBuyerPhone = shouldShowBuyerPhoneDetail(selectedBuyerPhone, selectedBuyerIdentity);
  const selectedCartId = selectedOrder?.moCartId?.trim() ?? '';
  const selectedPaypalOrderId = selectedOrder?.moPaypalOrderId?.trim() ?? '';
  const selectedManualEvidence = marketplaceCommerceQuery.data?.mpcManualEvidence ?? null;
  const manualReviewReady = Boolean(
    selectedManualEvidence
      && ['submitted', 'under_review'].includes(selectedManualEvidence.mmeStatus),
  );
  const manualReviewNotesValid = manualReviewNotes.trim().length >= 3
    && manualReviewNotes.trim().length <= 2000;
  const selectedStatusHistory = selectedOrder?.moStatusHistory ?? [];
  const latestStatusChange = selectedStatusHistory[selectedStatusHistory.length - 1];
  const showLatestStatusChangeSummary = selectedStatusHistory.length === 1 && Boolean(latestStatusChange);
  const showStatusHistorySection = selectedStatusHistory.length > 1;
  const showSelectedContactEmptyState = Boolean(selectedOrder && !selectedBuyerEmail && !selectedBuyerPhone);
  const ordersListSubheader = ordersQuery.isLoading
    ? 'La bandeja aparecerá cuando termine esta primera carga.'
    : showFirstOrderEmptyState
    ? undefined
    : showSingleVisibleOrderSummary
      ? singleVisibleOrderSummaryText
      : 'Haz clic en una fila para revisar estado, pago y datos del comprador.';
  const availableStatusUpdatePresets = selectedOrder
    ? STATUS_PRESETS.filter((statusPreset) => statusPreset.value !== selectedOrder.moStatus)
    : STATUS_PRESETS;
  const availableFulfillmentTransitions = fulfillmentTransitionsFor(
    selectedOrder?.moFulfillmentMethod,
    selectedOrder?.moFulfillmentStatus,
  );
  const availableRentalTransitions = selectedOrder?.moFulfillmentStatus
    ? RENTAL_TRANSITIONS[selectedOrder.moFulfillmentStatus] ?? []
    : [];
  const selectedRentalTransition = availableRentalTransitions.includes(rentalStatusInput)
    ? rentalStatusInput
    : '';
  const rentalConditionOutRequired = rentalStatusInput === 'checked_out'
    && !rentalConditionOutInput.trim()
    && !selectedOrder?.moConditionOut;
  const rentalConditionInRequired = rentalStatusInput === 'returned_pending_inspection'
    && !rentalConditionInInput.trim()
    && !selectedOrder?.moConditionIn;
  const parsedRentalDeduction = rentalDepositDeductionInput.trim() === ''
    ? 0
    : Number(rentalDepositDeductionInput);
  const rentalDeductionInvalid = !Number.isSafeInteger(parsedRentalDeduction)
    || parsedRentalDeduction < 0
    || parsedRentalDeduction > (selectedOrder?.moSecurityDepositUsdCents ?? 0);
  const selectedFulfillmentTransition = availableFulfillmentTransitions.includes(fulfillmentStatusInput)
    ? fulfillmentStatusInput
    : '';
  const fulfillmentTrackingRequired = fulfillmentStatusInput === 'shipped'
    && !fulfillmentTrackingInput.trim()
    && !selectedOrder?.moTrackingReference;
  const orderPaginationResetKey = [
    statusFilter,
    providerFilter,
    search.trim(),
    fromDate,
    toDate,
    activePaidOnlyFilter ? 'paid' : 'all',
  ].join('|');

  return (
    <Box p={2}>
      <Stack direction="row" alignItems="center" spacing={1} mb={0.5}>
        <LocalMallIcon color="primary" />
        <Typography variant="h4" fontWeight={700}>
          Órdenes del marketplace
        </Typography>
        <Box flex={1} />
        {showHeaderRefreshAction && (
          <Tooltip title="Recargar">
            <IconButton aria-label="Recargar órdenes" onClick={handleRefresh}>
              <RefreshIcon />
            </IconButton>
          </Tooltip>
        )}
      </Stack>
      <Typography variant="body2" color="text.secondary" sx={{ mb: 2 }}>
        {ordersSummary}
      </Typography>

      {showListChrome && (
        <>
          <Grid container spacing={2} mb={1}>
            <Grid item xs={12} md={5} lg={4}>
              <TextField
                fullWidth
                label="Buscar por comprador, contacto o pedido"
                value={search}
                autoComplete="off"
                onChange={(e: ChangeEvent<HTMLInputElement>) => {
                  const nextSearch = e.target.value;
                  setSearch(nextSearch.trim() === '' ? '' : nextSearch);
                }}
                inputProps={{ spellCheck: false }}
                InputProps={{
                  endAdornment: hasSearchInput ? (
                    <InputAdornment position="end">
                      <Tooltip title="Limpiar búsqueda">
                        <IconButton
                          edge="end"
                          size="small"
                          aria-label="Limpiar búsqueda"
                          onClick={() => setSearch('')}
                        >
                          <ClearIcon fontSize="small" />
                        </IconButton>
                      </Tooltip>
                    </InputAdornment>
                  ) : undefined,
                }}
              />
            </Grid>
            {combinedFilterContextHelperText ? (
              <Grid item xs={12} md={7} lg={6}>
                <Box
                  sx={{
                    height: '100%',
                    minHeight: 56,
                    display: 'flex',
                    alignItems: 'center',
                  }}
                >
                  <Typography variant="body2" color="text.secondary">
                    {combinedFilterContextHelperText}
                  </Typography>
                </Box>
              </Grid>
            ) : (
              <>
                <Grid item xs={12} md={3} lg={3}>
                  {showStatusFilter ? (
                    <FormControl fullWidth>
                      <InputLabel id="status-filter-label">Estado del listado</InputLabel>
                      <Select
                        labelId="status-filter-label"
                        label="Estado del listado"
                        value={statusFilter}
                        onChange={(e) => setStatusFilter(e.target.value)}
                      >
                        <MenuItem value="all">Todos</MenuItem>
                        {statusFilterOptions.map((st) => (
                          <MenuItem key={st.value} value={st.value}>
                            {st.label}
                          </MenuItem>
                        ))}
                      </Select>
                    </FormControl>
                  ) : (
                    <Box
                      sx={{
                        height: '100%',
                        minHeight: 56,
                        display: 'flex',
                        alignItems: 'center',
                      }}
                    >
                      <Typography variant="body2" color="text.secondary">
                        {statusFilterHelperText}
                      </Typography>
                    </Box>
                  )}
                </Grid>
                <Grid item xs={12} md={4} lg={3}>
                  {showProviderFilter ? (
                    <FormControl fullWidth>
                      <InputLabel id="provider-filter-label">Método de pago</InputLabel>
                      <Select
                        labelId="provider-filter-label"
                        label="Método de pago"
                        value={providerFilter}
                        onChange={(e) => setProviderFilter(e.target.value)}
                      >
                        <MenuItem value="all">Todos</MenuItem>
                        <MenuItem value="stripe">Stripe</MenuItem>
                        <MenuItem value="paypal">PayPal</MenuItem>
                        <MenuItem value="datafast">Tarjeta (Datafast)</MenuItem>
                        <MenuItem value="contact">Manual/otros</MenuItem>
                      </Select>
                    </FormControl>
                  ) : (
                    <Box
                      sx={{
                        height: '100%',
                        minHeight: 56,
                        display: 'flex',
                        alignItems: 'center',
                      }}
                    >
                      <Typography variant="body2" color="text.secondary">
                        {providerFilterHelperText}
                      </Typography>
                    </Box>
                  )}
                </Grid>
              </>
            )}
            <Grid item xs={12}>
              <Button
                size="small"
                variant="text"
                onClick={() => setShowAdvancedFilters((prev) => !prev)}
                sx={{ px: 0, textTransform: 'none' }}
              >
                {advancedFiltersButtonLabel}
              </Button>
            </Grid>
            {showAdvancedFilters && (
              <>
                <Grid item xs={6} md={6} lg={3}>
                  <TextField
                    label="Desde"
                    type="date"
                    fullWidth
                    value={fromDate}
                    onChange={(e) => setFromDate(e.target.value)}
                    InputLabelProps={{ shrink: true }}
                  />
                </Grid>
                <Grid item xs={6} md={6} lg={3}>
                  <TextField
                    label="Hasta"
                    type="date"
                    fullWidth
                    value={toDate}
                    onChange={(e) => setToDate(e.target.value)}
                    InputLabelProps={{ shrink: true }}
                    inputProps={{ min: fromDate }}
                  />
                </Grid>
                {showPaidOnlyAdvancedFilter && (
                  <Grid item xs={12} md={12} lg={3}>
                    <FormControlLabel
                      control={<Checkbox checked={paidOnly} onChange={(e) => setPaidOnly(e.target.checked)} />}
                      label="Solo con pago registrado"
                    />
                  </Grid>
                )}
              </>
            )}
          </Grid>
          <Stack
            direction={{ xs: 'column', lg: 'row' }}
            spacing={1.5}
            mb={2}
            alignItems={{ xs: 'stretch', lg: 'flex-start' }}
          >
            {showQuickViewControl ? (
              <TextField
                select
                size="small"
                label="Vista rápida"
                value=""
                onChange={(event) => {
                  const nextPreset = event.target.value as QuickViewPreset | '';
                  if (!nextPreset) return;
                  applyPreset(nextPreset);
                }}
                helperText="Aplica una vista base y reemplaza los filtros actuales antes de revisar resultados."
                sx={{ minWidth: { xs: '100%', sm: 280 }, flexShrink: 0 }}
                SelectProps={{ displayEmpty: true }}
              >
                <MenuItem value="" disabled>
                  Elegir…
                </MenuItem>
                {QUICK_VIEW_PRESETS.map((preset) => (
                  <MenuItem key={preset.value} value={preset.value}>
                    {preset.label}
                  </MenuItem>
                ))}
              </TextField>
            ) : null}
            <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap" useFlexGap sx={{ flex: 1 }}>
              {showActiveFiltersTray ? (
                <>
                  <Box flex={1} />
                  {showCopyFiltersLinkAction && (
                    <Button size="small" onClick={copyFiltersLink}>
                      Copiar enlace de filtros
                    </Button>
                  )}
                  {showStatusFilterTrayChip && (
                    <Chip
                      size="small"
                      data-testid="marketplace-active-filter-chip"
                      label={`Estado: ${statusLabel(statusFilter)}`}
                    />
                  )}
                  {showProviderFilterTrayChip && (
                    <Chip
                      size="small"
                      data-testid="marketplace-active-filter-chip"
                      label={`Pago: ${getMarketplacePaymentProviderLabel(providerFilter)}`}
                    />
                  )}
                  {fromDate && (
                    <Chip
                      size="small"
                      data-testid="marketplace-active-filter-chip"
                      label={`Desde: ${fromDate}`}
                    />
                  )}
                  {toDate && (
                    <Chip
                      size="small"
                      data-testid="marketplace-active-filter-chip"
                      label={`Hasta: ${toDate}`}
                    />
                  )}
                  {activePaidOnlyFilter && (
                    <Chip
                      size="small"
                      data-testid="marketplace-active-filter-chip"
                      label="Con pago"
                    />
                  )}
                  <Button onClick={clearFilters} variant="text">
                    {clearFiltersActionLabel}
                  </Button>
                </>
              ) : showFilterTrayHelper ? (
                <Typography variant="body2" color="text.secondary" sx={{ flex: 1 }}>
                  {searchOwnedFilterHelperText}
                </Typography>
              ) : null}
            </Stack>
          </Stack>
          {paidTotal > 0 && paidVisible === 0 && filtersDirty && (
            <Alert severity="info" sx={{ mb: 2 }}>
              {paidOrdersRecoveryMessage}
            </Alert>
          )}
        </>
      )}

      <Card variant="outlined">
        <CardHeader
          title="Pedidos recientes"
          subheader={ordersListSubheader}
          action={showOrderListHeaderActions ? (
            <Stack direction="row" spacing={1}>
              {showVisibleOrderBreakdown && (
                <>
                  <Chip
                    icon={<CheckCircleIcon />}
                    label={`${visiblePaidCount} pagados`}
                    color="success"
                    variant="outlined"
                  />
                  <Chip
                    icon={<InventoryIcon />}
                    label={`${visiblePendingCount} pendientes`}
                    color="warning"
                    variant="outlined"
                  />
                </>
              )}
              {showExportCsvAction && (
                <Button size="small" variant="outlined" onClick={exportCsv}>
                  Exportar CSV
                </Button>
              )}
            </Stack>
          ) : null}
        />
        <CardContent>
          {ordersQuery.isError && (
            <Alert
              severity="error"
              action={showInitialOrdersErrorState ? (
                <Button color="inherit" size="small" onClick={handleRefresh}>
                  Reintentar órdenes
                </Button>
              ) : undefined}
            >
              {ordersErrorMessage}
            </Alert>
          )}
          {ordersQuery.isLoading && <Typography color="text.secondary">Cargando órdenes...</Typography>}
          {showFirstOrderEmptyState && (
            <Alert
              severity="info"
              variant="outlined"
              action={(
                <Button
                  size="small"
                  variant="outlined"
                  component={RouterLink}
                  to="/marketplace"
                >
                  Ir al marketplace
                </Button>
              )}
            >
              {FIRST_ORDER_EMPTY_STATE_MESSAGE}
            </Alert>
          )}
          {singleVisibleOrder && (
            <Stack
              spacing={1.5}
              sx={{
                border: '1px solid',
                borderColor: 'divider',
                borderRadius: 2,
                p: 2,
                maxWidth: 720,
              }}
              data-testid="marketplace-single-order-summary"
            >
              <Typography variant="body2">
                <Box component="span" sx={{ fontWeight: 600 }}>Pedido:</Box> {singleVisibleOrder.moOrderId}
              </Typography>
              <Typography variant="body2" color="text.secondary">
                <Box component="span" sx={{ fontWeight: 600 }}>Comprador:</Box> {singleVisibleBuyerIdentity}
              </Typography>
              {showSingleVisibleBuyerEmail && (
                <Typography variant="body2" color="text.secondary">
                  <Box component="span" sx={{ fontWeight: 600 }}>Email:</Box> {singleVisibleBuyerEmail}
                </Typography>
              )}
              {showSingleVisibleBuyerPhone && (
                <Typography variant="body2" color="text.secondary">
                  <Box component="span" sx={{ fontWeight: 600 }}>Teléfono:</Box> {singleVisibleBuyerPhone}
                </Typography>
              )}
              {showSingleVisibleContactEmptyState && (
                <Typography variant="body2" color="text.secondary">
                  Sin email ni teléfono registrado.
                </Typography>
              )}
              <Typography variant="body2" color="text.secondary">
                <Box component="span" sx={{ fontWeight: 600 }}>Estado:</Box> {statusLabel(singleVisibleOrder.moStatus)}
              </Typography>
              <Typography variant="body2" color="text.secondary">
                <Box component="span" sx={{ fontWeight: 600 }}>Pago:</Box> {formatPaymentProvider(singleVisibleOrder.moPaymentProvider)}
              </Typography>
              <Typography variant="body2" color="text.secondary">
                <Box component="span" sx={{ fontWeight: 600 }}>Total:</Box> {singleVisibleOrder.moTotalDisplay}
              </Typography>
              <Typography variant="body2" color="text.secondary">
                <Box component="span" sx={{ fontWeight: 600 }}>Creado:</Box> {formatDate(singleVisibleOrder.moCreatedAt)}
              </Typography>
              {singleVisibleItemsSummary && (
                <Typography variant="body2" color="text.secondary">
                  <Box component="span" sx={{ fontWeight: 600 }}>Items:</Box> {singleVisibleItemsSummary}
                </Typography>
              )}
              <Button
                size="small"
                variant="outlined"
                onClick={() => openOrder(singleVisibleOrder.moOrderId)}
                sx={{ alignSelf: 'flex-start' }}
              >
                Abrir orden
              </Button>
            </Stack>
          )}
          {showEmptyOrdersState && (
            <Alert severity="info">
              {emptyOrdersMessage}
            </Alert>
          )}
          {sharedVisibleCurrencyCaption && !showFilteredSingleOrderSummary && (
            <Typography
              variant="body2"
              color="text.secondary"
              sx={{ mb: 1.5 }}
              data-testid="marketplace-orders-shared-currency"
            >
              Moneda visible: {sharedVisibleCurrencyCaption}.
            </Typography>
          )}
          {showSharedEmptyItemsSummary && (
            <Typography
              variant="body2"
              color="text.secondary"
              sx={{ mb: 1.5 }}
              data-testid="marketplace-orders-empty-items-summary"
            >
              Sin items registrados en las órdenes visibles.
            </Typography>
          )}
          {filtered.length > 0 && !singleVisibleOrder && (
            <LazyPaginatedList
              items={filtered}
              pagination={{ itemLabel: 'pedidos', initialRowsPerPage: 25, resetKey: orderPaginationResetKey }}
              renderItems={(visibleOrders) => (
                <TableContainer component={Paper}>
                  <Table size="small">
                    <TableHead>
                      <TableRow>
                        <TableCell>Pedido</TableCell>
                        <TableCell>Cliente</TableCell>
                        {showBuyerPhoneColumn && <TableCell>Contacto</TableCell>}
                        {showStatusColumn && <TableCell>Estado</TableCell>}
                        <TableCell align="right">Total</TableCell>
                        {showPaymentProviderColumn && <TableCell>Pago</TableCell>}
                        <TableCell>Creado</TableCell>
                        {showPaidAtColumn && <TableCell>Pagado</TableCell>}
                        {showItemsColumn && <TableCell>Items</TableCell>}
                      </TableRow>
                    </TableHead>
                    <TableBody>
                    {visibleOrders.map((order) => {
                      const orderCurrencyCaption = sharedVisibleCurrencyCaption ? '' : getOrderCurrencyCaption(order);
                      const itemCountLabel = formatItemCountLabel(order.moItems);
                      const itemSummary = summarizeItems(order.moItems);
                      const paypalPayerEmail = getDistinctPaypalPayerEmail(order);
                      const buyerIdentity = getOrderBuyerIdentity(order);
                      const buyerEmail = normalizeEmailValue(order.moBuyerEmail);
                      const buyerPhone = normalizeBuyerPhoneValue(order.moBuyerPhone);
                      const showBuyerEmail = shouldShowBuyerEmailDetail(buyerEmail, buyerIdentity);
                      const showBuyerPhone = shouldShowBuyerPhoneDetail(buyerPhone, buyerIdentity);

                      return (
                        <TableRow
                          key={order.moOrderId}
                          hover
                          tabIndex={0}
                          aria-label={`Abrir orden ${order.moOrderId} de ${buyerIdentity}`}
                          onClick={() => openOrder(order.moOrderId)}
                          onKeyDown={(event) => {
                            if (event.key !== 'Enter' && event.key !== ' ') return;
                            event.preventDefault();
                            openOrder(order.moOrderId);
                          }}
                          sx={{
                            cursor: 'pointer',
                            '&:focus-visible': {
                              outline: '2px solid',
                              outlineColor: 'primary.main',
                              outlineOffset: -2,
                            },
                          }}
                        >
                          <TableCell>
                            <Stack spacing={0.5}>
                              <Tooltip title={order.moOrderId}>
                                <Typography variant="body2" fontWeight={600} sx={{ width: 'fit-content' }}>
                                  {order.moOrderId.slice(0, 8)}
                                </Typography>
                              </Tooltip>
                              {orderCurrencyCaption && (
                                <Typography variant="caption" color="text.secondary">
                                  {orderCurrencyCaption}
                                </Typography>
                              )}
                            </Stack>
                          </TableCell>
                          <TableCell>
                            <Typography variant="body2" fontWeight={600}>
                              {buyerIdentity}
                            </Typography>
                            {showBuyerEmail ? (
                              <Link
                                href={`mailto:${buyerEmail}`}
                                underline="hover"
                                variant="caption"
                                color="text.secondary"
                                onClick={(e) => e.stopPropagation()}
                              >
                                {buyerEmail}
                              </Link>
                            ) : null}
                          </TableCell>
                          {showBuyerPhoneColumn && (
                            <TableCell>
                              {showBuyerPhone ? (
                                <Link
                                  href={`tel:${buyerPhone.replace(/\s+/g, '')}`}
                                  underline="hover"
                                  color="text.primary"
                                  variant="body2"
                                  onClick={(e) => e.stopPropagation()}
                                >
                                  {buyerPhone}
                                </Link>
                              ) : (
                                '—'
                              )}
                            </TableCell>
                          )}
                          {showStatusColumn && (
                            <TableCell>
                              <Chip size="small" label={statusLabel(order.moStatus)} color={statusColor(order.moStatus)} />
                            </TableCell>
                          )}
                          <TableCell align="right">{order.moTotalDisplay}</TableCell>
                          {showPaymentProviderColumn && (
                            <TableCell>
                              <Stack spacing={0.5}>
                                <Typography variant="body2">
                                  {formatPaymentProvider(order.moPaymentProvider)}
                                </Typography>
                                {paypalPayerEmail && (
                                  <Link
                                    href={`mailto:${paypalPayerEmail}`}
                                    underline="hover"
                                    variant="caption"
                                    color="text.secondary"
                                    onClick={(e) => e.stopPropagation()}
                                  >
                                    {paypalPayerEmail}
                                  </Link>
                                )}
                              </Stack>
                            </TableCell>
                          )}
                          <TableCell>{formatDate(order.moCreatedAt)}</TableCell>
                          {showPaidAtColumn && <TableCell>{formatDate(order.moPaidAt)}</TableCell>}
                          {showItemsColumn && (
                            <TableCell>
                              {itemCountLabel && (
                                <Typography variant="body2">{itemCountLabel}</Typography>
                              )}
                              <Typography
                                variant={itemCountLabel ? 'caption' : 'body2'}
                                color={itemCountLabel ? 'text.secondary' : 'text.primary'}
                              >
                                {itemSummary}
                              </Typography>
                            </TableCell>
                          )}
                        </TableRow>
                      );
                    })}
                    </TableBody>
                  </Table>
                </TableContainer>
              )}
            />
          )}
        </CardContent>
      </Card>

      <Dialog open={Boolean(selectedOrder)} onClose={closeDialog} fullWidth maxWidth="md">
        <DialogTitle>Detalle de la orden</DialogTitle>
        {selectedOrder && (
          <>
            <DialogContent dividers>
              <Stack spacing={2}>
                <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
                  <Card variant="outlined" sx={{ flex: 1 }}>
                    <CardHeader
                      title={`Pedido ${selectedOrder.moOrderId.slice(0, 8)}`}
                      subheader={`Total ${selectedOrder.moTotalDisplay}`}
                      action={
                        <Button
                          size="small"
                          startIcon={<ContentCopyIcon />}
                          aria-label={`Abrir opciones de copia para ${selectedOrder.moOrderId}`}
                          aria-haspopup="menu"
                          aria-expanded={Boolean(copyMenuAnchorEl)}
                          onClick={(event) => setCopyMenuAnchorEl(event.currentTarget)}
                        >
                          Copiar
                        </Button>
                      }
                    />
                    <CardContent>
                      <Stack spacing={1}>
                        <Typography variant="body2">
                          <strong>Comprador:</strong> {selectedBuyerIdentity}
                        </Typography>
                        {showSelectedBuyerEmail && (
                          <Typography variant="body2">
                            <strong>Email:</strong> {selectedBuyerEmail}
                          </Typography>
                        )}
                        {showSelectedBuyerPhone && (
                          <Typography variant="body2">
                            <strong>Teléfono:</strong> {selectedBuyerPhone}
                          </Typography>
                        )}
                        {showSelectedContactEmptyState && (
                          <Typography variant="body2" color="text.secondary">
                            Sin email ni teléfono registrado.
                          </Typography>
                        )}
                        {selectedCartId && (
                          <Typography variant="body2">
                            <strong>Carrito:</strong> {selectedCartId}
                          </Typography>
                        )}
                        <Stack direction="row" spacing={1} alignItems="center">
                          <Typography variant="body2">
                            <strong>Estado:</strong>
                          </Typography>
                          <Chip size="small" label={statusLabel(selectedOrder.moStatus)} color={statusColor(selectedOrder.moStatus)} />
                        </Stack>
                        {hasCanonicalFulfillment && (
                          <>
                            <Typography variant="body2">
                              <strong>Entrega:</strong> {fulfillmentStatusLabel(selectedOrder.moFulfillmentStatus)}
                            </Typography>
                            <Typography variant="body2">
                              <strong>Modalidad:</strong> {fulfillmentMethodLabel(selectedOrder.moFulfillmentMethod)}
                            </Typography>
                            {selectedOrder.moTrackingReference && (
                              <Typography variant="body2">
                                <strong>Guía:</strong> {selectedOrder.moTrackingReference}
                              </Typography>
                            )}
                          </>
                        )}
                        {isCanonicalRental && (
                          <Stack spacing={0.5}>
                            <Typography variant="body2">
                              <strong>Renta:</strong> {rentalStatusLabel(selectedOrder.moFulfillmentStatus)}
                            </Typography>
                            <Typography variant="body2">
                              <strong>Fechas:</strong> {selectedOrder.moRentalStartDate} → {selectedOrder.moRentalEndDate} ({selectedOrder.moRentalDurationDays} día(s))
                            </Typography>
                            <Typography variant="body2">
                              <strong>Cargo:</strong> {selectedOrder.moRentalChargeUsdCents ?? 0} centavos · <strong>depósito:</strong> {selectedOrder.moSecurityDepositUsdCents ?? 0} centavos
                            </Typography>
                            <Typography variant="body2">
                              <strong>Estado del depósito:</strong> {selectedOrder.moDepositStatus ?? '—'}
                              {(selectedOrder.moDepositDeductionUsdCents ?? 0) > 0
                                ? ` · deducción propuesta ${selectedOrder.moDepositDeductionUsdCents} centavos`
                                : ''}
                            </Typography>
                            <Typography variant="caption" color="text.secondary">
                              Términos {selectedOrder.moRentalTermsVersion ?? '—'} · {selectedOrder.moRentalTimezone ?? 'America/Guayaquil'}
                            </Typography>
                          </Stack>
                        )}
                        {showLatestStatusChangeSummary && latestStatusChange && (
                          <Typography variant="body2" color="text.secondary">
                            Último cambio: {formatDate(latestStatusChange[1])}
                          </Typography>
                        )}
                        <Typography variant="body2">
                          <strong>Creado:</strong> {formatDate(selectedOrder.moCreatedAt)}
                        </Typography>
                        <Typography variant="body2">
                          <strong>Pago:</strong> {formatPaymentProvider(selectedOrder.moPaymentProvider)}
                        </Typography>
                        {selectedPaypalOrderId && (
                          <Typography variant="caption" color="text.secondary">
                            PayPal order: {selectedPaypalOrderId}
                          </Typography>
                        )}
                      </Stack>
                    </CardContent>
                  </Card>
                  <Card variant="outlined" sx={{ flex: 1 }}>
                    <CardHeader title={isCanonicalRental ? 'Gestionar renta' : hasCanonicalFulfillment ? 'Gestionar entrega' : 'Actualizar estado'} />
                    <CardContent>
                      {isCanonicalRental ? (
                        <Stack spacing={2}>
                          <Alert severity="info" variant="outlined">
                            Pago, custodia y depósito son estados separados. Una deducción queda pendiente; no representa que el reembolso o cobro ya ocurrió.
                          </Alert>
                          <FormControl fullWidth>
                            <InputLabel id="rental-status-input-label" shrink>Siguiente estado de renta</InputLabel>
                            <Select
                              labelId="rental-status-input-label"
                              label="Siguiente estado de renta"
                              value={selectedRentalTransition}
                              displayEmpty
                              renderValue={(value) => value ? rentalStatusLabel(String(value)) : 'Selecciona una transición'}
                              onChange={(event) => setRentalStatusInput(event.target.value)}
                            >
                              <MenuItem value=""><em>Selecciona una transición</em></MenuItem>
                              {availableRentalTransitions.map((status) => (
                                <MenuItem key={status} value={status}>{rentalStatusLabel(status)}</MenuItem>
                              ))}
                            </Select>
                          </FormControl>
                          {(rentalStatusInput === 'checked_out' || Boolean(selectedOrder.moConditionOut)) && (
                            <TextField
                              label="Condición al entregar"
                              value={rentalConditionOutInput}
                              onChange={(event) => setRentalConditionOutInput(event.target.value)}
                              multiline
                              minRows={2}
                              required={rentalStatusInput === 'checked_out'}
                              error={rentalConditionOutRequired}
                              helperText={rentalConditionOutRequired ? 'El informe de salida es obligatorio antes de transferir custodia.' : undefined}
                              inputProps={{ maxLength: 1000 }}
                            />
                          )}
                          {(rentalStatusInput === 'returned_pending_inspection' || Boolean(selectedOrder.moConditionIn)) && (
                            <TextField
                              label="Condición al devolver"
                              value={rentalConditionInInput}
                              onChange={(event) => setRentalConditionInInput(event.target.value)}
                              multiline
                              minRows={2}
                              required={rentalStatusInput === 'returned_pending_inspection'}
                              error={rentalConditionInRequired}
                              helperText={rentalConditionInRequired ? 'El informe de retorno es obligatorio antes de iniciar inspección.' : undefined}
                              inputProps={{ maxLength: 1000 }}
                            />
                          )}
                          <TextField
                            label="Evidencia HTTPS (opcional)"
                            value={rentalEvidenceUrlInput}
                            onChange={(event) => setRentalEvidenceUrlInput(event.target.value)}
                            inputProps={{ maxLength: 2048 }}
                            placeholder="https://..."
                          />
                          {(rentalStatusInput === 'damage_review' || (selectedOrder.moDepositDeductionUsdCents ?? 0) > 0) && (
                            <TextField
                              label="Deducción propuesta del depósito (centavos)"
                              type="number"
                              value={rentalDepositDeductionInput}
                              onChange={(event) => setRentalDepositDeductionInput(event.target.value)}
                              inputProps={{ min: 0, max: selectedOrder.moSecurityDepositUsdCents ?? 0, step: 1 }}
                              error={rentalDeductionInvalid}
                              helperText={`Máximo: ${selectedOrder.moSecurityDepositUsdCents ?? 0} centavos. La propuesta no ejecuta un cobro ni un reembolso.`}
                            />
                          )}
                          <TextField
                            label="Código de motivo"
                            value={rentalReasonInput}
                            onChange={(event) => setRentalReasonInput(event.target.value)}
                            inputProps={{ maxLength: 80 }}
                          />
                          <TextField
                            label="Notas de operación"
                            value={rentalNotesInput}
                            onChange={(event) => setRentalNotesInput(event.target.value)}
                            multiline
                            minRows={2}
                            inputProps={{ maxLength: 500 }}
                          />
                          {availableRentalTransitions.length === 0 ? (
                            <Typography variant="body2" color="text.secondary">
                              Este estado no tiene una transición operativa posterior.
                            </Typography>
                          ) : (
                            <Button
                              variant="contained"
                              onClick={() => { void handleRentalSave(); }}
                              disabled={
                                !rentalStatusInput
                                || rentalConditionOutRequired
                                || rentalConditionInRequired
                                || rentalDeductionInvalid
                                || rentalMutation.isPending
                              }
                            >
                              Guardar transición de renta
                            </Button>
                          )}
                          {rentalMutation.isError && (
                            <Alert severity="error">
                              {rentalMutation.error?.message ?? 'No se pudo actualizar la renta'}
                            </Alert>
                          )}
                        </Stack>
                      ) : hasCanonicalFulfillment ? (
                        <Stack spacing={2}>
                          <Alert severity="info" variant="outlined">
                            El pago y la entrega son estados separados. El pago canónico solo cambia con evidencia verificada del proveedor o aprobación del pago manual.
                          </Alert>
                          <FormControl fullWidth>
                            <InputLabel id="fulfillment-status-input-label" shrink>Siguiente estado de entrega</InputLabel>
                            <Select
                              labelId="fulfillment-status-input-label"
                              label="Siguiente estado de entrega"
                              value={selectedFulfillmentTransition}
                              displayEmpty
                              renderValue={(value) => (value ? fulfillmentStatusLabel(String(value)) : 'Selecciona una transición')}
                              onChange={(event) => setFulfillmentStatusInput(event.target.value)}
                            >
                              <MenuItem value="">
                                <em>Selecciona una transición</em>
                              </MenuItem>
                              {availableFulfillmentTransitions.map((status) => (
                                <MenuItem key={status} value={status}>
                                  {fulfillmentStatusLabel(status)}
                                </MenuItem>
                              ))}
                            </Select>
                          </FormControl>
                          {(fulfillmentStatusInput === 'shipped' || Boolean(selectedOrder.moTrackingReference)) && (
                            <>
                              <TextField
                                label="Transportista"
                                fullWidth
                                value={fulfillmentCarrierInput}
                                onChange={(event) => setFulfillmentCarrierInput(event.target.value)}
                                inputProps={{ maxLength: 120 }}
                              />
                              <TextField
                                label="Referencia de seguimiento"
                                fullWidth
                                required={fulfillmentStatusInput === 'shipped'}
                                value={fulfillmentTrackingInput}
                                onChange={(event) => setFulfillmentTrackingInput(event.target.value)}
                                inputProps={{ maxLength: 160 }}
                                error={fulfillmentTrackingRequired}
                                helperText={fulfillmentTrackingRequired ? 'La guía es obligatoria antes de marcar un envío.' : undefined}
                              />
                            </>
                          )}
                          <TextField
                            label="Código de motivo"
                            fullWidth
                            value={fulfillmentReasonInput}
                            onChange={(event) => setFulfillmentReasonInput(event.target.value)}
                            inputProps={{ maxLength: 80 }}
                            placeholder="cancelled_by_customer, damaged, delivered..."
                          />
                          <TextField
                            label="Notas de operación"
                            fullWidth
                            multiline
                            minRows={2}
                            value={fulfillmentNotesInput}
                            onChange={(event) => setFulfillmentNotesInput(event.target.value)}
                            inputProps={{ maxLength: 500 }}
                          />
                          {availableFulfillmentTransitions.length === 0 ? (
                            <Typography variant="body2" color="text.secondary">
                              Este estado no tiene una transición operativa posterior.
                            </Typography>
                          ) : (
                            <Button
                              variant="contained"
                              onClick={() => { void handleFulfillmentSave(); }}
                              disabled={!fulfillmentStatusInput || fulfillmentTrackingRequired || fulfillmentMutation.isPending}
                            >
                              Guardar transición de entrega
                            </Button>
                          )}
                          {fulfillmentMutation.isError && (
                            <Alert severity="error">
                              {fulfillmentMutation.error?.message ?? 'No se pudo actualizar la entrega'}
                            </Alert>
                          )}
                        </Stack>
                      ) : (
                        <Stack spacing={2}>
                        <FormControl fullWidth>
                          <InputLabel id="status-input-label" shrink>Nuevo estado</InputLabel>
                          <Select
                            labelId="status-input-label"
                            label="Nuevo estado"
                            value={statusInput}
                            displayEmpty
                            renderValue={(value) => (value ? statusLabel(String(value)) : 'Sin cambios')}
                            onChange={(e) => setStatusInput(e.target.value)}
                          >
                            <MenuItem value="">
                              <em>Sin cambios</em>
                            </MenuItem>
                            {availableStatusUpdatePresets.map((st) => (
                              <MenuItem key={st.value} value={st.value}>
                                {st.label}
                              </MenuItem>
                            ))}
                          </Select>
                        </FormControl>
                        <TextField
                          label="Proveedor de pago"
                          fullWidth
                          value={paymentProviderInput}
                          onChange={(e) => setPaymentProviderInput(e.target.value)}
                          placeholder="stripe, paypal, transferencia, cash..."
                          helperText={paymentProviderHelperText}
                        />
                        {showPaymentTimestampInput && (
                          <TextField
                            label="Fecha de pago"
                            type="datetime-local"
                            fullWidth
                            value={paidAtInput}
                            onChange={(e) => setPaidAtInput(e.target.value)}
                            InputLabelProps={{ shrink: true }}
                          />
                        )}
                        {showCombinedPaidRequirementsWarning ? (
                          <Alert severity="warning" variant="outlined">
                            Completa el método de pago y la fecha del cobro para dejar la orden como pagada.
                          </Alert>
                        ) : (
                          <>
                            {warnMissingProvider && (
                              <Alert severity="warning" variant="outlined">
                                No hay método de pago registrado. Ingresa stripe, paypal, datafast o manual para dejar trazabilidad.
                              </Alert>
                            )}
                            {warnMissingPaidAt && (
                              <Alert severity="warning" variant="outlined">
                                Agrega la fecha y hora del cobro si marcas la orden como pagada.
                              </Alert>
                            )}
                          </>
                        )}
                        {statusHint && (
                          <Alert severity="info" variant="outlined">
                            {statusHint}
                          </Alert>
                        )}
                        <Stack direction="row" spacing={1}>
                          {showMarkPaidShortcut && (
                            <Button variant="outlined" onClick={markPaidNow} startIcon={<CheckCircleIcon />}>
                              Marcar pagado ahora
                            </Button>
                          )}
                          {hasOrderUpdateChange ? (
                            <Button
                              variant="contained"
                              onClick={() => {
                                void handleSave();
                              }}
                              disabled={updateMutation.isPending || blockSave}
                            >
                              Guardar cambios
                            </Button>
                          ) : (
                            <Typography
                              variant="body2"
                              color="text.secondary"
                              data-testid="marketplace-order-editor-idle"
                              sx={{ alignSelf: 'center' }}
                            >
                              Sin cambios pendientes.
                            </Typography>
                          )}
                        </Stack>
                        {updateMutation.isError && (
                          <Alert severity="error">{updateMutation.error?.message ?? 'No se pudo actualizar'}</Alert>
                        )}
                        </Stack>
                      )}
                    </CardContent>
                  </Card>
                </Stack>

                {['bank_transfer', 'cash', 'pos'].includes(selectedOrder.moPaymentProvider ?? '') && (
                  <Card variant="outlined">
                    <CardHeader
                      title="Revisión de pago manual"
                      subheader="La evidencia del cliente no confirma pago por sí sola."
                    />
                    <CardContent>
                      <Stack spacing={1.5}>
                        {marketplaceCommerceQuery.isLoading && (
                          <Typography variant="body2" color="text.secondary">
                            Cargando evidencia protegida…
                          </Typography>
                        )}
                        {marketplaceCommerceQuery.isError && (
                          <Alert severity="error">
                            No se pudo cargar la evidencia financiera. Se requiere acceso de facturación.
                          </Alert>
                        )}
                        {marketplaceCommerceQuery.data && !selectedManualEvidence && (
                          <Alert severity="info" variant="outlined">
                            El cliente aún no ha enviado una referencia. La orden continúa impaga.
                          </Alert>
                        )}
                        {selectedManualEvidence && (
                          <>
                            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                              <Chip label={selectedManualEvidence.mmeStatus.replace(/_/g, ' ')} size="small" />
                              <Typography variant="body2">
                                Método: {formatPaymentProvider(selectedManualEvidence.mmePaymentMethod)}
                              </Typography>
                              {selectedManualEvidence.mmeSubmittedAt && (
                                <Typography variant="body2" color="text.secondary">
                                  Enviada: {formatDate(selectedManualEvidence.mmeSubmittedAt)}
                                </Typography>
                              )}
                            </Stack>
                            <Typography variant="body2">
                              <strong>Referencia:</strong> {selectedManualEvidence.mmeCustomerReference ?? '—'}
                            </Typography>
                            <Typography variant="body2">
                              <strong>Monto declarado:</strong> {selectedManualEvidence.mmeSubmittedAmountMinor ?? '—'} {selectedManualEvidence.mmeCurrency ?? ''}
                            </Typography>
                            {selectedManualEvidence.mmeReviewNotes && (
                              <Typography variant="body2" color="text.secondary">
                                Revisión: {selectedManualEvidence.mmeReviewNotes}
                              </Typography>
                            )}
                            {manualReviewReady && (
                              <>
                                <TextField
                                  label="Notas de revisión"
                                  value={manualReviewNotes}
                                  onChange={(event) => setManualReviewNotes(event.target.value)}
                                  multiline
                                  minRows={2}
                                  inputProps={{ maxLength: 2000 }}
                                  helperText="Compara la referencia con el estado bancario. Debe revisar una persona distinta del remitente."
                                />
                                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                                  <Button
                                    variant="contained"
                                    color="success"
                                    disabled={!manualReviewNotesValid || manualReviewMutation.isPending}
                                    onClick={() => manualReviewMutation.mutate('approve')}
                                  >
                                    Aprobar pago verificado
                                  </Button>
                                  <Button
                                    variant="outlined"
                                    color="error"
                                    disabled={!manualReviewNotesValid || manualReviewMutation.isPending}
                                    onClick={() => manualReviewMutation.mutate('reject')}
                                  >
                                    Rechazar evidencia
                                  </Button>
                                </Stack>
                              </>
                            )}
                            {selectedManualEvidence.mmeStatus === 'approved' && marketplaceCommerceQuery.data?.mpcPaymentStatus === 'paid' && (
                              <Alert severity="success" variant="outlined">
                                Pago manual verificado. La entrega, custodia y depósito siguen sus propios estados.
                              </Alert>
                            )}
                            {selectedManualEvidence.mmeStatus === 'rejected' && (
                              <Alert severity="warning" variant="outlined">
                                Evidencia rechazada. La orden permanece impaga hasta que el cliente reenvíe evidencia.
                              </Alert>
                            )}
                          </>
                        )}
                        {manualReviewMutation.isError && (
                          <Alert severity="error">
                            {manualReviewMutation.error?.message ?? 'No se pudo completar la revisión'}
                          </Alert>
                        )}
                      </Stack>
                    </CardContent>
                  </Card>
                )}

                {(customerRequestsQuery.data?.length ?? 0) > 0 && (
                  <Card variant="outlined">
                    <CardHeader
                      title="Solicitudes del cliente"
                      subheader="La solicitud no cambia pago, entrega, custodia ni fechas hasta una transición autorizada."
                    />
                    <CardContent>
                      <Stack spacing={1.5}>
                        {customerRequestsQuery.data?.map((request) => {
                          const reviewable = request.mcrStatus === 'submitted'
                            || request.mcrStatus === 'needs_quote';
                          const extension = request.mcrRequestType === 'rental_extension';
                          return (
                            <Paper key={request.mcrRequestId} variant="outlined" sx={{ p: 1.5 }}>
                              <Stack spacing={1}>
                                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ sm: 'center' }}>
                                  <Chip size="small" label={request.mcrRequestType.replace(/_/g, ' ')} />
                                  <Chip size="small" variant="outlined" label={request.mcrStatus.replace(/_/g, ' ')} />
                                  <Typography variant="caption" color="text.secondary">
                                    {formatDate(request.mcrRequestedAt)}
                                  </Typography>
                                </Stack>
                                <Typography variant="body2">{request.mcrReason}</Typography>
                                {request.mcrRequestedEndDate && (
                                  <Typography variant="body2">
                                    Fecha solicitada: {request.mcrRequestedEndDate}
                                  </Typography>
                                )}
                                {request.mcrReviewNotes && (
                                  <Typography variant="body2" color="text.secondary">
                                    Revisión: {request.mcrReviewNotes}
                                  </Typography>
                                )}
                                {reviewable && (
                                  <>
                                    <TextField
                                      label="Notas para el cliente"
                                      value={customerRequestReviewNotes}
                                      onChange={(event) => setCustomerRequestReviewNotes(event.target.value)}
                                      multiline
                                      minRows={2}
                                      inputProps={{ minLength: 3, maxLength: 1000 }}
                                      helperText={extension
                                        ? 'Una extensión solo puede requerir cotización o rechazarse; no edites la fecha directamente.'
                                        : 'Confirma el estado actual antes de aplicar la transición de dominio.'}
                                    />
                                    <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                                      {request.mcrStatus === 'submitted' && !extension && (
                                        <Button
                                          variant="contained"
                                          color="success"
                                          disabled={customerRequestReviewNotes.trim().length < 3
                                            || customerRequestReviewMutation.isPending}
                                          onClick={() => customerRequestReviewMutation.mutate({
                                            requestId: request.mcrRequestId,
                                            action: 'approve',
                                          })}
                                        >
                                          Aprobar transición
                                        </Button>
                                      )}
                                      {request.mcrStatus === 'submitted' && extension && (
                                        <Button
                                          variant="contained"
                                          disabled={customerRequestReviewNotes.trim().length < 3
                                            || customerRequestReviewMutation.isPending}
                                          onClick={() => customerRequestReviewMutation.mutate({
                                            requestId: request.mcrRequestId,
                                            action: 'needs_quote',
                                          })}
                                        >
                                          Requiere cotización
                                        </Button>
                                      )}
                                      <Button
                                        variant="outlined"
                                        color="error"
                                        disabled={customerRequestReviewNotes.trim().length < 3
                                          || customerRequestReviewMutation.isPending}
                                        onClick={() => customerRequestReviewMutation.mutate({
                                          requestId: request.mcrRequestId,
                                          action: 'reject',
                                        })}
                                      >
                                        Rechazar
                                      </Button>
                                    </Stack>
                                  </>
                                )}
                              </Stack>
                            </Paper>
                          );
                        })}
                        {customerRequestReviewMutation.isError && (
                          <Alert severity="error">
                            {customerRequestReviewMutation.error?.message ?? 'No se pudo revisar la solicitud'}
                          </Alert>
                        )}
                      </Stack>
                    </CardContent>
                  </Card>
                )}

                {isCanonicalRental && (
                  ['refund_due', 'partial_refund_due'].includes(selectedOrder.moDepositStatus ?? '')
                    || (depositSettlementsQuery.data?.length ?? 0) > 0
                ) && (
                  <Card variant="outlined">
                    <CardHeader
                      title="Liquidación manual del depósito"
                      subheader="Registra evidencia real. Esto no ejecuta ni afirma un reembolso de Datafast o PayPal."
                    />
                    <CardContent>
                      <Stack spacing={1.5}>
                        {depositSettlementsQuery.isError && (
                          <Alert severity="error">No se pudo cargar la evidencia protegida de liquidación.</Alert>
                        )}
                        {(depositSettlementsQuery.data?.length ?? 0) === 0 && (
                          <>
                            <Alert severity="warning" variant="outlined">
                              Depósito: {selectedOrder.moSecurityDepositUsdCents ?? 0} {selectedOrder.moCurrency} centavos · deducción aprobada: {selectedOrder.moDepositDeductionUsdCents ?? 0} · devolución esperada: {(selectedOrder.moSecurityDepositUsdCents ?? 0) - (selectedOrder.moDepositDeductionUsdCents ?? 0)}.
                            </Alert>
                            <FormControl fullWidth size="small">
                              <InputLabel id="deposit-settlement-method-label">Método real de liquidación</InputLabel>
                              <Select
                                labelId="deposit-settlement-method-label"
                                label="Método real de liquidación"
                                value={depositSettlementMethod}
                                onChange={(event) => setDepositSettlementMethod(
                                  event.target.value as MarketplaceDepositSettlementSubmitPayload['mdssSettlementMethod'],
                                )}
                              >
                                <MenuItem value="bank_transfer">Transferencia bancaria</MenuItem>
                                <MenuItem value="cash">Efectivo</MenuItem>
                                <MenuItem value="pos">POS</MenuItem>
                                {(selectedOrder.moDepositDeductionUsdCents ?? 0)
                                  === (selectedOrder.moSecurityDepositUsdCents ?? 0) && (
                                  <MenuItem value="forfeiture">Retención total documentada</MenuItem>
                                )}
                              </Select>
                            </FormControl>
                            <TextField
                              label="Referencia externa"
                              value={depositExternalReference}
                              onChange={(event) => setDepositExternalReference(event.target.value)}
                              inputProps={{ minLength: 3, maxLength: 160 }}
                            />
                            <TextField
                              label="Evidencia privada (HTTPS o /assets/)"
                              value={depositEvidenceUrl}
                              onChange={(event) => setDepositEvidenceUrl(event.target.value)}
                              inputProps={{ maxLength: 2048 }}
                            />
                            <Button
                              variant="contained"
                              disabled={depositSettlementMutation.isPending
                                || depositExternalReference.trim().length < 3
                                || !(depositEvidenceUrl.trim().startsWith('https://')
                                  || depositEvidenceUrl.trim().startsWith('/assets/'))}
                              onClick={() => depositSettlementMutation.mutate({
                                mdssSettlementMethod: depositSettlementMethod,
                                mdssExternalReference: depositExternalReference.trim(),
                                mdssEvidenceUrl: depositEvidenceUrl.trim(),
                              })}
                            >
                              Enviar evidencia para revisión independiente
                            </Button>
                          </>
                        )}
                        {depositSettlementsQuery.data?.map((settlement) => (
                          <Paper key={settlement.mdsSettlementId} variant="outlined" sx={{ p: 1.5 }}>
                            <Stack spacing={1}>
                              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ sm: 'center' }}>
                                <Chip size="small" label={settlement.mdsStatus.replace(/_/g, ' ')} />
                                <Typography variant="body2">
                                  {settlement.mdsSettlementMethod.replace(/_/g, ' ')} · devolución {settlement.mdsRefundAmountMinor} {settlement.mdsCurrency} centavos · deducción {settlement.mdsDeductionAmountMinor}
                                </Typography>
                              </Stack>
                              <Typography variant="body2">
                                Referencia: {settlement.mdsExternalReference}
                              </Typography>
                              <Link href={settlement.mdsEvidenceUrl} target="_blank" rel="noreferrer">
                                Abrir evidencia protegida
                              </Link>
                              {settlement.mdsReviewNotes && (
                                <Typography variant="body2" color="text.secondary">
                                  Revisión: {settlement.mdsReviewNotes}
                                </Typography>
                              )}
                              {settlement.mdsStatus === 'submitted' && (
                                <>
                                  <TextField
                                    label="Notas de revisión independiente"
                                    value={depositReviewNotes}
                                    onChange={(event) => setDepositReviewNotes(event.target.value)}
                                    multiline
                                    minRows={2}
                                    inputProps={{ minLength: 3, maxLength: 1000 }}
                                  />
                                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                                    <Button
                                      variant="contained"
                                      color="success"
                                      disabled={depositReviewNotes.trim().length < 3 || depositReviewMutation.isPending}
                                      onClick={() => depositReviewMutation.mutate({
                                        settlementId: settlement.mdsSettlementId,
                                        action: 'approve',
                                      })}
                                    >
                                      Verificar evidencia real
                                    </Button>
                                    <Button
                                      variant="outlined"
                                      disabled={depositReviewNotes.trim().length < 3 || depositReviewMutation.isPending}
                                      onClick={() => depositReviewMutation.mutate({
                                        settlementId: settlement.mdsSettlementId,
                                        action: 'requires_reconciliation',
                                      })}
                                    >
                                      Requiere conciliación
                                    </Button>
                                    <Button
                                      variant="outlined"
                                      color="error"
                                      disabled={depositReviewNotes.trim().length < 3 || depositReviewMutation.isPending}
                                      onClick={() => depositReviewMutation.mutate({
                                        settlementId: settlement.mdsSettlementId,
                                        action: 'reject',
                                      })}
                                    >
                                      Rechazar
                                    </Button>
                                  </Stack>
                                </>
                              )}
                            </Stack>
                          </Paper>
                        ))}
                        {(depositSettlementMutation.isError || depositReviewMutation.isError) && (
                          <Alert severity="error">
                            {depositSettlementMutation.error?.message
                              ?? depositReviewMutation.error?.message
                              ?? 'No se pudo completar la revisión del depósito'}
                          </Alert>
                        )}
                      </Stack>
                    </CardContent>
                  </Card>
                )}

                {showStatusHistorySection && (
                  <>
                    <Divider />
                    <Stack spacing={1}>
                      <Typography variant="h6">Historial de estado</Typography>
                      <Stack spacing={0.5}>
                        {selectedStatusHistory.map(([st, ts], idx) => (
                          <Typography key={`${st}-${ts}-${idx}`} variant="body2" color="text.secondary">
                            {formatDate(ts)} — {statusLabel(st)}
                          </Typography>
                        ))}
                      </Stack>
                    </Stack>
                  </>
                )}
                {(hasCanonicalFulfillment || isCanonicalRental) && (selectedOrder.moFulfillmentHistory?.length ?? 0) > 0 && (
                  <>
                    <Divider />
                    <Stack spacing={1}>
                      <Typography variant="h6">{isCanonicalRental ? 'Historial de renta' : 'Historial de entrega'}</Typography>
                      <Stack spacing={0.5}>
                        {selectedOrder.moFulfillmentHistory?.map(([status, occurredAt], index) => (
                          <Typography key={`${status}-${occurredAt}-${index}`} variant="body2" color="text.secondary">
                            {formatDate(occurredAt)} — {isCanonicalRental ? rentalStatusLabel(status) : fulfillmentStatusLabel(status)}
                          </Typography>
                        ))}
                      </Stack>
                    </Stack>
                  </>
                )}
                <Divider />
                <Typography variant="h6">Items</Typography>
                {selectedOrder.moItems.length === 1 ? (
                  <Stack
                    spacing={0.5}
                    data-testid="marketplace-single-item-detail"
                  >
                    <Typography variant="body2" fontWeight={600}>
                      {selectedOrder.moItems[0]?.moiTitle}
                    </Typography>
                    <Typography variant="body2" color="text.secondary">
                      {selectedOrder.moItems[0]?.moiQuantity} × {selectedOrder.moItems[0]?.moiUnitPriceDisplay}
                      {' · '}
                      Subtotal {selectedOrder.moItems[0]?.moiSubtotalDisplay}
                    </Typography>
                  </Stack>
                ) : selectedOrder.moItems.length > 1 ? (
                  <Table size="small">
                    <TableHead>
                      <TableRow>
                        <TableCell>Producto</TableCell>
                        <TableCell>Cantidad</TableCell>
                        <TableCell>Precio</TableCell>
                        <TableCell>Subtotal</TableCell>
                      </TableRow>
                    </TableHead>
                    <TableBody>
                      {selectedOrder.moItems.map((it) => (
                        <TableRow key={it.moiListingId}>
                          <TableCell>{it.moiTitle}</TableCell>
                          <TableCell>{it.moiQuantity}</TableCell>
                          <TableCell>{it.moiUnitPriceDisplay}</TableCell>
                          <TableCell>{it.moiSubtotalDisplay}</TableCell>
                        </TableRow>
                      ))}
                    </TableBody>
                  </Table>
                ) : (
                  <Typography variant="body2" color="text.secondary">
                    Sin items registrados para esta orden.
                  </Typography>
                )}
              </Stack>
            </DialogContent>
            <DialogActions>
              <Button onClick={closeDialog}>Cerrar</Button>
            </DialogActions>
            <Menu anchorEl={copyMenuAnchorEl} open={Boolean(copyMenuAnchorEl)} onClose={closeCopyMenu}>
              <MenuItem onClick={() => runCopyMenuAction((order) => handleCopyOrderId(order.moOrderId))}>
                Copiar ID
              </MenuItem>
              <MenuItem onClick={() => runCopyMenuAction(copyOrderSummary)}>
                Copiar resumen
              </MenuItem>
            </Menu>
          </>
        )}
      </Dialog>
      <Snackbar
        open={Boolean(toast)}
        autoHideDuration={2500}
        onClose={() => setToast(null)}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'center' }}
      >
        <Alert onClose={() => setToast(null)} severity="success" sx={{ width: '100%' }}>
          {toast}
        </Alert>
      </Snackbar>
      <ConfirmDialog
        open={statusConfirmOpen}
        onClose={() => setStatusConfirmOpen(false)}
        onConfirm={() => {
          void handleStatusConfirm();
        }}
        title="Confirmar cambio de estado"
        description={`¿Confirmas cambiar el estado a "${pendingSavePayload?.payload.mouStatus ?? ''}"?`}
        severity="warning"
        confirming={updateMutation.isPending}
      />
    </Box>
  );
}
