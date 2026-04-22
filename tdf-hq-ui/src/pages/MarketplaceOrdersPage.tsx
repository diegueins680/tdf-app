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
import type { MarketplaceOrderDTO, MarketplaceOrderUpdatePayload } from '../api/types';
import { Marketplace } from '../api/marketplace';
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

const STATUS_PRESETS: { value: string; label: string; color: ChipProps['color'] }[] = [
  { value: 'paid', label: 'Pagado', color: 'success' },
  { value: 'pending', label: 'Pendiente', color: 'warning' },
  { value: 'paypal_pending', label: 'PayPal pendiente', color: 'info' },
  { value: 'datafast_init', label: 'Tarjeta iniciada', color: 'info' },
  { value: 'datafast_pending', label: 'Tarjeta en revisión', color: 'warning' },
  { value: 'datafast_failed', label: 'Tarjeta falló', color: 'error' },
  { value: 'contact', label: 'Contactar', color: 'default' },
  { value: 'cancelled', label: 'Cancelado', color: 'default' },
  { value: 'failed', label: 'Falló', color: 'error' },
  { value: 'refunded', label: 'Reembolsado', color: 'default' },
];

const QUICK_VIEW_PRESETS = [
  { value: 'last7', label: 'Últimos 7 días' },
  { value: 'paid', label: 'Pagado' },
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
  items.map((it) => `${it.moiQuantity} × ${it.moiTitle}`).join(' · ');

const normalizeProviderFilterValue = (value?: string | null) => value?.trim().toLowerCase() ?? '';
const normalizeBuyerPhoneValue = (value?: string | null) => value?.trim() ?? '';
const formatPaymentProvider = (value?: string | null) => (
  normalizeProviderFilterValue(value) ? getMarketplacePaymentProviderLabel(value ?? '') : '—'
);
const normalizeOrderCurrency = (value: string) => value.trim().toUpperCase();
const getOrderCurrencyCaption = (order: Pick<MarketplaceOrderDTO, 'moCurrency' | 'moTotalDisplay'>) => {
  const currency = normalizeOrderCurrency(order.moCurrency);
  if (!currency) return '';
  return order.moTotalDisplay.toUpperCase().includes(currency) ? '' : currency;
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

  useEffect(() => {
    if (!selectedOrder) return;
    setStatusInput(selectedOrder.moStatus);
    setPaymentProviderInput(selectedOrder.moPaymentProvider ?? '');
    setPaidAtInput(formatInputDate(selectedOrder.moPaidAt));
  }, [selectedOrder]);

  useEffect(() => {
    if (statusInput === 'paid' && !paidAtInput) {
      setPaidAtInput(DateTime.now().toFormat("yyyy-LL-dd'T'HH:mm"));
    }
  }, [statusInput, paidAtInput]);

  const normalizedProviderFilter = normalizeProviderFilterValue(providerFilter);

  const baseContextOrders = useMemo(() => {
    const term = search.trim().toLowerCase();
    const fromDt = fromDate ? DateTime.fromISO(fromDate) : null;
    const toDt = toDate ? DateTime.fromISO(toDate).endOf('day') : null;
    return sortedOrders.filter((order) => {
      if (paidOnly && !order.moPaidAt) return false;
      const created = DateTime.fromISO(order.moCreatedAt);
      if (fromDt && created < fromDt) return false;
      if (toDt && created > toDt) return false;
      if (!term) return true;
      const haystack = [
        order.moOrderId,
        order.moBuyerName,
        order.moBuyerEmail,
        order.moStatus,
        order.moPaymentProvider ?? '',
      ]
        .join(' ')
        .toLowerCase();
      return haystack.includes(term);
    });
  }, [sortedOrders, search, fromDate, toDate, paidOnly]);

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
  const hasVisiblePayerEmail = filtered.some((order) => Boolean(order.moPaypalPayerEmail?.trim()));
  const showPaymentProviderColumn =
    hasVisiblePayerEmail
    || visiblePaymentProviderSet.size > 1
    || (visiblePaymentProviderSet.size === 1 && (filtered.length === 1 || hasVisibleOrdersWithoutPaymentProvider));
  const showBuyerPhoneColumn = filtered.some((order) => normalizeBuyerPhoneValue(order.moBuyerPhone) !== '');
  const showPaidAtColumn = filtered.some((order) => Boolean(order.moPaidAt));

  const filtersDirty =
    statusFilter !== 'all' || providerFilter !== 'all' || search.trim() !== '' || Boolean(fromDate) || Boolean(toDate) || paidOnly;
  const hasSearchInput = search.trim() !== '';
  const hasNonSearchFiltersActive =
    statusFilter !== 'all' || providerFilter !== 'all' || Boolean(fromDate) || Boolean(toDate) || paidOnly;
  const showSearchOwnedFilterHelper = hasSearchInput && !hasNonSearchFiltersActive;
  const filtersActiveCount =
    (statusFilter !== 'all' ? 1 : 0) +
    (providerFilter !== 'all' ? 1 : 0) +
    (search.trim() ? 1 : 0) +
    (fromDate ? 1 : 0) +
    (toDate ? 1 : 0) +
    (paidOnly ? 1 : 0);
  const visiblePaidCount = filtered.filter((o) => isPaidOrderStatus(o.moStatus)).length;
  const visiblePendingCount = Math.max(filtered.length - visiblePaidCount, 0);
  const showVisibleOrderBreakdown = visiblePaidCount > 0 && visiblePendingCount > 0;
  const showExportCsvAction = filtered.length > 0;
  const paidTotal = orders.filter((o) => isPaidOrderStatus(o.moStatus)).length;
  const paidVisible = filtered.filter((o) => isPaidOrderStatus(o.moStatus)).length;
  const ordersSummary = summarizeMarketplaceOrderList({
    totalOrders: orders.length,
    visibleOrders: filtered.length,
    activeFilterCount: filtersActiveCount,
  });
  const showFirstOrderEmptyState = !ordersQuery.isLoading && !ordersQuery.isError && orders.length === 0;
  const showSingleOrderFocusedState =
    !ordersQuery.isLoading && !ordersQuery.isError && orders.length === 1 && !filtersDirty;
  const showOrderListHeaderActions =
    !showFirstOrderEmptyState
    && !showSingleOrderFocusedState
    && (showVisibleOrderBreakdown || showExportCsvAction);
  const showListChrome = ordersQuery.isLoading || (orders.length > 0 && !showSingleOrderFocusedState);
  const showQuickViewControl = !filtersDirty;
  const showActiveFiltersTray = hasNonSearchFiltersActive;
  const hasAdvancedFiltersActive = Boolean(fromDate) || Boolean(toDate) || paidOnly;
  const advancedFiltersButtonLabel = showAdvancedFilters
    ? 'Ocultar fechas y pago'
    : hasAdvancedFiltersActive
      ? 'Editar fechas y pago'
      : 'Mostrar fechas y pago';
  const showHeaderRefreshAction =
    Boolean(ordersQuery.error) || (!ordersQuery.isLoading && (orders.length > 1 || filtersDirty));
  const showPermissionNotice =
    !ordersQuery.isLoading && !showFirstOrderEmptyState && !showSingleOrderFocusedState;
  const emptyOrdersMessage = showSearchOwnedFilterHelper
    ? 'No hay órdenes para la búsqueda actual. Usa Limpiar dentro del campo de búsqueda para volver a la bandeja completa.'
    : 'No hay órdenes en la vista actual. Usa Limpiar filtros para volver a la bandeja completa.';
  const filterTrayHelperText = showSearchOwnedFilterHelper
    ? 'La búsqueda activa se maneja desde el campo superior. Usa Limpiar ahí para volver a la bandeja completa. Los demás filtros aparecerán aquí cuando combines más criterios.'
    : 'Los filtros activos aparecerán aquí cuando acotes la bandeja. Limpiar filtros aparecerá en ese momento.';

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
    params.set('status', statusFilter);
    params.set('provider', providerFilter);
    params.set('paidOnly', paidOnly ? '1' : '0');
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
    applyFilters(createDefaultMarketplaceOrderFilters());
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
    updateMutation.reset();
  };

  const handleSave = async () => {
    if (!selectedOrder) return;
    const payload: MarketplaceOrderUpdatePayload = {};
    const nextStatus = statusInput.trim();
    if (nextStatus && nextStatus !== selectedOrder.moStatus) {
      if (!confirmIfIrreversible(nextStatus)) return;
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
    await updateMutation.mutateAsync({ id: selectedOrder.moOrderId, payload });
  };

  const markPaidNow = () => {
    const nowStr = DateTime.now().toFormat("yyyy-LL-dd'T'HH:mm");
    setStatusInput('paid');
    setPaidAtInput(nowStr);
  };

  const handleCopyOrderId = async (orderId: string) => {
    try {
      await navigator.clipboard.writeText(orderId);
    } catch {
      // ignore clipboard failures silently
    }
  };

  const copyOrderSummary = async (order: MarketplaceOrderDTO) => {
    const summaryLines = [
      `Pedido: ${order.moOrderId}`,
      `Estado: ${statusLabel(order.moStatus)}`,
      `Total: ${order.moTotalDisplay} (${order.moCurrency.toUpperCase()})`,
      `Pago: ${order.moPaymentProvider ?? '—'}`,
      `Comprador: ${order.moBuyerName} (${order.moBuyerEmail ?? 'sin email'})`,
      `Items: ${summarizeItems(order.moItems)}`,
    ];
    try {
      await navigator.clipboard.writeText(summaryLines.join('\n'));
      setToast('Resumen copiado');
    } catch {
      // ignore clipboard failures silently
    }
  };

  const confirmIfIrreversible = (nextStatus: string): boolean => {
    const risky = ['paid', 'cancelled', 'refunded', 'failed'];
    if (!risky.includes(nextStatus)) return true;
    return window.confirm(`¿Confirmas cambiar el estado a "${nextStatus}"?`);
  };

  const effectiveStatus = (statusInput ?? selectedOrder?.moStatus ?? '').trim();
  const effectiveProvider = (paymentProviderInput ?? selectedOrder?.moPaymentProvider ?? '').trim();
  const warnMissingProvider = Boolean(selectedOrder && isPaidOrderStatus(effectiveStatus) && !effectiveProvider);
  const warnMissingPaidAt = Boolean(selectedOrder && isPaidOrderStatus(effectiveStatus) && !paidAtInput);
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
  const statusHint = (() => {
    if (!effectiveStatus) return null;
    if (effectiveStatus === 'datafast_pending') {
      return 'Pago con tarjeta en revisión. Espera confirmación o reintenta el cobro antes de marcar pagado.';
    }
    if (effectiveStatus === 'datafast_failed' || effectiveStatus === 'failed') {
      return 'Pago con tarjeta fallido. Reintenta el cobro o cambia el estado a contactar.';
    }
    if (effectiveStatus === 'paypal_pending') {
      return 'El cliente inició PayPal pero aún no confirma. Verifica en PayPal o comunícate con el cliente.';
    }
    return null;
  })();
  const showMarkPaidShortcut = Boolean(selectedOrder) && !isPaidOrderStatus(effectiveStatus);

  return (
    <Box p={2}>
      {showPermissionNotice && (
        <Alert severity="info" sx={{ mb: 2 }}>
          Órdenes del marketplace. Solo Admin/Operación pueden editar estados y pagos.
        </Alert>
      )}
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
                label="Buscar por comprador, email o ID"
                value={search}
                onChange={(e: ChangeEvent<HTMLInputElement>) => {
                  const nextSearch = e.target.value;
                  setSearch(nextSearch.trim() === '' ? '' : nextSearch);
                }}
                InputProps={{
                  endAdornment: hasSearchInput ? (
                    <InputAdornment position="end">
                      <Button
                        size="small"
                        aria-label="Limpiar búsqueda"
                        onClick={() => setSearch('')}
                        sx={{ minWidth: 0, px: 0.5, textTransform: 'none' }}
                      >
                        Limpiar
                      </Button>
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
                        {STATUS_PRESETS.map((st) => (
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
                <Grid item xs={12} md={12} lg={3}>
                  <FormControlLabel
                    control={<Checkbox checked={paidOnly} onChange={(e) => setPaidOnly(e.target.checked)} />}
                    label="Solo con pago registrado"
                  />
                </Grid>
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
                  {filtersActiveCount > 0 && (
                    <Button size="small" onClick={copyFiltersLink}>
                      Copiar enlace de filtros
                    </Button>
                  )}
                  {statusFilter !== 'all' && (
                    <Chip size="small" label={`Estado: ${statusLabel(statusFilter)}`} onDelete={() => setStatusFilter('all')} />
                  )}
                  {providerFilter !== 'all' && (
                    <Chip
                      size="small"
                      label={`Pago: ${getMarketplacePaymentProviderLabel(providerFilter)}`}
                      onDelete={() => setProviderFilter('all')}
                    />
                  )}
                  {fromDate && <Chip size="small" label={`Desde: ${fromDate}`} onDelete={() => setFromDate('')} />}
                  {toDate && <Chip size="small" label={`Hasta: ${toDate}`} onDelete={() => setToDate('')} />}
                  {paidOnly && <Chip size="small" label="Con pago" onDelete={() => setPaidOnly(false)} />}
                  <Button onClick={clearFilters} variant="text">
                    Limpiar filtros
                  </Button>
                </>
              ) : (
                <Typography variant="body2" color="text.secondary" sx={{ flex: 1 }}>
                  {filterTrayHelperText}
                </Typography>
              )}
            </Stack>
          </Stack>
          {paidTotal > 0 && paidVisible === 0 && filtersDirty && (
            <Alert severity="info" sx={{ mb: 2 }}>
              Hay órdenes pagadas, pero no coinciden con los filtros actuales. Ajusta los filtros o desmarca &quot;Solo con pago&quot;.
            </Alert>
          )}
        </>
      )}

      <Card variant="outlined">
        <CardHeader
          title="Pedidos recientes"
          subheader={
            showFirstOrderEmptyState
              ? 'La primera orden aparecerá aquí junto con su estado, pago y acciones de revisión.'
              : showSingleOrderFocusedState
                ? 'Solo hay una orden por ahora. Ábrela para revisar estado, pago y datos del comprador. Cuando llegue la segunda, aquí aparecerán filtros y exportación.'
                : 'Haz clic en una fila para revisar estado, pago y datos del comprador.'
          }
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
          {ordersQuery.isError && <Alert severity="error">{ordersQuery.error?.message ?? 'Error al cargar órdenes'}</Alert>}
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
              Todavía no hay órdenes. Cuando llegue la primera, aquí aparecerán búsqueda, filtros y exportación para revisar la bandeja.
            </Alert>
          )}
          {!showFirstOrderEmptyState && !ordersQuery.isLoading && filtered.length === 0 && (
            <Alert severity="info">
              {emptyOrdersMessage}
            </Alert>
          )}
          {sharedVisibleCurrencyCaption && (
            <Typography
              variant="body2"
              color="text.secondary"
              sx={{ mb: 1.5 }}
              data-testid="marketplace-orders-shared-currency"
            >
              Moneda visible: {sharedVisibleCurrencyCaption}.
            </Typography>
          )}
          {filtered.length > 0 && (
            <TableContainer component={Paper}>
              <Table size="small">
                <TableHead>
                  <TableRow>
                    <TableCell>Pedido</TableCell>
                    <TableCell>Cliente</TableCell>
                    {showBuyerPhoneColumn && <TableCell>Contacto</TableCell>}
                    <TableCell>Estado</TableCell>
                    <TableCell align="right">Total</TableCell>
                    {showPaymentProviderColumn && <TableCell>Pago</TableCell>}
                    <TableCell>Creado</TableCell>
                    {showPaidAtColumn && <TableCell>Pagado</TableCell>}
                    <TableCell>Items</TableCell>
                  </TableRow>
                </TableHead>
                <TableBody>
                {filtered.map((order) => {
                  const orderCurrencyCaption = sharedVisibleCurrencyCaption ? '' : getOrderCurrencyCaption(order);

                  return (
                    <TableRow
                      key={order.moOrderId}
                      hover
                      onClick={() => openOrder(order.moOrderId)}
                      sx={{ cursor: 'pointer' }}
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
                          {order.moBuyerName}
                        </Typography>
                        {order.moBuyerEmail ? (
                          <Link
                            href={`mailto:${order.moBuyerEmail}`}
                            underline="hover"
                            variant="caption"
                            color="text.secondary"
                            onClick={(e) => e.stopPropagation()}
                          >
                            {order.moBuyerEmail}
                          </Link>
                        ) : (
                          <Typography variant="caption" color="text.secondary">
                            —
                          </Typography>
                        )}
                      </TableCell>
                      {showBuyerPhoneColumn && (
                        <TableCell>
                          {normalizeBuyerPhoneValue(order.moBuyerPhone) ? (
                            <Link
                              href={`tel:${normalizeBuyerPhoneValue(order.moBuyerPhone).replace(/\s+/g, '')}`}
                              underline="hover"
                              color="text.primary"
                              variant="body2"
                              onClick={(e) => e.stopPropagation()}
                            >
                              {normalizeBuyerPhoneValue(order.moBuyerPhone)}
                            </Link>
                          ) : (
                            '—'
                          )}
                        </TableCell>
                      )}
                      <TableCell>
                        <Chip size="small" label={statusLabel(order.moStatus)} color={statusColor(order.moStatus)} />
                      </TableCell>
                      <TableCell align="right">{order.moTotalDisplay}</TableCell>
                      {showPaymentProviderColumn && (
                        <TableCell>
                          <Stack spacing={0.5}>
                            <Typography variant="body2">
                              {formatPaymentProvider(order.moPaymentProvider)}
                            </Typography>
                            {order.moPaypalPayerEmail && (
                              <Link
                                href={`mailto:${order.moPaypalPayerEmail}`}
                                underline="hover"
                                variant="caption"
                                color="text.secondary"
                                onClick={(e) => e.stopPropagation()}
                              >
                                {order.moPaypalPayerEmail}
                              </Link>
                            )}
                          </Stack>
                        </TableCell>
                      )}
                      <TableCell>{formatDate(order.moCreatedAt)}</TableCell>
                      {showPaidAtColumn && <TableCell>{formatDate(order.moPaidAt)}</TableCell>}
                      <TableCell>
                        <Typography variant="body2">{order.moItems.length}</Typography>
                        <Typography variant="caption" color="text.secondary">
                          {summarizeItems(order.moItems)}
                        </Typography>
                      </TableCell>
                    </TableRow>
                  );
                })}
                </TableBody>
              </Table>
            </TableContainer>
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
                        <Stack direction="row" spacing={1}>
                          <Tooltip title="Copiar ID de pedido">
                            <IconButton
                              size="small"
                              aria-label={`Copiar ID del pedido ${selectedOrder.moOrderId}`}
                              onClick={() => void handleCopyOrderId(selectedOrder.moOrderId)}
                            >
                              <ContentCopyIcon fontSize="small" />
                            </IconButton>
                          </Tooltip>
                          <Button size="small" onClick={() => void copyOrderSummary(selectedOrder)}>
                            Copiar resumen
                          </Button>
                        </Stack>
                      }
                    />
                    <CardContent>
                      <Stack spacing={1}>
                        <Typography variant="body2">
                          <strong>Comprador:</strong> {selectedOrder.moBuyerName}
                        </Typography>
                        <Typography variant="body2">
                          <strong>Email:</strong> {selectedOrder.moBuyerEmail}
                        </Typography>
                        <Typography variant="body2">
                          <strong>Teléfono:</strong> {selectedOrder.moBuyerPhone ?? '—'}
                        </Typography>
                        <Typography variant="body2">
                          <strong>Carrito:</strong> {selectedOrder.moCartId ?? '—'}
                        </Typography>
                        <Stack direction="row" spacing={1} alignItems="center">
                          <Typography variant="body2">
                            <strong>Estado:</strong>
                          </Typography>
                          <Chip size="small" label={statusLabel(selectedOrder.moStatus)} color={statusColor(selectedOrder.moStatus)} />
                        </Stack>
                        {selectedOrder.moStatusHistory.length > 0 && (
                          <Typography variant="body2" color="text.secondary">
                            Último cambio: {formatDate(selectedOrder.moStatusHistory[selectedOrder.moStatusHistory.length - 1]?.[1])}
                          </Typography>
                        )}
                        <Typography variant="body2">
                          <strong>Creado:</strong> {formatDate(selectedOrder.moCreatedAt)}
                        </Typography>
                        <Typography variant="body2">
                          <strong>Pago:</strong> {selectedOrder.moPaymentProvider ?? '—'}
                        </Typography>
                        {selectedOrder.moPaypalOrderId && (
                          <Typography variant="caption" color="text.secondary">
                            PayPal order: {selectedOrder.moPaypalOrderId}
                          </Typography>
                        )}
                      </Stack>
                    </CardContent>
                  </Card>
                  <Card variant="outlined" sx={{ flex: 1 }}>
                    <CardHeader title="Actualizar estado" />
                    <CardContent>
                      <Stack spacing={2}>
                        <FormControl fullWidth>
                          <InputLabel id="status-input-label">Nuevo estado</InputLabel>
                          <Select
                            labelId="status-input-label"
                            label="Nuevo estado"
                            value={statusInput}
                            onChange={(e) => setStatusInput(e.target.value)}
                          >
                            <MenuItem value="">
                              <em>Sin cambios</em>
                            </MenuItem>
                            {STATUS_PRESETS.map((st) => (
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
                          placeholder="paypal, transferencia, cash..."
                        />
                        <TextField
                          label="Fecha de pago"
                          type="datetime-local"
                          fullWidth
                          value={paidAtInput}
                          onChange={(e) => setPaidAtInput(e.target.value)}
                          InputLabelProps={{ shrink: true }}
                        />
                        {warnMissingProvider && (
                          <Alert severity="warning" variant="outlined">
                            No hay método de pago registrado. Ingresa paypal, datafast o manual para dejar trazabilidad.
                          </Alert>
                        )}
                        {warnMissingPaidAt && (
                          <Alert severity="warning" variant="outlined">
                            Agrega la fecha y hora del cobro si marcas la orden como pagada.
                          </Alert>
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
                          <Button
                            variant="contained"
                            onClick={() => {
                              void handleSave();
                            }}
                            disabled={updateMutation.isPending || blockSave || !hasOrderUpdateChange}
                          >
                            Guardar cambios
                          </Button>
                        </Stack>
                        {updateMutation.isError && (
                          <Alert severity="error">{updateMutation.error?.message ?? 'No se pudo actualizar'}</Alert>
                        )}
                      </Stack>
                    </CardContent>
                  </Card>
                </Stack>

                <Divider />
                {selectedOrder.moStatusHistory.length > 0 && (
                  <Stack spacing={1}>
                    <Typography variant="h6">Historial de estado</Typography>
                    <Stack spacing={0.5}>
                      {selectedOrder.moStatusHistory.map(([st, ts], idx) => (
                        <Typography key={`${st}-${ts}-${idx}`} variant="body2" color="text.secondary">
                          {formatDate(ts)} — {statusLabel(st)}
                        </Typography>
                      ))}
                    </Stack>
                  </Stack>
                )}
                <Divider />
                
                <Typography variant="h6">Items</Typography>
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
              </Stack>
            </DialogContent>
            <DialogActions>
              <Button onClick={closeDialog}>Cerrar</Button>
            </DialogActions>
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
    </Box>
  );
}
