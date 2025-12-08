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

const statusColor = (value: string): ChipProps['color'] => {
  const match = STATUS_PRESETS.find((p) => p.value === value);
  return match?.color ?? (value.toLowerCase().includes('fail') ? 'error' : 'default');
};

const statusLabel = (value: string): string => {
  const match = STATUS_PRESETS.find((p) => p.value === value);
  if (match) return match.label;
  if (value.toLowerCase().includes('datafast')) return 'Tarjeta';
  if (value.toLowerCase().includes('paypal')) return 'PayPal';
  return value;
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

export default function MarketplaceOrdersPage() {
  const qc = useQueryClient();
  const [statusFilter, setStatusFilter] = useState<string>('all');
  const [providerFilter, setProviderFilter] = useState<string>('all');
  const [fromDate, setFromDate] = useState<string>('');
  const [toDate, setToDate] = useState<string>('');
  const [search, setSearch] = useState('');
  const [paidOnly, setPaidOnly] = useState(false);
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
  });

  const orders = useMemo(() => ordersQuery.data ?? [], [ordersQuery.data]);
  const sortedOrders = useMemo(
    () => [...orders].sort((a, b) => (a.moCreatedAt > b.moCreatedAt ? -1 : 1)),
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

  const filtered = useMemo(() => {
    const term = search.trim().toLowerCase();
    const fromDt = fromDate ? DateTime.fromISO(fromDate) : null;
    const toDt = toDate ? DateTime.fromISO(toDate).endOf('day') : null;
    return sortedOrders.filter((order) => {
      if (statusFilter !== 'all' && order.moStatus !== statusFilter) return false;
      if (providerFilter !== 'all') {
        const provider = order.moPaymentProvider?.toLowerCase() ?? '';
        if (provider !== providerFilter.toLowerCase()) return false;
      }
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
  }, [sortedOrders, search, providerFilter, statusFilter, fromDate, toDate, paidOnly]);

  const filtersDirty =
    statusFilter !== 'all' || providerFilter !== 'all' || search.trim() !== '' || Boolean(fromDate) || Boolean(toDate) || paidOnly;
  const filtersActiveCount =
    (statusFilter !== 'all' ? 1 : 0) +
    (providerFilter !== 'all' ? 1 : 0) +
    (search.trim() ? 1 : 0) +
    (fromDate ? 1 : 0) +
    (toDate ? 1 : 0) +
    (paidOnly ? 1 : 0);
  const paidTotal = orders.filter((o) => o.moStatus === 'paid').length;
  const paidVisible = filtered.filter((o) => o.moStatus === 'paid').length;

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

  const clearFilters = () => {
    setStatusFilter('all');
    setProviderFilter('all');
    setSearch('');
    setFromDate('');
    setToDate('');
    setPaidOnly(false);
  };
  const applyPreset = (preset: 'last7' | 'paid' | 'paypal' | 'card') => {
    if (preset === 'last7') {
      const dt = DateTime.now().minus({ days: 7 }).toFormat("yyyy-LL-dd'T'00:00");
      setFromDate(dt);
      setToDate('');
    }
    if (preset === 'paid') {
      setStatusFilter('paid');
    }
    if (preset === 'paypal') {
      setProviderFilter('paypal');
    }
    if (preset === 'card') {
      setStatusFilter('datafast_pending');
      setProviderFilter('datafast');
    }
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

  const copyRow = async (order: MarketplaceOrderDTO) => {
    const row = [
      order.moOrderId,
      order.moBuyerName ?? '',
      order.moBuyerEmail ?? '',
      order.moTotalDisplay,
      order.moStatus,
      order.moPaymentProvider ?? '',
      formatDate(order.moCreatedAt),
      formatDate(order.moPaidAt),
    ].join('\t');
    try {
      await navigator.clipboard.writeText(row);
    } catch {
      // ignore clipboard failures
    }
  };

  const confirmIfIrreversible = (nextStatus: string): boolean => {
    const risky = ['paid', 'cancelled', 'refunded', 'failed'];
    if (!risky.includes(nextStatus)) return true;
    return window.confirm(`¿Confirmas cambiar el estado a "${nextStatus}"?`);
  };

  const effectiveStatus = (statusInput ?? selectedOrder?.moStatus ?? '').trim();
  const effectiveProvider = (paymentProviderInput ?? selectedOrder?.moPaymentProvider ?? '').trim();
  const warnMissingProvider = Boolean(selectedOrder && !effectiveProvider);
  const warnMissingPaidAt = Boolean(selectedOrder && effectiveStatus === 'paid' && !paidAtInput);
  const blockSave =
    effectiveStatus === 'paid' && (warnMissingProvider || warnMissingPaidAt);
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

  return (
    <Box p={2}>
      <Alert severity="info" sx={{ mb: 2 }}>
        Órdenes del marketplace. Solo Admin/Operación pueden editar estados y pagos.
      </Alert>
          <Stack direction="row" alignItems="center" spacing={1} mb={2}>
            <LocalMallIcon color="primary" />
            <Typography variant="h4" fontWeight={700}>
              Órdenes del marketplace
            </Typography>
            <Chip label={`${orders.length} órdenes`} color="info" size="small" />
            <Chip label={`${filtered.length} resultado${filtered.length === 1 ? '' : 's'}`} color="default" size="small" />
            <Box flex={1} />
            <Tooltip title="Recargar">
              <IconButton onClick={handleRefresh}>
                <RefreshIcon />
              </IconButton>
            </Tooltip>
          </Stack>

      <Grid container spacing={2} mb={1}>
        <Grid item xs={12} md={5} lg={4}>
          <TextField
            fullWidth
            label="Buscar por comprador, email o ID"
            value={search}
            onChange={(e: ChangeEvent<HTMLInputElement>) => setSearch(e.target.value)}
          />
        </Grid>
        <Grid item xs={12} md={3} lg={3}>
          <FormControl fullWidth>
            <InputLabel id="status-filter-label">Estado</InputLabel>
            <Select
              labelId="status-filter-label"
              label="Estado"
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
        </Grid>
        <Grid item xs={12} md={4} lg={3}>
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
        </Grid>
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
      </Grid>
      <Stack direction="row" spacing={1} mb={2} alignItems="center" flexWrap="wrap">
        <Button size="small" variant="outlined" onClick={() => applyPreset('last7')}>
          Últimos 7 días
        </Button>
        <Button size="small" variant="outlined" onClick={() => applyPreset('paid')}>
          Pagado
        </Button>
        <Button size="small" variant="outlined" onClick={() => applyPreset('paypal')}>
          PayPal
        </Button>
        <Button size="small" variant="outlined" onClick={() => applyPreset('card')}>
          Tarjeta pendiente
        </Button>
        <Box flex={1} />
        {filtersActiveCount > 0 && (
          <Button size="small" onClick={copyFiltersLink}>
            Copiar enlace de filtros
          </Button>
        )}
        {statusFilter !== 'all' && (
          <Chip size="small" label={`Estado: ${statusFilter}`} onDelete={() => setStatusFilter('all')} />
        )}
        {providerFilter !== 'all' && (
          <Chip size="small" label={`Pago: ${providerFilter}`} onDelete={() => setProviderFilter('all')} />
        )}
        {search.trim() && <Chip size="small" label={`Busca: ${search}`} onDelete={() => setSearch('')} />}
        {fromDate && <Chip size="small" label={`Desde: ${fromDate}`} onDelete={() => setFromDate('')} />}
        {toDate && <Chip size="small" label={`Hasta: ${toDate}`} onDelete={() => setToDate('')} />}
        {paidOnly && <Chip size="small" label="Con pago" onDelete={() => setPaidOnly(false)} />}
        {filtersActiveCount > 0 && (
          <Chip label={`${filtersActiveCount} filtro${filtersActiveCount === 1 ? '' : 's'} activos`} size="small" />
        )}
        <Button onClick={clearFilters} disabled={!filtersDirty} variant="text">
          Limpiar filtros
        </Button>
      </Stack>
      {paidTotal > 0 && paidVisible === 0 && filtersDirty && (
        <Alert severity="info" sx={{ mb: 2 }}>
          Hay órdenes pagadas, pero no coinciden con los filtros actuales. Ajusta los filtros o desmarca &quot;Solo con pago&quot;.
        </Alert>
      )}

      <Card variant="outlined">
        <CardHeader
          title="Pedidos recientes"
          subheader="Revisa el estado, pagos y detalles de cada pedido."
          action={
            <Stack direction="row" spacing={1}>
              <Chip
                icon={<CheckCircleIcon />}
                label={`${orders.filter((o) => o.moStatus === 'paid').length} pagados`}
                color="success"
                variant="outlined"
              />
              <Chip
                icon={<InventoryIcon />}
                label={`${orders.filter((o) => o.moStatus !== 'paid').length} pendientes`}
                color="warning"
                variant="outlined"
              />
              <Button size="small" variant="outlined" onClick={exportCsv} disabled={filtered.length === 0}>
                Exportar CSV
              </Button>
              {filtersDirty && (
                <Chip label="Filtros activos" size="small" color="info" variant="filled" />
              )}
            </Stack>
          }
        />
        <CardContent>
          {ordersQuery.isError && <Alert severity="error">{ordersQuery.error?.message ?? 'Error al cargar órdenes'}</Alert>}
          {ordersQuery.isLoading && <Typography color="text.secondary">Cargando órdenes...</Typography>}
          {!ordersQuery.isLoading && filtered.length === 0 && (
            <Alert
              severity="info"
              action={
                <Button
                  size="small"
                  variant="outlined"
                  href="/marketplace"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  Ir al marketplace
                </Button>
              }
            >
              No hay órdenes con estos filtros. Revisa el marketplace si necesitas crear un pedido.
            </Alert>
          )}
          {filtered.length > 0 && (
            <TableContainer component={Paper}>
              <Table size="small">
                <TableHead>
                  <TableRow>
                    <TableCell>Pedido</TableCell>
                    <TableCell>Cliente</TableCell>
                    <TableCell>Contacto</TableCell>
                    <TableCell>Estado</TableCell>
                  <TableCell align="right">Total</TableCell>
                  <TableCell>Pago</TableCell>
                  <TableCell>Creado</TableCell>
                  <TableCell>Pagado</TableCell>
                  <TableCell>Items</TableCell>
                  <TableCell align="right">Acciones</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {filtered.map((order) => (
                  <TableRow
                      key={order.moOrderId}
                      hover
                      onClick={() => openOrder(order.moOrderId)}
                      sx={{ cursor: 'pointer' }}
                    >
                      <TableCell>
                        <Stack spacing={0.5}>
                          <Stack direction="row" spacing={0.5} alignItems="center">
                            <Tooltip title={order.moOrderId}>
                              <Typography variant="body2" fontWeight={600}>
                                {order.moOrderId.slice(0, 8)}
                              </Typography>
                            </Tooltip>
                            <Tooltip title="Copiar ID">
                              <IconButton
                                size="small"
                                onClick={(e) => {
                                  e.stopPropagation();
                                  void handleCopyOrderId(order.moOrderId);
                                }}
                              >
                                <ContentCopyIcon fontSize="inherit" />
                              </IconButton>
                            </Tooltip>
                          </Stack>
                          <Typography variant="caption" color="text.secondary">
                            {order.moCurrency.toUpperCase()}
                          </Typography>
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
                      <TableCell>
                        {order.moBuyerPhone ? (
                          <Link
                            href={`tel:${order.moBuyerPhone.replace(/\s+/g, '')}`}
                            underline="hover"
                            color="text.primary"
                            variant="body2"
                            onClick={(e) => e.stopPropagation()}
                          >
                            {order.moBuyerPhone}
                          </Link>
                        ) : (
                          '—'
                        )}
                      </TableCell>
                      <TableCell>
                        <Chip size="small" label={statusLabel(order.moStatus)} color={statusColor(order.moStatus)} />
                      </TableCell>
                      <TableCell align="right">{order.moTotalDisplay}</TableCell>
                      <TableCell>
                        <Stack spacing={0.5}>
                          <Typography variant="body2">{order.moPaymentProvider ?? '—'}</Typography>
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
                      <TableCell>{formatDate(order.moCreatedAt)}</TableCell>
                      <TableCell>{formatDate(order.moPaidAt)}</TableCell>
                      <TableCell>
                        <Typography variant="body2">{order.moItems.length}</Typography>
                        <Typography variant="caption" color="text.secondary">
                          {summarizeItems(order.moItems)}
                        </Typography>
                      </TableCell>
                      <TableCell align="right">
                        <Tooltip title="Copiar fila (TSV)">
                          <IconButton
                            size="small"
                            onClick={(e) => {
                              e.stopPropagation();
                              void copyRow(order);
                            }}
                          >
                            <ContentCopyIcon fontSize="inherit" />
                          </IconButton>
                        </Tooltip>
                      </TableCell>
                    </TableRow>
                  ))}
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
                            <IconButton size="small" onClick={() => void handleCopyOrderId(selectedOrder.moOrderId)}>
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
                          <InputLabel id="status-input-label">Estado</InputLabel>
                          <Select
                            labelId="status-input-label"
                            label="Estado"
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
                          <Alert severity={effectiveStatus === 'paid' ? 'warning' : 'info'} variant="outlined">
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
                          <Button variant="outlined" onClick={markPaidNow} startIcon={<CheckCircleIcon />}>
                            Marcar pagado ahora
                          </Button>
                          <Button
                            variant="contained"
                            onClick={() => {
                              void handleSave();
                            }}
                            disabled={updateMutation.isPending || blockSave}
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
