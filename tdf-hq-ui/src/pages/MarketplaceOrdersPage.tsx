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
  FormControl,
  Grid,
  IconButton,
  InputLabel,
  MenuItem,
  Paper,
  Select,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  TextField,
  Tooltip,
  Typography,
  type ChipProps,
} from '@mui/material';
import RefreshIcon from '@mui/icons-material/Refresh';
import LocalMallIcon from '@mui/icons-material/LocalMall';
import InventoryIcon from '@mui/icons-material/Inventory';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';
import type { MarketplaceOrderDTO, MarketplaceOrderUpdatePayload } from '../api/types';
import { Marketplace } from '../api/marketplace';
import { DateTime } from 'luxon';

const STATUS_PRESETS: { value: string; label: string; color: ChipProps['color'] }[] = [
  { value: 'paid', label: 'Pagado', color: 'success' },
  { value: 'pending', label: 'Pendiente', color: 'warning' },
  { value: 'paypal_pending', label: 'PayPal pendiente', color: 'info' },
  { value: 'contact', label: 'Contactar', color: 'default' },
  { value: 'cancelled', label: 'Cancelado', color: 'default' },
  { value: 'refunded', label: 'Reembolsado', color: 'default' },
];

const statusColor = (value: string): ChipProps['color'] => {
  const match = STATUS_PRESETS.find((p) => p.value === value);
  return match?.color ?? 'default';
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
  const [search, setSearch] = useState('');
  const [selectedId, setSelectedId] = useState<string | null>(null);
  const [statusInput, setStatusInput] = useState<string>('');
  const [paymentProviderInput, setPaymentProviderInput] = useState<string>('');
  const [paidAtInput, setPaidAtInput] = useState<string>('');

  const ordersQuery = useQuery<MarketplaceOrderDTO[], Error>({
    queryKey: ['marketplace-orders', statusFilter],
    queryFn: () =>
      Marketplace.listOrders({
        status: statusFilter === 'all' ? undefined : statusFilter,
        limit: 200,
      }),
  });

  const orders = useMemo(() => ordersQuery.data ?? [], [ordersQuery.data]);
  const selectedOrder = useMemo(
    () => orders.find((o) => o.moOrderId === selectedId) ?? null,
    [orders, selectedId],
  );

  useEffect(() => {
    if (!selectedOrder) return;
    setStatusInput(selectedOrder.moStatus);
    setPaymentProviderInput(selectedOrder.moPaymentProvider ?? '');
    setPaidAtInput(formatInputDate(selectedOrder.moPaidAt));
  }, [selectedOrder]);

  const filtered = useMemo(() => {
    const term = search.trim().toLowerCase();
    if (!term) return orders;
    return orders.filter((order) => {
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
  }, [orders, search]);

  const updateMutation = useMutation<MarketplaceOrderDTO, Error, { id: string; payload: MarketplaceOrderUpdatePayload }>({
    mutationFn: ({ id, payload }) => Marketplace.updateOrder(id, payload),
    onSuccess: (data) => {
      qc.setQueryData(['marketplace-orders', statusFilter], (prev: MarketplaceOrderDTO[] | undefined) =>
        prev ? prev.map((o) => (o.moOrderId === data.moOrderId ? data : o)) : prev,
      );
      void qc.invalidateQueries({ queryKey: ['marketplace-orders'] });
    },
  });

  const handleRefresh = () => {
    void qc.invalidateQueries({ queryKey: ['marketplace-orders'] });
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
    if (nextStatus && nextStatus !== selectedOrder.moStatus) payload.mouStatus = nextStatus;
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

  return (
    <Box p={2}>
      <Stack direction="row" alignItems="center" spacing={1} mb={2}>
        <LocalMallIcon color="primary" />
        <Typography variant="h4" fontWeight={700}>
          Órdenes del marketplace
        </Typography>
        <Chip label={`${orders.length} órdenes`} color="info" size="small" />
        <Box flex={1} />
        <Tooltip title="Recargar">
          <IconButton onClick={handleRefresh}>
            <RefreshIcon />
          </IconButton>
        </Tooltip>
      </Stack>

      <Grid container spacing={2} mb={2}>
        <Grid item xs={12} md={6} lg={4}>
          <TextField
            fullWidth
            label="Buscar por comprador, email o ID"
            value={search}
            onChange={(e: ChangeEvent<HTMLInputElement>) => setSearch(e.target.value)}
          />
        </Grid>
        <Grid item xs={12} md={6} lg={4}>
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
      </Grid>

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
            </Stack>
          }
        />
        <CardContent>
          {ordersQuery.isError && <Alert severity="error">{ordersQuery.error?.message ?? 'Error al cargar órdenes'}</Alert>}
          {ordersQuery.isLoading && <Typography color="text.secondary">Cargando órdenes...</Typography>}
          {!ordersQuery.isLoading && filtered.length === 0 && (
            <Alert severity="info">No hay órdenes con estos filtros.</Alert>
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
                          <Typography variant="body2" fontWeight={600}>
                            {order.moOrderId.slice(0, 8)}
                          </Typography>
                          <Typography variant="caption" color="text.secondary">
                            {order.moCurrency.toUpperCase()}
                          </Typography>
                        </Stack>
                      </TableCell>
                      <TableCell>
                        <Typography variant="body2" fontWeight={600}>
                          {order.moBuyerName}
                        </Typography>
                        <Typography variant="caption" color="text.secondary">
                          {order.moBuyerEmail}
                        </Typography>
                      </TableCell>
                      <TableCell>{order.moBuyerPhone || '—'}</TableCell>
                      <TableCell>
                        <Chip size="small" label={order.moStatus} color={statusColor(order.moStatus)} />
                      </TableCell>
                      <TableCell align="right">{order.moTotalDisplay}</TableCell>
                      <TableCell>
                        <Stack spacing={0.5}>
                          <Typography variant="body2">{order.moPaymentProvider ?? '—'}</Typography>
                          {order.moPaypalPayerEmail && (
                            <Typography variant="caption" color="text.secondary">
                              {order.moPaypalPayerEmail}
                            </Typography>
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
                          <strong>Teléfono:</strong> {selectedOrder.moBuyerPhone || '—'}
                        </Typography>
                        <Typography variant="body2">
                          <strong>Creado:</strong> {formatDate(selectedOrder.moCreatedAt)}
                        </Typography>
                        <Typography variant="body2">
                          <strong>Pago:</strong> {selectedOrder.moPaymentProvider || '—'}
                        </Typography>
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
                        <Stack direction="row" spacing={1}>
                          <Button variant="outlined" onClick={markPaidNow} startIcon={<CheckCircleIcon />}>
                            Marcar pagado ahora
                          </Button>
                          <Button
                            variant="contained"
                            onClick={handleSave}
                            disabled={updateMutation.isPending}
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
    </Box>
  );
}
