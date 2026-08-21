import { logger } from '../utils/logger';
import { useEffect, useMemo, useState } from 'react';
import { useParams, Link as RouterLink } from 'react-router-dom';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  Divider,
  FormControl,
  InputLabel,
  Link,
  MenuItem,
  Select,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import ArrowBackIcon from '@mui/icons-material/ArrowBack';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import type {
  MarketplaceCustomerRequestDTO,
  MarketplaceCustomerRequestType,
  MarketplaceOrderDTO,
} from '../api/types';
import {
  Marketplace,
  clearMarketplaceRequestIdempotencyKey,
  getMarketplaceRequestIdempotencyKey,
  loadMarketplaceLookupToken,
} from '../api/marketplace';
import { getOrderStatusMeta } from '../utils/marketplace';
import ExperienceReviews from '../components/reviews/ExperienceReviews';

const requestLabels: Record<MarketplaceCustomerRequestType, string> = {
  sale_cancellation: 'Cancelar antes de la entrega',
  sale_return: 'Solicitar devolución',
  rental_cancellation: 'Cancelar antes de la entrega del equipo',
  rental_extension: 'Solicitar extensión de fechas',
  rental_dispute: 'Disputar condición, cargo o depósito',
};

const requestStatusLabels: Record<MarketplaceCustomerRequestDTO['mcrStatus'], string> = {
  submitted: 'En revisión',
  needs_quote: 'Requiere cotización',
  approved: 'Aprobada',
  rejected: 'Rechazada',
};

const availableCustomerRequestTypes = (order: MarketplaceOrderDTO): MarketplaceCustomerRequestType[] => {
  const state = order.moFulfillmentStatus ?? '';
  if (order.moOrderKind === 'sale') {
    if (['ready_to_fulfill', 'picking', 'ready_for_pickup'].includes(state)) return ['sale_cancellation'];
    if (state === 'delivered') return ['sale_return'];
    return [];
  }
  if (order.moOrderKind !== 'rental') return [];
  const result: MarketplaceCustomerRequestType[] = [];
  if (['confirmed', 'ready_for_handoff'].includes(state)) result.push('rental_cancellation');
  if (['confirmed', 'ready_for_handoff', 'checked_out', 'return_due'].includes(state)) {
    result.push('rental_extension');
  }
  if (['checked_out', 'return_due', 'returned_pending_inspection', 'damage_review', 'deposit_refund_due', 'lost'].includes(state)) {
    result.push('rental_dispute');
  }
  return result;
};

export default function MarketplaceOrderTrackingPage() {
  const { orderId } = useParams<{ orderId: string }>();
  const [order, setOrder] = useState<MarketplaceOrderDTO | null>(null);
  const [status, setStatus] = useState<'loading' | 'error' | 'success'>('loading');
  const [manualReference, setManualReference] = useState('');
  const [manualSubmitting, setManualSubmitting] = useState(false);
  const [manualError, setManualError] = useState<string | null>(null);
  const [customerRequests, setCustomerRequests] = useState<MarketplaceCustomerRequestDTO[]>([]);
  const [requestType, setRequestType] = useState<MarketplaceCustomerRequestType | ''>('');
  const [requestReason, setRequestReason] = useState('');
  const [requestedEndDate, setRequestedEndDate] = useState('');
  const [requestSubmitting, setRequestSubmitting] = useState(false);
  const [requestError, setRequestError] = useState<string | null>(null);

  useEffect(() => {
    const run = async () => {
      if (!orderId) {
        setStatus('error');
        return;
      }
      try {
        const lookupToken = loadMarketplaceLookupToken(orderId);
        if (!lookupToken) throw new Error('Missing secure order lookup token');
        const [dto, requests] = await Promise.all([
          Marketplace.getOrder(orderId, lookupToken),
          Marketplace.listCustomerRequests(orderId, lookupToken),
        ]);
        setOrder(dto);
        setCustomerRequests(requests);
        setStatus('success');
      } catch {
        setStatus('error');
      }
    };
    void run();
  }, [orderId]);

  const timeline = useMemo(() => order?.moStatusHistory ?? [], [order]);
  const fulfillmentTimeline = useMemo(() => order?.moFulfillmentHistory ?? [], [order]);
  const currentStatusMeta = useMemo(() => getOrderStatusMeta(order?.moStatus ?? ''), [order?.moStatus]);
  const availableRequestTypes = useMemo(
    () => order ? availableCustomerRequestTypes(order) : [],
    [order],
  );
  const formatMinor = (amount?: number | null) => new Intl.NumberFormat('es-EC', {
    style: 'currency',
    currency: order?.moCurrency ?? 'USD',
  }).format((amount ?? 0) / 100);

  const copyOrderId = () => {
    if (typeof window === 'undefined') return;
    if (orderId && navigator?.clipboard?.writeText) {
      navigator.clipboard.writeText(orderId).catch((err) => logger.warn('No se pudo copiar el ID del pedido', err));
    }
  };

  const submitManualEvidence = async () => {
    if (!orderId || manualReference.trim().length < 3) return;
    const lookupToken = loadMarketplaceLookupToken(orderId);
    if (!lookupToken) {
      setManualError('Falta la credencial segura de seguimiento. Abre el enlace desde este navegador o contacta soporte.');
      return;
    }
    setManualSubmitting(true);
    setManualError(null);
    try {
      const updated = await Marketplace.submitManualEvidence(
        orderId,
        manualReference.trim(),
        lookupToken,
      );
      setOrder(updated);
      setManualReference('');
    } catch {
      setManualError('No pudimos registrar la referencia. El pedido no fue marcado como pagado.');
    } finally {
      setManualSubmitting(false);
    }
  };

  const submitCustomerRequest = async () => {
    if (!orderId || !requestType || requestReason.trim().length < 3) return;
    if (requestType === 'rental_extension' && !requestedEndDate) return;
    const lookupToken = loadMarketplaceLookupToken(orderId);
    if (!lookupToken) {
      setRequestError('Falta la credencial segura de seguimiento. Abre el enlace desde este navegador o contacta soporte.');
      return;
    }
    setRequestSubmitting(true);
    setRequestError(null);
    try {
      const idempotencyKey = getMarketplaceRequestIdempotencyKey(orderId, requestType);
      const created = await Marketplace.submitCustomerRequest(
        orderId,
        {
          mcrsRequestType: requestType,
          mcrsReason: requestReason.trim(),
          ...(requestType === 'rental_extension'
            ? { mcrsRequestedEndDate: requestedEndDate }
            : {}),
        },
        lookupToken,
        idempotencyKey,
      );
      clearMarketplaceRequestIdempotencyKey(orderId, requestType);
      setCustomerRequests((current) => [
        created,
        ...current.filter((request) => request.mcrRequestId !== created.mcrRequestId),
      ]);
      setRequestType('');
      setRequestReason('');
      setRequestedEndDate('');
    } catch {
      setRequestError('No pudimos registrar la solicitud. El pago y la entrega no cambiaron; vuelve a intentar.');
    } finally {
      setRequestSubmitting(false);
    }
  };

  return (
    <Box sx={{ minHeight: '100vh', bgcolor: 'background.default', display: 'flex', alignItems: 'center', justifyContent: 'center', p: 2 }}>
      <Stack spacing={2} maxWidth={720} width="100%">
        <Stack direction="row" spacing={1} alignItems="center">
          <Button component={RouterLink} to="/marketplace" startIcon={<ArrowBackIcon />} size="small" variant="text">
            Volver al marketplace
          </Button>
          <Box flex={1} />
          <Button
            size="small"
            startIcon={<ContentCopyIcon />}
            variant="outlined"
            onClick={copyOrderId}
          >
            Copiar ID
          </Button>
        </Stack>

        <Typography variant="h4" fontWeight={800}>
          Seguimiento de pedido
        </Typography>
        {status === 'loading' && (
          <Stack spacing={1} alignItems="center">
            <CircularProgress size={24} />
            <Typography variant="body2" color="text.secondary">
              Cargando pedido...
            </Typography>
          </Stack>
        )}
        {status === 'error' && (
          <Alert severity="error">No pudimos encontrar este pedido. Verifica el enlace o contacta soporte.</Alert>
        )}
        {status === 'success' && order && (
          <Stack spacing={2}>
            <Card variant="outlined">
              <CardContent>
                <Stack direction="row" justifyContent="space-between" alignItems="center" spacing={1}>
                  <Typography variant="h6" fontWeight={700}>
                    Pedido {order.moOrderId}
                  </Typography>
                  <Chip label={currentStatusMeta.label} color={currentStatusMeta.color} />
                </Stack>
                <Typography variant="body2" color="text.secondary">
                  Total: {order.moTotalDisplay} · Creado: {new Date(order.moCreatedAt).toLocaleString()}
                </Typography>
                {order.moPaidAt && (
                  <Typography variant="body2" color="text.secondary">
                    Pagado el {new Date(order.moPaidAt).toLocaleString()} via {order.moPaymentProvider?.toUpperCase() ?? '—'}
                  </Typography>
                )}
                {order.moFulfillmentStatus && (
                  <Typography variant="body2" color="text.secondary">
                    {order.moOrderKind === 'rental' ? 'Renta' : 'Entrega'}: {order.moFulfillmentStatus.replace(/_/g, ' ')}
                    {order.moTrackingReference ? ` · Guía: ${order.moTrackingReference}` : ''}
                  </Typography>
                )}
                {order.moPaymentProvider === 'bank_transfer' && order.moCheckoutStatus !== 'paid' && (
                  <Box sx={{ mt: 1.5 }}>
                    {(!order.moManualPaymentStatus || order.moManualPaymentStatus === 'awaiting_evidence') && (
                      <Alert severity="warning" variant="outlined">
                        <Stack spacing={1}>
                          <Typography variant="body2">
                            El pedido está creado, pero no está pagado. Ingresa la referencia de tu transferencia para revisión.
                          </Typography>
                          <TextField
                            label="Referencia de transferencia"
                            value={manualReference}
                            onChange={(event) => setManualReference(event.target.value)}
                            inputProps={{ maxLength: 120 }}
                            size="small"
                          />
                          <Button
                            variant="contained"
                            disabled={manualSubmitting || manualReference.trim().length < 3}
                            onClick={() => { void submitManualEvidence(); }}
                          >
                            {manualSubmitting ? 'Enviando…' : 'Enviar evidencia para revisión'}
                          </Button>
                        </Stack>
                      </Alert>
                    )}
                    {order.moManualPaymentStatus === 'submitted' && (
                      <Alert severity="info" variant="outlined">
                        Referencia recibida. Sigue pendiente de revisión; este pedido aún no está pagado.
                      </Alert>
                    )}
                    {order.moManualPaymentStatus === 'under_review' && (
                      <Alert severity="info" variant="outlined">
                        La transferencia está bajo revisión. El pago todavía no está confirmado.
                      </Alert>
                    )}
                    {order.moManualPaymentStatus === 'approved' && (
                      <Alert severity="warning" variant="outlined">
                        La evidencia fue aprobada, pero el estado canónico de pago aún requiere conciliación.
                      </Alert>
                    )}
                    {order.moManualPaymentStatus === 'rejected' && (
                      <Alert severity="error" variant="outlined">
                        <Stack spacing={1}>
                          <Typography variant="body2">
                            La referencia fue rechazada. El pedido continúa impago; puedes enviar una referencia corregida.
                          </Typography>
                          <TextField
                            label="Referencia corregida"
                            value={manualReference}
                            onChange={(event) => setManualReference(event.target.value)}
                            inputProps={{ maxLength: 120 }}
                            size="small"
                          />
                          <Button
                            variant="contained"
                            disabled={manualSubmitting || manualReference.trim().length < 3}
                            onClick={() => { void submitManualEvidence(); }}
                          >
                            {manualSubmitting ? 'Enviando…' : 'Reenviar evidencia'}
                          </Button>
                        </Stack>
                      </Alert>
                    )}
                    {order.moManualPaymentStatus === 'requires_reconciliation' && (
                      <Alert severity="error" variant="outlined">
                        La evidencia requiere conciliación de soporte. El pedido no se considera pagado.
                      </Alert>
                    )}
                    {manualError && <Alert severity="error" sx={{ mt: 1 }}>{manualError}</Alert>}
                  </Box>
                )}
                {order.moOrderKind === 'rental' && (
                  <Alert severity="info" variant="outlined" sx={{ mt: 1.5 }}>
                    <Stack spacing={0.5}>
                      <Typography variant="body2">
                        Fechas: {order.moRentalStartDate} → {order.moRentalEndDate} ({order.moRentalDurationDays} día(s))
                      </Typography>
                      <Typography variant="body2">
                        Renta: {formatMinor(order.moRentalChargeUsdCents)} · depósito reembolsable: {formatMinor(order.moSecurityDepositUsdCents)}
                      </Typography>
                      <Typography variant="body2">
                        Estado del depósito: {(order.moDepositStatus ?? 'pendiente').replace(/_/g, ' ')}
                        {(order.moDepositDeductionUsdCents ?? 0) > 0
                          ? ` · deducción propuesta ${formatMinor(order.moDepositDeductionUsdCents)}`
                          : ''}
                      </Typography>
                      <Typography variant="caption">
                        El pago, la custodia del equipo y la devolución del depósito se confirman por separado.
                      </Typography>
                    </Stack>
                  </Alert>
                )}
                {(availableRequestTypes.length > 0 || customerRequests.length > 0) && (
                  <Box sx={{ mt: 2 }}>
                    <Divider sx={{ mb: 2 }} />
                    <Stack spacing={1.5}>
                      <Typography variant="subtitle1" fontWeight={700}>
                        Solicitudes de esta orden
                      </Typography>
                      {customerRequests.map((request) => (
                        <Alert
                          key={request.mcrRequestId}
                          severity={request.mcrStatus === 'approved'
                            ? 'success'
                            : request.mcrStatus === 'rejected' ? 'warning' : 'info'}
                          variant="outlined"
                        >
                          <Typography variant="body2" fontWeight={700}>
                            {requestLabels[request.mcrRequestType]} · {requestStatusLabels[request.mcrStatus]}
                          </Typography>
                          <Typography variant="body2">{request.mcrReason}</Typography>
                          {request.mcrRequestedEndDate && (
                            <Typography variant="body2">
                              Nueva fecha solicitada: {request.mcrRequestedEndDate}
                            </Typography>
                          )}
                          {request.mcrReviewNotes && (
                            <Typography variant="caption">Respuesta: {request.mcrReviewNotes}</Typography>
                          )}
                        </Alert>
                      ))}
                      {availableRequestTypes.length > 0 && (
                        <Stack spacing={1.25}>
                          <Alert severity="info" variant="outlined">
                            Enviar una solicitud no cancela, devuelve ni extiende automáticamente la orden. TDF debe revisarla. Las extensiones requieren disponibilidad y una nueva cotización.
                          </Alert>
                          <FormControl size="small" fullWidth>
                            <InputLabel id="marketplace-request-type-label">Tipo de solicitud</InputLabel>
                            <Select
                              labelId="marketplace-request-type-label"
                              label="Tipo de solicitud"
                              value={requestType}
                              onChange={(event) => {
                                setRequestType(event.target.value as MarketplaceCustomerRequestType);
                                setRequestError(null);
                              }}
                            >
                              {availableRequestTypes.map((type) => (
                                <MenuItem key={type} value={type}>{requestLabels[type]}</MenuItem>
                              ))}
                            </Select>
                          </FormControl>
                          {requestType === 'rental_extension' && (
                            <TextField
                              label="Nueva fecha de devolución"
                              type="date"
                              value={requestedEndDate}
                              onChange={(event) => setRequestedEndDate(event.target.value)}
                              InputLabelProps={{ shrink: true }}
                              inputProps={{ min: order.moRentalEndDate ?? undefined }}
                              size="small"
                              required
                            />
                          )}
                          <TextField
                            label="Motivo"
                            value={requestReason}
                            onChange={(event) => setRequestReason(event.target.value)}
                            multiline
                            minRows={2}
                            inputProps={{ minLength: 3, maxLength: 1000 }}
                            helperText="Describe el cambio solicitado. El estado actual se mantiene hasta revisión."
                            required
                          />
                          <Button
                            variant="outlined"
                            disabled={requestSubmitting
                              || !requestType
                              || requestReason.trim().length < 3
                              || (requestType === 'rental_extension' && !requestedEndDate)}
                            onClick={() => { void submitCustomerRequest(); }}
                          >
                            {requestSubmitting ? 'Enviando…' : 'Enviar solicitud'}
                          </Button>
                          {requestError && <Alert severity="error">{requestError}</Alert>}
                        </Stack>
                      )}
                    </Stack>
                  </Box>
                )}
                <Divider sx={{ my: 2 }} />
                <Stack spacing={0.75}>
                  <Typography variant="subtitle2">Ítems</Typography>
                  {order.moItems.map((it) => (
                    <Stack key={it.moiListingId} direction="row" justifyContent="space-between" alignItems="center">
                      <Typography variant="body2">
                        {it.moiQuantity} × {it.moiTitle || 'Ítem'}
                      </Typography>
                      <Typography variant="body2" fontWeight={700}>
                        {it.moiSubtotalDisplay}
                      </Typography>
                    </Stack>
                  ))}
                  <Typography variant="body2" fontWeight={800}>
                    Total: {order.moTotalDisplay}
                  </Typography>
                </Stack>
              </CardContent>
            </Card>

            {Array.from(new Map(order.moItems.map((item) => [item.moiListingId, item])).values()).map((item) => (
              <ExperienceReviews
                key={item.moiListingId}
                targetKind="marketplace_listing"
                targetId={item.moiListingId}
                title={`Reseñas de ${item.moiTitle || 'este artículo'}`}
              />
            ))}

            {fulfillmentTimeline.length > 0 && (
              <Card variant="outlined">
                <CardContent>
                  <Stack spacing={1}>
                    <Typography variant="subtitle1" fontWeight={700}>
                      {order.moOrderKind === 'rental' ? 'Historial de renta' : 'Historial de entrega'}
                    </Typography>
                    {fulfillmentTimeline.map(([fulfillmentStatus, timestamp]) => (
                      <Stack key={`${fulfillmentStatus}-${timestamp}`} direction="row" spacing={1} alignItems="center">
                        <Chip size="small" label={fulfillmentStatus.replace(/_/g, ' ')} />
                        <Typography variant="body2" color="text.secondary">
                          {new Date(timestamp).toLocaleString()}
                        </Typography>
                      </Stack>
                    ))}
                  </Stack>
                </CardContent>
              </Card>
            )}

            <Card variant="outlined">
              <CardContent>
                <Stack spacing={1}>
                  <Typography variant="subtitle1" fontWeight={700}>
                    Historial de estado
                  </Typography>
                  {timeline.length === 0 && (
                    <Typography variant="body2" color="text.secondary">
                      Aún no hay cambios registrados.
                    </Typography>
                  )}
                  {timeline.map(([st, ts]) => {
                    const meta = getOrderStatusMeta(st);
                    return (
                      <Stack key={`${st}-${ts}`} direction="row" spacing={1} alignItems="center">
                        <Chip size="small" label={meta.label} color={meta.color} />
                        <Typography variant="body2" color="text.secondary">
                          {new Date(ts).toLocaleString()}
                        </Typography>
                      </Stack>
                    );
                  })}
                </Stack>
              </CardContent>
            </Card>

            <Typography variant="caption" color="text.secondary">
              ¿Dudas? Contáctanos en <Link href="mailto:hola@tdf.lat">hola@tdf.lat</Link> con tu ID de pedido.
            </Typography>
          </Stack>
        )}
      </Stack>
    </Box>
  );
}
