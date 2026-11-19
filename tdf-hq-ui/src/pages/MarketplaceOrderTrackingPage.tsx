import { DateTime } from 'luxon';
import MarkdownText from '../components/MarkdownText';
import { logger } from '../utils/logger';
import { useEffect, useMemo, useRef, useState } from 'react';
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
  FormHelperText,
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
  MarketplaceCustomerRequestSubmitPayload,
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
import { marketplaceDepositStatusLabel, marketplaceFulfillmentStatusLabel } from '../utils/marketplaceStatusLabel';
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

// Match the existing server reference bounds and unsupported text categories.
function manualReferenceValidationError(value: string): string | null {
  const reference = value.trim();
  const length = Array.from(reference).length;
  if (length < 3 || length > 120) return 'Ingresa una referencia bancaria de 3 a 120 caracteres.';
  if (/[\p{Cc}\p{Cf}\p{Zl}\p{Zp}]/u.test(reference.replace(/[\n\r\t]/g, ''))) {
    return 'La referencia contiene caracteres no admitidos. Revísala sin borrar los demás datos.';
  }
  return null;
}

function requestReasonValidationError(value: string): string | null {
  const reason = value.trim();
  if (Array.from(reason).length < 3 || Array.from(reason).length > 1000) return 'Describe el motivo con 3 a 1000 caracteres.';
  if (/[\p{Cc}\p{Cf}\p{Zl}\p{Zp}]/u.test(reason.replace(/[\n\r\t]/g, ''))) return 'El motivo contiene caracteres no admitidos. Revisa el texto.';
  return null;
}

function extensionDateValidationError(value: string, currentEnd?: string | null): string | null {
  const valid = (date?: string | null) => Boolean(date && /^\d{4}-\d{2}-\d{2}$/.test(date) && DateTime.fromISO(date, { zone: 'UTC' }).isValid);
  if (!currentEnd || !valid(currentEnd)) return 'No pudimos validar la fecha actual de devolución. Actualiza el seguimiento.';
  if (!valid(value) || value <= currentEnd) return 'Elige una fecha posterior a la devolución actual.';
  return null;
}

export default function MarketplaceOrderTrackingPage() {
  const { orderId } = useParams<{ orderId: string }>();
  return <MarketplaceOrderTrackingContent key={orderId ?? 'missing'} orderId={orderId} />;
}

// A different order owns fresh drafts and pending operations; old responses cannot
// update the new order's mounted view.
function MarketplaceOrderTrackingContent({ orderId }: { orderId: string | undefined }) {
  const [order, setOrder] = useState<MarketplaceOrderDTO | null>(null);
  const [status, setStatus] = useState<'loading' | 'error' | 'success'>('loading');
  const [manualReference, setManualReference] = useState('');
  const [manualSubmitting, setManualSubmitting] = useState(false);
  const [manualError, setManualError] = useState<string | null>(null);
  const [manualReferenceError, setManualReferenceError] = useState<string | null>(null);
  const manualInFlight = useRef(false);
  const manualReferenceInput = useRef<HTMLInputElement>(null);
  const [customerRequests, setCustomerRequests] = useState<MarketplaceCustomerRequestDTO[]>([]);
  const [requestType, setRequestType] = useState<MarketplaceCustomerRequestType | ''>('');
  const [requestReason, setRequestReason] = useState('');
  const [requestedEndDate, setRequestedEndDate] = useState('');
  const [requestSubmitting, setRequestSubmitting] = useState(false);
  const [requestError, setRequestError] = useState<string | null>(null);
  const [requestTypeError, setRequestTypeError] = useState<string | null>(null);
  const [requestReasonError, setRequestReasonError] = useState<string | null>(null);
  const [requestedEndDateError, setRequestedEndDateError] = useState<string | null>(null);
  const requestInFlight = useRef(false);
  const requestTypeInput = useRef<HTMLInputElement>(null);
  const requestReasonInput = useRef<HTMLTextAreaElement>(null);
  const requestedEndDateInput = useRef<HTMLInputElement>(null);
  const requestForm = useRef<HTMLFormElement>(null);
  const requestAttempt = useRef<{
    type: MarketplaceCustomerRequestType; payload: MarketplaceCustomerRequestSubmitPayload;
    rawReason: string; rawEndDate: string; key: string;
  } | null>(null);

  useEffect(() => {
    let current = true;
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
        if (!current) return;
        setOrder(dto);
        setCustomerRequests(requests);
        setStatus('success');
      } catch {
        if (current) setStatus('error');
      }
    };
    void run();
    return () => { current = false; };
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
    if (manualInFlight.current || !orderId || status !== 'success'
      || order?.moPaymentProvider !== 'bank_transfer' || order.moCheckoutStatus === 'paid'
      || (order.moManualPaymentStatus && order.moManualPaymentStatus !== 'awaiting_evidence'
        && order.moManualPaymentStatus !== 'rejected')) return;
    const error = manualReferenceValidationError(manualReference);
    setManualReferenceError(error);
    if (error) {
      manualReferenceInput.current?.focus({ preventScroll: true });
      manualReferenceInput.current?.scrollIntoView?.({ block: 'center', behavior: 'instant' });
      return;
    }
    const reference = manualReference.trim();
    const lookupToken = loadMarketplaceLookupToken(orderId);
    if (!lookupToken) {
      setManualError('Falta la credencial segura de seguimiento. Abre el enlace desde este navegador o contacta soporte.');
      return;
    }
    manualInFlight.current = true;
    setManualSubmitting(true);
    setManualError(null);
    try {
      const updated = await Marketplace.submitManualEvidence(
        orderId,
        reference,
        lookupToken,
      );
      if (updated.moOrderId !== order.moOrderId) throw new Error('Mismatched order response');
      setOrder(updated);
      setManualReference('');
    } catch {
      setManualError('No pudimos registrar la referencia. El pedido no fue marcado como pagado.');
    } finally {
      manualInFlight.current = false;
      setManualSubmitting(false);
    }
  };

  const requestAttemptChanged = Boolean(requestAttempt.current && (
    requestAttempt.current.type !== requestType
    || requestAttempt.current.payload.mcrsReason !== requestReason.trim()
    || (requestType === 'rental_extension' && requestAttempt.current.payload.mcrsRequestedEndDate !== requestedEndDate)
  ));
  const restoreRequestAttempt = () => {
    const attempt = requestAttempt.current;
    if (!attempt || requestInFlight.current) return;
    setRequestType(attempt.type); setRequestReason(attempt.rawReason); setRequestedEndDate(attempt.rawEndDate);
    setRequestTypeError(null); setRequestReasonError(null); setRequestedEndDateError(null); setRequestError(null);
    requestReasonInput.current?.focus({ preventScroll: true });
  };
  const submitCustomerRequest = async () => {
    if (requestInFlight.current || !orderId || !order || status !== 'success') return;
    const typeError = !requestType || !availableRequestTypes.includes(requestType) ? 'Elige un tipo de solicitud disponible para esta orden.' : null;
    const reasonError = requestReasonValidationError(requestReason);
    const dateError = requestType === 'rental_extension' ? extensionDateValidationError(requestedEndDate, order.moRentalEndDate) : null;
    setRequestTypeError(typeError); setRequestReasonError(reasonError); setRequestedEndDateError(dateError);
    if (typeError || dateError || reasonError) {
      requestForm.current?.scrollIntoView?.({ block: 'center', behavior: 'instant' });
      if (typeError) requestTypeInput.current?.focus();
      else if (dateError) requestedEndDateInput.current?.focus({ preventScroll: true });
      else requestReasonInput.current?.focus({ preventScroll: true });
      return;
    }
    if (!requestType || requestAttemptChanged) return;
    const lookupToken = loadMarketplaceLookupToken(orderId);
    if (!lookupToken) {
      setRequestError('Falta la credencial segura de seguimiento. Abre el enlace desde este navegador o contacta soporte.');
      return;
    }
    requestInFlight.current = true; setRequestSubmitting(true); setRequestError(null);
    try {
      const attempt = requestAttempt.current ?? {
        type: requestType,
        payload: { mcrsRequestType: requestType, mcrsReason: requestReason.trim(),
          ...(requestType === 'rental_extension' ? { mcrsRequestedEndDate: requestedEndDate } : {}) },
        rawReason: requestReason, rawEndDate: requestedEndDate,
        key: getMarketplaceRequestIdempotencyKey(orderId, requestType),
      };
      requestAttempt.current = attempt;
      const created = await Marketplace.submitCustomerRequest(orderId, attempt.payload, lookupToken, attempt.key);
      if (created.mcrOrderId !== order.moOrderId || created.mcrRequestType !== attempt.type) throw new Error('Mismatched request response');
      clearMarketplaceRequestIdempotencyKey(orderId, attempt.type);
      requestAttempt.current = null;
      setCustomerRequests((current) => [created, ...current.filter((request) => request.mcrRequestId !== created.mcrRequestId)]);
      setRequestType(''); setRequestReason(''); setRequestedEndDate('');
    } catch {
      setRequestError('No pudimos confirmar la solicitud. El pago y la entrega no cambiaron. Reintenta con los mismos datos para recuperar el resultado.');
    } finally {
      requestInFlight.current = false; setRequestSubmitting(false);
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
            <Card variant="outlined" data-testid="marketplace-order-card">
              <CardContent>
                <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between"
                  alignItems={{ xs: 'flex-start', sm: 'center' }} spacing={1}>
                  <Typography variant="h6" fontWeight={700} sx={{ minWidth: 0, overflowWrap: 'anywhere' }}>
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
                  <Typography variant="body2" color="text.secondary" sx={{ overflowWrap: 'anywhere' }}>
                    {order.moOrderKind === 'rental' ? 'Renta' : 'Entrega'}: {marketplaceFulfillmentStatusLabel(order.moFulfillmentStatus, order.moOrderKind)}
                    {order.moTrackingReference ? ` · Guía: ${order.moTrackingReference}` : ''}
                  </Typography>
                )}
                {order.moPaymentProvider === 'bank_transfer' && order.moCheckoutStatus !== 'paid' && (
                  <Box sx={{ mt: 1.5 }}>
                    {(!order.moManualPaymentStatus || order.moManualPaymentStatus === 'awaiting_evidence'
                      || order.moManualPaymentStatus === 'rejected') && (
                      <Alert severity={order.moManualPaymentStatus === 'rejected' ? 'error' : 'warning'} variant="outlined"
                        sx={{ '& .MuiAlert-message': { width: '100%', minWidth: 0, overflowWrap: 'anywhere' } }}>
                        <Box component="form" noValidate data-testid="marketplace-reference-form"
                          onSubmit={(event) => { event.preventDefault(); void submitManualEvidence(); }}>
                          <Stack spacing={1}>
                            <Typography variant="body2">
                              {order.moManualPaymentStatus === 'rejected'
                                ? 'La referencia fue rechazada. El pedido continúa impago; puedes enviar una referencia corregida.'
                                : 'El pedido está creado, pero no está pagado. Ingresa la referencia de tu transferencia para revisión.'}
                            </Typography>
                            <TextField
                              label={order.moManualPaymentStatus === 'rejected' ? 'Referencia corregida' : 'Referencia de transferencia'}
                              inputRef={manualReferenceInput}
                              value={manualReference}
                              onChange={(event) => {
                                if (manualInFlight.current) return;
                                setManualReference(event.target.value);
                                if (manualReferenceError) setManualReferenceError(manualReferenceValidationError(event.target.value));
                              }}
                              onBlur={() => {
                                if (!manualInFlight.current) setManualReferenceError(manualReferenceValidationError(manualReference));
                              }}
                              error={Boolean(manualReferenceError)}
                              helperText={manualReferenceError ?? '3–120 caracteres. Enviar la referencia no confirma el pago.'}
                              disabled={manualSubmitting}
                              required
                              fullWidth
                              size="small"
                            />
                            <Button type="submit" variant="contained" disabled={manualSubmitting}>
                              {manualSubmitting ? 'Enviando…' : order.moManualPaymentStatus === 'rejected'
                                ? 'Reenviar evidencia' : 'Enviar evidencia para revisión'}
                            </Button>
                          </Stack>
                        </Box>
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
                    {order.moManualPaymentStatus === 'requires_reconciliation' && (
                      <Alert severity="error" variant="outlined">
                        La evidencia requiere conciliación de soporte. El pedido no se considera pagado.
                      </Alert>
                    )}
                    {manualError && <Alert severity="error" sx={{ mt: 1 }}>{manualError}</Alert>}
                  </Box>
                )}
                {order.moOrderKind === 'rental' && (
                  <Alert severity="info" variant="outlined" sx={{ mt: 1.5, '& .MuiAlert-message': { minWidth: 0, overflowWrap: 'anywhere' } }}>
                    <Stack spacing={0.5}>
                      <Typography variant="body2">
                        Fechas: {order.moRentalStartDate} → {order.moRentalEndDate} ({order.moRentalDurationDays} día(s))
                      </Typography>
                      <Typography variant="body2">
                        Renta: {formatMinor(order.moRentalChargeUsdCents)} · depósito reembolsable: {formatMinor(order.moSecurityDepositUsdCents)}
                      </Typography>
                      <Typography variant="body2">
                        Estado del depósito: {marketplaceDepositStatusLabel(order.moDepositStatus)}
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
                          <MarkdownText variant="body2">{request.mcrReason}</MarkdownText>
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
                        <Box component="form" ref={requestForm} noValidate data-testid="marketplace-request-form"
                          onSubmit={(event) => { event.preventDefault(); void submitCustomerRequest(); }}>
                          <Stack spacing={1.25}>
                            <Alert severity="info" variant="outlined">
                              Enviar una solicitud no cancela, devuelve ni extiende automáticamente la orden. TDF debe revisarla. Las extensiones requieren disponibilidad y una nueva cotización.
                            </Alert>
                            <FormControl size="small" fullWidth required error={Boolean(requestTypeError)} disabled={requestSubmitting}>
                              <InputLabel id="marketplace-request-type-label">Tipo de solicitud</InputLabel>
                              <Select labelId="marketplace-request-type-label" label="Tipo de solicitud" value={requestType}
                                inputRef={requestTypeInput} aria-describedby="marketplace-request-type-help"
                                sx={{ '& .MuiSelect-select.MuiSelect-select': { whiteSpace: 'normal', overflowWrap: 'anywhere', height: 'auto' } }}
                                MenuProps={{ PaperProps: { sx: { maxWidth: 'calc(100vw - 32px)' } } }}
                                onBlur={() => { if (!requestInFlight.current && !requestType) setRequestTypeError('Elige un tipo de solicitud disponible para esta orden.'); }}
                                onChange={(event) => {
                                  if (requestInFlight.current) return;
                                  setRequestType(event.target.value as MarketplaceCustomerRequestType);
                                  setRequestTypeError(null); setRequestedEndDateError(null);
                                }}>
                                {availableRequestTypes.map((type) => (
                                  <MenuItem key={type} value={type} sx={{ whiteSpace: 'normal' }}>{requestLabels[type]}</MenuItem>
                                ))}
                              </Select>
                              <FormHelperText id="marketplace-request-type-help">{requestTypeError ?? 'Las opciones dependen del estado actual de la orden.'}</FormHelperText>
                            </FormControl>
                            {requestType === 'rental_extension' && (
                              <TextField label="Nueva fecha de devolución" type="date" inputRef={requestedEndDateInput}
                                value={requestedEndDate} disabled={requestSubmitting}
                                onChange={(event) => {
                                  if (requestInFlight.current) return;
                                  setRequestedEndDate(event.target.value);
                                  if (requestedEndDateError) setRequestedEndDateError(extensionDateValidationError(event.target.value, order.moRentalEndDate));
                                }}
                                onBlur={() => { if (!requestInFlight.current) setRequestedEndDateError(extensionDateValidationError(requestedEndDate, order.moRentalEndDate)); }}
                                error={Boolean(requestedEndDateError)}
                                helperText={requestedEndDateError ?? `Debe ser posterior a ${order.moRentalEndDate ?? 'la fecha actual'}. La extensión requiere revisión.`}
                                InputLabelProps={{ shrink: true }}
                                inputProps={{ min: order.moRentalEndDate ? DateTime.fromISO(order.moRentalEndDate, { zone: 'UTC' }).plus({ days: 1 }).toISODate() ?? undefined : undefined }}
                                size="small" fullWidth required />
                            )}
                            <TextField label="Motivo" inputRef={requestReasonInput} value={requestReason}
                              disabled={requestSubmitting} multiline minRows={2} maxRows={6} fullWidth
                              onChange={(event) => {
                                if (requestInFlight.current) return;
                                setRequestReason(event.target.value);
                                if (requestReasonError) setRequestReasonError(requestReasonValidationError(event.target.value));
                              }}
                              onBlur={() => { if (!requestInFlight.current) setRequestReasonError(requestReasonValidationError(requestReason)); }}
                              error={Boolean(requestReasonError)}
                              helperText={requestReasonError ?? '3–1000 caracteres. El estado actual se mantiene hasta revisión.'} required />
                            {requestAttemptChanged && (
                              <Alert severity="warning" sx={{ '& .MuiAlert-message': { minWidth: 0, width: '100%' } }}>
                                <Stack spacing={1}>
                                  <Typography variant="body2">Ya intentaste enviar una solicitud con otros datos. Recupera ese intento antes de enviar cambios.</Typography>
                                  <Button type="button" disabled={requestSubmitting} onClick={restoreRequestAttempt}>Usar datos del intento anterior</Button>
                                </Stack>
                              </Alert>
                            )}
                            <Button type="submit" variant="outlined" disabled={requestSubmitting || requestAttemptChanged}>
                              {requestSubmitting ? 'Enviando…' : 'Enviar solicitud'}
                            </Button>
                            {requestError && <Alert severity="error">{requestError}</Alert>}
                          </Stack>
                        </Box>
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
                      <Stack key={`${fulfillmentStatus}-${timestamp}`} direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ xs: 'flex-start', sm: 'center' }}>
                        <Chip size="small" label={marketplaceFulfillmentStatusLabel(fulfillmentStatus, order.moOrderKind)}
                          sx={{ maxWidth: '100%', height: 'auto', '& .MuiChip-label': { whiteSpace: 'normal', overflowWrap: 'anywhere', py: 0.5 } }} />
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
