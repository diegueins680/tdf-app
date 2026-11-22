import { useRef, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Alert, Box, Button, Chip, CircularProgress, Divider, FormControl, InputLabel, MenuItem, Paper, Select, Stack, TextField, Typography } from '@mui/material';
import { useNavigate, useParams } from 'react-router-dom';
import { useTranslation } from 'react-i18next';
import { Merch, createMerchIdempotencyKey, readStoredMerchOrder } from '../api/merch';
import { MerchReputation } from '../api/merchReputation';
import { formatMerchMoney, merchLanguage, merchStatusLabel } from '../utils/merch';
import ConfirmDialog from '../components/ConfirmDialog';
import PaymentAlternativesNotice from '../components/payments/PaymentAlternativesNotice';

// Retain the forms' existing ten-character minimum and API text limits.
function messageError(raw: string, maximum: number, en: boolean): string | null {
  const value = raw.trim(), count = [...value].length;
  if (count < 10 || count > maximum) return en ? `Use between 10 and ${maximum} characters.` : `Usa entre 10 y ${maximum} caracteres.`;
  if (/\p{Cc}/u.test(value)) return en ? 'Use one paragraph without line breaks or unsupported characters.' : 'Usa un párrafo sin saltos de línea ni caracteres no admitidos.';
  return null;
}
interface Cancellation { reason: string; key: string; orderNumber: string }
interface Report { message: string; type: string; key: string }

function OrderTracking({ orderId }: { orderId: string }) {
  const client = useQueryClient();
  const navigate = useNavigate();
  const { i18n } = useTranslation();
  const language = merchLanguage(i18n.resolvedLanguage);
  const stored = readStoredMerchOrder(orderId);
  const en = language === 'en';
  const issueInput = useRef<HTMLTextAreaElement>(null);
  const cancelInput = useRef<HTMLTextAreaElement>(null);
  const [issueTouched, setIssueTouched] = useState(false);
  const [cancelTouched, setCancelTouched] = useState(false);
  const [intent, setIntent] = useState<Cancellation | null>(null);
  const [issue, setIssue] = useState('');
  const [issueType, setIssueType] = useState('general');
  const [issueKey, setIssueKey] = useState(() => createMerchIdempotencyKey('issue'));
  const [cancelReason, setCancelReason] = useState('');
  const [cancelKey, setCancelKey] = useState(() => createMerchIdempotencyKey('cancel-order'));
  const order = useQuery({ queryKey: ['merch-order', orderId], queryFn: () => Merch.order(orderId, stored!.token), enabled: Boolean(stored), retry: false, refetchInterval: 30_000 });
  const report = useMutation({
    mutationFn: (request: Report) => Merch.reportIssue(orderId, stored!.token, request.type, request.message, request.key),
    onSuccess: async () => {
      setIssue(''); setIssueTouched(false); setIssueKey(createMerchIdempotencyKey('issue'));
      await order.refetch();
    },
  });
  const cancel = useMutation({
    mutationFn: (request: Cancellation) => Merch.cancelUnpaidOrder(orderId, stored!.token, request.reason, request.key),
    onSuccess: async (result) => {
      setCancelReason(''); setCancelTouched(false); setCancelKey(createMerchIdempotencyKey('cancel-order')); setIntent(null);
      client.setQueryData(['merch-order', orderId], result);
      await order.refetch();
    },
    onError: () => { setIntent(null); void order.refetch(); },
  });
  const claimReview = useMutation({
    mutationFn: () => MerchReputation.claimBuyer(orderId, stored!.token),
    onSuccess: () => navigate(`/merch/orden/${encodeURIComponent(orderId)}/evaluar`),
  });

  if (!stored) return <Box py={4}><Alert severity="warning">{language === 'en' ? 'Open this order in the browser where you placed it, or contact support.' : 'Abre este pedido en el navegador donde lo hiciste o contacta a soporte.'}</Alert></Box>;
  if (order.isLoading) return <Box py={8} textAlign="center"><CircularProgress aria-label={en ? 'Loading order' : 'Cargando pedido'} /></Box>;
  if (!order.data) return <Box py={4}><Alert severity="error">{en ? 'We could not load this order. Try again from the original browser.' : 'No pudimos cargar este pedido. Reintenta desde el navegador original.'}</Alert><Button onClick={() => void order.refetch()} disabled={order.isFetching}>{en ? 'Try again' : 'Reintentar'}</Button></Box>;
  const data = order.data;
  const lines = data.lines as { id?: string; quantity?: number; product?: { name?: string }; variant?: { name?: string } }[];
  const timeline = data.timeline ?? [];
  const issues = data.issues ?? [];
  const canCancelUnpaid = !cancel.isSuccess && data.commercialStatus === 'created' && data.paymentStatus === 'pending' && data.fulfillmentStatus === 'pending';
  const canReviewExperience = data.commercialStatus === 'cancelled' || data.fulfillmentStatus === 'delivered';
  const busy = report.isPending || cancel.isPending || claimReview.isPending;
  const canWrite = !busy && !order.isFetching && !order.isError;
  const issueError = messageError(issue, 5000, en), cancelError = messageError(cancelReason, 2000, en);
  const focus = (input: HTMLTextAreaElement | null) => { input?.focus(); input?.scrollIntoView?.({ block: 'center' }); };
  const requestCancellation = (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    if (busy || intent) return;
    setCancelTouched(true);
    if (cancelError) { focus(cancelInput.current); return; }
    if (!canWrite || !canCancelUnpaid) return;
    setIntent({ reason: cancelReason.trim(), key: cancelKey, orderNumber: data.orderNumber });
  };
  const reportProblem = (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    if (busy || intent) return;
    setIssueTouched(true);
    if (issueError) { focus(issueInput.current); return; }
    if (!canWrite) return;
    report.mutate({ message: issue.trim(), type: issueType, key: issueKey });
  };
  return (
    <Box component="main" py={{ xs: 2, md: 5 }} maxWidth="md" mx="auto">
      <Stack spacing={3}>
        <Box><Typography component="h1" variant="h3" fontWeight={900}>{language === 'en' ? 'Order tracking' : 'Seguimiento del pedido'}</Typography><Typography>{data.orderNumber}</Typography></Box>
        {data.paymentStatus !== 'paid' && <Alert severity="warning">{language === 'en' ? 'Payment is pending verification. This order is not paid yet.' : 'El pago está pendiente de verificación. Esta orden aún no está pagada.'}</Alert>}
        {data.paymentStatus === 'failed' && <PaymentAlternativesNotice methods={[]} blocked english={language === 'en'} />}
        <Paper variant="outlined" sx={{ p: 3, borderRadius: 3 }}><Stack spacing={2}><Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}><Chip label={`${language === 'en' ? 'Payment' : 'Pago'}: ${merchStatusLabel(data.paymentStatus, language)}`} /><Chip label={`${language === 'en' ? 'Fulfillment' : 'Preparación'}: ${merchStatusLabel(data.fulfillmentStatus, language)}`} /><Chip label={`${language === 'en' ? 'Refund' : 'Reembolso'}: ${merchStatusLabel(data.refundStatus, language)}`} /></Stack><Divider />{lines.map((line, index) => <Stack key={line.id ?? index} direction="row" justifyContent="space-between"><Typography>{line.product?.name} · {line.variant?.name} × {line.quantity}</Typography></Stack>)}<Divider /><Stack direction="row" justifyContent="space-between"><Typography fontWeight={900}>Total</Typography><Typography fontWeight={900}>{formatMerchMoney(data.totalMinor, data.currency)}</Typography></Stack></Stack></Paper>
        {canReviewExperience && <Paper component="section" variant="outlined" sx={{ p: 3, borderRadius: 3 }}><Stack spacing={2}><Typography component="h2" variant="h5" fontWeight={800}>{language === 'en' ? 'Verified purchase review' : 'Evaluación de compra verificada'}</Typography><Typography>{language === 'en' ? 'Link this private order to your signed-in account to review the store and any eligible received products. Your private order access is not shown publicly.' : 'Vincula esta orden privada con tu cuenta iniciada para evaluar la tienda y los productos recibidos que sean elegibles. El acceso privado a tu pedido no se muestra públicamente.'}</Typography>{claimReview.isError && <Alert severity="error">{language === 'en' ? 'Sign in with the buyer account and try again. This order may already belong to another account.' : 'Inicia sesión con la cuenta compradora e inténtalo de nuevo. Esta orden puede estar vinculada a otra cuenta.'}</Alert>}<Button variant="contained" disabled={!canWrite || Boolean(intent)} onClick={() => { if (canWrite && !intent && canReviewExperience) claimReview.mutate(); }}>{claimReview.isPending ? (language === 'en' ? 'Linking…' : 'Vinculando…') : (language === 'en' ? 'Review verified purchase' : 'Evaluar compra verificada')}</Button></Stack></Paper>}
        <Box component="section" aria-labelledby="order-timeline"><Typography id="order-timeline" component="h2" variant="h5" fontWeight={800}>{en ? 'Updates' : 'Novedades'}</Typography>{timeline.length === 0 ? <Typography color="text.secondary">{language === 'en' ? 'No public updates yet.' : 'Todavía no hay novedades públicas.'}</Typography> : <Stack component="ol" spacing={1} sx={{ pl: 3 }}>{timeline.map((raw, index) => { const event = raw as { eventType?: string; publicNote?: string; createdAt?: string }; return <Box component="li" key={`${event.createdAt}-${index}`}><Typography fontWeight={700}>{merchStatusLabel(event.eventType ?? '', language)}</Typography>{event.publicNote && <Typography>{event.publicNote}</Typography>}</Box>; })}</Stack>}</Box>
        {issues.length > 0 && <Box component="section" aria-labelledby="order-issues"><Typography id="order-issues" component="h2" variant="h5" fontWeight={800}>{language === 'en' ? 'Requests and issues' : 'Solicitudes e incidencias'}</Typography><Stack component="ul" spacing={1} sx={{ pl: 3 }}>{issues.map((raw) => { const item = raw as { id: string; issueType: string; status: string; message: string; resolution?: string | null }; return <Box component="li" key={item.id}><Typography fontWeight={700}>{merchStatusLabel(item.issueType, language)} · {merchStatusLabel(item.status, language)}</Typography><Typography>{item.message}</Typography>{item.resolution && <Typography color="text.secondary">{item.resolution}</Typography>}</Box>; })}</Stack></Box>}
        {order.isFetching && <Typography role="status">{en ? 'Updating your order…' : 'Actualizando tu pedido…'}</Typography>}
        {order.isError && <Alert severity="warning" action={<Button onClick={() => void order.refetch()}>{en ? 'Try again' : 'Reintentar'}</Button>}>{en ? 'We could not refresh this order. These are the last loaded values. Refresh before sending a request.' : 'No pudimos actualizar el pedido. Estos son los últimos valores cargados. Actualiza antes de enviar una solicitud.'}</Alert>}
        {cancel.isError && <Alert severity="error">{en ? 'The order could not be cancelled. Payment may have started; report an issue instead. Your reason was kept.' : 'No se pudo cancelar la orden. Es posible que el pago haya comenzado; registra una incidencia. Conservamos tu motivo.'}</Alert>}
        {cancel.isSuccess && <Alert severity="success">{en ? 'The unpaid order was cancelled and its stock was released.' : 'La orden sin pagar fue cancelada y su stock quedó liberado.'}</Alert>}
        {canCancelUnpaid && <Paper component="form" noValidate aria-label={en ? 'Cancel unpaid order' : 'Cancelar orden sin pagar'} variant="outlined" sx={{ p: 3, borderRadius: 3 }} onSubmit={requestCancellation}>
          <Stack spacing={2}>
            <Typography component="h2" variant="h5" fontWeight={800}>{en ? 'Cancel this unpaid order' : 'Cancelar esta orden sin pagar'}</Typography>
            <Alert severity="info">{en ? 'Cancellation is immediate only while payment processing and fulfillment have not started. Reserved stock will be released.' : 'La cancelación es inmediata únicamente antes de iniciar el procesamiento del pago y la preparación. El stock reservado será liberado.'}</Alert>
            <TextField required multiline minRows={2} maxRows={4} inputRef={cancelInput} label={en ? 'Reason for cancellation' : 'Motivo de cancelación'} value={cancelReason}
              disabled={busy || Boolean(intent)} onChange={(event) => setCancelReason(event.target.value)} onBlur={() => setCancelTouched(true)} error={cancelTouched && Boolean(cancelError)}
              helperText={cancelTouched && cancelError ? cancelError : en ? '10–2000 characters. Use one paragraph.' : '10–2000 caracteres. Usa un párrafo.'} />
            <Button type="submit" color="error" variant="outlined" disabled={!canWrite || Boolean(intent)}>{en ? 'Review cancellation' : 'Revisar cancelación'}</Button>
          </Stack>
        </Paper>}
        <Paper component="form" noValidate aria-label={en ? 'Report a problem' : 'Reportar un problema'} variant="outlined" sx={{ p: 3, borderRadius: 3 }} onSubmit={reportProblem}>
          <Stack spacing={2}>
            <Typography component="h2" variant="h5" fontWeight={800}>{en ? 'Report a problem' : 'Reportar un problema'}</Typography>
            <FormControl fullWidth disabled={busy || Boolean(intent)}><InputLabel id="merch-issue-type-label">{en ? 'Request type' : 'Tipo de solicitud'}</InputLabel><Select labelId="merch-issue-type-label" label={en ? 'Request type' : 'Tipo de solicitud'} value={issueType} onChange={(event) => setIssueType(event.target.value)}><MenuItem value="general">{en ? 'General problem' : 'Problema general'}</MenuItem><MenuItem value="cancellation">{en ? 'Cancellation' : 'Cancelación'}</MenuItem><MenuItem value="return">{en ? 'Return' : 'Devolución'}</MenuItem><MenuItem value="refund">{en ? 'Refund' : 'Reembolso'}</MenuItem><MenuItem value="dispute">{en ? 'Dispute' : 'Disputa'}</MenuItem></Select></FormControl>
            <Typography variant="body2" color="text.secondary">{en ? 'Sending a request does not cancel the order, issue a refund or confirm a payment.' : 'Enviar una solicitud no cancela la orden, emite un reembolso ni confirma un pago.'}</Typography>
            <TextField required multiline minRows={3} maxRows={6} inputRef={issueInput} label={en ? 'What happened?' : '¿Qué ocurrió?'} value={issue}
              disabled={busy || Boolean(intent)} onChange={(event) => setIssue(event.target.value)} onBlur={() => setIssueTouched(true)} error={issueTouched && Boolean(issueError)}
              helperText={issueTouched && issueError ? issueError : en ? '10–5000 characters. Use one paragraph.' : '10–5000 caracteres. Usa un párrafo.'} />
            {report.isError && <Alert severity="error">{en ? 'The report could not be sent. Your message was kept. Try the same request again.' : 'No se pudo registrar el reporte. Conservamos tu mensaje. Reintenta la misma solicitud.'}</Alert>}
            {report.isSuccess && <Alert severity="success">{en ? 'The issue was recorded.' : 'La incidencia quedó registrada.'}</Alert>}
            <Button type="submit" variant="outlined" disabled={!canWrite || Boolean(intent)}>{report.isPending ? (en ? 'Sending…' : 'Enviando…') : (en ? 'Submit issue' : 'Enviar incidencia')}</Button>
          </Stack>
        </Paper>
        <ConfirmDialog open={intent !== null} onClose={() => { if (!cancel.isPending) setIntent(null); }}
          title={en ? `Cancel order ${intent?.orderNumber ?? ''}?` : `¿Cancelar el pedido ${intent?.orderNumber ?? ''}?`}
          description={!canCancelUnpaid || order.isError
            ? (en ? 'Check the current order status before cancelling. Keep the order and report a problem if you need help.' : 'Revisa el estado actual antes de cancelar. Conserva el pedido y registra una incidencia si necesitas ayuda.')
            : (en ? `Reason: ${intent?.reason ?? ''}. Reserved stock will be released. This cannot be undone.` : `Motivo: ${intent?.reason ?? ''}. El stock reservado será liberado. No se puede deshacer.`)}
          confirmLabel={en ? 'Cancel unpaid order' : 'Cancelar orden sin pagar'} cancelLabel={en ? 'Keep order' : 'Conservar pedido'} confirmingLabel={en ? 'Cancelling…' : 'Cancelando…'}
          severity="danger" confirming={cancel.isPending} confirmDisabled={!canWrite || !canCancelUnpaid}
          onConfirm={() => { if (intent && canWrite && canCancelUnpaid) cancel.mutate(intent); }} />
      </Stack>
    </Box>
  );
}

// A different order starts with its own reasons and idempotency keys.
export default function MerchOrderTrackingPage() {
  const { orderId = '' } = useParams();
  return <OrderTracking key={orderId} orderId={orderId} />;
}
