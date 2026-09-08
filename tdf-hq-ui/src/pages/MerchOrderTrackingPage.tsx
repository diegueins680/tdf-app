import { useState } from 'react';
import { useMutation, useQuery } from '@tanstack/react-query';
import { Alert, Box, Button, Chip, CircularProgress, Divider, FormControl, InputLabel, MenuItem, Paper, Select, Stack, TextField, Typography } from '@mui/material';
import { useParams } from 'react-router-dom';
import { useTranslation } from 'react-i18next';
import { Merch, createMerchIdempotencyKey, readStoredMerchOrder } from '../api/merch';
import { formatMerchMoney, merchLanguage, merchStatusLabel } from '../utils/merch';

export default function MerchOrderTrackingPage() {
  const { orderId = '' } = useParams();
  const { i18n } = useTranslation();
  const language = merchLanguage(i18n.resolvedLanguage);
  const stored = readStoredMerchOrder(orderId);
  const [issue, setIssue] = useState('');
  const [issueType, setIssueType] = useState('general');
  const [issueKey, setIssueKey] = useState(() => createMerchIdempotencyKey('issue'));
  const order = useQuery({ queryKey: ['merch-order', orderId], queryFn: () => Merch.order(orderId, stored!.token), enabled: Boolean(stored), retry: false, refetchInterval: 30_000 });
  const report = useMutation({ mutationFn: () => Merch.reportIssue(orderId, stored!.token, issueType, issue.trim(), issueKey), onSuccess: () => { setIssue(''); setIssueKey(createMerchIdempotencyKey('issue')); } });

  if (!stored) return <Box py={4}><Alert severity="warning">{language === 'en' ? 'This browser does not have the private tracking capability for this order. Use the original browser or support channel.' : 'Este navegador no tiene la capacidad privada de seguimiento de esta orden. Usa el navegador original o el canal de soporte.'}</Alert></Box>;
  if (order.isLoading) return <Box py={8} textAlign="center"><CircularProgress aria-label="Cargando orden" /></Box>;
  if (order.isError || !order.data) return <Box py={4}><Alert severity="error">{language === 'en' ? 'The order could not be found with this tracking capability.' : 'No se encontró la orden con esta capacidad de seguimiento.'}</Alert></Box>;
  const data = order.data;
  const lines = data.lines as { id?: string; quantity?: number; product?: { name?: string }; variant?: { name?: string } }[];
  const timeline = data.timeline ?? [];
  return (
    <Box component="main" py={{ xs: 2, md: 5 }} maxWidth="md" mx="auto">
      <Stack spacing={3}>
        <Box><Typography component="h1" variant="h3" fontWeight={900}>{language === 'en' ? 'Order tracking' : 'Seguimiento del pedido'}</Typography><Typography>{data.orderNumber}</Typography></Box>
        {data.paymentStatus !== 'paid' && <Alert severity="warning">{language === 'en' ? 'Payment is pending verification. This order is not paid yet.' : 'El pago está pendiente de verificación. Esta orden aún no está pagada.'}</Alert>}
        <Paper variant="outlined" sx={{ p: 3, borderRadius: 3 }}><Stack spacing={2}><Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}><Chip label={`${language === 'en' ? 'Payment' : 'Pago'}: ${merchStatusLabel(data.paymentStatus, language)}`} /><Chip label={`${language === 'en' ? 'Fulfillment' : 'Preparación'}: ${merchStatusLabel(data.fulfillmentStatus, language)}`} /><Chip label={`${language === 'en' ? 'Refund' : 'Reembolso'}: ${merchStatusLabel(data.refundStatus, language)}`} /></Stack><Divider />{lines.map((line, index) => <Stack key={line.id ?? index} direction="row" justifyContent="space-between"><Typography>{line.product?.name} · {line.variant?.name} × {line.quantity}</Typography></Stack>)}<Divider /><Stack direction="row" justifyContent="space-between"><Typography fontWeight={900}>Total</Typography><Typography fontWeight={900}>{formatMerchMoney(data.totalMinor, data.currency)}</Typography></Stack></Stack></Paper>
        <Box component="section" aria-labelledby="order-timeline"><Typography id="order-timeline" component="h2" variant="h5" fontWeight={800}>Timeline</Typography>{timeline.length === 0 ? <Typography color="text.secondary">{language === 'en' ? 'No public updates yet.' : 'Todavía no hay novedades públicas.'}</Typography> : <Stack component="ol" spacing={1} sx={{ pl: 3 }}>{timeline.map((raw, index) => { const event = raw as { eventType?: string; publicNote?: string; createdAt?: string }; return <Box component="li" key={`${event.createdAt}-${index}`}><Typography fontWeight={700}>{merchStatusLabel(event.eventType ?? '', language)}</Typography>{event.publicNote && <Typography>{event.publicNote}</Typography>}</Box>; })}</Stack>}</Box>
        <Paper component="form" variant="outlined" sx={{ p: 3, borderRadius: 3 }} onSubmit={(event) => { event.preventDefault(); report.mutate(); }}><Stack spacing={2}><Typography component="h2" variant="h5" fontWeight={800}>{language === 'en' ? 'Report a problem' : 'Reportar un problema'}</Typography><FormControl fullWidth><InputLabel id="merch-issue-type-label">{language === 'en' ? 'Request type' : 'Tipo de solicitud'}</InputLabel><Select labelId="merch-issue-type-label" label={language === 'en' ? 'Request type' : 'Tipo de solicitud'} value={issueType} onChange={(event) => setIssueType(event.target.value)}><MenuItem value="general">{language === 'en' ? 'General problem' : 'Problema general'}</MenuItem><MenuItem value="cancellation">{language === 'en' ? 'Cancellation' : 'Cancelación'}</MenuItem><MenuItem value="return">{language === 'en' ? 'Return' : 'Devolución'}</MenuItem><MenuItem value="refund">{language === 'en' ? 'Refund' : 'Reembolso'}</MenuItem><MenuItem value="dispute">{language === 'en' ? 'Dispute' : 'Disputa'}</MenuItem></Select></FormControl><TextField required multiline minRows={3} inputProps={{ minLength: 10, maxLength: 5000 }} label={language === 'en' ? 'What happened?' : '¿Qué ocurrió?'} value={issue} onChange={(event) => setIssue(event.target.value)} />{report.isError && <Alert severity="error">{language === 'en' ? 'The report could not be sent. Try again.' : 'No se pudo registrar el reporte. Inténtalo de nuevo.'}</Alert>}{report.isSuccess && <Alert severity="success">{language === 'en' ? 'The issue was recorded.' : 'La incidencia quedó registrada.'}</Alert>}<Button type="submit" variant="outlined" disabled={issue.trim().length < 10 || report.isPending}>{language === 'en' ? 'Submit issue' : 'Enviar incidencia'}</Button></Stack></Paper>
      </Stack>
    </Box>
  );
}
