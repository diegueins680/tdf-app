import { useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Alert, Box, Button, Checkbox, Chip, CircularProgress, FormControlLabel, MenuItem, Paper, Stack, TextField, Typography } from '@mui/material';
import { useTranslation } from 'react-i18next';
import { Merch, createMerchIdempotencyKey, type MerchIssueTriageRequest } from '../api/merch';
import { formatMerchMoney, merchLanguage, merchStatusLabel } from '../utils/merch';

const optionalTrimmed = (value?: string) => {
  const trimmed = value?.trim();
  if (!trimmed) return null;
  return trimmed;
};

const dateInputValue = (date: Date) => date.toISOString().slice(0, 10);
const dateTimeInputValue = (date: Date) => date.toISOString().slice(0, 16);

function SettlementPaymentEvidenceForm({ settlementId, language, onRecorded }: { settlementId: string; language: 'es' | 'en'; onRecorded: () => void }) {
  const [file, setFile] = useState<File | null>(null);
  const [paidAt, setPaidAt] = useState(() => dateTimeInputValue(new Date()));
  const [externalReference, setExternalReference] = useState('');
  const [evidenceNotes, setEvidenceNotes] = useState('');
  const [idempotencyKey, setIdempotencyKey] = useState(() => createMerchIdempotencyKey('settlement-payment'));
  const record = useMutation({
    mutationFn: () => Merch.recordSettlementPayment(
      settlementId,
      file!,
      new Date(paidAt).toISOString(),
      externalReference.trim(),
      optionalTrimmed(evidenceNotes),
      idempotencyKey,
    ),
    onSuccess: () => {
      setIdempotencyKey(createMerchIdempotencyKey('settlement-payment'));
      setFile(null);
      onRecorded();
    },
  });

  return <Paper component="form" variant="outlined" sx={{ p: 2 }} onSubmit={(event) => { event.preventDefault(); record.mutate(); }}>
    <Stack spacing={1.5}>
      <Alert severity="warning">
        {language === 'en' ? 'Use this only after independently executing and verifying the manual transfer. This form records evidence; it never sends money.' : 'Usa esto solo después de ejecutar y verificar independientemente la transferencia manual. Este formulario registra evidencia; nunca envía dinero.'}
      </Alert>
      <Button component="label" variant="outlined" sx={{ alignSelf: 'flex-start' }}>
        {file?.name ?? (language === 'en' ? 'Choose JPEG/PNG receipt' : 'Elegir comprobante JPEG/PNG')}
        <input hidden type="file" accept="image/jpeg,image/png" onChange={(event) => setFile(event.target.files?.[0] ?? null)} />
      </Button>
      <TextField required type="datetime-local" InputLabelProps={{ shrink: true }} label={language === 'en' ? 'Verified payment time' : 'Hora de pago verificada'} value={paidAt} onChange={(event) => setPaidAt(event.target.value)} />
      <TextField required inputProps={{ minLength: 3, maxLength: 160 }} label={language === 'en' ? 'Bank/accounting reference' : 'Referencia bancaria/contable'} value={externalReference} onChange={(event) => setExternalReference(event.target.value)} helperText={language === 'en' ? 'Do not enter account numbers or secrets.' : 'No ingreses números de cuenta ni secretos.'} />
      <TextField multiline minRows={2} inputProps={{ minLength: 3, maxLength: 2000 }} label={language === 'en' ? 'Reconciliation note (optional)' : 'Nota de conciliación (opcional)'} value={evidenceNotes} onChange={(event) => setEvidenceNotes(event.target.value)} />
      {record.isError && <Alert severity="error">{language === 'en' ? 'Evidence was rejected. Check approval, separation of duties, timestamp, reference, and image.' : 'La evidencia fue rechazada. Comprueba aprobación, separación de funciones, hora, referencia e imagen.'}</Alert>}
      {record.isSuccess && <Alert severity="success">{language === 'en' ? 'Payment evidence recorded. No transfer was initiated by TDF.' : 'Evidencia de pago registrada. TDF no inició ninguna transferencia.'}</Alert>}
      <Button type="submit" variant="contained" disabled={!file || externalReference.trim().length < 3 || !paidAt || record.isPending}>
        {language === 'en' ? 'Record verified payment' : 'Registrar pago verificado'}
      </Button>
    </Stack>
  </Paper>;
}

export default function MerchAdminPage() {
  const { i18n } = useTranslation();
  const language = merchLanguage(i18n.resolvedLanguage);
  const client = useQueryClient();
  const [notes, setNotes] = useState<Record<string, string>>({});
  const [issueResponses, setIssueResponses] = useState<Record<string, string>>({});
  const [issueNotes, setIssueNotes] = useState<Record<string, string>>({});
  const [refundAmounts, setRefundAmounts] = useState<Record<string, string>>({});
  const [refundNotes, setRefundNotes] = useState<Record<string, string>>({});
  const [refundKeys, setRefundKeys] = useState<Record<string, string>>({});
  const [refundReviewNotes, setRefundReviewNotes] = useState<Record<string, string>>({});
  const [settlementStoreId, setSettlementStoreId] = useState('');
  const [settlementOrderIds, setSettlementOrderIds] = useState<string[]>([]);
  const [settlementPeriodStart, setSettlementPeriodStart] = useState(() => dateInputValue(new Date(Date.now() - 30 * 24 * 60 * 60 * 1000)));
  const [settlementPeriodEnd, setSettlementPeriodEnd] = useState(() => dateInputValue(new Date(Date.now() + 24 * 60 * 60 * 1000)));
  const [settlementNotes, setSettlementNotes] = useState('');
  const [settlementReviewNotes, setSettlementReviewNotes] = useState<Record<string, string>>({});
  const capabilities = useQuery({ queryKey: ['merch-capabilities'], queryFn: () => Merch.capabilities(), retry: false });
  const stores = useQuery({ queryKey: ['merch-admin-stores'], queryFn: () => Merch.adminStores(), retry: false });
  const products = useQuery({ queryKey: ['merch-admin-products'], queryFn: () => Merch.adminProducts('pending_review'), retry: false });
  const issues = useQuery({ queryKey: ['merch-admin-issues'], queryFn: () => Merch.adminIssues(), retry: false });
  const refundOperations = capabilities.data?.features.refundOperations === true;
  const disputeMonitoring = capabilities.data?.features.disputeMonitoring === true;
  const refunds = useQuery({
    queryKey: ['merch-admin-refunds'],
    queryFn: () => Merch.adminRefunds(),
    enabled: refundOperations,
    retry: false,
  });
  const disputes = useQuery({
    queryKey: ['merch-admin-disputes'],
    queryFn: () => Merch.adminDisputes(),
    enabled: disputeMonitoring,
    retry: false,
  });
  const settlements = useQuery({ queryKey: ['merch-admin-settlements'], queryFn: () => Merch.adminSettlements(), retry: false });
  const eligibleSettlementOrders = useQuery({
    queryKey: ['merch-admin-settlement-orders', settlementStoreId],
    queryFn: () => Merch.settlementEligibleOrders(settlementStoreId),
    enabled: Boolean(settlementStoreId),
    retry: false,
  });
  const storeReview = useMutation({
    mutationFn: ({ id, decision }: { id: string; decision: 'approve' | 'reject' | 'suspend' | 'reactivate' }) => Merch.reviewStore(id, {
      decision,
      reviewerNotes: notes[id]?.trim() ? notes[id].trim() : (decision === 'approve' ? 'Aprobado para piloto cerrado.' : 'Revisión administrativa.'),
      commissionBps: decision === 'approve' ? 0 : null,
      commissionReason: decision === 'approve' ? 'Banda piloto: comisión temporal de 0%, sujeta a revisión.' : null,
    }),
    onSuccess: () => void client.invalidateQueries({ queryKey: ['merch-admin-stores'] }),
  });
  const productReview = useMutation({
    mutationFn: ({ id, status }: { id: string; status: 'published' | 'rejected' }) => Merch.reviewProduct(id, status, status === 'rejected' ? notes[id] : undefined),
    onSuccess: () => void client.invalidateQueries({ queryKey: ['merch-admin-products'] }),
  });
  const issueReview = useMutation({
    mutationFn: ({ id, status }: { id: string; status: MerchIssueTriageRequest['status'] }) => Merch.updateAdminIssue(id, {
      status,
      publicResponse: optionalTrimmed(issueResponses[id]),
      internalNotes: optionalTrimmed(issueNotes[id]),
    }),
    onSuccess: () => void client.invalidateQueries({ queryKey: ['merch-admin-issues'] }),
  });
  const createRefund = useMutation({
    mutationFn: ({ issueId, orderId, key }: { issueId: string; orderId: string; key: string }) => {
      const rawAmount = refundAmounts[issueId]?.trim();
      return Merch.createAdminRefund(orderId, {
        issueId,
        amountMinor: rawAmount ? Number(rawAmount) : null,
        reasonCode: 'customer_request',
        note: optionalTrimmed(refundNotes[issueId]),
      }, key);
    },
    onSuccess: (_, variables) => {
      setRefundKeys((current) => ({ ...current, [variables.issueId]: createMerchIdempotencyKey('refund') }));
      void client.invalidateQueries({ queryKey: ['merch-admin-refunds'] });
      void client.invalidateQueries({ queryKey: ['merch-admin-issues'] });
      void client.invalidateQueries({ queryKey: ['merch-admin-settlement-orders'] });
    },
  });
  const reviewRefund = useMutation({
    mutationFn: ({ id, decision }: { id: string; decision: 'approve' | 'cancel' }) => Merch.reviewAdminRefund(id, {
      decision,
      reviewNote: refundReviewNotes[id]?.trim() ?? '',
    }),
    onSuccess: () => {
      void client.invalidateQueries({ queryKey: ['merch-admin-refunds'] });
      void client.invalidateQueries({ queryKey: ['merch-admin-issues'] });
    },
  });
  const createSettlement = useMutation({
    mutationFn: () => Merch.createSettlement({
      storeId: settlementStoreId,
      periodStart: settlementPeriodStart,
      periodEnd: settlementPeriodEnd,
      orderIds: settlementOrderIds,
      reviewNotes: optionalTrimmed(settlementNotes),
    }),
    onSuccess: () => {
      setSettlementOrderIds([]);
      setSettlementNotes('');
      void client.invalidateQueries({ queryKey: ['merch-admin-settlements'] });
      void client.invalidateQueries({ queryKey: ['merch-admin-settlement-orders', settlementStoreId] });
    },
  });
  const reviewSettlement = useMutation({
    mutationFn: ({ id, status }: { id: string; status: 'approved' | 'held' }) => Merch.updateSettlementStatus(id, status, optionalTrimmed(settlementReviewNotes[id])),
    onSuccess: () => void client.invalidateQueries({ queryKey: ['merch-admin-settlements'] }),
  });

  if (capabilities.isLoading || stores.isLoading || products.isLoading || issues.isLoading || settlements.isLoading || (refundOperations && refunds.isLoading) || (disputeMonitoring && disputes.isLoading)) {
    return <Box py={8} textAlign="center"><CircularProgress aria-label={language === 'en' ? 'Loading merch review' : 'Cargando revisión de merch'} /></Box>;
  }
  if (capabilities.isError || stores.isError || products.isError || issues.isError || settlements.isError || (refundOperations && refunds.isError) || (disputeMonitoring && disputes.isError)) {
    return <Box py={4}><Alert severity="error">{language === 'en' ? 'The moderation queues could not be loaded, or you lack administrator permission.' : 'No se pudieron cargar las colas de moderación o no tienes permiso administrativo.'}</Alert></Box>;
  }

  return <Box component="main" py={4}>
    <Stack spacing={4}>
      <Box>
        <Typography component="h1" variant="h3" fontWeight={900}>{language === 'en' ? 'Merch pilot review' : 'Revisión del piloto de merch'}</Typography>
        <Typography color="text.secondary">{language === 'en' ? 'Approval never enables payment providers or production flags.' : 'Aprobar no habilita proveedores de pago ni flags de producción.'}</Typography>
      </Box>

      <Paper variant="outlined" sx={{ p: 3, borderRadius: 3 }}>
        <Typography component="h2" variant="h5" fontWeight={800} mb={2}>{language === 'en' ? 'Seller applications' : 'Solicitudes de vendedores'}</Typography>
        <Stack spacing={2}>{stores.data?.map((store) => <Paper key={store.id} variant="outlined" sx={{ p: 2 }}>
          <Stack spacing={1.5}>
            <Stack direction="row" spacing={1} alignItems="center"><Typography fontWeight={800}>{store.displayName}</Typography><Chip size="small" label={merchStatusLabel(store.applicationStatus ?? '', language)} /><Chip size="small" label={merchStatusLabel(store.operationalStatus ?? '', language)} /></Stack>
            <Typography variant="body2">@{store.slug}</Typography>
            <TextField multiline minRows={2} label={language === 'en' ? 'Audit note' : 'Nota de auditoría'} value={notes[store.id] ?? ''} onChange={(event) => setNotes({ ...notes, [store.id]: event.target.value })} />
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
              {store.applicationStatus === 'requested' && <><Button variant="contained" onClick={() => storeReview.mutate({ id: store.id, decision: 'approve' })}>{language === 'en' ? 'Approve pilot at 0%' : 'Aprobar piloto al 0%'}</Button><Button color="error" onClick={() => storeReview.mutate({ id: store.id, decision: 'reject' })}>{language === 'en' ? 'Reject' : 'Rechazar'}</Button></>}
              {store.operationalStatus === 'active' && <Button color="error" onClick={() => storeReview.mutate({ id: store.id, decision: 'suspend' })}>{language === 'en' ? 'Suspend safely' : 'Suspender de forma segura'}</Button>}
              {store.operationalStatus === 'suspended' && <Button onClick={() => storeReview.mutate({ id: store.id, decision: 'reactivate' })}>{language === 'en' ? 'Reactivate' : 'Reactivar'}</Button>}
            </Stack>
          </Stack>
        </Paper>)}{stores.data?.length === 0 && <Typography color="text.secondary">{language === 'en' ? 'No applications.' : 'No hay solicitudes.'}</Typography>}</Stack>
      </Paper>

      <Paper variant="outlined" sx={{ p: 3, borderRadius: 3 }}>
        <Typography component="h2" variant="h5" fontWeight={800} mb={1}>{language === 'en' ? 'Refund authorization' : 'Autorización de reembolsos'}</Typography>
        {!refundOperations ? <Alert severity="info">{language === 'en' ? 'Refund operations are disabled in this environment. Support cases remain available, but no financial state can be prepared here.' : 'Las operaciones de reembolso están deshabilitadas en este entorno. Los casos de soporte siguen disponibles, pero aquí no se puede preparar ningún estado financiero.'}</Alert> : <>
          <Alert severity="warning" sx={{ mb: 2 }}>{language === 'en' ? 'Approval requires a different administrator. “Approved” does not contact the provider; execution stays unavailable until the merch adapter passes sandbox verification.' : 'La aprobación requiere otro administrador. «Aprobado» no contacta al proveedor; la ejecución seguirá bloqueada hasta que el adaptador de merch pase la verificación sandbox.'}</Alert>
          <Stack spacing={2}>{refunds.data?.map((refund) => {
            const reviewNote = refundReviewNotes[refund.id] ?? '';
            return <Paper key={refund.id} variant="outlined" sx={{ p: 2 }}>
              <Stack spacing={1.5}>
                <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" spacing={1}>
                  <Box><Typography fontWeight={800}>{refund.storeName} · {refund.orderNumber}</Typography><Typography variant="body2">{formatMerchMoney(refund.amountMinor, refund.currency)} · {refund.reasonCode}</Typography></Box>
                  <Chip label={merchStatusLabel(refund.status, language)} />
                </Stack>
                <Typography variant="body2" color="text.secondary">{language === 'en' ? 'Requested by' : 'Solicitado por'} {refund.requestedByName}{refund.approvedByName ? ` · ${language === 'en' ? 'Approved by' : 'Aprobado por'} ${refund.approvedByName}` : ''}</Typography>
                {refund.requestNote && <Typography>{refund.requestNote}</Typography>}
                {!refund.executionAvailable && <Alert severity="info">{language === 'en' ? 'Provider execution is unavailable; no money has moved from this workflow.' : 'La ejecución con el proveedor no está disponible; este flujo no ha movido dinero.'}</Alert>}
                {['requested', 'approved', 'failed'].includes(refund.status) && <>
                  <TextField multiline minRows={2} required inputProps={{ minLength: 10, maxLength: 2000 }} label={language === 'en' ? 'Independent review note' : 'Nota de revisión independiente'} value={reviewNote} onChange={(event) => setRefundReviewNotes({ ...refundReviewNotes, [refund.id]: event.target.value })} />
                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                    {refund.status === 'requested' && <Button variant="contained" disabled={reviewNote.trim().length < 10 || reviewRefund.isPending} onClick={() => reviewRefund.mutate({ id: refund.id, decision: 'approve' })}>{language === 'en' ? 'Approve request only' : 'Aprobar solo la solicitud'}</Button>}
                    <Button color="error" disabled={reviewNote.trim().length < 10 || reviewRefund.isPending} onClick={() => reviewRefund.mutate({ id: refund.id, decision: 'cancel' })}>{language === 'en' ? 'Cancel before execution' : 'Cancelar antes de ejecutar'}</Button>
                  </Stack>
                </>}
              </Stack>
            </Paper>;
          })}{refunds.data?.length === 0 && <Typography color="text.secondary">{language === 'en' ? 'No canonical refund requests.' : 'No hay solicitudes canónicas de reembolso.'}</Typography>}</Stack>
          {reviewRefund.isError && <Alert severity="error" sx={{ mt: 2 }}>{language === 'en' ? 'The refund review was rejected. Check separation of duties and current state.' : 'La revisión fue rechazada. Comprueba la separación de funciones y el estado actual.'}</Alert>}
        </>}
      </Paper>

      <Paper variant="outlined" sx={{ p: 3, borderRadius: 3 }}>
        <Typography component="h2" variant="h5" fontWeight={800} mb={1}>{language === 'en' ? 'Provider disputes' : 'Disputas del proveedor'}</Typography>
        {!disputeMonitoring ? <Alert severity="info">{language === 'en' ? 'Dispute monitoring is disabled until verified provider ingestion is operational.' : 'El monitoreo de disputas está deshabilitado hasta que opere la ingestión verificada del proveedor.'}</Alert> : <>
          <Alert severity="warning" sx={{ mb: 2 }}>{language === 'en' ? 'Read-only evidence. This screen cannot create a dispute or decide a chargeback outcome.' : 'Evidencia de solo lectura. Esta pantalla no puede crear una disputa ni decidir el resultado de un contracargo.'}</Alert>
          <Stack spacing={2}>{disputes.data?.map((dispute) => <Paper key={dispute.id} variant="outlined" sx={{ p: 2 }}>
            <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" spacing={1}>
              <Box><Typography fontWeight={800}>{dispute.storeName} · {dispute.orderNumber}</Typography><Typography variant="body2">{dispute.providerDisputeId} · {formatMerchMoney(dispute.amountMinor, dispute.currency)}</Typography></Box>
              <Stack direction="row" spacing={1}><Chip label={merchStatusLabel(dispute.kind, language)} /><Chip label={merchStatusLabel(dispute.status, language)} /></Stack>
            </Stack>
          </Paper>)}{disputes.data?.length === 0 && <Typography color="text.secondary">{language === 'en' ? 'No canonical provider disputes.' : 'No hay disputas canónicas del proveedor.'}</Typography>}</Stack>
        </>}
      </Paper>

      <Paper variant="outlined" sx={{ p: 3, borderRadius: 3 }}>
        <Typography component="h2" variant="h5" fontWeight={800} mb={2}>{language === 'en' ? 'Products awaiting review' : 'Productos pendientes de revisión'}</Typography>
        <Stack spacing={2}>{products.data?.map((product) => <Paper key={product.id} variant="outlined" sx={{ p: 2 }}>
          <Stack spacing={1.5}>
            <Typography fontWeight={800}>{product.name}</Typography>
            <Typography variant="body2">{product.storeName} · {product.category}</Typography>
            <TextField multiline minRows={2} label={language === 'en' ? 'Rejection reason (required to reject)' : 'Motivo (obligatorio para rechazar)'} value={notes[product.id] ?? ''} onChange={(event) => setNotes({ ...notes, [product.id]: event.target.value })} />
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}><Button variant="contained" onClick={() => productReview.mutate({ id: product.id, status: 'published' })}>{language === 'en' ? 'Publish' : 'Publicar'}</Button><Button color="error" disabled={(notes[product.id]?.trim().length ?? 0) < 5} onClick={() => productReview.mutate({ id: product.id, status: 'rejected' })}>{language === 'en' ? 'Reject' : 'Rechazar'}</Button></Stack>
          </Stack>
        </Paper>)}{products.data?.length === 0 && <Typography color="text.secondary">{language === 'en' ? 'No products awaiting review.' : 'No hay productos pendientes.'}</Typography>}</Stack>
      </Paper>

      <Paper variant="outlined" sx={{ p: 3, borderRadius: 3 }}>
        <Typography component="h2" variant="h5" fontWeight={800} mb={1}>{language === 'en' ? 'Support and financial review' : 'Soporte y revisión financiera'}</Typography>
        <Alert severity="warning" sx={{ mb: 2 }}>{language === 'en' ? 'Closing a case records a resolution only. It does not execute or imply a refund, dispute outcome, or seller settlement.' : 'Cerrar un caso solo registra una resolución. No ejecuta ni implica un reembolso, resultado de disputa o liquidación al vendedor.'}</Alert>
        <Stack spacing={2}>{issues.data?.map((item) => {
          const response = issueResponses[item.id] ?? '';
          const terminal = ['resolved', 'rejected', 'cancelled'].includes(item.status);
          const financial = ['cancellation', 'return', 'refund', 'damaged', 'missing', 'fraud'].includes(item.issueType);
          const rawRefundAmount = refundAmounts[item.id]?.trim() ?? '';
          const parsedRefundAmount = rawRefundAmount ? Number(rawRefundAmount) : null;
          const remainingRefund = Math.max(0, (item.totalMinor ?? 0) - (item.refundedMinor ?? 0));
          const validRefundAmount = parsedRefundAmount === null || (Number.isInteger(parsedRefundAmount) && parsedRefundAmount > 0 && parsedRefundAmount <= remainingRefund);
          const linkedRefund = refunds.data?.find((refund) => refund.issueId === item.id && refund.status !== 'cancelled');
          return <Paper key={item.id} variant="outlined" sx={{ p: 2 }}>
            <Stack spacing={1.5}>
              <Box><Typography fontWeight={800}>{item.storeName} · {item.orderNumber}</Typography><Typography variant="body2">{merchStatusLabel(item.issueType, language)} · {merchStatusLabel(item.status, language)}</Typography></Box>
              <Typography>{item.message}</Typography>
              {item.paymentStatus && <Typography variant="body2" color="text.secondary">
                {language === 'en' ? 'Payment' : 'Pago'}: {merchStatusLabel(item.paymentStatus, language)}
                {item.totalMinor != null ? ` · ${formatMerchMoney(item.totalMinor, item.currency ?? 'USD')}` : ''}
                {` · ${language === 'en' ? 'Refund' : 'Reembolso'}: ${merchStatusLabel(item.refundStatus ?? 'none', language)}`}
              </Typography>}
              {item.resolution && <Alert severity="success">{item.resolution}</Alert>}
              {refundOperations && financial && item.status === 'staff_review' && ['paid', 'partially_refunded'].includes(item.paymentStatus ?? '') && <Paper variant="outlined" sx={{ p: 2 }}>
                <Stack spacing={1.5}>
                  <Typography fontWeight={800}>{language === 'en' ? 'Prepare canonical refund' : 'Preparar reembolso canónico'}</Typography>
                  <Alert severity="warning">{language === 'en' ? 'This records a request only. It never contacts the provider or returns money.' : 'Esto registra únicamente una solicitud. Nunca contacta al proveedor ni devuelve dinero.'}</Alert>
                  {linkedRefund ? <Alert severity="info">{language === 'en' ? `Linked refund: ${merchStatusLabel(linkedRefund.status, language)}.` : `Reembolso vinculado: ${merchStatusLabel(linkedRefund.status, language)}.`}</Alert> : <>
                    <TextField
                      type="number"
                      inputProps={{ min: 1, max: remainingRefund, step: 1 }}
                      label={language === 'en' ? 'Amount in cents (blank = full remaining)' : 'Monto en centavos (vacío = saldo completo)'}
                      value={refundAmounts[item.id] ?? ''}
                      onChange={(event) => setRefundAmounts({ ...refundAmounts, [item.id]: event.target.value })}
                      error={!validRefundAmount}
                      helperText={`${language === 'en' ? 'Remaining verified balance' : 'Saldo verificado restante'}: ${formatMerchMoney(remainingRefund, item.currency ?? 'USD')}`}
                    />
                    <TextField multiline minRows={2} inputProps={{ maxLength: 2000 }} label={language === 'en' ? 'Operational note (optional)' : 'Nota operativa (opcional)'} value={refundNotes[item.id] ?? ''} onChange={(event) => setRefundNotes({ ...refundNotes, [item.id]: event.target.value })} />
                    <Button variant="contained" disabled={!validRefundAmount || remainingRefund < 1 || createRefund.isPending} onClick={() => {
                      const key = refundKeys[item.id] ?? createMerchIdempotencyKey('refund');
                      if (!refundKeys[item.id]) setRefundKeys((current) => ({ ...current, [item.id]: key }));
                      createRefund.mutate({ issueId: item.id, orderId: item.orderId, key });
                    }}>{language === 'en' ? 'Record refund request' : 'Registrar solicitud de reembolso'}</Button>
                  </>}
                  {createRefund.isError && <Alert severity="error">{language === 'en' ? 'The refund request was rejected. Recheck payment, case state, amount, and idempotency.' : 'La solicitud fue rechazada. Revisa pago, estado del caso, monto e idempotencia.'}</Alert>}
                </Stack>
              </Paper>}
              {!terminal && <>
                <TextField multiline minRows={2} inputProps={{ maxLength: 5000 }} label={language === 'en' ? 'Public response to buyer' : 'Respuesta pública para el comprador'} value={response} onChange={(event) => setIssueResponses({ ...issueResponses, [item.id]: event.target.value })} />
                <TextField multiline minRows={2} inputProps={{ maxLength: 5000 }} label={language === 'en' ? 'Internal note (never shown to buyer)' : 'Nota interna (nunca visible al comprador)'} value={issueNotes[item.id] ?? ''} onChange={(event) => setIssueNotes({ ...issueNotes, [item.id]: event.target.value })} />
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                  {item.status !== 'staff_review' && <Button onClick={() => issueReview.mutate({ id: item.id, status: 'staff_review' })}>{language === 'en' ? 'Start staff review' : 'Iniciar revisión TDF'}</Button>}
                  <Button disabled={response.trim().length < 1} onClick={() => issueReview.mutate({ id: item.id, status: 'awaiting_buyer' })}>{language === 'en' ? 'Ask buyer' : 'Consultar al comprador'}</Button>
                  <Button disabled={response.trim().length < 10} onClick={() => issueReview.mutate({ id: item.id, status: 'resolved' })}>{language === 'en' ? 'Record resolved' : 'Registrar resolución'}</Button>
                  <Button color="error" disabled={response.trim().length < 10} onClick={() => issueReview.mutate({ id: item.id, status: 'rejected' })}>{language === 'en' ? 'Reject request' : 'Rechazar solicitud'}</Button>
                </Stack>
              </>}
            </Stack>
          </Paper>;
        })}{issues.data?.length === 0 && <Typography color="text.secondary">{language === 'en' ? 'No support cases.' : 'No hay casos de soporte.'}</Typography>}</Stack>
      </Paper>

      <Paper variant="outlined" sx={{ p: 3, borderRadius: 3 }}>
        <Stack spacing={3}>
          <Box>
            <Typography component="h2" variant="h5" fontWeight={800}>{language === 'en' ? 'Manual seller settlements' : 'Liquidaciones manuales a vendedores'}</Typography>
            <Typography color="text.secondary">{language === 'en' ? 'Preparing, approving, and recording evidence are separate. TDF never sends money from this screen.' : 'Preparar, aprobar y registrar evidencia son acciones separadas. TDF nunca envía dinero desde esta pantalla.'}</Typography>
          </Box>

          <Paper component="form" variant="outlined" sx={{ p: 2 }} onSubmit={(event) => { event.preventDefault(); createSettlement.mutate(); }}>
            <Stack spacing={2}>
              <Typography component="h3" variant="h6" fontWeight={800}>{language === 'en' ? '1. Prepare settlement' : '1. Preparar liquidación'}</Typography>
              <Alert severity="info">{language === 'en' ? 'Only paid or partially refunded orders that were delivered or returned are eligible. Buyer personal data is not shown.' : 'Solo son elegibles pedidos pagados o parcialmente reembolsados que fueron entregados o devueltos. No se muestran datos personales del comprador.'}</Alert>
              <TextField select required label={language === 'en' ? 'Pilot store' : 'Tienda piloto'} value={settlementStoreId} onChange={(event) => { setSettlementStoreId(event.target.value); setSettlementOrderIds([]); }}>
                {stores.data?.filter((store) => store.operationalStatus === 'active').map((store) => <MenuItem key={store.id} value={store.id}>{store.displayName} · @{store.slug}</MenuItem>)}
              </TextField>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
                <TextField fullWidth required type="date" InputLabelProps={{ shrink: true }} label={language === 'en' ? 'Period start' : 'Inicio del período'} value={settlementPeriodStart} onChange={(event) => setSettlementPeriodStart(event.target.value)} />
                <TextField fullWidth required type="date" InputLabelProps={{ shrink: true }} label={language === 'en' ? 'Period end' : 'Fin del período'} value={settlementPeriodEnd} onChange={(event) => setSettlementPeriodEnd(event.target.value)} />
              </Stack>
              {eligibleSettlementOrders.isLoading && <CircularProgress size={24} aria-label={language === 'en' ? 'Loading eligible orders' : 'Cargando pedidos elegibles'} />}
              {eligibleSettlementOrders.isError && <Alert severity="error">{language === 'en' ? 'Eligible orders could not be loaded.' : 'No se pudieron cargar los pedidos elegibles.'}</Alert>}
              <Stack spacing={1}>{eligibleSettlementOrders.data?.map((order) => <Paper key={order.id} variant="outlined" sx={{ px: 1.5, py: 0.5 }}>
                <FormControlLabel
                  control={<Checkbox checked={settlementOrderIds.includes(order.id)} onChange={(event) => setSettlementOrderIds(event.target.checked ? [...settlementOrderIds, order.id] : settlementOrderIds.filter((id) => id !== order.id))} />}
                  label={`${order.orderNumber} · ${formatMerchMoney(order.sellerNetMinor - order.refundsMinor + order.adjustmentsMinor, order.currency)} ${language === 'en' ? 'net' : 'neto'} · ${merchStatusLabel(order.fulfillmentStatus, language)}`}
                />
              </Paper>)}{settlementStoreId && eligibleSettlementOrders.data?.length === 0 && <Typography color="text.secondary">{language === 'en' ? 'No eligible orders for this store.' : 'No hay pedidos elegibles para esta tienda.'}</Typography>}</Stack>
              <TextField multiline minRows={2} inputProps={{ maxLength: 2000 }} label={language === 'en' ? 'Preparation note (optional)' : 'Nota de preparación (opcional)'} value={settlementNotes} onChange={(event) => setSettlementNotes(event.target.value)} />
              {createSettlement.isError && <Alert severity="error">{language === 'en' ? 'The settlement could not be prepared. Check dates, eligibility, and concurrent changes.' : 'No se pudo preparar la liquidación. Comprueba fechas, elegibilidad y cambios concurrentes.'}</Alert>}
              {createSettlement.isSuccess && <Alert severity="success">{language === 'en' ? 'Settlement prepared for independent review. No payment was made.' : 'Liquidación preparada para revisión independiente. No se realizó ningún pago.'}</Alert>}
              <Button type="submit" variant="contained" disabled={!settlementStoreId || settlementOrderIds.length === 0 || !settlementPeriodStart || !settlementPeriodEnd || settlementPeriodEnd <= settlementPeriodStart || createSettlement.isPending}>
                {language === 'en' ? `Prepare ${settlementOrderIds.length} order(s)` : `Preparar ${settlementOrderIds.length} pedido(s)`}
              </Button>
            </Stack>
          </Paper>

          <Stack spacing={2}>{settlements.data?.map((settlement) => <Paper key={settlement.id} variant="outlined" sx={{ p: 2 }}>
            <Stack spacing={1.5}>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} justifyContent="space-between" alignItems={{ sm: 'center' }}>
                <Box><Typography fontWeight={800}>{settlement.storeName ?? settlement.storeId}</Typography><Typography variant="body2">{settlement.orderCount} {language === 'en' ? 'orders' : 'pedidos'} · {formatMerchMoney(settlement.sellerNetMinor, settlement.currency)} {language === 'en' ? 'seller net' : 'neto vendedor'}</Typography></Box>
                <Chip label={merchStatusLabel(settlement.status, language)} />
              </Stack>
              <Typography variant="body2" color="text.secondary">
                {language === 'en' ? 'Prepared by' : 'Preparada por'} {settlement.preparedByName ?? `#${settlement.preparedBy}`}
                {settlement.approvedByName ? ` · ${language === 'en' ? 'Approved by' : 'Aprobada por'} ${settlement.approvedByName}` : ''}
              </Typography>
              {(settlement.status === 'under_review' || settlement.status === 'held') && <>
                <Alert severity="warning">{language === 'en' ? 'The preparer cannot approve this settlement. Sign in as the independent reviewer.' : 'Quien preparó la liquidación no puede aprobarla. Ingresa como revisor independiente.'}</Alert>
                <TextField multiline minRows={2} inputProps={{ maxLength: 2000 }} label={language === 'en' ? 'Review note' : 'Nota de revisión'} value={settlementReviewNotes[settlement.id] ?? ''} onChange={(event) => setSettlementReviewNotes({ ...settlementReviewNotes, [settlement.id]: event.target.value })} />
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                  <Button variant="contained" onClick={() => reviewSettlement.mutate({ id: settlement.id, status: 'approved' })}>{settlement.status === 'held' ? (language === 'en' ? 'Resolve hold and approve' : 'Resolver espera y aprobar') : (language === 'en' ? 'Approve for manual payment' : 'Aprobar para pago manual')}</Button>
                  {settlement.status === 'under_review' && <Button color="warning" disabled={(settlementReviewNotes[settlement.id]?.trim().length ?? 0) < 3} onClick={() => reviewSettlement.mutate({ id: settlement.id, status: 'held' })}>{language === 'en' ? 'Place on hold' : 'Poner en espera'}</Button>}
                </Stack>
              </>}
              {settlement.status === 'approved' && <SettlementPaymentEvidenceForm settlementId={settlement.id} language={language} onRecorded={() => void client.invalidateQueries({ queryKey: ['merch-admin-settlements'] })} />}
              {settlement.status === 'paid' && <Alert severity="success">
                {language === 'en' ? 'Manual payment recorded from private immutable evidence.' : 'Pago manual registrado con evidencia privada e inmutable.'}
                {settlement.externalReference ? ` ${language === 'en' ? 'Reference' : 'Referencia'}: ${settlement.externalReference}.` : ''}
                {settlement.evidenceChecksumSha256 ? ` SHA-256: ${settlement.evidenceChecksumSha256.slice(0, 12)}…` : ''}
              </Alert>}
            </Stack>
          </Paper>)}{settlements.data?.length === 0 && <Typography color="text.secondary">{language === 'en' ? 'No settlements yet.' : 'Todavía no hay liquidaciones.'}</Typography>}</Stack>
        </Stack>
      </Paper>

      {(storeReview.isError || productReview.isError || issueReview.isError || reviewSettlement.isError) && <Alert severity="error">{language === 'en' ? 'The review could not be saved. Check state, separation of duties, and permissions.' : 'No se pudo guardar la revisión. Comprueba el estado, la separación de funciones y los permisos.'}</Alert>}
    </Stack>
  </Box>;
}
