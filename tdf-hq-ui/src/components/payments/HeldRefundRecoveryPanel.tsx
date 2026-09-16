import { useEffect, useRef, useState } from 'react';
import { useTranslation } from 'react-i18next';
import { Alert, Button, Card, CardContent, Stack, TextField, Typography } from '@mui/material';
import { ServiceStorefront, type ServiceRefundRecovery } from '../../api/serviceStorefront';
import { formatReconciliationMinor } from './ReconciliationEvidencePanel';

const COPY = {
  es: {
    title: 'Consultar un reembolso retenido', id: 'UUID del reembolso de mezcla/masterización',
    notice: 'Solo para reembolsos PayPal ya identificados. Consultar no envía otro reembolso. Una confirmación exacta puede actualizar la contabilidad local; los demás resultados conservan la retención.',
    inspect: 'Revisar estado local', query: 'Consultar reembolso original', busy: 'Verificando…',
    disabled: 'Consulta no habilitada para este reembolso. Revisa la configuración y la evidencia original; no lo vuelvas a emitir.',
    held: 'El resultado sigue sin confirmar. El monto permanece reservado; no envíes otro reembolso.',
    completed: 'Reembolso confirmado en la contabilidad local.',
    error: 'No se pudo verificar el reembolso. No se muestran datos anteriores. Revisa el UUID y los permisos; ante un límite temporal, espera antes de consultar de nuevo. No lo vuelvas a emitir.',
    invalid: 'Ingresa un UUID de reembolso válido.', sandbox: 'Pruebas', production: 'Producción',
    amount: 'Monto', checked: 'Verificado', awaiting: 'Retenido o pendiente de revisión',
  },
  en: {
    title: 'Check a held refund', id: 'Mixing/mastering refund UUID',
    notice: 'Only for already-identified PayPal refunds. Checking does not send another refund. An exact confirmation may update local accounting; other outcomes retain the hold.',
    inspect: 'Inspect local status', query: 'Check original refund', busy: 'Checking…',
    disabled: 'Querying is not enabled for this refund. Review configuration and original evidence; do not reissue it.',
    held: 'The outcome remains unconfirmed. Funds stay reserved; do not send another refund.',
    completed: 'Refund confirmed in local accounting.',
    error: 'The refund could not be verified. Previous data is hidden. Check the UUID and permissions; after a temporary limit, wait before checking again. Do not reissue it.',
    invalid: 'Enter a valid refund UUID.', sandbox: 'Sandbox', production: 'Production',
    amount: 'Amount', checked: 'Checked', awaiting: 'Held or awaiting review',
  },
} as const;

const isUuid = (value: string) => /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(value);

function validResponse(value: ServiceRefundRecovery, refundId: string): boolean {
  return !!value && value.ssrrRefundId === refundId
    && (value.ssrrEnvironment === 'sandbox' || value.ssrrEnvironment === 'production')
    && value.ssrrCurrency === 'USD' && typeof value.ssrrAmountMinor === 'string'
    && /^[1-9][0-9]{0,18}$/.test(value.ssrrAmountMinor)
    && BigInt(value.ssrrAmountMinor) <= 2n ** 63n - 1n
    && typeof value.ssrrCanQuery === 'boolean'
    && (value.ssrrStatus === 'processing' || value.ssrrStatus === 'succeeded'
      || value.ssrrStatus === 'failed' || value.ssrrStatus === 'approved'
      || value.ssrrStatus === 'requested' || value.ssrrStatus === 'cancelled')
    && (!value.ssrrCanQuery || value.ssrrStatus === 'processing')
    && (value.ssrrOutcome === 'not_queried' || value.ssrrOutcome === 'held'
      || value.ssrrOutcome === 'completed' || value.ssrrOutcome === 'already_completed')
    && ((value.ssrrOutcome !== 'completed' && value.ssrrOutcome !== 'already_completed')
      || value.ssrrStatus === 'succeeded')
    && (value.ssrrOutcome !== 'held' || value.ssrrStatus === 'processing')
    && (value.ssrrCheckedAt === null || (typeof value.ssrrCheckedAt === 'string'
      && !Number.isNaN(new Date(value.ssrrCheckedAt).getTime())));
}

export default function HeldRefundRecoveryPanel() {
  const { i18n } = useTranslation();
  const english = i18n.resolvedLanguage?.startsWith('en') ?? false;
  const copy = COPY[english ? 'en' : 'es'];
  const [input, setInput] = useState('');
  const [record, setRecord] = useState<ServiceRefundRecovery>();
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<'invalid' | 'error'>();
  const inFlight = useRef(false);
  const generation = useRef(0);
  useEffect(() => () => { generation.current += 1; }, []);

  const check = async (remote: boolean) => {
    if (inFlight.current) return;
    const refundId = input.trim().toLowerCase();
    if (!isUuid(refundId)) { setError('invalid'); setRecord(undefined); return; }
    if (remote && (!record?.ssrrCanQuery || record.ssrrRefundId !== refundId)) return;
    const requestGeneration = ++generation.current;
    inFlight.current = true;
    setBusy(true); setError(undefined); setRecord(undefined);
    try {
      const result = await (remote ? ServiceStorefront.reconcileRefund(refundId)
        : ServiceStorefront.readRefundRecovery(refundId));
      if (generation.current !== requestGeneration) return;
      if (!validResponse(result, refundId)) throw new Error('Invalid refund view');
      if (remote && (result.ssrrEnvironment !== record?.ssrrEnvironment
        || result.ssrrAmountMinor !== record.ssrrAmountMinor)) throw new Error('Refund binding changed');
      setRecord(result);
    } catch {
      if (generation.current === requestGeneration) setError('error');
    } finally {
      inFlight.current = false;
      if (generation.current === requestGeneration) setBusy(false);
    }
  };

  return <Card component="section" aria-labelledby="held-refund-recovery-title" aria-busy={busy}>
    <CardContent><Stack spacing={2}>
      <Typography id="held-refund-recovery-title" variant="h6">{copy.title}</Typography>
      <Alert severity="info">{copy.notice}</Alert>
      <TextField label={copy.id} value={input} disabled={busy}
        onChange={(event) => { setInput(event.target.value); setRecord(undefined); setError(undefined); }} />
      <Button disabled={busy} onClick={() => { void check(false); }}>{busy ? copy.busy : copy.inspect}</Button>
      {error && <Alert severity="error">{copy[error]}</Alert>}
      {record && <Stack spacing={1}>
        <Typography>{record.ssrrEnvironment === 'sandbox' ? copy.sandbox : copy.production}</Typography>
        <Typography>{copy.amount}: {formatReconciliationMinor(record.ssrrAmountMinor, 'USD', english)}</Typography>
        <Typography>{record.ssrrStatus === 'succeeded' ? copy.completed : copy.awaiting}</Typography>
        {record.ssrrCheckedAt && <Typography>{copy.checked}: {new Date(record.ssrrCheckedAt).toLocaleString(english ? 'en-US' : 'es-EC')}</Typography>}
        {record.ssrrOutcome === 'held' && <Alert severity="warning">{copy.held}</Alert>}
        {record.ssrrCanQuery
          ? <Button variant="contained" disabled={busy} onClick={() => { void check(true); }}>{copy.query}</Button>
          : record.ssrrStatus !== 'succeeded' && <Alert severity="warning">{copy.disabled}</Alert>}
      </Stack>}
    </Stack></CardContent>
  </Card>;
}
