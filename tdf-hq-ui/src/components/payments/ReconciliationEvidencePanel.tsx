import { useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { useTranslation } from 'react-i18next';
import { Alert, Box, Button, Card, CardContent, Chip, FormControl, InputLabel, NativeSelect, Stack, TextField, Typography } from '@mui/material';
import { CommerceOperations, type CommerceReconciliationEntry, type CommerceReconciliationReport } from '../../api/commerceOperations';

const COPY = {
  es: {
    title: 'Evidencia de conciliación',
    notice: 'Solo lectura. Los montos observados no son un asiento contable ni una liquidación. Resuelto o ignorado no libera un pago retenido. Esta vista no ejecuta cobros, reembolsos ni pagos a vendedores.',
    environment: 'Entorno de conciliación', status: 'Estado de revisión', all: 'Todos', sandbox: 'Pruebas', production: 'Producción',
    open: 'Abierta', assigned: 'Asignada', resolved: 'Resuelta', ignored: 'Ignorada', unknown: 'Sin clasificación verificada',
    checkoutFilter: 'Filtrar por UUID de checkout', apply: 'Aplicar filtro de checkout', invalid: 'Ingresa un UUID de checkout válido o deja el filtro vacío.',
    refresh: 'Actualizar evidencia de conciliación', loading: 'Cargando evidencia de conciliación…',
    error: 'Evidencia no disponible. No se muestran datos anteriores; vuelve a actualizar.',
    missing: 'El esquema de conciliación no está disponible. Esto no confirma que no existan excepciones.',
    empty: 'No hay excepciones para este entorno y filtro.', observed: 'Informe solicitado',
    expected: 'Monto esperado', actual: 'Monto observado', exception: 'Excepción', checkout: 'Checkout vinculado', attempt: 'Intento vinculado',
    unlinked: 'Sin vínculo único verificado. No significa que no hubo un pago.', detected: 'Detectada', resolvedAt: 'Fecha de resolución registrada',
    closed: 'Aprobación observada en checkout cerrado', scheduled: 'Consulta programada requiere revisión', mismatch: 'La consulta no coincide con el vínculo',
    providerUnknown: 'Estado del proveedor no reconocido', previous: 'Página anterior de evidencia', more: 'Página siguiente de evidencia',
    limit: 'Límite de navegación alcanzado. Filtra por checkout o solicita revisión operativa.',
    pagination: 'Las páginas pueden cambiar. Este informe no es una exportación contable inmutable.',
  },
  en: {
    title: 'Reconciliation evidence',
    notice: 'Read only. Observed amounts are not a ledger posting or settlement. Resolved or ignored does not release a held payment. This view does not execute charges, refunds or seller payouts.',
    environment: 'Reconciliation environment', status: 'Review status', all: 'All', sandbox: 'Sandbox', production: 'Production',
    open: 'Open', assigned: 'Assigned', resolved: 'Resolved', ignored: 'Ignored', unknown: 'Unverified classification',
    checkoutFilter: 'Filter by checkout UUID', apply: 'Apply checkout filter', invalid: 'Enter a valid checkout UUID or leave the filter empty.',
    refresh: 'Refresh reconciliation evidence', loading: 'Loading reconciliation evidence…',
    error: 'Evidence unavailable. Previous data is hidden; refresh to try again.',
    missing: 'Reconciliation schema is unavailable. This does not establish that no exceptions exist.',
    empty: 'No exceptions match this environment and filter.', observed: 'Report requested',
    expected: 'Expected amount', actual: 'Observed amount', exception: 'Exception', checkout: 'Linked checkout', attempt: 'Linked attempt',
    unlinked: 'No unique verified link. This does not establish that no payment occurred.', detected: 'Detected', resolvedAt: 'Recorded resolution time',
    closed: 'Approval observed on closed checkout', scheduled: 'Scheduled query requires review', mismatch: 'Query does not match its binding',
    providerUnknown: 'Unrecognized provider state', previous: 'Previous evidence page', more: 'Next evidence page',
    limit: 'Navigation limit reached. Filter by checkout or request operational review.',
    pagination: 'Pages may shift. This report is not an immutable accounting export.',
  },
} as const;

const isUuid = (value: string) => /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(value);

// Never parse minor-unit strings through Number. Only USD's known scale is
// rendered as dollars; other currencies retain explicit integer minor units.
export function formatReconciliationMinor(value: unknown, currency: unknown, english = false): string {
  if (typeof value !== 'string' || !/^-?(0|[1-9][0-9]{0,18})$/.test(value)) return '—';
  const minor = BigInt(value);
  if (minor < -(2n ** 63n) || minor > 2n ** 63n - 1n) return '—';
  if (currency === 'USD') {
    const absolute = minor < 0n ? -minor : minor;
    return `USD ${minor < 0n ? '-' : ''}${absolute / 100n}.${String(absolute % 100n).padStart(2, '0')}`;
  }
  const safeCurrency = typeof currency === 'string' && /^[A-Z]{3}$/.test(currency) ? currency : '—';
  return `${minor} ${english ? 'minor units' : 'unidades menores'} · ${safeCurrency}`;
}

export default function ReconciliationEvidencePanel() {
  const { i18n } = useTranslation();
  const english = i18n.resolvedLanguage?.startsWith('en') ?? false;
  const copy = COPY[english ? 'en' : 'es'];
  const [environment, setEnvironment] = useState<CommerceReconciliationReport['crrEnvironment']>('sandbox');
  const [status, setStatus] = useState<NonNullable<CommerceReconciliationReport['crrStatus']> | ''>('open');
  const [checkoutInput, setCheckoutInput] = useState('');
  const [checkout, setCheckout] = useState<string | undefined>();
  const [inputError, setInputError] = useState(false);
  const [offset, setOffset] = useState(0);
  const query = useQuery({
    queryKey: ['commerce-reconciliation-evidence', environment, status, checkout ?? null, offset],
    queryFn: () => CommerceOperations.listReconciliationExceptions({ environment, status: status || undefined, checkoutId: checkout, offset, limit: 25 }),
    retry: false, staleTime: 0, gcTime: 0, refetchOnMount: 'always',
  });
  const mismatched = query.data && (query.data.crrEnvironment !== environment
    || query.data.crrStatus !== (status || null) || query.data.crrCheckoutId !== (checkout ?? null)
    || query.data.crrOffset !== offset || query.data.crrLimit !== 25
    || typeof query.data.crrSchemaReady !== 'boolean' || !Array.isArray(query.data.crrEntries));
  const report = !inputError && !query.isFetching && !query.isError && !mismatched ? query.data : undefined;
  const date = (value: string | null) => {
    if (!value) return '—';
    const parsed = new Date(value);
    return Number.isNaN(parsed.getTime()) ? '—' : parsed.toLocaleString(english ? 'en-US' : 'es-EC');
  };
  const statusLabel = (value: CommerceReconciliationEntry['creStatus']) => {
    if (value === 'open') return copy.open;
    if (value === 'assigned') return copy.assigned;
    if (value === 'resolved') return copy.resolved;
    if (value === 'ignored') return copy.ignored;
    return copy.unknown;
  };
  const reasonLabel = (value: CommerceReconciliationEntry['creReason']) => {
    if (value === 'closed_checkout_approval') return copy.closed;
    if (value === 'scheduled_query_review') return copy.scheduled;
    if (value === 'binding_mismatch') return copy.mismatch;
    if (value === 'unknown_provider_state') return copy.providerUnknown;
    return copy.unknown;
  };

  return <Stack component="section" aria-labelledby="reconciliation-evidence-title" spacing={2}>
    <Typography id="reconciliation-evidence-title" variant="h6">{copy.title}</Typography>
    <Alert severity="info">{copy.notice}</Alert>
    <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
      <FormControl>
        <InputLabel htmlFor="reconciliation-environment">{copy.environment}</InputLabel>
        <NativeSelect inputProps={{ id: 'reconciliation-environment' }} value={environment} onChange={(event) => {
          setEnvironment(event.target.value === 'production' ? 'production' : 'sandbox'); setOffset(0);
        }}>
          <option value="sandbox">{copy.sandbox}</option><option value="production">{copy.production}</option>
        </NativeSelect>
      </FormControl>
      <FormControl>
        <InputLabel htmlFor="reconciliation-status">{copy.status}</InputLabel>
        <NativeSelect inputProps={{ id: 'reconciliation-status' }} value={status} onChange={(event) => {
          setStatus(event.target.value as typeof status); setOffset(0);
        }}>
          <option value="">{copy.all}</option><option value="open">{copy.open}</option>
          <option value="assigned">{copy.assigned}</option><option value="resolved">{copy.resolved}</option>
          <option value="ignored">{copy.ignored}</option>
        </NativeSelect>
      </FormControl>
      <Button variant="outlined" disabled={query.isFetching} onClick={() => void query.refetch()}>{copy.refresh}</Button>
    </Stack>
    <Stack component="form" direction={{ xs: 'column', sm: 'row' }} spacing={2} onSubmit={(event) => {
      event.preventDefault();
      const normalized = checkoutInput.trim().toLowerCase();
      if (normalized && !isUuid(normalized)) { setInputError(true); return; }
      setInputError(false); setCheckout(normalized || undefined); setOffset(0);
    }}>
      <TextField label={copy.checkoutFilter} value={checkoutInput} error={inputError}
        helperText={inputError ? copy.invalid : undefined} autoComplete="off"
        onChange={(event) => setCheckoutInput(event.target.value)} inputProps={{ maxLength: 64 }} />
      <Button type="submit" variant="outlined">{copy.apply}</Button>
    </Stack>
    {query.isFetching && <Typography role="status">{copy.loading}</Typography>}
    {(query.isError || mismatched) && <Alert severity="error">{copy.error}</Alert>}
    {report && !report.crrSchemaReady && <Alert severity="warning">{copy.missing}</Alert>}
    {report?.crrSchemaReady && <>
      <Typography variant="caption">{copy.observed}: {date(report.crrGeneratedAt)} · {report.crrEnvironment}</Typography>
      {report.crrEntries.length === 0 && <Typography>{copy.empty}</Typography>}
      <Box sx={{ display: 'grid', gap: 1.5, gridTemplateColumns: { xs: '1fr', lg: 'repeat(2, minmax(0, 1fr))' } }}>
        {report.crrEntries.map((entry) => <Card component="article" key={entry.creId} variant="outlined">
          <CardContent sx={{ overflowWrap: 'anywhere' }}>
            <Stack direction="row" spacing={1} alignItems="center">
              <Typography fontWeight={700}>{entry.creProvider}</Typography>
              <Chip size="small" label={statusLabel(entry.creStatus)} />
            </Stack>
            <Typography>{reasonLabel(entry.creReason)}</Typography>
            <Typography variant="body2">{copy.exception}: {entry.creId}</Typography>
            <Typography variant="body2">{copy.expected}: {formatReconciliationMinor(entry.creExpectedMinor, entry.creCurrency, english)}</Typography>
            <Typography variant="body2">{copy.actual}: {formatReconciliationMinor(entry.creActualMinor, entry.creCurrency, english)}</Typography>
            {entry.creCheckoutId && entry.crePaymentAttemptId ? <>
              <Typography variant="body2">{copy.checkout}: {entry.creCheckoutId}</Typography>
              <Typography variant="body2">{copy.attempt}: {entry.crePaymentAttemptId}</Typography>
            </> : <Typography variant="body2">{copy.unlinked}</Typography>}
            <Typography variant="body2">{copy.detected}: {date(entry.creDetectedAt)}</Typography>
            {entry.creResolvedAt && <Typography variant="body2">{copy.resolvedAt}: {date(entry.creResolvedAt)}</Typography>}
          </CardContent>
        </Card>)}
      </Box>
      <Typography variant="caption">{copy.pagination}</Typography>
      <Stack direction="row" spacing={1}>
        <Button disabled={offset === 0} onClick={() => setOffset(Math.max(0, offset - 25))}>{copy.previous}</Button>
        <Button disabled={!report.crrHasMore || offset + 25 > 10000} onClick={() => setOffset(offset + 25)}>{copy.more}</Button>
      </Stack>
      {report.crrHasMore && offset + 25 > 10000 && <Alert severity="warning">{copy.limit}</Alert>}
    </>}
  </Stack>;
}
